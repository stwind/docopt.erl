-module(docopt_spec).

-export([parse/1]).
-export([opt/2]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-record(option, {
          short :: integer() | undefined,
          long = <<>> :: binary(),
          val :: binary() | undefined,
          desc = <<>> :: binary(),
          has_arg = false
         }).

-record(option_shortcut, { }).

-record(argument, { name = <<>> :: binary() }).
-record(command, { name = <<>> :: binary() }).

-type pattern() :: #option{}.

-record(one_or_more, { patterns = [] :: list(pattern()) }).
-record(required, { patterns = [] :: list(pattern()) }).
-record(optional, { patterns = [] :: list(pattern()) }).
-record(either, { patterns = [] :: list(pattern()) }).
-define(one_or_more(Patterns), #one_or_more{patterns = Patterns}).
-define(required(Patterns), #required{patterns = Patterns}).
-define(optional(Patterns), #optional{patterns = Patterns}).
-define(either(Patterns), #either{patterns = Patterns}).

-record(spec, {
          options = [] :: list(#option{}),
          patterns = [] :: list(pattern())
         }).

%% ===================================================================
%% Public
%% ===================================================================

parse(Doc) ->
    Options = parse_options(Doc),
    #spec{
       options = Options,
       patterns = parse_expr(tokenize_usage(Doc), Options)
      }.

opt(Key, #spec{options = Options}) ->
    getopt(Key, Options).

%% ===================================================================
%% Public
%% ===================================================================

section_usage(Spec) ->
    case sections("usage", Spec) of
        [] ->
            throw(no_usage);
        [Usage] ->
            Usage;
        _ ->
            throw(more_than_one_usage)
    end.

sections(Name, Spec) ->
    Re = "^([^\n]*" ++ Name ++ ":[^\n]*\n?(?:[ \t].*?(?:\n|$))*)",
    NameLen = length(Name),
    ReOpts = [global,{capture,all_but_first,binary},caseless,dotall,multiline],
    case re:run(Spec,Re,ReOpts) of
        {match, Sections} ->
            [S || [<<_:NameLen/binary,":",S/binary>>] <- Sections];
        nomatch ->
            []
    end.

parse_options(Doc) ->
    lists:foldl(fun parse_section_option/2, [], sections("options", Doc)).

parse_section_option(Text, Acc) ->
    [_|Parts] = re:split(<<"\n",Text/binary>>, "\n[ \t]*(-[^ ]+?)"),
    lists:foldl(fun add_option/2, Acc, merge_parts(Parts, [])).

merge_parts([], Acc) ->
    Acc;
merge_parts([A, B | Rest], Acc) ->
    merge_parts(Rest, [<<A/binary,B/binary>> | Acc]).

getopt({long, Long}, Options) ->
    getopt1(Long, #option.long, Options);
getopt({short, Short}, Options) ->
    getopt1(Short, #option.short, Options).

getopt1(undefined, _, _) ->
    undefined;
getopt1(Key, N, Options) ->
    case lists:keyfind(Key, N, Options) of
        #option{} = Option -> Option;
        false -> undefined
    end.

add_option(Str, Acc) ->
    #option{short = S, long = L} = Option = parse_option_str(Str),
    case getopt({short, S}, Acc) of
        undefined -> ok;
        #option{short = S, long = L} -> ok;
        _ -> throw({duplicate_option_def, S})
    end,
    case getopt({long, L}, Acc) of
        undefined -> ok;
        #option{short = S, long = L} -> ok;
        _ -> throw({duplicate_option_def, L})
    end,
    [Option | Acc].

parse_option_str(Str) ->
    {OptDef, OptDesc} = case notempty(re:split(Str, <<"  ">>)) of
                           [ODef] -> {ODef, <<>>};
                           [ODef, ODesc] -> {ODef, trim(ODesc)}
                       end,
    parse_option_def(OptDef, #option{desc = OptDesc}).

parse_option_def(OptDef0, Opt) ->
    OptDef = re:replace(OptDef0,"[,=]"," ",[{return,binary},global]),
    Tokens = notempty(binary:split(OptDef, <<" ">>, [global])),
    case Tokens of
        [<<"--",Long/binary>>] ->
            Opt#option{long = Long};
        [<<"--",Long/binary>>, _] ->
            maybe_option_default_value(Opt#option{long = Long});
        [<<"-",Short/integer>>] ->
            Opt#option{short = Short};
        [<<"-",Short/integer>>,<<"--",Long/binary>>] ->
            Opt#option{short = Short, long = Long};
        [<<"-",Short/integer>>, _] ->
            maybe_option_default_value(Opt#option{short = Short});
        [<<"-",Short/integer>>, Arg, <<"--",Long/binary>>, Arg] ->
            maybe_option_default_value(Opt#option{short = Short, long = Long});
        _ ->
            throw(invalid_option_def)
    end.

maybe_option_default_value(#option{desc = Desc} = Opt) ->
    ReOpt = [global, {capture,all_but_first,binary}],
    case re:run(Desc,"\\[default: (.*)\\]",ReOpt) of
        {match, [[Val] | _]} ->
            Opt#option{val = Val, has_arg = true};
        nomatch ->
            Opt#option{has_arg = true}
    end.

formal_usage(Usage) ->
    Splitted = notempty(re:split(Usage,"[\n\s]",[{return,list}])),
    case Splitted of
        [Cmd | Rest] ->
            Rest1 = [case R of Cmd -> ") | ("; _ -> R end || R <- Rest],
            "( " ++ string:join(Rest1, " ") ++ " )";
        _ ->
            throw(invalid_usage_def)
    end.

tokenize_usage(Doc) ->
    Usage = section_usage(Doc),
    {Re, ReOpt} = {"([\\[\\]\\(\\)\\|]|\\.\\.\\.)", [global,{return,binary}]},
    Usage1 = re:replace(formal_usage(Usage), Re, " \\1 ",ReOpt),
    notempty(re:split(Usage1, "\\s+|(\\S*<.*?>)")).

notempty(Vals) ->
    [V || V <- Vals, V /= <<>>, V /= []].

parse_expr(Tokens, Options) ->
    {Patterns, _Rest} = parse_expr1(Tokens, Options),
    ?required(Patterns).

parse_expr1(Tokens, Options) ->
    case parse_seq(Tokens, Options, []) of
        {Seq, [<<"|">> | _] = Rest} ->
            parse_expr2(Rest, Options, maybe_required(Seq));
        {Seq, Rest} ->
            {Seq, Rest}
    end.

parse_expr2([<<"|">> | Tokens], Options, Acc) ->
    {Seq, Rest} = parse_seq(Tokens, Options, []),
    parse_expr2(Rest, Options, maybe_required(Seq) ++ Acc);
parse_expr2(Tokens, _, Acc) ->
    {[?either(lists:reverse(Acc))], Tokens}.

parse_seq([], _, Acc) ->
    {lists:reverse(Acc), []};
parse_seq([End | _] = Rest, _, Acc) when End == <<")">>; End == <<"]">>; End == <<"|">> ->
    {lists:reverse(Acc), Rest};
parse_seq(Tokens, Options, Acc) ->
    case parse_atom(Tokens, Options) of
        {Atoms, [<<"...">> | Rest]} ->
            parse_seq(Rest, Options, [?one_or_more(Atoms)] ++ Acc);
        {Atoms, Rest} ->
            parse_seq(Rest, Options, Atoms ++ Acc)
    end.

parse_atom([Start | Tokens], Options) when Start == <<"(">>;Start == <<"[">> ->
    {Patterns, [_|Rest]} = parse_expr1(Tokens, Options),
    {[wrap(end_of(Start), Patterns)], Rest};
parse_atom([<<"--",Long/binary>> | Tokens], Options) ->
    parse_long(Long, Tokens, Options);
parse_atom([<<"-",Short/binary>> | Rest], Options) ->
    {parse_short(Short, Options, []), Rest};
parse_atom([<<"options">> | Rest], _) ->
    {[#option_shortcut{}], Rest};
parse_atom([<<"<",Arg/binary>> | Rest], _) ->
    {[#argument{name = arg_name(Arg, <<>>)}], Rest};
parse_atom([Cmd | Rest], _) ->
    {[#command{name = Cmd}], Rest}.

parse_long(Long, Tokens, Options) ->
    case binary:split(Long, <<"=">>, [global]) of
        [Name, _] ->
            case getopt({long, Name}, Options) of
                undefined ->
                    {[#option{long = Name, has_arg = true}], Tokens};
                #option{has_arg = true} = Option ->
                    {[Option], Tokens};
                #option{has_arg = false} ->
                    throw({conflict_option_def, Name})
            end;
        [Name] ->
            case getopt({long, Name}, Options) of
                undefined ->
                    {[#option{long = Name}], Tokens};
                #option{has_arg = false} = Option ->
                    {[Option], Tokens};
                #option{has_arg = true} = Option ->
                    {[Option], tl(Tokens)}
            end
    end.

parse_short(<<>>, _, Acc) ->
    Acc;
parse_short(<<Name:8/integer,Left/binary>>, Options, Acc) ->
    case getopt({short, Name}, Options) of
        undefined ->
            parse_short(Left, Options, [#option{short = Name} | Acc]);
        #option{has_arg = false} = Option ->
            parse_short(Left, Options, [Option | Acc]);
        #option{has_arg = true} = Option ->
            [Option | Acc]
    end.

maybe_required([_] = Seq) -> Seq;
maybe_required([_ | _] = Seq) -> [?required(Seq)].

end_of(<<"(">>) -> <<")">>;
end_of(<<"[">>) -> <<"]">>.

trim(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
trim(Str) when is_list(Str) ->
    string:strip(Str).

wrap(<<")">>, Patterns) -> ?required(Patterns);
wrap(<<"]">>, Patterns) -> ?optional(Patterns).

arg_name(<<>>, _) -> throw(invalid_arg_def);
arg_name(<<">">>, Acc) -> Acc;
arg_name(<<C, Rest/binary>>, Acc) ->
    arg_name(Rest, <<Acc/binary, C>>).

%% ===================================================================
%% Eunit
%% ===================================================================

-ifdef(TEST).  
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Doc = "Usage:\n"
    "  naval_fate ship new <name>...\n"
    "  naval_fate ship <name> move <x> <y> [--speed=<kn>]\n"
    "  naval_fate ship shoot <x> <y> [--ammu A]\n"
    "  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]\n"
    "  naval_fate test -foBAR\n"
    "  naval_fate -h | --help\n"
    "  naval_fate --version\n"
    "\n"
    "Options:\n"
    "  -h --help        Show this screen.\n"
    "  -o BAR           test option.\n"
    "  --version        Show version.\n"
    "  --speed=<kn>     Speed in knots [default: 10].\n"
    "  --ammu=<type>    Ammu to use [default: a].\n"
    "  --moored         Moored (anchored) mine.\n"
    "  --drifting       Drifting mine.\n",
    ?debugFmt("~p",[parse(Doc)]).

-endif.
