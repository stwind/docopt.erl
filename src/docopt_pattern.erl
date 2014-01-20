-module(docopt_pattern).

-export([parse/1]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-record(option, {
          short :: integer() | undefined,
          long = <<>> :: binary(),
          val :: binary() | undefined,
          desc = <<>> :: binary()
         }).

-record(option_shortcut, { }).

-record(argument, {
          name = <<>> :: binary()
         }).

-record(command, {
          name = <<>> :: binary()
         }).

-type pattern() :: #option{}.

-record(seq, {
          patterns = [] :: list(pattern())
         }).

-record(one_or_more, {
          patterns = [] :: list(pattern())
         }).

-record(required, {
          patterns = [] :: list(pattern())
         }).

-record(optional, {
          patterns = [] :: list(pattern())
         }).

-record(either, {
          patterns = [] :: list(pattern())
         }).

-define(seq(Patterns), #seq{patterns = Patterns}).
-define(one_or_more(Patterns), #one_or_more{patterns = Patterns}).
-define(required(Patterns), #required{patterns = Patterns}).
-define(optional(Patterns), #optional{patterns = Patterns}).
-define(either(A, B), #either{patterns = [A, B]}).

-record(spec, {
          options = [] :: list(#option{}),
          patterns = [] :: list(pattern())
         }).

%% ===================================================================
%% Public
%% ===================================================================

parse(Doc) ->
    #spec{
       options = parse_options(Doc),
       patterns = parse_expr(tokenize_usage(Doc))
      }.

%% ===================================================================
%% Public
%% ===================================================================

section_usage(Spec) ->
    case sections("usage", Spec) of
        [] ->
            throw(no_usage);
        [<<"usage:",Usage/binary>>] ->
            Usage;
        _ ->
            throw(more_than_one_usage)
    end.

sections(Name, Spec) ->
    Re = "^([^\n]*" ++ Name ++ ":[^\n]*\n?(?:[ \t].*?(?:\n|$))*)",
    ReOpts = [global,{capture,all_but_first,binary},caseless,dotall,multiline],
    case re:run(Spec,Re,ReOpts) of
        {match, Sections} ->
            [S || [S] <- Sections];
        nomatch ->
            []
    end.

parse_options(Doc) ->
    lists:foldl(fun parse_section_option/2, [], sections("options", Doc)).

parse_section_option(<<"options:",Text/binary>>, Acc) ->
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
        _ -> throw(duplicate_option_def)
    end,
    case getopt({long, L}, Acc) of
        undefined -> ok;
        #option{short = S, long = L} -> ok;
        _ -> throw(duplicate_option_def)
    end,
    [Option | Acc].

parse_option_str(Str) ->
    {OptDef, OptDesc} = case binary:split(Str, <<"  ">>) of
                           [ODef] -> {ODef, <<>>};
                           [ODef, ODesc] -> {ODef, ODesc}
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
            Opt#option{val = Val};
        nomatch ->
            Opt
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

parse_expr(Tokens) ->
    {Patterns, _Rest} = parse_expr(Tokens, []),
    Patterns.

parse_expr([], Acc) ->
    {lists:reverse(Acc), []};
parse_expr([<<"|">> | Tokens], [Last | Acc]) ->
    {Seq, Rest} = parse_seq(Tokens, []),
    parse_expr(Rest, [?either(Last, Seq) | Acc]);
parse_expr(Tokens, Acc) ->
    {Seq, Rest} = parse_seq(Tokens, []),
    parse_expr(Rest, [Seq | Acc]).

parse_seq([], Acc) ->
    {?seq(lists:reverse(Acc)), []};
parse_seq([End | Rest], Acc) when End == <<")">>; End == <<"]">>; End == <<"|">> ->
    {?seq(lists:reverse(Acc)), Rest};
parse_seq(Tokens, Acc) ->
    case parse_atom(Tokens) of
        {Atom, [<<"...">> | Rest]} ->
            parse_seq(Rest, [?one_or_more(Atom) | Acc]);
        {Atom, Rest} ->
            parse_seq(Rest, [Atom | Acc])
    end.

parse_atom([Start | Tokens]) when Start == <<"(">>;Start == <<"[">> ->
    {Patterns, Rest} = parse_expr(Tokens, []),
    {wrap(end_of(Start), Patterns), Rest};
%parse_atom([<<"--",Long>> | Rest]) ->
    %parse_long(Long, Rest);
%parse_atom([<<"-",Short>> | Rest]) ->
    %parse_short(Short, Rest);
parse_atom([<<"options">> | Rest]) ->
    {#option_shortcut{}, Rest};
parse_atom([<<"<",Arg/binary>> | Rest]) ->
    {#argument{name = arg_name(Arg, <<>>)}, Rest};
parse_atom([Cmd | Rest]) ->
    {#command{name = Cmd}, Rest}.

end_of(<<"(">>) -> <<")">>;
end_of(<<"[">>) -> <<"]">>.

%ensure_start_with(Token, [Token | _]) -> ok;
%ensure_start_with(Token, _) -> throw({unmatch, Token}).

wrap(<<")">>, Patterns) -> ?required(Patterns);
wrap(<<"]">>, Patterns) -> ?optional(Patterns).

arg_name(<<>>, _) -> throw(invalid_arg_def);
arg_name(<<">">>, Acc) -> Acc;
arg_name(<<C, Rest/binary>>, Acc) ->
    arg_name(Rest, <<Acc/binary, C>>).

-ifdef(TEST).  

parse_test() ->
    Shit = parse("usage:\n"
                 " fuck you mother fucker\n"
                 " fuck you <diao>"),
    ?debugFmt("~p", [Shit]).

-endif.
