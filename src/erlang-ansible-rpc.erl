-module('erlang-ansible-rpc').

%% API exports
-export([main/1]).

-record(opts, {node, function, args, cookie, module, nametype}).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Result = do(Args),
    io:format("~s~n", [jsx:encode(Result)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
do(Opts) ->
    try
        ModuleOpts = read_opts(Opts),
        case check_opts(ModuleOpts) of
            ok ->
                start_network(ModuleOpts),
                Result = make_call(ModuleOpts),
                success(Result);
            {missing, Missing} ->
                failure([io_lib:format("Missing parameters: ~p", [Missing])])
        end
    catch
        Error:Reason ->
            failure([io_lib:format("~p ~p ~p",
                                   [Error, Reason, erlang:get_stacktrace()])])
    end.

start_network(#opts{nametype=NameType, cookie=Cookie} = Opts) ->
    {ok, _Pid} = net_kernel:start([node_name(Opts), NameType]),
    true = erlang:set_cookie(node(), Cookie).

make_call(#opts{node=Node, module=Module, function=Function, args=Args}) ->
    case rpc:call(Node, Module, Function, Args) of
        {badrpc, Reason} ->
            throw({badrpc, Reason});
        Result ->
            Result
    end.

read_opts([OptsFile]) ->
    {ok, Content} = file:read_file(OptsFile),
    {match, Values} = re:run(Content, <<"([^=]*)=\"([^\"]*)\"\s*">>,
                             [global, {capture, [1,2], binary}]),
    lists:foldl(fun value_to_opt/2, #opts{}, Values).

check_opts(#opts{}=Opts) ->
    [opts|Values] = tuple_to_list(Opts),
    Fields = record_info(fields, opts),
    case lists:filtermap(fun check_opt/1, lists:zip(Fields, Values)) of
        [] -> ok;
        Missing -> {missing, Missing}
    end.

check_opt({Name, undefined}) -> {true, Name};
check_opt(_) -> false.

value_to_opt([<<"node">>, Node], Opts) ->
    Opts#opts{node = atom(Node)};
value_to_opt([<<"function">>, Function], Opts) ->
    Opts#opts{function = atom(Function)};
value_to_opt([<<"args">>, Args], Opts) ->
    Opts#opts{args = parse_args(fix_arguments_opt(Args))};
value_to_opt([<<"cookie">>, Cookie], Opts) ->
    Opts#opts{cookie = atom(Cookie)};
value_to_opt([<<"module">>, Module], Opts) ->
    Opts#opts{module = atom(Module)};
value_to_opt([<<"nametype">>, NameType], Opts) ->
    Opts#opts{nametype = atom(NameType)}.

fix_arguments_opt(Args) ->
    {match, [Match]} = re:run(Args, <<"'(.*)'">>, [{capture, [1], binary}]),
    binary_to_list(Match).

parse_args(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Args} = erl_parse:parse_term(Tokens),
    Args.

success(Result) ->
    [{<<"result">>, iolist_to_binary(io_lib:format("~p", [Result]))}].

failure(Msg) ->
    [{<<"failed">>, <<"true">>}, {<<"msg">>, iolist_to_binary(Msg)}].

node_name(#opts{node = Node}) ->
    list_to_atom("rpc-caller-" ++ atom_to_list(Node)).

atom(List) when is_list(List) ->
    list_to_atom(List);
atom(Binary) when is_binary(Binary) ->
    binary_to_atom(Binary, utf8).
