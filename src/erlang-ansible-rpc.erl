-module('erlang-ansible-rpc').

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    Result = do(),
    io:format("~s~n", [jsx:encode(Result)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
do() ->
    try
        start_network(),
        Result = make_call(),
        success(Result)
    catch
        throw:{no_env, Name} ->
            failure(["Env variable ", Name, " does not exist"]);
        throw:{badrpc, Reason} ->
            failure(["Error during RPC: ", io_lib:format("~p", [Reason])]);
        Error:Reason ->
            failure([io_lib:format("~p ~p", [Error, Reason])])
    end.

start_network() ->
    {ok, _Pid} = net_kernel:start([atom(env("NAME")), atom(env("NAMETYPE"))]),
    true = erlang:set_cookie(node(), atom(env("COOKIE"))).

make_call() ->
    Node = atom(env("NODE")),
    Module = atom(env("MODULE")),
    Function = atom(env("FUNCTION")),
    Args = parse_args(env("ARGS")),
    case rpc:call(Node, Module, Function, Args) of
        {badrpc, Reason} ->
            throw({badrpc, Reason});
        Result ->
            Result
    end.

success(Result) ->
    [{<<"result">>, iolist_to_binary(io_lib:format("~p", [Result]))}].

failure(Msg) ->
    [{<<"failed">>, <<"true">>}, {<<"msg">>, iolist_to_binary(Msg)}].

parse_args(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Args} = erl_parse:parse_term(Tokens),
    Args.

atom(List) when is_list(List) ->
    list_to_atom(List).

env(Name) ->
    case os:getenv(Name) of
        false -> throw({no_env, Name});
        Value -> Value
    end.
