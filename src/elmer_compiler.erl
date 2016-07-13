-module(elmer_compiler).

%% API exports
-export([compile/3]).

%%====================================================================
%% API functions
%%====================================================================
compile(Files, Output, Options) ->
    elm_compile(Files, Output, Options).


%%====================================================================
%% Internal functions
%%====================================================================

-define(ELM_MAKE_BIN, "elm-make").
-define(ELM_MAKE_NULL_OUTPUT, ["--output", "/dev/null"]).
-define(ELM_MAKE_OPTIONS, ["--yes"]).


elm_compile(ElmFiles, {beam, OutDir}, Options) ->
    Cmd = elm_make_cmd(ElmFiles, Options),
    Pid = erlang:open_port({spawn, Cmd}, [stderr_to_stdout, in, exit_status, stream, {line, 255}]),
    wait_for_elm_make(Pid).

elm_make_cmd(ElmFiles, Options) ->
    ElmMake = proplists:get_value(elm_make, Options, ?ELM_MAKE_BIN),
    ElmOptions = proplists:get_value(elm_make_options, Options, ?ELM_MAKE_OPTIONS),
    string:join([ElmMake] ++ ?ELM_MAKE_NULL_OUTPUT ++ ElmOptions ++ ElmFiles, " ").

wait_for_elm_make(Pid) ->
    receive
        {Pid, {data, {eol, Line}}} -> io:format("~s~n", [Line]),
                                      wait_for_elm_make(Pid);
        {Pid, {exit_status, 0}} -> ok
    end.


