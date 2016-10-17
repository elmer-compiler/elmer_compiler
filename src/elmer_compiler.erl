-module(elmer_compiler).

%% API exports
-export([compile/3]).

%%====================================================================
%% API functions
%%====================================================================
compile(Files, Output, Options) ->
    elm_files(Files, Output, Options).


%%====================================================================
%% Internal functions
%%====================================================================

-define(ELM_MAKE_BIN, "elm-make").
-define(ELM_MAKE_NULL_OUTPUT, ["--output", "/dev/null"]).
-define(ELM_MAKE_OPTIONS, ["--yes"]).


elm_files(ElmFiles, Output, Options) ->
    Cmd = elm_make_cmd(ElmFiles, Options),
    %% io:format("~s~n", [Cmd]),
    Pid = erlang:open_port({spawn, Cmd}, [stderr_to_stdout, exit_status, stream, {line, 255}]),
    case wait_for_elm_make(Pid) of
      error -> error;
      ok -> compile_elmo_files(Output)
    end.

elm_make_cmd(ElmFiles, Options) ->
    ElmMake = proplists:get_value(elm_make, Options, ?ELM_MAKE_BIN),
    ElmOptions = proplists:get_value(elm_make_options, Options, ?ELM_MAKE_OPTIONS),
    Cmd = [ElmMake] ++ ?ELM_MAKE_NULL_OUTPUT ++ ElmOptions ++ ElmFiles,
    string:join(Cmd, " ").

wait_for_elm_make(Pid) ->
    receive
        {Pid, {data, {eol, Line}}} -> io:format("~s~n", [Line]),
                                      wait_for_elm_make(Pid);
        {Pid, {exit_status, 0}} -> ok;
        {Pid, {exit_status, _}} -> error
    end.

compile_elmo_files(Output) ->
    ElmoFiles = filelib:wildcard("elm-stuff/**/*.elmo"),
    lists:map(fun (ElmoFile) ->
               compile_elmo_file(ElmoFile, Output)
              end, ElmoFiles).

compile_elmo_file(ElmoFile, Output) ->
    Erl = elmer_to_erl:from_file(ElmoFile),
    dump(Output, ElmoFile, Erl).

dump(absform, ElmoFile, Erl) ->
    {ElmoFile, Erl};

dump(erlsrc, ElmoFile, Erl) ->
    Src = lists:map(fun erl_pp:form/1, Erl),
    Bin = erlang:iolist_to_binary(Src),
    {ElmoFile, Bin};

dump({beam, OutputDir}, ElmoFile, Erl) ->
    io:format("~s~n", [ElmoFile]),
    %% io:format("~p~n", [Erl]),
    {ok, Module, Bin} = compile:forms(Erl, [report_errors, report_warnings]),
    %% OutputFilename = filename:basename(ElmoFile, ".elmo") ++ ".beam",
    OutputFilename = filename:basename(ElmoFile, ".elmo") ++ ".beam",
    OutputFilepath = filename:join(filename:absname(OutputDir), OutputFilename),
    io:format("~s~n", [OutputFilepath]),
    io:format("~s~n", [Module]),
    ok = file:write_file(OutputFilepath, io_lib:fwrite("~p.\n", [Bin])),
    {ElmoFile, Bin};

dump(stdout, ElmoFile, Erl) ->
    io:format("-----~n|~s~n-----~n~s~n", [
                                          ElmoFile,
                                          lists:concat(lists:map(fun erl_pp:form/1, Erl))]).

