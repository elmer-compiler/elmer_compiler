-module(run_elmer_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ELINE, 0).
-define(USER_BUILD_DIR, "elm-stuff/build-artifacts/0.17.1/user/project/1.0.0").
-define(CORE_BUILD_DIR, "elm-stuff/build-artifacts/0.17.1/elm-lang/core/4.0.5").

erl_parse_file(ElmoModuleName) ->
    ErlFile = "test/files/Elm." ++ ElmoModuleName ++ ".erl",
    {ok, Forms} = epp:parse_file(ErlFile, []),
    Forms.

on_cwd(Cwd, Fun) ->
    {ok, OldDir} = file:get_cwd(),
    try file:set_cwd(Cwd) of
        ok -> Fun()
    after
        ok = file:set_cwd(OldDir)
    end.

elm_compile(ElmModuleName, Format) ->
    ElmFileName = ElmModuleName ++ ".elm",
    on_cwd("test/files", fun () -> elmer_compiler:compile([ElmFileName], Format, []) end).


user_build_filepath(ElmModuleName) ->
    ?USER_BUILD_DIR ++ "/" ++ ElmModuleName ++ ".elmo".

core_build_filepath(ElmModuleName) ->
    ?CORE_BUILD_DIR ++ "/" ++ ElmModuleName ++ ".elmo".

elm_compile_module(ElmModuleName, Format) ->
    Compiled = elm_compile(ElmModuleName, Format),
    ElmoFileName = user_build_filepath(ElmModuleName),
    proplists:get_value(ElmoFileName, Compiled, elm_not_compiled).

elm_load_module(ElmModuleName) ->
    {ok, Module, CompiledBinary} = elm_compile_module(ElmModuleName, binary),
    {module, Module} = code:load_binary(Module, ElmModuleName, CompiledBinary),
    Module.

elm_load_module(ElmModuleName, CoreModuleName) ->
    Compiled = elm_compile(ElmModuleName, binary),
    UserElmoFileName = user_build_filepath(ElmModuleName),
    {ok, Module, CompiledBinary} = proplists:get_value(UserElmoFileName, Compiled, elm_not_compiled),

    %% TODO Resolve many core module dependencies
    CoreElmoFileName = core_build_filepath(CoreModuleName),
    {ok, CoreModule, CoreCompiledBinary} = proplists:get_value(CoreElmoFileName, Compiled, elm_not_compiled),
    {module, CoreModule} = code:load_binary(CoreModule, CoreModuleName, CoreCompiledBinary),
    Module.

runs_RunExample_test() ->
    elm_load_module("RunExample", "Basics"),
    %% TODO Figure out where to put Native modules
    Result = ('Elm.RunExample':greet())([<<"doodie">>]),
    ?assertEqual(<<"Howdy, doodie">>, Result).

-endif. %%  TEST

