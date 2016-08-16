-module(elmer_to_erl_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ELINE, 0).
-define(BUILD_DIR, "elm-stuff/build-artifacts/0.17.1/user/project/1.0.0").

reset_forms_line(Forms) ->
    lists:map(fun (Form) ->
                      erl_parse:map_anno(fun (Anno) ->
                                                 erl_anno:set_line(?ELINE, Anno)
                                         end, Form)
              end, Forms).

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

elm_compile(ElmModuleName) ->
    ElmFileName = ElmModuleName ++ ".elm",
    on_cwd("test/files", fun () -> elmer_compiler:compile([ElmFileName], absform, []) end).

elm_compile_module(ElmModuleName) ->
    ElmoFileName = ?BUILD_DIR ++ "/" ++ ElmModuleName ++ ".elmo",
    Compiled = elm_compile(ElmModuleName),
    proplists:get_value(ElmoFileName, Compiled, elm_not_compiled).

absforms_to_bin(AbsForms) ->
    Src = lists:map(fun erl_pp:form/1, AbsForms),
    erlang:iolist_to_binary(Src).

assert_elm_compiles_to_erl(ElmModuleName) ->
    CompiledForms = reset_forms_line(elm_compile_module(ElmModuleName)),
    %% Ignore file attribute from expected erl code. (TODO: fix when we have original elm file name)
    [{attribute, _, file, _} | ExpectedForms ] = reset_forms_line(erl_parse_file(ElmModuleName)),
    ?assertEqual(ExpectedForms, CompiledForms).

does_not_compile_Invalid_test() ->
    ?assertEqual(error, elm_compile("Invalid")).

compiles_StringLit_test() ->
    assert_elm_compiles_to_erl("StringLit").

compiles_CharLit_test() ->
    assert_elm_compiles_to_erl("CharLit").

compiles_IntLit_test() ->
    assert_elm_compiles_to_erl("IntLit").

compiles_FloatLit_test() ->
    assert_elm_compiles_to_erl("FloatLit").

compiles_BoolLit_test() ->
    assert_elm_compiles_to_erl("BoolLit").

compiles_ListLit_test() ->
    assert_elm_compiles_to_erl("ListLit").

compiles_ListRangeLit_test() ->
    assert_elm_compiles_to_erl("ListRangeLit").

compiles_ConsLit_test() ->
    assert_elm_compiles_to_erl("ConsLit").

compiles_TupleLit_test() ->
    assert_elm_compiles_to_erl("TupleLit").

compiles_DefFun_test() ->
    assert_elm_compiles_to_erl("DefFun").

compiles_DefPartialFun_test() ->
    assert_elm_compiles_to_erl("DefPartialFun").

compiles_DefPartialBinopFun_test() ->
    assert_elm_compiles_to_erl("DefPartialBinopFun").

compiles_IfElse_test() ->
    assert_elm_compiles_to_erl("IfElse").

compiles_IfElseNested_test() ->
    assert_elm_compiles_to_erl("IfElseNested").

compiles_ImportExposing_testPending() ->
    assert_elm_compiles_to_erl("ImportExposing").

-endif. %%  TEST

