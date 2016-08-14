-module(elmer_to_erl_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

happy_test() ->
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd("test/files"),
    ElmFiles = ["Constant.elm"],
    Sources = elmer_compiler:compile(ElmFiles,
                                     erlsrc, []),
    CompiledErl = proplists:get_value("elm-stuff/build-artifacts/0.17.1/user/project/1.0.0/Constant.elmo", Sources),
    {ok, ExpectedErl} = file:read_file("Elm.Constant.erl"),
    ok = file:set_cwd(OldDir),
    ?assertEqual(ExpectedErl, CompiledErl).

-endif.

