%% Compile Elmo JSON AST into Abstract Erlang.
%% http://erlang.org/doc/apps/erts/absform.html

-module(elmer_to_erl).

-include("elmer_json.hrl").

%% TODO: make elm-compiler export line numbers.
-define(ELINE, 0).

-record(elmo,
        { json
        , atts = []
        , defs = []
        , inlines = []
        , exports = []
        }).

-export([from_file/1]).

from_file(JsonFileName) ->
    Json = elmer_json:from_file(JsonFileName),
    Elmo = #elmo{json = Json},
    compile(Elmo).

forms(Compiled = #elmo{}) ->
    forms(module, Compiled) ++
        forms(inlines, Compiled) ++
        forms(exports, Compiled) ++
        forms(defs, Compiled) ++
        forms(eof);

forms(eof) ->
    [{eof, ?ELINE}].

forms(module, #elmo{json = ?JSON_MODNAME(ElmModule, ElmPackage)}) ->
    Module = elmer_util:elm_module_to_atom(ElmModule, ElmPackage),
    [{attribute, ?ELINE, module, Module}];

forms(inlines, #elmo{inlines = []}) -> [];
forms(inlines, #elmo{inlines = Inlines}) ->
    [{attribute, ?ELINE, compile, {inline, Inlines}}];

forms(exports, #elmo{exports = []}) -> [];
forms(exports, #elmo{exports = Exports}) ->
    [{attribute, ?ELINE, export, Exports}];

forms(defs, #elmo{ defs = Defs}) ->
     Defs.

compile(Elmo = #elmo{}) ->
    ?JSON_MODNAME(Name, Package) = Elmo#elmo.json,
    io:format("=====> ~p~n", [{Name, Package}]),
    Compiled = lists:foldl(fun compile/2, Elmo, [programs, exports]),
    forms(Compiled).

compile(exports, Elmo = #elmo{json = ?JSON_EXPORTS(Exports)}) ->
    lists:foldl(fun (?JSON_VALUE(Name), NewElmo = #elmo{exports = OldExports}) ->
                     Export = {elmer_util:btoa(Name), 0},
                        NewElmo#elmo{exports = [Export | OldExports]} ;
                     (_, NewElmo) -> NewElmo
              end, Elmo, Exports);

compile(programs, Elmo = #elmo{json = ?JSON_PROGRAM(Programs)}) ->
    lists:foldl(fun compile/2, Elmo, Programs);

compile(?JSON_DEF_FROM_MOD(Name, Var, Module, Package),
       Elmo = #elmo{defs = Defs, inlines = Inlines }) ->
    Def = to_erl({def, Name, to_erl({call, {Var, Module, Package}, []})}),
    Inline = {elmer_util:btoa(Name), 0},
    Elmo#elmo{ defs = [Def | Defs], inlines = [Inline | Inlines]};

compile(?JSON_DEF(Name, Value),
         Elmo = #elmo{ defs = Defs }) ->
    Def = to_erl({def, Name, to_erl(Value)}),
    Elmo#elmo{ defs = [Def | Defs]};

compile(?JSON_TAILDEF(_,_),
        Elmo = #elmo{ })->
    Elmo.

%%
% to_erl/1 -- Transform Elm JSON-AST program to Abstract Erlang
%%

to_erl(?JSON_BOOLEAN(Value)) when Value == true orelse Value == false ->
    {atom, ?ELINE, Value};

to_erl(?JSON_RECORD(Content)) ->
    {map, ?ELINE, [to_erl({map_field_assoc, C}) || C <- Content]};

to_erl(?JSON_UPDATE(At, Updates)) ->
    {map, ?ELINE, to_erl(At), [to_erl({map_field_exact, U}) || U <- Updates]};

to_erl({X, [Name, Value]}) when X == map_field_assoc orelse X == map_field_exact ->
    {X, ?ELINE, {atom, ?ELINE, elmer_util:btoa(Name)}, to_erl(Value)};


%% to_erl(?JSON_DATA(Name, Fields)) when
%%       Name == <<"_Tuple0">> orelse
%%       Name == <<"_Tuple1">> orelse
%%       Name == <<"_Tuple2">> ->
%%     {tuple, ?ELINE, [to_erl(F) || F <- Fields]};

%% to_erl(?JSON_DATA(Name, [])) ->
%%     {atom, ?ELINE, elmer_util:btoa(Name)};

to_erl(?JSON_DATA(Name, Fields)) ->
    %% {DataName, {DataFields...}}
    {tuple, ?ELINE, [{atom, ?ELINE, elmer_util:btoa(Name)}, {tuple, ?ELINE, [to_erl(F) || F <- Fields]}]};

to_erl(?JSON_DATAACCESS(At, Index)) when is_number(Index) ->
    Idx = {number, ?ELINE, Index + 1},
    {call, ?ELINE, {atom, ?ELINE, element}, [Idx, {call, ?ELINE, {atom, ?ELINE, element}, [2, to_erl(At)]}]};

to_erl(?JSON_ACCESS(At, Slot)) when is_binary(Slot) ->
    {call, ?ELINE, {remote, ?ELINE, {atom, ?ELINE, maps}, {atom, ?ELINE, get}}, [{atom, ?ELINE, elmer_util:btoa(Slot)}, to_erl(At)]};

to_erl(?JSON_LOCALVAR(Name)) ->
    {var, ?ELINE, elmer_util:btoa(Name)};

to_erl(?JSON_LET(Defs, Body)) ->
    [{match, ?ELINE, {var, ?ELINE, elmer_util:btoa(Name)}, to_erl(Val)} || ?JSON_DEF(Name, Val) <- Defs] ++ exps(to_erl(Body));

to_erl(?JSON_IF(Conds, Else)) ->
    CondClauses = [{clause, ?ELINE, [], [to_erl(Cond)], exps(to_erl(Then))} || [Cond, Then] <- Conds],
    ElseClause = {clause, ?ELINE, [], [{atom, 0, else}], exps(to_erl(Else)) },
    {'if', ?ELINE, CondClauses ++ [ElseClause]};

to_erl(?JSON_CASE(_Name, _Decider, _JumpsAry)) ->
    %% Jumps = jumps_proplist(JumpsAry),
    %% Var = {var, ?ELINE, elmer_util:btoa(Name)},
    %% case_erl(Var, Decider, Jumps);
    {atom, ?ELINE, todo_vic};


%% Function definition that just calls binop with params in same order
%% is exactly the same as just a reference to the binop.
to_erl(?JSON_FUN([A, B], ?JSON_BINOP(Fun, ?JSON_LOCALVAR(A), ?JSON_LOCALVAR(B)))) ->
    to_erl(Fun);

to_erl(?JSON_FUN(Args, Content)) ->
    VArgs = [{var, ?ELINE, elmer_util:btoa(A)} || A <- Args],
    {'fun', ?ELINE, {clauses, [{clause, ?ELINE, VArgs, [], exps(to_erl(Content))}]}};

to_erl(?JSON_BINOP(Op, Left, Right)) ->
    {call, ?ELINE, to_erl(Op), [to_erl(Left), to_erl(Right)]};

to_erl(?JSON_REF(Name, ?JSON_MODULE(Module, Package))) ->
    to_erl({call, {Name, Module, Package}, []});

to_erl(?JSON_REF(Name, ?JSON_TOPLEVEL)) ->
    {call, ?ELINE, {atom, ?ELINE, elmer_util:btoa(Name)}, []};

to_erl(?JSON_CALL(Fun, Args)) ->
    {call, ?ELINE, to_erl(Fun), [to_erl(A) || A <- Args]};

to_erl(?JSON_TOPLVAR(Name)) ->
    {call, ?ELINE, {atom, ?ELINE, elmer_util:btoa(Name)}, []};

to_erl(?JSON_MODVAR(Name, Module, Package)) ->
    to_erl({call, {Name, Module, Package}, []});

to_erl(?JSON_RANGE(A, B)) ->
    {tuple, ?ELINE, [{atom, ?ELINE, range}, to_erl(A), to_erl(B)]};

to_erl(?JSON_TUPLE0) ->
    {tuple, ?ELINE, []};

to_erl(?JSON_TUPLE1(A)) ->
    {tuple, ?ELINE, [to_erl(A)]};

to_erl(?JSON_TUPLE2(A, B)) ->
    {tuple, ?ELINE, [to_erl(A), to_erl(B)]};

to_erl(?JSON_LIST(List)) ->
    to_erl({cons, List});

to_erl(?JSON_LIT(Literal)) ->
    to_erl(Literal);

to_erl(?JSON_INLINE(Expr)) ->
    to_erl(Expr);

to_erl(?JSON_STR(Binary)) when is_binary(Binary) ->
    {bin, ?ELINE, [{bin_element, ?ELINE, {string, ?ELINE, binary_to_list(Binary)}, default, default}]};

to_erl(?JSON_INT(Binary)) when is_binary(Binary) ->
    {integer, ?ELINE, binary_to_integer(Binary)};

to_erl(?JSON_INT(Int)) when is_number(Int) ->
    {integer, ?ELINE, Int};

to_erl(?JSON_CHR(<<Chr>>)) ->
    {integer, ?ELINE, Chr};

to_erl(?JSON_CMD(?JSON_MODPKG(Module, Package))) ->
    {tuple, ?ELINE, [{atom, ?ELINE, 'Cmd'}, {atom, ?ELINE, elmer_util:elm_module_to_atom(Module, Package)}]};

to_erl(?JSON_SUB(?JSON_MODPKG(Module, Package))) ->
    {tuple, ?ELINE, [{atom, ?ELINE, 'Sub'}, {atom, ?ELINE, elmer_util:elm_module_to_atom(Module, Package)}]};

to_erl({cons, []}) ->
    {nil, ?ELINE};

to_erl({cons, [X | Rest]}) ->
    {cons, ?ELINE, to_erl(X), to_erl({cons, Rest})};

to_erl({def, Name, Value}) ->
    {function, ?ELINE, elmer_util:btoa(Name), 0, [{clause, ?ELINE, [], [], [Value]}]};

to_erl({call, {Name, Module, Package}, Args}) ->
    NameAtom = {atom, ?ELINE, elmer_util:btoa(Name)},
    ModuleAtom = {atom, ?ELINE, elmer_util:elm_module_to_atom(Module, Package)},
    {call, ?ELINE, {remote, ?ELINE, ModuleAtom, NameAtom}, Args}.

exps(B) when is_list(B) -> B;
exps(E) -> [E].



%%%%%
%% TODO: these were broken case impl

%% jumps_proplist(List) -> jumps_proplist(List, []).
%% jumps_proplist([], Acc) -> Acc;
%% jumps_proplist([[Key, Value] | Rest], Acc) ->
%%     jumps_proplist(Rest, [{Key, Value}] ++ Acc).

%% case_erl(_Var, ?JSON_LEAF_INLINE(Body), _Jumps) ->
%%     to_erl(Body);
%% case_erl(_Var, ?JSON_LEAF_JUMP(Target), Jumps) ->
%%     Jump = proplists:get_value(Target, Jumps),
%%     to_erl(Jump);

%% case_erl(Var, ?JSON_CHAIN(TestChain, Then, Else), Jumps) ->
%%     Cond = case_cond(Var, TestChain),
%%     ThenClause = {clause, ?ELINE, [], [Cond], exps(case_erl(Var, Then, Jumps))},
%%     ElseClause = {clause, ?ELINE, [{atom, ?ELINE, 'else'}], [], exps(case_erl(Var, Else, Jumps))},
%%     {'if', ?ELINE, Var, [ThenClause, ElseClause]}.

%% case_cond(Var, TestChain) ->
%%     case_and([case_test(Var, Test) || Test <- TestChain]).

%% case_and([A | B]) ->
%%     {op, ?ELINE, 'andalso', A, case_and(B)};
%% case_and([A]) -> A.

%% case_test(Var, [At, Cond]) ->
%%     case_pattern(Cond).

%% case_pattern(?JSON_CONSTRUCTOR(?JSON_REF(Name, ?JSON_MODULE(Module, Package)))) ->
%%     22.

