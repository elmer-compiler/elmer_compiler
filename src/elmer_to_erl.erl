%% Compile Elmo JSON AST into Abstract Erlang.
%% http://erlang.org/doc/apps/erts/absform.html

-module(elmer_to_erl).

-include("elmer_json.hrl").

%% TODO: make elm-compiler export line numbers.
-define(ELINE, 0).

-define(ELMER_PARTIAL(Fun, Arity), 
        {call, ?ELINE, {remote, ?ELINE, {atom, ?ELINE, elmer_runtime},
                        {atom, ?ELINE, partial}},
         [Fun, {integer, ?ELINE, Arity}]}).

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

elm_export(?JSON_VALUE(Name), NewElmo = #elmo{exports = OldExports}) ->
    Export = {elmer_util:btoa(Name), 0},
    NewElmo#elmo{exports = [Export | OldExports]};

elm_export(?JSON_UNION_EXPLICITS(_UnionName, Explicits), NewElmo = #elmo{exports = OldExports}) ->
    Constructors = [{elmer_util:btoa(Name), 0} || Name <- Explicits],
    NewElmo#elmo{exports = Constructors ++ OldExports};

elm_export(_, Elmo) -> %% TODO: missing exports
    Elmo.

compile(Elmo = #elmo{}) ->
    ?JSON_MODNAME(Name, Package) = Elmo#elmo.json,
    io:format("=====> ~p~n", [{Name, Package}]),
    Compiled = lists:foldl(fun compile/2, Elmo, [programs, exports]),
    forms(Compiled).

compile(exports, Elmo = #elmo{json = ?JSON_EXPORTS(Exports)}) ->
    lists:foldl(fun elm_export/2, Elmo, Exports);

compile(programs, Elmo = #elmo{json = ?JSON_PROGRAM(Programs)}) ->
    lists:foldl(fun compile/2, Elmo, Programs);

compile(?JSON_DEF_FROM_MOD(Name, Var, Module, Package),
       Elmo = #elmo{defs = Defs, inlines = Inlines }) ->
    Def = to_erl({def, Name, to_erl({call, {Var, Module, Package}, []})}),
    Inline = {elmer_util:btoa(Name), 0},
    Elmo#elmo{ defs = [Def | Defs], inlines = [Inline | Inlines]};

compile(?JSON_DEF(Name, Value), Elmo = #elmo{ defs = Defs }) ->
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

to_erl(?JSON_DATA(Name, Fields)) when
      Name == <<"_Tuple0">> orelse
      Name == <<"_Tuple1">> orelse
      Name == <<"_Tuple2">> orelse
      Name == <<"_Tuple3">> orelse
      Name == <<"_Tuple4">> orelse
      Name == <<"_Tuple5">> orelse
      Name == <<"_Tuple6">> orelse
      Name == <<"_Tuple7">> orelse
      Name == <<"_Tuple8">> orelse
      Name == <<"_Tuple9">> ->
    {tuple, ?ELINE, [to_erl(F) || F <- Fields]};

to_erl(?JSON_DATA(Name, Fields)) ->
    {tuple, ?ELINE, [{atom, ?ELINE, elmer_util:btoa(Name)}, {tuple, ?ELINE, [to_erl(F) || F <- Fields]}]};

to_erl(?JSON_DATAACCESS(At, Index)) when is_number(Index) ->
    Idx = {number, ?ELINE, Index + 1},
    {call, ?ELINE, {atom, ?ELINE, element}, [Idx, {call, ?ELINE, {atom, ?ELINE, element}, [2, to_erl(At)]}]};

to_erl(?JSON_ACCESS(At, Slot)) when is_binary(Slot) ->
    {call, ?ELINE, {remote, ?ELINE, {atom, ?ELINE, maps}, {atom, ?ELINE, get}}, [{atom, ?ELINE, elmer_util:btoa(Slot)}, to_erl(At)]};

to_erl(?JSON_LOCALVAR(Name)) ->
    {var, ?ELINE, elmer_util:var(Name)};

to_erl(?JSON_LET(Defs, Body)) ->
    [{match, ?ELINE, {var, ?ELINE, elmer_util:var(Name)}, to_erl(Val)} || ?JSON_DEF(Name, Val) <- Defs] ++ exps(to_erl(Body));

to_erl(?JSON_IF(Conds, Else)) ->
    to_erl({ifelse, Conds, Else});

to_erl(?JSON_CASE(_Name, _Decider, _JumpsAry)) ->
    %% Jumps = jumps_proplist(JumpsAry),
    %% Var = {var, ?ELINE, elmer_util:btoa(Name)},
    %% case_erl(Var, Decider, Jumps);
    {atom, ?ELINE, todo_vic};

%% Optimize anonymous functions that just call a binop with
%% same exact arguments.
to_erl(?JSON_FUN_INLINE_BINOP(Op)) ->
    to_erl(Op);

to_erl(?JSON_FUN(Args, Content)) ->
    VArgs = [{var, ?ELINE, elmer_util:var(A)} || A <- Args],
    Fun = {'fun', ?ELINE, {clauses, [{clause, ?ELINE, VArgs, [], exps(to_erl(Content))}]}},
    ?ELMER_PARTIAL(Fun, length(Args));

to_erl(?JSON_BINOP(Op, Left, Right)) ->
    {call, ?ELINE, to_erl(Op), [to_erl({cons, [Left, Right]})]};

to_erl(?JSON_REF(Name, ?JSON_MODULE(Module, Package))) ->
    to_erl({call, {Name, Module, Package}, []});

to_erl(?JSON_REF(Name, ?JSON_TOPLEVEL)) ->
    {call, ?ELINE, {atom, ?ELINE, elmer_util:btoa(Name)}, []};

to_erl(?JSON_CALL(Fun, Args)) ->
    {call, ?ELINE, to_erl(Fun), [to_erl({cons, Args})]};

to_erl(?JSON_TOPLVAR(Name)) ->
    {call, ?ELINE, {atom, ?ELINE, elmer_util:btoa(Name)}, []};

to_erl(?JSON_MODVAR(Name, Module, Package)) ->
    to_erl({call, {Name, Module, Package}, []});

to_erl(?JSON_RANGE(A, B)) ->
    Range = {tuple, ?ELINE, [{atom, ?ELINE, range}, to_erl(A), to_erl(B)]},
    {call, ?ELINE, {remote, ?ELINE, {atom, ?ELINE, elmer_runtime},
                    {atom, ?ELINE, list}}, [Range]};

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

to_erl(?JSON_FLT(Flt)) when is_number(Flt) ->
    {float, ?ELINE, Flt};

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
    {call, ?ELINE, {remote, ?ELINE, ModuleAtom, NameAtom}, Args};

%% single if-else
to_erl({ifelse, [[Cond, Then]], Else}) ->
    to_erl({ifelse, Cond, Then, Else});

%% nested if-else
to_erl({ifelse, [[CondA, ThenA] | Conds], Else}) ->
    to_erl({ifelse, CondA, ThenA, {ifelse, Conds, Else}});

to_erl({ifelse, Cond, Then, Else}) ->
    ThenClause = {clause, ?ELINE, [{atom, 0, true}], [], exps(to_erl(Then))},
    ElseClause = {clause, ?ELINE, [{var, 0, '_else'}], [], exps(to_erl(Else)) },
    {'case', ?ELINE, to_erl(Cond), [ThenClause, ElseClause]}.

exps(B) when is_list(B) -> B;
exps(E) -> [E].

