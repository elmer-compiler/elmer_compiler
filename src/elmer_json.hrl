%% Erlang macros to simplify access to Elmo JSON AST.

-define(JSON_EXPORTS(Exports),
        #{ <<"info">> := #{ <<"exports">> := Exports } }).

-define(JSON_PROGRAM(Program),
        #{ <<"info">> := #{ <<"program">> := Program } }).

-define(JSON_MODPKG(Module, Package),
        #{<<"_module">> := Module,
          <<"_package">> := Package
         }).

-define(JSON_MODNAME(Module, Package),
        #{ <<"name">> := ?JSON_MODPKG(Module, Package) }).

-define(JSON_TYPEINFO(Name, Content),
        #{ <<"info">> := #{ <<"types">> := #{ Name := Content } } }).

-define(JSON_VALUE(Content),
        #{ <<"tag">> := <<"Value">>, <<"contents">> := Content}).

-define(JSON_LAMBDA(Arg, Content),
        #{ <<"tag">> := <<"Lambda">>, 
           <<"contents">> := [ Arg, Content ]}).

-define(JSON_VAR(Content),
        #{ <<"tag">> := <<"Var">>, <<"contents">> := Content}).

-define(JSON_CMD(Content),
        #{ <<"tag">> := <<"Cmd">>, <<"contents">> := Content}).

-define(JSON_SUB(Content),
        #{ <<"tag">> := <<"Sub">>, <<"contents">> := Content}).

-define(JSON_MODULE(Module, Package),
        #{ <<"tag">> := <<"Module">>,
           <<"contents">> := ?JSON_MODPKG(Module, Package) }).

-define(JSON_REF(Name, Home),
        #{ <<"name">> := Name, <<"home">> := Home }).

-define(JSON_TOPLEVEL,
        #{ <<"tag">> := <<"TopLevel">> }).

-define(JSON_LOCAL,
        #{ <<"tag">> := <<"Local">> }).

-define(JSON_EMPTY,
        #{ <<"tag">> := <<"Empty">>, <<"contents">> := [] }).

-define(JSON_POSITION(Pos, Content),
        #{<<"tag">> := <<"Position">>, <<"contents">> := [Position, Content]}).

-define(JSON_LOCALVAR(Name),
        ?JSON_VAR(?JSON_REF(Name, ?JSON_LOCAL))).

-define(JSON_TOPLVAR(Name),
        ?JSON_VAR(?JSON_REF(Name, ?JSON_TOPLEVEL))).

-define(JSON_MODVAR(Name, Module, Package),
        ?JSON_VAR(?JSON_REF(Name, ?JSON_MODULE(Module, Package)))).

-define(JSON_DEF(Name, Content),
        #{ <<"tag">> := <<"Def">>, <<"contents">> := [_, Name, Content]}).

-define(JSON_DEF_FROM_MOD(Name, Var, Module, Package),
        ?JSON_DEF(Name, ?JSON_MODVAR(Var, Module, Package))).

-define(JSON_TAILDEF(Name, Content),
        #{ <<"tag">> := <<"TailDef">>, <<"contents">> := [_, Name, _, Content]}).

-define(JSON_CONSTRUCTOR(Content), 
        #{ <<"tag">> := <<"Constructor">>, <<"contents">> := Content}).

-define(JSON_DATA(Name, Content),
        #{ <<"tag">> := <<"Data">>, <<"contents">> := [Name, Content]}).

-define(JSON_DATAACCESS(At, Item),
        #{ <<"tag">> := <<"DataAccess">>, <<"contents">> := [At, Item]}).

-define(JSON_ACCESS(At, Slot),
        #{ <<"tag">> := <<"Access">>, <<"contents">> := [At, Slot]}).

-define(JSON_LIT(Content),
        #{ <<"tag">> := <<"Literal">>, <<"contents">> := Content}).

-define(JSON_STR(Content),
        #{ <<"tag">> := <<"Str">>, <<"contents">> := Content }).

-define(JSON_INT(Content),
        #{ <<"tag">> := <<"IntNum">>, <<"contents">> := Content }).

-define(JSON_FLT(Content),
        #{ <<"tag">> := <<"FloatNum">>, <<"contents">> := Content }).

-define(JSON_CHR(Content),
        #{ <<"tag">> := <<"Chr">>, <<"contents">> := Content }).

-define(JSON_LIST(Content),
        #{ <<"tag">> := <<"ExplicitList">>, <<"contents">> := Content }).

-define(JSON_FUN(Args, Content),
        #{ <<"tag">> := <<"Function">>, <<"contents">> := [Args, Content]}).

-define(JSON_BINOP(Op, Left, Right),
        #{ <<"tag">> := <<"Binop">>,
           <<"contents">> := [Op, Left, Right]
         }).

-define(JSON_CALL(Fun, Args),
        #{ <<"tag">> := <<"Call">>,
           <<"contents">> := [Fun, Args]}).

%% We can inline anonymous functions that just
%% delegate all of its arguments to a binop
-define(JSON_FUN_INLINE_BINOP(Op),
        ?JSON_FUN([L, R], ?JSON_BINOP(Op, ?JSON_LOCALVAR(L), ?JSON_LOCALVAR(R)))).


-define(JSON_RECORD(Content),
        #{<<"tag">> := <<"Record">>, <<"contents">> := Content}).

-define(JSON_UPDATE(At, Updates),
        #{<<"tag">> := <<"Update">>,
          <<"contents">> := [At, Updates]}).

-define(JSON_RANGE(A, B),
        #{<<"tag">> := <<"Range">>, <<"contents">> := [A, B]}).

-define(JSON_BOOLEAN(Value),
        #{<<"tag">> := <<"Boolean">>, <<"contents">> := Value}).

-define(JSON_IF(Thens, Else),
        #{<<"tag">> := <<"If">>, <<"contents">> := [Thens, Else]}).

-define(JSON_INLINE(Content),
        #{<<"tag">> := <<"Inline">>, <<"contents">> := Content}).

-define(JSON_JUMP(Content),
        #{<<"tag">> := <<"Jump">>, <<"contents">> := Content}).

-define(JSON_LEAF(Content),
        #{<<"tag">> := <<"Leaf">>, <<"contents">> := Content}).

-define(JSON_CHAIN(TestChain, Success, Failure),
        #{<<"tag">> := <<"Chain">>, <<"_success">> := Success, <<"_failure">> := Failure, <<"_testChain">> := TestChain}).

-define(JSON_FANOUT(Tests, Fallback, Path),
        #{<<"tag">> := <<"FanOut">>, <<"_tests">> := Tests, <<"_fallback">> := Fallback, <<"_path">> := Path}).

-define(JSON_CASE(On, Decider, Jumps),
        #{<<"tag">> := <<"Case">>, <<"contents">> := [On, Decider, Jumps]}).

-define(JSON_LET(Defs, Body),
        #{<<"tag">> := <<"Let">>, <<"contents">> := [ Defs, Body ]}).

-define(JSON_UNION_EXPLICITS(UnionName, Explicits),
        #{<<"tag">> := <<"Union">>,
          <<"contents">> := [ UnionName, #{
                                <<"_explicits">> := Explicits
                               }]}).
