-include("elmer_json.hrl").
-module(elmer_json).

-export([ from_file/1
        , program/1
        , arity/2
        , arg_types/2
        ]).

from_file(JsonFileName) ->
    {ok, Bin} = file:read_file(JsonFileName),
    Json = jsx:decode(Bin, [return_maps]),
    Json.

program(?JSON_PROGRAM(Program)) ->
     Program.

arity(Name, Json) ->
    ?JSON_TYPEINFO(Name, Content) = Json,
    length(arg_types_acc([], Content)).

arg_types(Name, Json) ->
    ?JSON_TYPEINFO(Name, Content) = Json,
    lists:reverse(arg_types_acc([], Content)).

arg_types_acc(L, ?JSON_LAMBDA(A, X)) ->
    arg_types_acc([A | L], X);
arg_types_acc(L, _) ->
    L.
