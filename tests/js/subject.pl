use_module(library(http/json)).
use_module(library(http/json_convert)).
use_module(library(http/http_json)).


subject_class("Todo", c).
constructor(c, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(c, Base) :- triple(Base, "todo://state", _).

property(c, "state").
property_through(c, "state", "todo://state").

property_getter(Class, Base, Property, Value) :- 
    property_through(Class, Property, Predicate),
    triple(Base, Predicate, Value).

property_setter(Class, Property, Actions) :-
    property_through(Class, Property, Predicate),
    atom_json_term(Actions, json([action="setSingleTarget", source="this", predicate=Predicate, target="value"]), Options).

collection_getter(Class, Base, Property, List) :- 
    property_through(Class, Property, Predicate),
    findall(C, triple(Base, Predicate, C), List).

collection_adder(Class, Property, json([action="addLink", source="this", predicate=Predicate, target="value"])) :-
    property_through(Class, Property, Predicate).


%property_getter(c, Base, "state", Value) :- triple(Base, "todo://state", Value).
%property_setter(c, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

property(c, "title").
property_resolve(c, "title").
property_resolve_language(c, "title", "literal").
property_through(c, "title", "todo://has_title").

%property_getter(c, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
%property_setter(c, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

collection(c, "comments").
collection_through(c, "comments", "todo://comment").

%collection_getter(c, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
%collection_adder(c, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
