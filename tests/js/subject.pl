subject_class("Todo", c).
constructor(c, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(c, Base) :- triple(Base, "todo://state", _).

property(c, "state").
property_getter(c, Base, "state", Value) :- triple(Base, "todo://state", Value).
property_setter(c, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

property(c, "title").
property_resolve(c, "title").
property_resolve_language(c, "title", "literal").
property_getter(c, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
property_setter(c, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

collection(c, "comments").
collection_getter(c, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
collection_adder(c, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').

collection(c, "entries").
collection_getter(c, Base, "entries", List) :- findall(C, triple(Base, "flux://entry_type", C), List).
collection_adder(c, "entries", '[{action: "addLink", source: "this", predicate: "flux://entry_type", target: "value"}]').

collection(c, "messages").
collection_getter(c, Base, "messages", List) :- findall(C, triple(Base, "flux://entry_type", C), List).
