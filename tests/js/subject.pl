subject_class("TODO", c).
constructor(c, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(c, X) :- triple(X, "todo://state", _).

property(c, "state").
property_getter(c, X, "state", State) :- triple(X, "todo://state", State).
property_setter(c, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

property(c, "title").
property_getter(c, X, "title", T) :- triple(X, "flux://has_title", T).
property_setter(c, "title", '[{action: "setSingleTarget", source: "this", predicate: "flux://has_title", target: "value"}]').

collection(c, "comments").
collection_getter(c, X, "comments", Comments) :- findall(C, triple(X, "todo://comment", C), Comments).
collection_adder(c, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
