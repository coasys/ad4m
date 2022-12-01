subject_class("TODO", c).

instantiate_class(c, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
is_instance(c, X) :- triple(X, "todo://state", _).

instance_property(c, X, "state", State) :- triple(X, "todo://state", State).
instance_property_setter(c, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

instance_property(c, X, "title", T) :- triple(X, "flux://has_title", T).
instance_property_setter(c, "title", '[{action: "setSingleTarget", source: "this", predicate: "flux://has_title", target: "value"}]').

instance_collection(c, X, "comments", Comments) :- findall(C, triple(X, "todo://comment", C), Comments).
instance_collection_add(c, X)
instance_method(c, X, "add_comment", ["comment"], '[{action: "addLink", source: "this", predicate: "todo://comment", target: "comment"}]').
