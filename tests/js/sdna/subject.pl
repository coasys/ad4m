subject_class("Todo", c).
constructor(c, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(c, Base) :- triple(Base, "todo://state", _).

destructor(c, '[{action: "removeLink", source: "this", predicate: "todo://state", target: "*"}]').

property(c, "state").
property_getter(c, Base, "state", Value) :- triple(Base, "todo://state", Value).
property_setter(c, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

property(c, "title").
property_resolve(c, "title").
property_resolve_language(c, "title", "literal").
property_getter(c, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
property_setter(c, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

property(c, "isLiked").
property_getter(c, Base, "isLiked", Value) :- triple(Base, "flux://has_reaction", "flux://thumbsup"), Value = true.

collection(c, "comments").
collection_getter(c, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
collection_adder(c, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
collection_remover(c, "comments", '[{action: "removeLink", source: "this", predicate: "todo://comment", target: "value"}]').
collection_setter(c, "comments", '[{action: "collectionSetter", source: "this", predicate: "todo://comment", target: "value"}]').

collection(c, "entries").
collection_getter(c, Base, "entries", List) :- findall(C, triple(Base, "flux://entry_type", C), List).
collection_adder(c, "entries", '[{action: "addLink", source: "this", predicate: "flux://entry_type", target: "value"}]').
collection_remover(c, "entries", '[{action: "removeLink", source: "this", predicate: "flux://entry_type", target: "value"}]').
collection_setter(c, "entries", '[{action: "collectionSetter", source: "this", predicate: "flux://entry_type", target: "value"}]').

collection(c, "messages").
collection_getter(c, Base, "messages", List) :- setof(Target, (triple(Base, "flux://entry_type", Target), instance(OtherClass, Target), subject_class("Message", OtherClass)), List).
collection_adder(c, "messages", '[{action: "addLink", source: "this", predicate: "flux://entry_type", target: "value"}]').
collection_remover(c, "messages", '[{action: "removeLink", source: "this", predicate: "flux://entry_type", target: "value"}]').
collection_setter(c, "messages", '[{action: "collectionSetter", source: "this", predicate: "flux://entry_type", target: "value"}]').

collection(c, "likedMessages").
collection_getter(c, Base, "likedMessages", List) :- setof(Target, (triple(Base, "flux://entry_type", Target), triple(Target, "flux://has_reaction", "flux://thumbsup")), List).
collection_adder(c, "likedMessages", '[{action: "addLink", source: "this", predicate: "flux://entry_type", target: "value"}]').
collection_remover(c, "likedMessages", '[{action: "removeLink", source: "this", predicate: "flux://entry_type", target: "value"}]').
collection_setter(c, "likedMessages", '[{action: "collectionSetter", source: "this", predicate: "flux://entry_type", target: "value"}]').
