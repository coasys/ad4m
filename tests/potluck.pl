
message(X) :- triple("ad4m://self", _, X).

is_kind_of_food(Food) :- triple(Food, "liminal://is_kind_of_food", _).
is_kind_of_food(Food, Kind) :- triple(Food, "liminal://is_kind_of_food", Kind).

subject_class("Dish", dish).
constructor(dish, '[{action: "addLink", source: "this", predicate: "liminal://is_kind_of_food", target: "liminal://unknown_kind"}]').
instance(dish, Base) :- is_kind_of_food(Base).
property(dish, "kind").
property_getter(dish, Base, "kind", Value) :- is_kind_of_food(Base, Value).
property_setter(dish, "kind", '[{action: "setSingleTarget", source: "this", predicate: "liminal://is_kind_of_food", target: "value"}]').



subject_class("Potluck", potluck).
constructor(potluck, '[{action: "addLink", source: "this", predicate: "liminal://is_event", target: "liminal://potluck"}]').
instance(potluck, Base) :- triple(Base, "liminal://is_event", "liminal://potluck").

is_contributed_dish(Potluck, Dish) :- 
    instance(potluck, Potluck), 
    instance(dish, Dish), 
    triple(Potluck, "liminal://has_contribution", Dish).

collection(potluck, "dishes").
collection_getter(potluck, Base, "dishes", List) :- findall(C, is_contributed_dish(Base, C), List).
collection_adder(potluck, "dishes", '[{action: "addLink", source: "this", predicate: "liminal://has_contribution", target: "value"}]').


contributions_order(Potluck, C1, C2) :- 
    is_contributed_dish(Potluck, Dish), 
    is_contributed_dish(Potluck, Before), 
    link(Potluck, "liminal://has_contribution", C1, Time1, _),
    link(Potluck, "liminal://has_contribution", C2, Time2, _),
    Time1 < Time2.

same_kind(C1, C2) :- 
    triple(C1, "liminal://is_kind_of_food", Kind),
    triple(C2, "liminal://is_kind_of_food", Kind).

count_same_contributions_happened_before(Potluck, C1, 0) :- 
    aggregate_all(count, (setof(C2, (contributions_order(Potluck, C2, C1), same_kind(C1, C2)), C2)), 0).

count_same_contributions_happened_before(Potluck, C1, Number) :- 
    setof(C2, (contributions_order(Potluck, C2, C1), same_kind(C1, C2)), FoundSet),
    length(FoundSet, Number).

%count_same_contributions_happened_before(Potluck, C1, 0) :- 
%    setof(_, (contributions_order(Potluck, C2, C1), same_kind(C1, C2)), []),


%count_same_contributions_happened_before(Potluck, C1, Number) :- 
%    aggregate_all(count, (contributions_order(Potluck, C2, C1), same_kind(C1, C2), dif(C1,C2)), Number).
contribution_uniqueness(Potluck, Contribution, Uniqueness) :- 
    count_same_contributions_happened_before(Potluck, Contribution, Number),
    Uniqueness is 1 / (Number + 1).
    

property(dish, "uniqueness").
property_getter(dish, Base, "uniqueness", Value) :- contribution_uniqueness(_, Base, Value).
