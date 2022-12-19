% This is a sample Social DNA implementing a simple potluck organizer.
% The point of this is to track what people are bringing to a potluck
% and to make sure that people don't bring the same thing twice.
% Therefore, contributions get a calculated uniqueness score that
% is 1 / (number of times the same kind of food has been brought
% before + 1).  So, if you're the first person to bring a dish, it
% gets a score of 1.  If you're the second person to bring a dish,
% it gets a score of 0.5.  If you're the third person to bring a
% dish, it gets a score of 0.33, and so on.
%
% We define two Subject classes: Dish and Potluck.
% Dishes have a "kind" property that is used for the uniqueness
% calculation.  Potlucks have a "dishes" collection that contains
% all the dishes that have been contributed to the potluck.

% Anything that has a link going off with predicate "liminal://is_kind_of_food"
% is a kind of food.
is_kind_of_food(Food) :- triple(Food, "liminal://is_kind_of_food", _).
% And the target of that link is the kind of food.
is_kind_of_food(Food, Kind) :- triple(Food, "liminal://is_kind_of_food", Kind).


% === Dish ===
subject_class("Dish", dish).
% ..defined over a link with predicate "liminal://is_kind_of_food"
constructor(dish, '[{action: "addLink", source: "this", predicate: "liminal://is_kind_of_food", target: "liminal://unknown_kind"}]').
instance(dish, Base) :- is_kind_of_food(Base).
% ..with the target of that link being the value for the "kind" property.
property(dish, "kind").
property_getter(dish, Base, "kind", Value) :- is_kind_of_food(Base, Value).
property_setter(dish, "kind", '[{action: "setSingleTarget", source: "this", predicate: "liminal://is_kind_of_food", target: "value"}]').


% === Potluck ===
subject_class("Potluck", potluck).
% ..defined over a link with predicate "liminal://is_event" and target "liminal://potluck"
constructor(potluck, '[{action: "addLink", source: "this", predicate: "liminal://is_event", target: "liminal://potluck"}]').
instance(potluck, Base) :- triple(Base, "liminal://is_event", "liminal://potluck").

% We want to make sure that the dishes collection only contains dishes.
% So, we define a helper predicate that checks if a dish is contributed
% and actually an instance of the Dish class.
is_contributed_dish(Potluck, Dish) :- 
    instance(potluck, Potluck), 
    instance(dish, Dish), 
    triple(Potluck, "liminal://has_contribution", Dish).

% With this, the collection definition is straightforward.
collection(potluck, "dishes").
% We use the built-in findall to get all the expressions for which the above predicate is true
% when binding "Potluck" to the subect's base.
collection_getter(potluck, Base, "dishes", List) :- findall(C, is_contributed_dish(Base, C), List).
collection_adder(potluck, "dishes", '[{action: "addLink", source: "this", predicate: "liminal://has_contribution", target: "value"}]').


% The above really just defines the data model and binds it to our specific 'liminal'
% vocabulary and link ontology.  Now we need to define the uniqueness calculation.

% One crucial aspect of the uniqueness calculation is the order in which contributions
% were made to the potluck. AD4M links are stored and shared as link expressions with
% author, timestamp and cryptographic signature.
% AD4M implments two predicates to make links accessible in the Prolog engine:
% triple/3 and link/6.
% triple/3 just associates source, predicate and target of a link expression.
% link/5 adds timestamp and author to that.
% So we use timestamp of the links that associate the dish to the potluck
% to define a contribution order.
contributions_order(Potluck, C1, C2) :- 
    is_contributed_dish(Potluck, Dish), 
    is_contributed_dish(Potluck, Before), 
    link(Potluck, "liminal://has_contribution", C1, Time1, _),
    link(Potluck, "liminal://has_contribution", C2, Time2, _),
    Time1 < Time2.

% Now we can easily get all contributions that came before a given one.
% But we are only interested in the same type of food:
same_kind(C1, C2) :- 
    triple(C1, "liminal://is_kind_of_food", Kind),
    triple(C2, "liminal://is_kind_of_food", Kind).

% We can easily put those two predicates together,
% but counting all the results is a bit more tricky.
% aggregate_all/3 (Prolog built-in) only works to detect the case when there is no solution
% (i.e. Number = 0). ...
count_same_contributions_happened_before(Potluck, C1, 0) :- 
    aggregate_all(count, (setof(C2, (contributions_order(Potluck, C2, C1), same_kind(C1, C2)), C2)), 0).

% ... the real deal is to use setof/3 to get rid of duplicates (in the results
% wich stem from multiple routes in Prologs backtracking)
% So here we collect all the C2s that are the same kind as C1 and came before C1,
% put them into "FoundSet" and then bind "Number" to the length of that set.
count_same_contributions_happened_before(Potluck, C1, Number) :- 
    setof(C2, (contributions_order(Potluck, C2, C1), same_kind(C1, C2)), FoundSet),
    length(FoundSet, Number).

% Finally, we can define the uniqueness calculation.
contribution_uniqueness(Potluck, Contribution, Uniqueness) :- 
    count_same_contributions_happened_before(Potluck, Contribution, Number),
    Uniqueness is 1 / (Number + 1).
    

% Only thing left is to bind the uniqueness calculation to the "uniqueness" property
% on the Dish class:
property(dish, "uniqueness").
property_getter(dish, Base, "uniqueness", Value) :- contribution_uniqueness(_, Base, Value).


% ====================
% Next steps:
% A problem with this implementation is that agents could just change the kind
% of a dish after contributing it to the potluck. This would result in uniqueness
% values of other Dishes changing.
% To prevent this, we could could just invalidate a contribution if its kind
% link is younger than the contribution link.
% This would still allow people to change their dish kind, but it would 
% reset them to the back of the queue.
% So uniqueness scores of other could only increase, but never decrease.

