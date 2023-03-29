register_sdna_flow("TODO", t).
flowable(_, t).

flow_state(ExprAddr, 0, t) :- triple(ExprAddr, "todo://state", "todo://ready").
flow_state(ExprAddr, 0.5, t) :- triple(ExprAddr, "todo://state", "todo://doing").
flow_state(ExprAddr, 1, t) :- triple(ExprAddr, "todo://state", "todo://done").

start_action('[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]', t).
action(0, "Start", 0.5, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://doing"}, {action: "removeLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
action(0.5, "Finish", 1, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://done"}, {action: "removeLink", source: "this", predicate: "todo://state", target: "todo://doing"}]').