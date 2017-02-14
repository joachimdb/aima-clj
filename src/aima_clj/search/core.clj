(ns aima-clj.search.core)

(defprotocol Problem
  (initial-state [this] "A state in the problem domain")
  (actions [this state] "Set of available actions from a state")
  (result [this state action] "The result state after performing action on state")
  (goal-state? [this state] "Is state a target or goal state for search?")
  (cost [this state action] "The cost of going from one node to the next state by taking action")
  (h-cost [this state] "The estimated cost from state to a target state."))
