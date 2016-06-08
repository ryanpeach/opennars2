(ns narjure.defaults)

(def belief-frequency 1.0)
(def belief-confidence 0.9)

(def truth-value
  [belief-frequency belief-confidence])

(def belief-priority 0.6)
(def belief-durability 0.5)
;todo clarify this
(def belief-quality 0.5)

(def belief-budget
  [belief-priority belief-durability belief-quality])

(def question-priority 0.9)
(def question-durability 0.7)
;todo clarify this
(def question-quality 0.5)

(def question-budget
  [belief-priority belief-durability belief-quality])

(def goal-confidence 0.9)
(def goal-priority 0.7)
(def goal-durability 0.9)

(def budgets
  {:belief belief-budget
   :question question-budget
   :goal belief-budget
   :quest question-budget})

(def ^{:type double} horizon 1)

(def temporal-window-duration 10)

(def max-term-complexity 15)
