(ns narjure.memory-management.local-inference.quest-processor
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.bag :as b]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.global-atoms :refer :all]
    [narjure.memory-management.local-inference.local-inference-utils :refer :all]
    [nal.deriver.truth :refer [t-or confidence frequency]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))

(defn process-quest
  "Process a quest task: Check how much its answered and add to bag."
  [state quest]
  (let [goals (filter #(and (= (:task-type %) :goal)
                            (question-unifies (:statement quest) (:statement %)))
                      (get-tasks state))]
    ;filter beliefs matching concept content
    ;project to task time
    ;select best ranked
    (let [projected-goal-tuples (map (fn [a] [a (project-eternalize-to (:occurrence quest) a @nars-time)]) goals)]
      (if (not-empty projected-goal-tuples)
        ;select best solution
        (let [[goal projected-goal] (apply max-key (fn [a] (answer-quality quest (second a))) projected-goal-tuples)
              answerered-quest (assoc quest :solution goal)]
          (if (or (= (:solution quest) nil)
                  (better-solution projected-goal
                                   quest))
            ;update budget and tasks
            (let [result (decrease-quest-budget-by-solution answerered-quest)]

              ;Update goal also:
              (let [new-goal (increased-goal-budget-by-quest projected-goal quest)]
                (update-task-in-tasks state (assoc goal :budget (:budget new-goal)) goal))

              (add-to-tasks state result)                   ;its a new quest

              ;if answer to user quest ouput answer
              (potential-output-answer state (get-task-id quest) quest (:solution result)))

            (add-to-tasks state quest)        ;it was not better, we just add the question and dont replace the solution
            ))
        ;was empty so just add
        (add-to-tasks state quest)))))
