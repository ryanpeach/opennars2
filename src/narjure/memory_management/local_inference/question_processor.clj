(ns narjure.memory-management.local-inference.question-processor
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

(defn process-question [state question]
  (println "A_0")
  (let [beliefs (filter #(and (= (:task-type %) :belief)
                              (question-unifies (:statement question) (:statement %)))
                        (get-tasks state))]
    (println "A")
    ;filter beliefs matching concept content
    ;project to task time
    ;select best ranked
    (let [projected-belief-tuples (map (fn [a] [a (project-eternalize-to (:occurrence question) a @nars-time)]) beliefs)]
      (if (not-empty projected-belief-tuples)

        ;select best solution
        (let [[belief projected-belief] (apply max-key (fn [a] (answer-quality question (second a))) projected-belief-tuples)
              answerered-question (assoc question :solution belief)]
          (if (or (= (:solution question) nil)
                  (better-solution projected-belief
                                   question))
            ;update budget and tasks
            (let [result (decrease-question-budget-by-solution answerered-question)]

              ;Update goal also:
              (let [new-belief (increased-belief-budget-by-question projected-belief question)]
                (update-task-in-tasks state (assoc belief :budget (:budget new-belief)) belief))

              (add-to-tasks state result)                   ;its a new question

              ;if answer to user quest ouput answer
              (potential-output-answer state (get-task-id question) question (:solution result)))

            (add-to-tasks state question)        ;it was not better, we just add the question and dont replace the solution
            ))
        ;was empty so just add
        (add-to-tasks state question))))

  )
