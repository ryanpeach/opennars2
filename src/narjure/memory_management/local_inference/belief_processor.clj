(ns narjure.memory-management.local-inference.belief-processor
  (:require
    [co.paralleluniverse.pulsar.actors
     :refer [! spawn gen-server register! cast! Server self
             shutdown! unregister! set-state! state whereis]]
    [narjure.actor.utils :refer [defactor]]
    [taoensso.timbre :refer [debug info]]
    [narjure.bag :as b]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.global-atoms :refer :all]
    [narjure.memory-management.local-inference.local-inference-utils :refer :all]
    [nal.deriver.truth :refer [t-or confidence frequency]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))

(defn expired? [anticipation]
  (> @nars-time (:expiry anticipation)))

(defn create-negative-confirmation-task [anticipation]
  (assoc anticipation :task-type :belief
                      :truth (nal.deriver.truth/negation (:truth anticipation) 0)
                      :budget [(min 1.0
                                 (* (first (:budget anticipation))
                                    1.5))
                               (second (:budget anticipation))]))

(defn confirmable-observable? [task]
  ;todo check state for observable
  (not= (:occurrence task) :eternal))

(defn create-anticipation-task [task]
  (assoc task :task-type :anticipation :expiry (+ (:occurrence task) 100)))

(defn satisfaction-based-budget-change [state belief-task goals]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence a) belief-task @nars-time)])
             (filter #(= (:statement %) (:statement belief-task)) goals))]
    (when (not-empty projected-list)
      (doseq [[goal belief-task-projected-to-goal] projected-list]
        (when (better-solution belief-task goal)
          ;update budget and solution
          (potential-output-answer state goal belief-task)
          (let [new-goal (reduced-goal-budget-by-belief goal belief-task-projected-to-goal)
                new-goal-with-solution (assoc new-goal :solution belief-task)]
            (update-task-in-tasks state new-goal-with-solution goal))
          (let [new-belief (increased-belief-budget-by-goal belief-task-projected-to-goal goal)]
            (update-task-in-tasks state new-belief belief-task)))))))

(defn conditionalprint [state st stru]
  (when (= (:id @state) st)
    (println stru)))

(defn answer-based-budget-change [state belief-task questions]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence a) belief-task @nars-time)])
             (filter #(= (:statement %) (:statement belief-task)) questions))]
    (println "empty?1")
    (when (not-empty projected-list)
      (println "1")
      (doseq [[question belief-task-projected-to-question] projected-list]
        (conditionalprint state '[--> a A] "1.1...")
        (conditionalprint state '[--> a A] (str "new" belief-task (:id @state)))
        (conditionalprint state '[--> a A] (str "old" (:solution question) (:id @state)))
        (when (better-solution belief-task question)
          (println "2")
          (potential-output-answer state question belief-task)
          ;update budget and solution
          (let [new-question (reduced-question-budget-by-belief question belief-task-projected-to-question)
                new-question-with-solution (assoc new-question :solution belief-task)]
            (update-task-in-tasks state new-question-with-solution question))
          (let [new-belief (increased-belief-budget-by-question belief-task-projected-to-question question)]
            (update-task-in-tasks state new-belief belief-task)))))))

(defn process-belief [state task]
  ;group-by :task-type tasks
  (let [tasks (get-tasks state)
        goals (filter #(= (:task-type %) :goal) tasks)
        beliefs (filter #(= (:task-type %) :belief) tasks)
        anticipations (filter #(= (:task-type %) :anticipation) tasks)
        questions (filter #(= (:task-type %) :question ) tasks)]

    ;also allow revision in subterm concepts! this is why statement is compared to task statement, not to ID!!
    (let [projected-beliefs (map #(project-eternalize-to (:occurrence task) % @nars-time) (filter #(= (:statement %) (:statement task)) beliefs))]
      (when (= (:source task) :input)
        (doseq [projected-anticipation (map #(project-eternalize-to (:occurrence task) % @nars-time) anticipations)]
          ;revise anticpation and add to tasks
          (add-to-tasks state (revise projected-anticipation task))))
      (doseq [revisable (filter #(revisable? task %) projected-beliefs)]
        ;revise beliefs and add to tasks
        (process-belief state (revise task revisable))))

    (println "added")
    ;add task to bag
    (add-to-tasks state task)
    ;check if it satisfies a goal or question and change budget accordingly
    (satisfaction-based-budget-change state task goals)
    (answer-based-budget-change state task questions)

      ;generate neg confirmation for expired anticipations
    ;and add to tasks
    (doseq [anticipation anticipations]
      (when (expired? anticipation)
        (let [neg-confirmation (create-negative-confirmation-task anticipation)]
          ;add to tasks
          (add-to-tasks state neg-confirmation))))

    ;when task is confirmable and observabnle
    ;add an anticipation tasks to tasks
    (when (confirmable-observable? task)
      (let [anticipated-task (create-anticipation-task task)]
        (add-to-tasks state anticipated-task))))
  )
