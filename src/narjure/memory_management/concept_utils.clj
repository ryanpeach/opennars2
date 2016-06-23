(ns narjure.memory-management.concept-utils
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure
     [global-atoms :refer :all]
     [bag :as b]
     [debug-util :refer :all]
     [control-utils :refer :all]
     [defaults :refer :all]
     [projection-utils :refer [max-statement-confidence-projected-to-now]]]
    [narjure.memory-management.local-inference
     [local-inference-utils :refer [get-task-id get-tasks]]
     [belief-processor :refer [process-belief]]
     [goal-processor :refer [process-goal]]
     [quest-processor :refer [process-quest]]
     [question-processor :refer [process-question]]]
    [clojure.core.unify :refer [unifier]]
    [nal.term_utils :refer [syntactic-complexity]]
    [nal.deriver
     [truth :refer [w2c t-or t-and confidence frequency expectation revision]]
     [projection-eternalization :refer [project-eternalize-to]]])
  (:refer-clojure :exclude [promise await]))

(defn concept-quality []
  (:quality ((:elements-map @c-bag) (:id @state))))

(defn concept-priority [term]
  (:priority ((:elements-map @c-bag) term)))

(defn get-ref-from-term [term]
  (:ref ((:elements-map @c-bag) term)))

(defn forget-task [el last-forgotten]
  (let [task (:task el)
        budget (:budget task)
        lambda (/ (- 1.0 (second budget)) decay-rate)
        fr (Math/exp (* -1.0 (* lambda (- @nars-time last-forgotten))))
        new-priority (max (round2 4 (* (:priority el) fr))
                          (/ (concept-quality) (+ 1.0 (b/count-elements (:tasks @state)))) ;dont fall below 1/N*concept_quality
                          (nth budget 2)) ;quality of task
        new-budget [new-priority (second budget) (nth budget 2)]]
    (let [updated-task (assoc task :budget new-budget)]
      (assoc el :priority new-priority
                :task updated-task))))

(defn forget-tasks []
  (let [tasks (:elements-map (:tasks @state))
        last-forgotten (:last-forgotten @state)]
    (set-state! (assoc @state :tasks (b/default-bag max-tasks)))
    (doseq [[id el] tasks]                       ;{ id {:staement :type :occurrence}
      (let [el' (forget-task el last-forgotten)]
        ;(println (str "forgetting: " (get-in el' [:task :statement])))
        (set-state! (assoc @state :tasks (b/add-element (:tasks @state) el')))))
    (set-state! (assoc @state :last-forgotten @nars-time))))

(defn update-concept-priority-rec
  "sum task priority for concept"
  [coll p-sum]
  (if (empty? coll)
    p-sum
    (let [{id :id priority :priority} (first coll)
          coll' (dissoc coll key)]
      (update-concept-priority-rec coll' (t-or p-sum priority))))

(defn update-concept-budget [state, self]
  "Update the concept budget"
  (let [tasks (:priority-index (:tasks state))      ; :priority-index ok here
        ;priority-sum (round2 3 (reduce t-or (for [x tasks] (:priority x))))
        priority-sum (update-concept-priority-rec tasks 0.0)
        quality-rescale 0.1
        el {:id       (:id state)
            :priority priority-sum
            :quality  (round2 3 (max (concept-quality) (* quality-rescale priority-sum)))
            :ref      self
            :strongest-belief-about-now (max-statement-confidence-projected-to-now state :belief)
            :strongest-desire-about-now (max-statement-confidence-projected-to-now state :goal)
            ;:strongest-desire-about-now
            }]
    (swap! c-bag b/add-element el)))

#_(defn belief? [task]
  (= (:task-type task) :belief))

#_(defn goal? [task]
  (= (:task-type task)) :goal)

(defn update-concept-priority-rec
  "sum task priority for concept"
  [coll p-sum]
  (if (empty? coll)
    p-sum
    (let [{key :id priority :priority task :task} (second (first coll))
          coll' (dissoc coll key)]
      (update-concept-priority-rec coll' (t-or p-sum priority))))

#_(defn update-concept-stats-rec
  "collects stats for concept:
    concept-priority as t-or of task priority
    best-proj-belief-task as max-statement-confidence-projected-to-now2
    best-proj-goal-task statement
   only one iteration of the task bag is required due to the recursion"
  [coll content p-sum proj-belief-task proj-goal-task]
  (if (empty? coll)
    {:p-sum p-sum :proj-belief-task proj-belief-task :proj-goal-task proj-goal-task}
    (let [{key :id priority :priority task :task} (second (first coll))
          coll' (dissoc coll key)]
      (update-concept-stats-rec coll'
                                content
                                (t-or p-sum priority)
                                (if (and (belief? task) (= content (:statement task)))
                                  (max-statement-confidence-projected-to-now2 task proj-belief-task)
                                  proj-belief-task)
                                (if (and (goal? task) (= content (:statement task)))
                                  (max-statement-confidence-projected-to-now2 task proj-goal-task)
                                  proj-goal-task)))))

#_(defn update-concept-budget
  "Processes concept stats results"
  [state, self]
  (let [result (update-concept-stats-rec (:elements-map (:tasks state)) (:id state) 0.0 nil nil)
        {p-sum :p-sum
         strongest-belief-about-now :proj-belief-task
         strongest-desire-about-now :proj-goal-task} result
        priority-sum (round2 4 p-sum)
        quality-rescale 0.1
        el {:id       (:id state)
            :priority priority-sum
            :quality  (round2 3 (max (concept-quality) (* quality-rescale priority-sum)))
            :ref      self
            :strongest-belief-about-now strongest-belief-about-now
            :strongest-desire-about-now strongest-desire-about-now}]
    (swap! c-bag b/add-element el)))

(defn unifies [b a]
  (= a (unifier a b)))

(defn qu-var-transform [term]
  (if (coll? term)
    (if (= (first term) 'qu-var)
      (symbol (str "?" (second term)))
      (apply vector (for [x term]
                      (qu-var-transform x))))
    term))

(defn question-unifies [question solution]
  (unifies (qu-var-transform question) solution))

(defn solution-update-handler
  ""
  [from [_ oldtask newtask]]
  (let [concept-state @state
        task (first (filter (fn [a] (let [it (:task (second a))]
                                      (and (= (:statement it) (:statement oldtask))
                                           (= (:occurrence it) (:occurrence oldtask))
                                           (= (:task-type it) (:task-type oldtask))
                                           (= (:solution it) (:solution oldtask)))))
                            (:elements-map (:tasks concept-state))))]
    (println "in solution-update-handler")
    (when (not= nil task)
      (let [[_ bag2] (b/get-by-id (:tasks concept-state) (get-task-id task)) ;todo merge old and new tasks?

            bag3 (b/add-element bag2 {:id (get-task-id newtask) :priority (first (:budget newtask)) :task newtask})]
        (set-state! (assoc concept-state :tasks bag3))))))