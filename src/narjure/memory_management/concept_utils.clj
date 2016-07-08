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
    [nal.deriver
     [truth :refer [w2c t-or t-and confidence frequency expectation revision]]
     [projection-eternalization :refer [project-eternalize-to]]])
  (:refer-clojure :exclude [promise await]))

(defn concept-quality []
  (let [value (:quality ((:elements-map @c-bag) (:id @state)))]
    (if value
      value
      0.0)))

(defn concept-priority [term]
  (let [value (:priority ((:elements-map @c-bag) term))]
    (if value
      value
      0.0)))

(defn concept-observable [term]
  (:observable ((:elements-map @c-bag) term)))

(defn forget-task [el last-forgotten n]
  (let [task (:task el)
        el-time (:occurrence task)
        budget (:budget task)
        lambda (/ (- 1.0 (second budget)) inverse-decay-rate)
        temporal-distance (if (= el-time :eternal) 0.0 (Math/abs (- el-time @nars-time)))
        occurrence-decay (if (= el-time :eternal) 1.0 (/ 1.0 (+ 1.0 (* temporal-distance
                                                                       temporal-distance))))
        k-quality-occurrence-decay 10000.0
        distance-for-quality (/ temporal-distance k-quality-occurrence-decay)
        occurrence-decay-for-quality (if (= el-time :eternal) 1.0 (/ 1.0 (+ 1.0 (* distance-for-quality
                                                                                   distance-for-quality))))
        new-quality (* occurrence-decay-for-quality (nth budget 2))
        fr (Math/exp (* -1.0 (* lambda (- @nars-time last-forgotten))))
        new-priority (max (round2 4 (* (:priority el) fr occurrence-decay))
                          new-quality) ;quality of task
        new-budget [new-priority (second budget) new-quality]]
    (let [updated-task (assoc task :budget new-budget)]
      (assoc el :priority new-priority
                :task updated-task))))

(defn forget-tasks []
  (let [tasks (:elements-map (:tasks @state))
        last-forgotten (:last-forgotten @state)
        n (b/count-elements (:tasks @state))]
    (set-state! (assoc @state :tasks (b/default-bag max-tasks)))
    (doseq [[id el] tasks]                                 ;{ id {:staement :type :occurrence}
      (let [el' (forget-task el last-forgotten n)]
        ;(println (str "forgetting: " (get-in el' [:task :statement])))
        (set-state! (assoc @state :tasks (b/add-element (:tasks @state) el')))))
    (set-state! (assoc @state :last-forgotten @nars-time))))

(defn update-concept-budget [state, self]
  "Update the concept budget"
  (let [els (:elements-map (:tasks state))      ; :priority-index ok here
        n (count els)
        p (round2 3 (reduce max 0 (for [[id {task :task}] els] (first (:budget task)))))
        q (round2 3 (reduce + 0 (for [[id {task :task}] els] (nth (:budget task) 2))))
        k  0.9999                                             ; long term quality forgetting
        new-q (if (pos? n) (* k (/ q n)) 0.0)
        el {:id       (:id state)
            :priority (max p new-q)
            :quality new-q
            :observable (:observable state)
            :ref      self
            :strongest-belief-about-now (max-statement-confidence-projected-to-now state :belief nil)
            :strongest-desire-about-now (max-statement-confidence-projected-to-now state :goal nil)
            :strongest-belief-event-about-now (max-statement-confidence-projected-to-now state :belief :event)
            }]
    (swap! c-bag b/add-element el)))

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
  (let [[task bag] (b/get-by-id (:tasks @state) (get-task-id oldtask))]
    (when task
      (let [el {:id (get-task-id task)
                :priority (first (:budget newtask))
                :task newtask}
            bag' (b/add-element bag el)]
        (set-state! (assoc @state :tasks bag'))))))

(defn match-belief-to-question [task belief]
  ;1. check whether belief matches by unifying the question vars in task

  (when (and (= (:task-type task) :question)
             (some #{'qu-var} (flatten (:statement task)))
             (question-unifies (:statement task) (:statement belief)))
      ;2. if it unifies, check whether it is a better solution than the solution we have

      (let [get-quality (fn [task] (if task
                           (/ (expectation (:truth task)) (:sc task))
                           0))
            newqual (get-quality (project-eternalize-to (:occurrence task) belief @nars-time))
            oldqual (get-quality (project-eternalize-to (:occurrence task) (:solution task) @nars-time))] ;PROJECT!!
        (when (> newqual oldqual)
          ;3. if it is a better solution, set belief as solution of task
          (let [budget (:budget task)
                new-prio (* (- 1.0 (expectation (:truth belief))) (first budget))
                new-budget [new-prio (second budget) (nth budget 2)]
                newtask (assoc task :solution belief :priority new-prio :budget new-budget)]
             ;4. print our result
            (if (= (:source task) :input)
              (do
                (doseq [f @answer-handlers]
                  (f task (:solution newtask)))
                (potentially-ouput-question-var-question-solution (get-task-id task) (:id @state) task (:solution newtask))))
            ;5. send answer-update-msg OLD NEW to the task concept so that it can remove the old task bag entry
            ;and replace it with the one having the better solution. (reducing priority here though according to solution before send)
            (when-let [{c-ref :ref} ((:elements-map @c-bag) (:statement task))]
              (cast! c-ref [:solution-update-msg task newtask])))))))