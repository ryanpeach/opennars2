(ns narjure.memory-management.local-inference.local-inference-utils
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.bag :as b]
    [narjure.global-atoms :refer :all]
    [narjure.control-utils :refer [make-evidence round2]]
    [narjure.perception-action.task-creator :refer :all]
    [nal.term_utils :refer :all]
    [nal.deriver.truth :refer [t-and t-or frequency confidence expectation]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]]
    [narjure.debug-util :refer :all])
  (:refer-clojure :exclude [promise await]))

(defn truth-round [[f c :as truth]]
  (if (= nil truth)
    nil
    [(round2 2 f)
    (round2 2 c)]))

(defn occurrence-type [occ]
  (case occ
    :eternal :eternal
    :event))

(defn get-task-id [task]
  [(:statement task) (:task-type task) (occurrence-type (:occurrence task))])

#_(defn get-task-id [task]
  [(:statement task) (:evidence task) (:task-type task) (:occurrence task) (truth-round (:truth task))])

(defn item [task]
  {:id (get-task-id task) :priority (first (:budget task))})

(defn measure-budget [budg]
  (first budg))                                             ;higher priority means higher evaluation similar as for confidence in truth consideration

(defn max-budget [budg1 budg2]                            ;the one with higher priority determines the budget
  (apply max-key measure-budget [budg1 budg2]))

(defn measure-truth [tv]                                    ; higher confidence means higher evaluation
  (second tv))

(defn make-element [task]
  {:id (get-task-id task) :priority (first (:budget task)) :task task})

(defn choice [t1 t2]
  (if (= t2 nil)
    t1
    (if (not= nil (:truth t1) (:truth t2))
      (max-key (comp measure-truth :truth) t1 t2)
      (max-key (comp measure-budget :budget) t1 t2))))

#_(defn choice [t1 t2]
  (if (= t2 nil)
    t1
    (if (not= nil (:truth t1) (:truth t2))
      (case (>= (second (:truth t1)) (second (:truth t2)))
        true t1
        false t2)
      (max-key (comp measure-truth :truth) t1 t2)
      (max-key (comp measure-budget :budget) t1 t2))))

(defn add-to-tasks [state task]
  (let [bag (:tasks @state)
        el (make-element task)
        [{t2 :task :as existing-el} _] (b/get-by-id bag (:id el))
        chosen-task (choice task t2)
        new-budget (if existing-el
                     (max-budget (:budget task)
                                 (:budget t2))
                     (:budget task))
        new-el (make-element (assoc chosen-task :budget new-budget))
        bag' (b/add-element bag new-el)]
    (set-state! (assoc @state :tasks bag'))))

(defn add-to-anticipations [state task]
  (let [bag (:anticipations @state)
        el {:id (get-task-id task) :priority (first (:budget task)) :task task}
        bag' (b/add-element bag el)]
    (set-state! (assoc @state :anticipations bag'))))

(defn remove-anticipation [state anticipation]
  (let [bag (:anticipations @state)
        id (get-task-id anticipation)
        [_ bag'] (b/get-by-id bag id)]
    (set-state! (assoc @state :anticipations bag'))))

(defn update-task-in-tasks [state task old-task]
  (let [[element bag] (b/get-by-id (:tasks @state) old-task)]
    (when (not= nil element)
      (set-state! (assoc @state :tasks bag))))
  (add-to-tasks state task))

(defn create-revised-task
  "create a revised task with the provided sentence, truth and default value"
  [sentence truth evidence budget]
  ;todo budget should be updated
  (assoc sentence :truth truth :evidence evidence :budget budget))

(defn no-duplicate [M]
  (= (count (set M)) (count M)))

(defn revise [t1 t2 kw]
  (let [revised-truth (nal.deriver.truth/revision (:truth t1) (:truth t2))
        evidence (make-evidence (:evidence t1) (:evidence t2))]
    (when-not (no-duplicate evidence)
      (println (str "nope " kw)))
    (create-revised-task t1 revised-truth evidence (max-budget (:budget t1) (:budget t2)))))

(defn better-solution [solution task]
  (let [projected-solution (project-eternalize-to (:occurrence task) solution @nars-time)
        cur-solution (project-eternalize-to (:occurrence task) (:solution task) @nars-time)]
    (or (= nil cur-solution)
        (> (confidence projected-solution)
           (confidence cur-solution)))))


(defn reduced-goal-budget-by-belief [goal belief]                     ;by belief satisfied goal
  (let [satisfaction (- 1.0 (Math/abs (- (expectation (:truth goal))
                                         (expectation (:truth belief)))))
        budget (:budget goal)
        p (first budget)
        p-new (t-and p (- 1.0 satisfaction))]
    (assoc goal :budget [p-new (second budget)])))

(defn reduced-question-budget-by-belief [question belief]
  (let [budget (:budget question)
        p (first budget)
        p-new (t-and p (- 1.0 (confidence belief)))]
    (assoc question :budget [p-new (second budget)])))

(defn increased-belief-budget-by-question [belief question]  ;useful belief, answered a question
  (let [budget (:budget belief)
        d (second budget)
        k 1.0
        d-new (t-or d (* k (confidence belief)))]
    (assoc belief :budget [(first budget) d-new])))                                                 ;1-confidence(solution)

(defn reduced-quest-budget-by-goal [quest goal]                     ;by goal satisfied quest
  (reduced-question-budget-by-belief quest goal))

(defn increased-goal-budget-by-quest [goal quest]                     ;useful goal, answered a quest
  (increased-belief-budget-by-question goal quest))

(defn increased-belief-budget-by-goal [belief goal]                     ;by belief satisfied goal
  (increased-belief-budget-by-question belief goal))

;increased-belief-budget-by-goal

;TODO handle in question/quest handling:
;increased-goal-budget-by-quest
;increased-belief-budget-by-question

;decrease-quest-question-budget-by-solution:
(defn decrease-question-budget-by-solution [question]
  (let [budget (:budget question)
        solution (:solution question)]
    ;todo improve budget function here
    (let [new-budget [(* (- 1.0 (confidence solution)) (first budget))
                      (second budget)]] ;TODO dependent on solution confidence
      (assoc question :budget new-budget))))

(defn decrease-quest-budget-by-solution [quest]
  (decrease-question-budget-by-solution quest))

(defn get-tasks [state]
  (let [tasks (vec (for [x (:elements-map (:tasks @state))] (:task (val x))))]
    ;(println (str "count: "  (count (:elements-map (:tasks @state))) " gt tasks: " tasks))
    tasks))

(defn get-anticipations [state]
  (vec (for [x (:elements-map (:anticipations @state))] (:task (val x)))))