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
    [nal.deriver :refer [occurrence-type]]
    [nal.deriver.truth :refer [t-and t-or frequency confidence expectation]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]]
    [narjure.debug-util :refer :all])
  (:refer-clojure :exclude [promise await]))

(defn truth-round [[f c :as truth]]
  (if (= nil truth)
    nil
    [(round2 2 f)
    (round2 2 c)]))

(defn get-task-id [task]
  [(:statement task) (:task-type task) (occurrence-type (:occurrence task))])

(defn get-anticipation-id [task]
  [(:statement task) (:task-type task) (:occurrence task)])

(defn item [task]
  {:id (get-task-id task) :priority (first (:budget task))})

(defn measure-budget [budg]
  (first budg))                                             ;higher priority means higher evaluation similar as for confidence in truth consideration

(defn max-budget [budg1 budg2]                            ;the one with higher priority determines the budget
  (apply max-key measure-budget [budg1 budg2]))

(defn measure-truth [task]                                    ; higher confidence means higher evaluation
  (second (:truth (project-eternalize-to @nars-time task @nars-time)))) ;prefer current events

(defn make-element [task]
  {:id (get-task-id task) :priority (first (:budget task)) :task task})

(defn take-better-solution [result t1 t2]
  (let [sol1 (:solution t1)
        sol2  (:solution t2)]
    (if (or (not= nil sol1)
            (not= nil sol2)) ;project both to result time and then use the better solution for our result:
      (let [sol1-proj (project-eternalize-to (:occurrence result) sol1 @nars-time)
            sol2-proj (project-eternalize-to (:occurrence result) sol2 @nars-time)
            best-solution (apply max-key
                                 (fn [a] (second (:truth a)))
                                 (filter #(not= nil %) [sol1-proj sol2-proj]))]
        (assoc result :solution best-solution))
      result)))

(defn better-task [t1 t2]                                   ;take care here with t1 t2!! ^^
  (if (= t1 nil)                                            ;no previous bag entry exists
    t2                                                      ;so take the new one
    (if (not= nil (:truth t1))
      (take-better-solution (max-key (comp measure-truth) t1 t2) t1 t2)
      (take-better-solution (max-key (comp measure-budget :budget) t1 t2) t1 t2))))

(defn add-to-tasks [state task]
  (let [bag (:tasks @state)
        el (make-element task)
        [{t2 :task :as existing-el} _] (b/get-by-id bag (:id el))
        chosen-task (better-task t2 task)                   ;the second one is kept if equal so the order is important here!!
        new-budget (if existing-el
                     (max-budget (:budget task)
                                 (:budget t2))
                     (:budget task))
        new-el (make-element (assoc chosen-task :budget new-budget))
        bag' (b/add-element bag new-el)]
    (set-state! (assoc @state :tasks bag'))))

(defn update-task-in-tasks [state task old-task]
  (let [[element bag] (b/get-by-id (:tasks @state) (get-task-id old-task))]
    (when (not= nil element)
      (set-state! (assoc @state :tasks bag))))
  (set-state! (assoc @state :tasks (b/add-element (:tasks @state) (make-element task)))))

(defn no-duplicate [M]
  (= (count (set M)) (count M)))

(defn revise [t1 t2]
  (let [revised-truth (nal.deriver.truth/revision (:truth t1) (:truth t2))
        evidence (make-evidence (:evidence t1) (:evidence t2))]
    (assoc t1 :truth revised-truth :source :derived :evidence evidence :budget (max-budget (:budget t1) (:budget t2)))))

(defn better-solution [solution task]
  (let [projected-solution (project-eternalize-to (:occurrence task) solution @nars-time)
        cur-solution (project-eternalize-to (:occurrence task) (:solution task) @nars-time)]
    (or (= nil cur-solution)
        (>= (confidence projected-solution)
            (confidence cur-solution)))))


(defn reduced-goal-budget-by-belief [goal belief]                     ;by belief satisfied goal
  (let [satisfaction (- 1.0 (Math/abs (- (expectation (:truth goal))
                                         (expectation (:truth belief)))))
        budget (:budget goal)
        p (first budget)
        p-new (t-and p (- 1.0 satisfaction))]
    (assoc goal :budget [p-new (second budget) (nth budget 2)])))

(defn reduced-question-budget-by-belief [question belief]
  (let [budget (:budget question)
        p (first budget)
        p-new (t-and p (- 1.0 (confidence belief)))]
    (assoc question :budget [p-new (second budget) (nth budget 2)])))

(defn increased-belief-budget-by-question [belief question]  ;useful belief, answered a question
  (let [budget (:budget belief)
        q (nth budget 2)
        k 0.001
        ;d-new (t-or d (* k (confidence belief)))
        q-new (t-or q (* k (nth (:budget belief) 2)))]
    (assoc belief :budget [(first budget) (second budget) q-new])))                                                 ;1-confidence(solution)

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
                      (second budget)
                      (nth budget 2)]] ;TODO dependent on solution confidence
      (assoc question :budget new-budget))))

(defn decrease-quest-budget-by-solution [quest]
  (decrease-question-budget-by-solution quest))

(defn get-tasks [state]
  (let [tasks (vec (for [x (:elements-map (:tasks @state))] (:task (val x))))]
    ;(println (str "count: "  (count (:elements-map (:tasks @state))) " gt tasks: " tasks))
    tasks))

(defn same-occurrence-type [t1 t2]
  (or (and (= (:occurrence t1) :eternal) (= (:occurrence t2) :eternal))
      (and (not= (:occurrence t1) :eternal) (not= (:occurrence t2) :eternal))))