(ns narjure.memory-management.local-inference.goal-processor
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer [narsese-print]]
    [narjure.bag :as b]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.global-atoms :refer :all]
    [clojure.core.unify :refer [unify]]
    [nal.term_utils :refer [operation? negation-of-operation?]]
    [narjure.memory-management.local-inference.local-inference-utils :refer :all]
    [nal.deriver.truth :refer [t-or frequency confidence expectation desire-strong]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))


(defn satisfaction-based-budget-change [state goal-task beliefs]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence goal-task) a @nars-time)])
             (filter #(= (:statement %) (:statement goal-task)) beliefs))]
    (when (not-empty projected-list)
      (doseq [[belief belief-task-projected-to-goal] projected-list]
        (when (better-solution belief goal-task)
          ;update budget and solution
          ;(potential-output-answer state goal-task belief)
          (let [new-goal (reduced-goal-budget-by-belief goal-task belief-task-projected-to-goal)
                new-goal-with-solution (assoc new-goal :solution belief)]
            (update-task-in-tasks state new-goal-with-solution goal-task))
          (let [new-belief (increased-belief-budget-by-goal belief-task-projected-to-goal goal-task)]
            (update-task-in-tasks state (assoc belief :budget (:budget new-belief)) belief)))))))



(def decision-threshold 0.5)

(defn execute? [task]
  (> (expectation (:truth task)) decision-threshold))

(defn answer-based-budget-change [state goal-task quests]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence a) goal-task @nars-time)])
             (filter #(= (:statement %) (:statement goal-task)) quests))]
    (when (not-empty projected-list)
      (doseq [[quest goal-task-projected-to-quest] projected-list]
        (when (better-solution goal-task quest)
          (potential-output-answer state quest goal-task)
          ;update budget and solution
          (let [new-quest (reduced-quest-budget-by-goal quest goal-task-projected-to-quest)
                new-quest-with-solution (assoc new-quest :solution goal-task)]
            (update-task-in-tasks state new-quest-with-solution quest))
          (let [new-goal (increased-goal-budget-by-quest goal-task-projected-to-quest quest)]
            (update-task-in-tasks state (assoc goal-task :budget (:budget new-goal)) goal-task)))))))

(def truth-tolerance 0.005)                                 ;TODO put to nal.deriver.truth?
;<(&&,<(*,{SELF}) --> op_down>,(&/,<ballpos --> [above]>,i2048)) =/> <ballpos --> [below]>>. :|12197|: %1.0;0.20954585215002955%" "
(defn best-operation-selection [beliefs goal]
  "1. find all beliefs that predict the goal ''=/> =|>  <|> </>''
   2. filter those with (precondition,op) => goal
   3. use the one whose precondition is highest fullfilled and statement truth is highest
     3.1 truth_A = getting the strongest truth value of the precondition of the precondition concept projected to current moment
     3.2 truth_B = get the truth value of the implication statement
     3.3 desire = desire value of goal
     3.4 execution desire expectation is: D=desire_strong(desire_strong(desire,truth_B),truth_A)
   by using the one whose expectation(D) is highest
   4. create a result operation goal task with the from the predictive statement first operation
      and evidence trail being the summary of all evidence trails

   <(&/,<{ball} --> [down]>,move_up({SELF})) =/> <{ball} --> [equal]>>. %0.4;09% Premise   <- truth_B
    <{ball} --> [equal]>!  Premise                                                        <- desire
    |-
    (&/,<{ball} --> [down]>,move_up({SELF}))!  Desire:Strong
    <{ball} --> [down]>.         Premise                                        <- truth_A
    |-
    move_up({SELF})!  Desire:Strong

  for goal <{ball} --> [equal]>!
  <(&/,<{ball} --> [up]>,move_up({SELF})) =/>
  <{ball} --> [equal]>>
  <(&/,<{ball} --> [up]>,move_down({SELF})) =/>
  <{ball} --> [equal]>>  %0.3;0.9%
  <(&/,<{ball} --> [down]>,move_up({SELF})) =/>
  <{ball} --> [equal]>>. %0.4;09%"

  ;1.: find all beliefs that predict the goal ''=/> =|>  <|> </>''
  (when (and (= (:occurrence goal) :eternal)                ;this should not change as long as intervals are not handled
          (= (:id @state) (:statement goal)))               ;properly it wont be necessary anyway though in this case
    (let [;2. filter those with (precondition,op) => goal   ;so this one is for future improvement
          #_print1 #_(println "step 1,2")
         precondition-op-forms ['[pred-impl [conj ?precondition [seq-conj ?operation ?interval]] ?goal]
                                '[pred-impl [conj [seq-conj ?operation ?interval] ?precondition] ?goal]
                                '[pred-impl [conj ?operation [seq-conj ?precondition ?interval]] ?goal]
                                '[pred-impl [conj [seq-conj ?precondition ?interval] ?operation] ?goal]
                                '[pred-impl [seq-conj ?precondition ?interval1 [seq-conj ?operation ?interval]] ?goal]
                                '[pred-impl [seq-conj [seq-conj ?operation ?interval] ?interval1 ?precondition] ?goal]
                                '[pred-impl [seq-conj ?operation ?interval1 [seq-conj ?precondition ?interval]] ?goal]
                                '[pred-impl [seq-conj [seq-conj ?precondition ?interval] ?interval1 ?operation] ?goal]
                                '[pred-impl [seq-conj ?precondition ?interval1 ?operation ?interval2] ?goal]

                                '[</> [conj ?precondition [seq-conj ?operation ?interval]] ?goal]
                                '[</> [conj [seq-conj ?operation ?interval] ?precondition] ?goal]
                                '[</> [conj ?operation [seq-conj ?precondition ?interval]] ?goal]
                                '[</> [conj [seq-conj ?precondition ?interval] ?operation] ?goal]
                                '[</> [seq-conj ?precondition ?interval1 [seq-conj ?operation ?interval]] ?goal]
                                '[</> [seq-conj [seq-conj ?operation ?interval] ?interval1 ?precondition] ?goal]
                                '[</> [seq-conj ?operation ?interval1 [seq-conj ?precondition ?interval]] ?goal]
                                '[</> [seq-conj [seq-conj ?precondition ?interval] ?interval1 ?operation] ?goal]
                                '[</> [seq-conj ?precondition ?interval1 ?operation ?interval2] ?goal]] ;TODO add others
         precondition-op-beliefs-and-assigment-tuple (filter
                                                       (fn [z] (and (not= (second z) nil)
                                                                    (= ((second z) '?goal) (:statement goal))
                                                                    (operation? ((second z) '?operation))
                                                                    (not (operation? ((second z) '?precondition)))
                                                                    (not (negation-of-operation? ((second z) '?precondition)))
                                                                    (not (operation? ((second z) '?goal)))
                                                                    (not (negation-of-operation? ((second z) '?goal)))))
                                                       (for [form precondition-op-forms
                                                             belief (for [b beliefs]
                                                                      (project-eternalize-to @nars-time b @nars-time))]
                                                         [belief (unify form (:statement belief))]))
          #_print2 #_(println (str "step 3.1, 3.2\n" (vec precondition-op-beliefs-and-assigment-tuple)))
         ;3. use the one whose precondition is highest fullfilled and statement truth is highest:
         ;3.1 truth_A = getting the strongest truth value of the precondition concept projected to current moment
         ;3.2 truth_B = get the truth value of the implication statement
         truth-A-B-unification-maps (for [[belief unificaton-map] precondition-op-beliefs-and-assigment-tuple]
                                      (do
                                        ;reward belief uality also for having this for control useful structure
                                        (println (str "rewarded belief" (narsese-print (:statement belief)) " " (:truth belief) " budg: " (:budget belief)))
                                        (let [new-quality 0.95]
                                          (update-task-in-tasks state (assoc belief :budget [(max new-quality
                                                                                                 (first (:budget belief)))
                                                                                            (second (:budget belief))
                                                                                             new-quality]) ;new quality
                                                               belief))
                                        (let [precondition (unificaton-map '?precondition)
                                             [precondition-concept bag] (b/get-by-id @c-bag precondition)
                                             strongest-belief-about-now (:strongest-belief-about-now precondition-concept)]
                                         (when precondition-concept
                                           [(:truth strongest-belief-about-now) (:truth belief) (:evidence strongest-belief-about-now) (:evidence belief) unificaton-map]))))
         #_print3 #_(println (str "step 3.3\n" (vec truth-A-B-unification-maps)))
         ;3.3 desire = desire value of goal
         desire (:truth (project-eternalize-to @nars-time goal @nars-time))
          #_print4 #_(println (str "step 3.4\n" desire))
         ;3.4 execution desire expectation is: D=desire_strong(desire_strong(desire,truth_B),truth_A)
         D-unification-maps (for [[truth-A truth-B evidence-A evidence-B unification-map] truth-A-B-unification-maps]
                              {:D                (desire-strong (desire-strong desire truth-B) truth-A)
                               :evidence-A       evidence-A
                               :evidence-B       evidence-B
                               :unification-map unification-map})

          #_print5 #_(println (str "3 get best\n" (vec D-unification-maps)))
         ;by using the one whose expectation(D) is highest
         best-option (apply max-key (comp expectation :D)
                            (filter (fn [z] (and (non-overlapping-evidence? (:evidence goal) (:evidence-A z))
                                                 (non-overlapping-evidence? (:evidence-A z) (:evidence-B z)) ;overlap check is not transitive: A {1 2 3} B {5} C {1 2 3}
                                                 (non-overlapping-evidence? (:evidence goal) (:evidence-B z)))) D-unification-maps))
          #_print6 #_(println (str "finished 3 \n"best-option))]

     ;4. create a result operation goal task with the from the predictive statement first operation and evidence trail being the summary of all evidence trails
     (when (and best-option
                (best-option :D)
                (> (second (best-option :D)) truth-tolerance))
       #_(println "had best operator option")
       (let [new-task {:statement  ((:unification-map best-option) '?operation)
                       :truth      (best-option :D)
                       :evidence   (make-evidence (make-evidence (best-option :evidence-A) (best-option :evidence-B)) (:evidence goal))
                       :occurrence @nars-time               ;needs to be occurrence time of now since task creator leaves derived task occurrences un-touched
                       :budget     (:budget goal)
                       :depth      (inc (:depth goal))
                       :task-type  :goal}]
         (println (str "operator selector sending to task-creator " (:truth new-task)))
         (cast! (whereis :task-creator) [:derived-sentence-msg [nil nil new-task]]))))))

(defn process-goal [state task cnt]
  ;group-by :task-type tasks
  (let [tasks (get-tasks state)
        goals (filter #(= (:task-type %) :goal) tasks)
        beliefs (filter #(= (:task-type %) :belief) tasks)
        quests (filter #(= (:task-type %) :quest ) tasks)]

    ;also allow revision in subterm concepts! this is why statement is compared to task statement, not to ID!!
    (let [related-goals (filter (fn [z] (and (same-occurrence-type z task)
                                             (= (:statement z) (:statement task))))  goals)]

      (let [total-revision (reduce (fn [a b] (if (non-overlapping-evidence? (:evidence a) (:evidence b))
                                               (revise a (project-eternalize-to (:occurrence a) b @nars-time) :goal)
                                               a))
                                   task (shuffle related-goals))]
        ;add revised task to bag
        (add-to-tasks state total-revision)
        ; check to see if revised or task is answer to quest and increase budget accordingly
        ;check whether it is fullfilled by belief and decrease budget accordingly
        (satisfaction-based-budget-change state total-revision beliefs)
        (answer-based-budget-change state (:task (first (b/get-by-id (:tasks @state) (get-task-id total-revision)))) quests)
        (try
          (best-operation-selection (filter #(= (:task-type %) :belief) (get-tasks state))
                                   (:task (first (b/get-by-id (:tasks @state) (get-task-id total-revision)))))
          (catch Exception e () #_(println "operator selector error")))
        ))

    ;best operation project goal to current time
    ; if above decision threshold then execute
    (let [projected-goals (map #(project-eternalize-to @nars-time % @nars-time)
                               (filter #(= (:statement %) (:statement task))
                                       (filter #(= (:task-type %) :goal) ;re-getting the goals because we also want our just added goal
                                               (conj tasks task))))] ;needs new task as well
     (if (not-empty projected-goals)
       (let [possible-operations (filter #(and (operation? (:statement %)) (execute? %) (= (:statement %) (:id @state))) projected-goals)
             operation (if (not-empty possible-operations)
                    (apply max-key confidence possible-operations)
                    nil)]
         (when (not= (:occurrence task) :eternal)
           (when-not (= nil operation)                      ;todo change to - when operation
            ;(println (str  "goal: " operation))
            (cast! (whereis :operator-executor) [:operator-execution-msg operation]))))))))
