(ns narjure.memory-management.concept
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [clojure.core.unify :refer [unifier]]
    [nal.term_utils :refer [syntactic-complexity]]
    [nal.deriver
     [truth :refer [w2c t-or t-and confidence frequency expectation revision]]
     [projection-eternalization :refer [project-eternalize-to]]]
    [narjure
     [global-atoms :refer :all]
     [bag :as b]
     [debug-util :refer :all]
     [control-utils :refer :all]
     [defaults :refer :all]
     [projection-utils :refer [max-statement-confidence-projected-to-now]]]
    [narjure.memory-management
     [concept-utils :refer :all]
     [termlink-utils :refer :all]]
    [narjure.memory-management.local-inference
     [local-inference-utils :refer [get-task-id get-tasks]]
     [belief-processor :refer [process-belief]]
     [goal-processor :refer [process-goal]]
     [quest-processor :refer [process-quest]]
     [question-processor :refer [process-question]]])
  (:refer-clojure :exclude [promise await]))

(def display (atom '()))
(def search (atom ""))

(defn task-handler
  [from [_ [task]]]
  (debuglogger search display ["task processed:" task])

  (refresh-termlinks task)

  ; check observable and set if necessary
  (when-not (:observable @state)
    ;(println "obs1")
    (let [{:keys [occurrence source]} task]
      (when (and (not= occurrence :eternal) (= source :input) (= (:statement task) (:id @state)))
        (set-state! (assoc @state :observable true)))))

  (case (:task-type task)
    :belief (process-belief state task 0)
    :goal (process-goal state task 0)
    :question (process-question state task)
    :quest (process-quest state task)))

(defn belief-request-handler
  ""
  [from [_ [task-concept-id termlink-strength task]]]
  ;todo get a belief which has highest confidence when projected to task time
  (try                                                      ;update termlinks at first
    #_(update-termlink (:statement task))          ;task concept here
    (catch Exception e (debuglogger search display (str "belief side termlink strength error " (.toString e)))))
  (try (let [tasks (get-tasks state)
             beliefs (filter #(and (= (:statement %) (:id @state))
                                   (= (:task-type %) :belief)) tasks)
             projected-belief-tuples (map (fn [z] [z (project-eternalize-to (:occurrence task) z @nars-time)]) beliefs)]
         (if (not-empty projected-belief-tuples)
           ;(println projected-beliefs)
           ;(println "not empty pb: " projected-beliefs)
           (let [#_[not-projected-belief belief] #_(apply max-key (comp confidence second) projected-belief-tuples)]
             #_(cast! (:general-inferencer @state) [:do-inference-msg [task not-projected-belief]])
             (doseq [belief beliefs]
               (debuglogger search display ["selected belief:" belief "§"])
               (cast! (:inference-request-router @state) [:do-inference-msg [task-concept-id (:id @state) termlink-strength task belief]])
             (try
               ;1. check whether belief matches by unifying the question vars in task
               (when (and (= (:task-type task) :question)
                          (some #{'qu-var} (flatten (:statement task)))
                          (question-unifies (:statement task) (:statement belief)))
                 ;2. if it unifies, check whether it is a better solution than the solution we have
                 (let [answer-fqual (fn [answer] (if (= nil answer)
                                                   0
                                                   (/ (expectation (:truth answer)) (syntactic-complexity (:statement answer)))))
                       newqual (answer-fqual (project-eternalize-to (:occurrence task) belief @nars-time))
                       oldqual (answer-fqual (project-eternalize-to (:occurrence task) (:solution task) @nars-time))] ;PROJECT!!
                   (when (> newqual oldqual)
                     ;3. if it is a better solution, set belief as solution of task
                     (let [budget (:budget task)
                           new-prio (* (- 1.0 (expectation (:truth belief))) (first budget))
                           new-budget [new-prio (second budget) (nth budget 2)]
                           newtask (assoc task :solution belief :priority new-prio :budget new-budget)]
                       ;4. print our result
                       (output-task [:answer-to (str (narsese-print (:statement task)) "?")] (:solution newtask))
                       ;5. send answer-update-msg OLD NEW to the task concept so that it can remove the old task bag entry
                       ;and replace it with the one having the better solution. (reducing priority here though according to solution before send)
                       (when-let [{c-ref :ref} ((:elements-map @c-bag) (:statement task))]
                         (cast! c-ref [:solution-update-msg task newtask]))))))
               (catch Exception e (debuglogger search display (str "what-question error " (.toString e))))))

             ))
         ;dummy? belief as "empty" termlink belief selection for structural inference
         (let [belief {:statement (:id @state) :task-type :question :occurrence @nars-time :evidence '()}]
           (debuglogger search display ["selected belief:" belief "§"])
           (cast! (:inference-request-router @state) [:do-inference-msg [task-concept-id (:id @state) termlink-strength task belief]]))
         )
       (catch Exception e (debuglogger search display (str "belief request error " (.toString e))))))

(defn inference-request-handler
  ""
  [from message]
  (let [concept-state @state
        task-bag (:tasks concept-state)
        concept-priority (first (:budget @state))]
    ; and sending budget update message to concept mgr
    (when true
      (try
       (when (pos? (b/count-elements task-bag))
         (let [[el] (b/lookup-by-index task-bag (selection-fn (b/count-elements task-bag)))]
           (debuglogger search display ["selected inference task:" el])

           ;now search through termlinks, get the endpoint concepts, and form a bag of them
           (let [initbag (b/default-bag concept-max-termlinks)
                 resbag (reduce (fn [a b] (b/add-element a b)) initbag (for [[k v] (:termlinks @state)]
                                                                         {:priority #_(:priority (first (b/get-by-id @c-bag k)))
                                                                                         (t-or (first v)
                                                                                          (:priority (first (b/get-by-id @c-bag k))))
                                                                          :id       k}))
                 ;now select an element from this bag
                 [beliefconcept bag1] (b/get-by-index resbag (selection-fn (b/count-elements resbag)))]

             #_(doseq [[id beliefconcept] (:elements-map resbag)]
               (when-let [c-ref (get-ref-from-term (:id beliefconcept))]
                (cast! c-ref [:belief-request-msg [(:id @state) ((:termlinks @state) (:id beliefconcept)) (:task el)]])))
             ;and create a belief request message
             (set-state! (assoc @state :termlinks
                                       (assoc (:termlinks @state)
                                         (:id beliefconcept)
                                         (let [[p d] ((:termlinks @state) (:id beliefconcept))] ;apply forgetting for termlinks only on selection
                                           [(* p d) d]))))


             (when-let [c-ref (get-ref-from-term (:id beliefconcept))]
               (cast! c-ref [:belief-request-msg [(:id @state) ((:termlinks @state) (:id beliefconcept)) (:task el)]])
               ))))
       (catch Exception e (debuglogger search display (str "inference request error " (.toString e))))))))

(defn termlink-strengthen-handler
  "Strenghtens the termlink between two concepts or creates it if not existing.
   A link is {key value] where key is term and value is budget [priority durability]"
  [from [_ [term]]]
  (let [termlinks (if (:termlinks @state) (:termlinks @state) {})
        old-link-strength (termlinks term)
        new-link-strength (get-strengthened-termlink old-link-strength)]
    (set-state! (assoc-in @state [:termlinks term] new-link-strength))))

(defn concept-state-handler
  "Sends a copy of the actor state to requesting actor"
  [from _]
  (let [concept-state @state]
    (cast! from [:concept-state-msg concept-state])))

(defn set-concept-state-handler
  "set concept state to value passed in message"
  [from [_ new-state]]
  (set-state! (merge @state new-state)))

(defn concept-forget-handler
  "update cocnept budget"
  [from [_ new-state]]
  (forget-tasks)
  (forget-termlinks)
  (update-concept-budget @state @self))

(defn shutdown-handler
  "Processes :shutdown-msg and shuts down actor"
  [from msg]
  (set-state! {})
  (unregister!)
  (shutdown!))

(defn initialise
  "Initialises actor: registers actor and sets actor state"
  [name]
  (set-state! {:id                 name
               :quality            0.0
               :tasks              (b/default-bag max-tasks)
               :termlinks          {}
               :anticipation       nil
               :concept-manager    (whereis :concept-manager)
               :inference-request-router (whereis :inference-request-router)
               :last-forgotten     @nars-time
               :observable false}))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message"
  [from [type :as message]]
  (when-not (= type :concept-forget-msg) (debuglogger search display message))

  (when (b/exists? @c-bag (:id @state))                     ;check concept has not been removed first
      (case type
       :termlink-strenghten-msg (termlink-strengthen-handler from message)
       :task-msg (task-handler from message)
       :link-feedback-msg (link-feedback-handler from message)
       :belief-request-msg (belief-request-handler from message)
       :inference-request-msg (inference-request-handler from message)
       :concept-state-request-msg (concept-state-handler from message)
       :set-concept-state-msg (set-concept-state-handler from message)
       :solution-update-msg (solution-update-handler from message)
       :concept-forget-msg (concept-forget-handler from message)
       :shutdown (shutdown-handler from message)
       (debug (str "unhandled msg: " type))))

    (when (pos? debug-messages)
      ;(reset! lense-anticipations (:anticipation @state))
      (swap! lense-taskbags
             (fn [dic]
               (assoc dic (:id @state) (:tasks @state))))
      (swap! lense-termlinks
             (fn [dic]
               (assoc dic (:id @state) (:termlinks @state))))))

(defn concept [name]
  (gen-server
    (reify Server
      (init [_] (initialise name))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))