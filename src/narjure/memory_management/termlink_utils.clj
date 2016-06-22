(ns narjure.memory-management.termlink-utils
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.global-atoms :refer :all]
    [narjure.bag :as b]
    [clojure.core.unify :refer [unifier]]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.defaults :refer :all]
    [nal.term_utils :refer [syntactic-complexity]]
    [narjure.memory-management.local-inference.local-inference-utils :refer [get-task-id get-tasks]]
    [narjure.memory-management.local-inference.belief-processor :refer [process-belief]]
    [narjure.memory-management.local-inference.goal-processor :refer [process-goal]]
    [narjure.memory-management.local-inference.quest-processor :refer [process-quest]]
    [narjure.memory-management.local-inference.question-processor :refer [process-question]]
    [nal.deriver.truth :refer [w2c t-or t-and confidence frequency expectation revision]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))

(defn get-linkable-terms
  "disallow linking to itself"
  [task]
  (filter #(not= % (:id @state)) (:terms task)))

(defn get-existing-terms-from-links
  "only use links whose target concept still exists"
  [links]
  (filter #(b/exists? @c-bag %) (keys links)))

(defn refresh-termlinks [task]
  ""
  ; :termlinks {term [budget]}
  (let [newtermlinks (merge (apply merge
                                   (for [tl (get-linkable-terms task)] ;prefer existing termlinks strengths
                                     {tl  termlink-default-budget}))
                            (:termlinks @state))
        valid-links (select-keys newtermlinks (get-existing-terms-from-links newtermlinks))];only these keys which exist in concept bag
    (set-state! (merge @state {:termlinks valid-links}))))