(ns narjure.projection-utils
  (:require
    [nal.deriver
     [projection-eternalization :refer [project-eternalize-to]]]
    [narjure
     [global-atoms :refer :all]]))


(defn max-statement-confidence-projected-to-now [state task-type]
  (let [fil (filter (fn [z] (and (= (:task-type (:task (second z))) task-type)
                                 (= (:statement (:task (second z))) (:id state))))
                    (:elements-map (:tasks state)))]
    (if (not= fil [])
      (project-eternalize-to @nars-time
                             (:task (second (apply max-key
                                                   (fn [y]
                                                     (second (:truth
                                                               (project-eternalize-to
                                                                 @nars-time
                                                                 (:task (second y))
                                                                 @nars-time))))
                                                   fil)))
                             @nars-time)
      nil)))