(ns nal.core
  (:require [nal.deriver.truth :as t]
            [nal.deriver :refer [generate-conclusions]]
            [nal.rules :as r]))

(defn choice
  "<a href=\"NAL-Specification.pdf#page=25\" style=\"text-decoration:none\"Truth formula</a>"
  [[f1 c1] [f2 c2]]
  (if (>= c1 c2) [f1 c1] [f2 c2]))

(defn inference
  "Apply these rules to task and belief which apply for the task-type."
  [{:keys [task-type] :as task} belief]
  (generate-conclusions (r/rules task-type) task belief))

(def revision t/revision)

(comment
  :shift-occurrence-forward                                 ;pre
  :shift-occurrence-backward                                 ;pre
  :linkage-temporal)
