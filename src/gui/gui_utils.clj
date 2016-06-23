(ns gui.gui-utils
  (:require [seesaw.core :refer :all]
            [gui.globals :refer :all]
            [co.paralleluniverse.pulsar.actors :refer [whereis cast!]]
            [clojure.string :as str]
            [narjure.core :refer [start-timers shutdown run stop-timers]]))

(defn invert-color
  [[colr colg colb cola]]
  (if (= (deref invert-colors) true)
    [(- 255 colr) (- 255 colg) (- 255 colb) (if cola cola 255)]
    [colr colg colb (if cola cola 255)]))

(defn invert-comp [v]
  (if (= (deref invert-colors) true)
    (- 255 v)
    v))

(defn get-clipboard []
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))

(defn slurp-clipboard []
  (try
    (.getTransferData (.getContents (get-clipboard) nil) (java.awt.datatransfer.DataFlavor/stringFlavor))
    (catch Exception e nil)))

(defn spit-clipboard [text]
  (try
    (.setContents (get-clipboard) (java.awt.datatransfer.StringSelection. text) nil)
    (catch Exception e nil)))

(defn input-str [st]
  (println (str "input narsese " st))
  (doseq [x (str/split st #"\n")]
    (cast! (whereis :sentence-parser) [:narsese-string-msg x])))