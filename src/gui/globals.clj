(ns gui.globals)

(def init-size 800)
(def input-string (atom (atom "")))
(def invert-colors (atom true))
(def prio-threshold (atom 0.0))
(def link-labels (atom false))
(def concept-filter (atom "_DELETE_ME_"))

(defn invert-color
  [[colr colg colb]]
  (if (= (deref invert-colors) true)
    [(- 255 colr) (- 255 colg) (- 255 colb)]
    [colr colg colb]))

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