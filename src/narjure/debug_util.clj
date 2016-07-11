(ns narjure.debug-util
  (:require [narjure.global-atoms :refer :all]
            [clojure.string :as str]))

(def debug-messages 21)

(defn limit-string
  "Limit a string to cnt elements"
  [st cnt]
  (subs st 0 (min (count st) cnt)))

(defn narsese-print
  "Prints out a statement in Narsese-representation."
  ([st]
    (narsese-print st false))
  ([st dictparent]
  (let [beautify (fn [co]
                   (case co
                     nil "nil"
                     pred-impl "=/>"
                     retro-impl "=\\>"
                     ext-inter "&"
                     int-dif "~"
                     ext-image "/"
                     int-image "\\"
                     conj "&&"
                     seq-conj "&/"
                     (let [outp (str co)]
                       (if (clojure.string/starts-with? outp "op_")
                         (clojure.string/replace outp "op_" "^")
                         outp))))]
    (if (coll? st)
      (let [isvector (vector? st)
            isdict (map? st)
            cop (first st)
            [left right] (case cop
                           ext-set ["{" "}"]
                           int-set ["[" "]"]
                           --> ["<" ">"]
                           <-> ["<" ">"]
                           ==> ["<" ">"]
                           pred-impl ["<" ">"]
                           =|> ["<" ">"]
                           retro-impl ["<" ">"]
                           <=> ["<" ">"]
                           <|> ["<" ">"]
                           </> ["<" ">"]
                           ["(" ")"])
            syll-cop ['--> '<-> '==> '=|>
                      'pred-impl 'retro-impl
                      '<=> '<|> '</>]
            seperator (if (or dictparent
                              (some #{cop} syll-cop)
                              (not isvector))
                        " "
                        ",")
            infixprint (if (some #{cop} syll-cop)
                         [(second st) (first st) (nth st 2)]
                         st)
            var-and-ival (fn [st]
                           (if (= (first st) :interval)
                             [(str "i" (second st))]
                             (if (= (first st) 'dep-var)
                               [(str "#" (second st))]
                               (if (= (first st) 'ind-var)
                                 [(str "$" (second st))]
                                 (if (= (first st) 'qu-var)
                                   [(str "?" (second st))]
                                   st)))))
            ivar-val (var-and-ival infixprint)
            [leftres rightres] (if (and (= ivar-val infixprint)
                                        (not dictparent))
                                 [left right]
                                 ["" ""])
            res (if (or (= (first ivar-val) 'ext-set)
                        (= (first ivar-val) 'int-set))
                  (rest ivar-val)
                  ivar-val)]
        (str leftres
             (apply str (for [x res]
                          (if (= x (first res))
                            (narsese-print x isdict)
                            (str seperator (narsese-print x isdict)))))
             rightres))
      (str (beautify st))))))

(defn debuglogger
  "Debuglogger as used by all actor components mainly for (Lense) display purposes."
  ([display message]
    (debuglogger (atom "") display message))
  ([filter display message]
   (if (> debug-messages 0)
     (swap! display (fn [d] (let [msg (narsese-print message)]
                              (if (every? (fn [x] (.contains msg x))
                                          (str/split (deref filter) #"\n"))
                                (if (< (count d) debug-messages)
                                 (conj d [(limit-string msg 750) "§"])
                                 (conj (drop-last d) [(limit-string msg 750) "§"]))
                                d)))))))

(defn punctuation-print
  "Print the punctuation of a task in ASCII representation."
  [task-type]
  (case task-type
    :goal "!"
    :quest "@"
    :question "?"
    :belief "."))

(defn task-to-narsese
  "Convert a task to Narsese representation."
  [task]
  (let [time-print (fn [occurrence]
                     (if (= occurrence :eternal)
                       ""
                       (str ":|" (- occurrence @nars-time) "|:")))
        truth-print (fn [truth]
                      (if (= truth nil)
                        ""
                        (str "%" (first truth) ";" (second truth) "%")))]
    (str (narsese-print (:statement task))
         (punctuation-print (:task-type task))
         " "
         (time-print (:occurrence task))
         " "
         (truth-print (:truth task)))))

(defn output-task
  "Output a task into the display."
  [type task]
  (let [type-print (fn [t] t)]
    (debuglogger output-search output-display (str (type-print type)
                                                   " "
                                                   (task-to-narsese task)))))

(defn get-solution-id
  "The ID that makes solutions unique for output."
  [task]
  [(:statement task) (:task-type task) (:occurrence task) (:truth task)])

(defn user?
  "Is the task an input-task?"
  [task]
  (= (:source task) :input))

(def max-qu-track 50)
(defn potential-output-answer
  "Potentially output the answer to a task, if it wasn't already."
  [state task-id task solution]
  (when (user? task)
    (let [solution-id (get-solution-id solution)]
     (when (not (some #{[task-id solution-id]} @last-qu-answers))
       (reset! last-qu-answers (concat [[task-id solution-id]] @last-qu-answers))
       (while (> (count @last-qu-answers) max-qu-track)
         (reset! last-qu-answers (drop-last @last-qu-answers)))
       (doseq [f @answer-handlers]
         (f task solution))
       (output-task [:answer-to (str (narsese-print (:statement task)) (punctuation-print (:task-type task)) #_" c: " #_concept-id)] solution)))))

(defn conditionalprint
  "For debugging purposes, allowing to add print statements for specific concepts."
  [state st stru]
  (when (= (:id @state) st)
    (println stru)))