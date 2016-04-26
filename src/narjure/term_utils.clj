(ns narjure.term_utils)

(defn interval?                                             
  "Is the term an interval?"
  [content]
  (and (sequential? content) (= (first content) 'interval)))

(defn compound?
  "Is the term a compound term?"
  [content]
  (and (sequential? content) (not= (first content) 'interval)))

(defn syntactic-complexity
  "Calculates the syntactic complexity of a content term,
  for example (| (& a b) c) has complexity 5"
  [content]
  (if (compound? content)
    (reduce + (map syntactic-complexity content))
    1))

(defn termlink-subterms
  "Extract the termlink relevant subterms of the term up to 3 levels as demanded by the NAL rules"
  ([level content]
   (if (and (< level 3) (compound? content))
     (reduce set/union #{content} (map (partial termlink-subterms (+ level 1)) content))
     #{content}))
  ([content]
   (filter (fn [z] (and (not (contains? logic-ops z))
                        (not (interval? z))))
           (termlink-subterms 0 content))))