(ns purr.parse
  (:require [instaparse.core :refer [parser]]))

(def purr-parser
  (parser
    "PROGRAM = TERM+;
     WORD = #'\\w[\\w\\d]*' ;
     STR = #'\"' QUOTED * #'\"' ;
     ESC_QUOTE = #'\\\"' ;
     QUOTED = #'[^\"]' | ESC_QUOTE ;
     BLOCK = '[' TERM* ']' ;
     NUM = #'\\-?\\d+(\\.\\d+)?' ;
     OP = '+' | '/' | '-' | '*' | '>' | '<' | '<=' | '>=' ;
     TERM = (WORD | BLOCK | STR | NUM | OP) #' ?' ; "))

(defn collapse [program]
  (clojure.walk/postwalk
    (fn [node]
      (if (coll? node)
        (let [[rule & value] node]
          (case rule
            :BLOCK
            (drop 1 (butlast value))
            :STR (clojure.string/join (drop 1 (butlast (flatten value))))
            :WORD (read-string (first value))
            :NUM (read-string (first value))
            :OP (read-string (first value))
            :PROGRAM value
            (first value)))
        node))
    program))

(defn parse [code]
  (-> code purr-parser collapse))

