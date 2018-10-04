(ns purr.core
  (:require [purr.parse :refer [parse]])
  (:gen-class))

(defn binary [op [b a & rest]]
  [[(op a b)] rest])

(defn resolve-expr
  "Run a block if necessary"
  [expr stack]
  [(if (coll? expr)
     expr
     [expr])
   stack])

(def words
  {"+"      (partial binary +)
   "-"      (partial binary -)
   "*"      (partial binary *)
   "/"      (partial binary /)
   ">"      (partial binary >)
   "<"      (partial binary <)
   "<="     (partial binary <=)
   ">="     (partial binary >=)
   "!="     (partial binary not=)
   "dup"    (fn [stack] [[(first stack)] stack])
   "dup2"   (fn [stack] [(reverse (take 2 stack)) stack])
   "drop"   (fn [stack] [[] (rest stack)])
   "swap"   (fn [stack] [(take 2 stack)
                         (drop 2 stack)])
   "over"   (fn [stack] [[(second stack)] stack])
   "rot"    (fn [stack] [[(second stack) (first stack) (nth stack 2)] (drop 3 stack)])
   "apply"  (fn [stack] [
                         (take 2 (first stack))
                         (rest stack)])
   "dip"    (fn [stack]
              [[(second stack)]
               (cons (ffirst stack) (drop 2 stack))])
   "dip2"   (fn [stack]
              [(concat (first stack) (reverse (take 2 (drop 1 stack))))
               (drop 3 stack)])
   "if"     (fn [[orelse ifso check & rest]]
              (resolve-expr (if check ifso orelse)
                            rest))
   ; TODO this should be provided in some kind of stdlib, since it's not a primitive
   "repeat" (fn [stack]
              [
               (parse "dup 0 > [1 - swap dup dip2 swap repeat] [drop drop] if")
               stack])})

(defn run-word [word lib stack]
  (let [task (get lib word)]
    (task stack)))

(defn exec [stack-0 lib exprs-0]
  (loop [stack stack-0 exprs exprs-0]
    (let [expr (first exprs)]
      (cond
        (nil? expr) stack
        (symbol? expr)
        (let [[newexprs newstack] (run-word (str expr) lib stack)]
          (recur newstack (concat newexprs (rest exprs))))
        (or (= expr true) (= expr false) (number? expr) (string? expr) (char? expr) (coll? expr))
        (recur (cons expr stack) (rest exprs))
        true (throw (Exception. (str "unrecognized: "
                                     expr " of " (type expr))))))))

(def eager
  (partial
    clojure.walk/postwalk
    (fn [form]
      (if (coll? form)
        (vec form)
        form))))

(defn run [program]
  (->> program
       parse
       (exec [] words)
       eager
       reverse
       (clojure.string/join " ")))

