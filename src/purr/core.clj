(ns purr.core
  (:gen-class))

(defn over-head
  [func coll]
  (cons
    (func (first coll))
    (rest coll)))

(defn cons-path [depth-0]
  (loop [func (fn [item coll] (cons item coll)) depth depth-0]
    (if (< depth 1)
      func
      (recur
        (fn [item coll]
          (over-head
            (partial func item)
            coll))
        (dec depth)))))

(defn insert-at [depth word stack]
  ((cons-path depth) word stack))

(defn finish-word [{:keys [chars depth stack] :as state}]
  (-> state
      (merge {:stack (if (> (count chars) 0)
                       (insert-at depth (read-string (clojure.string/join (reverse chars))) stack)
                       stack)
              :chars []})))

(def reverse-all
  (partial clojure.walk/postwalk
           (fn [form]
             (if (coll? form)
               (reverse form)
               form))))

(defn parse
  "Should be a recursive delimited regex or a legit parser,
    but proved surprisingly simple to just type out."
  [program]
  (->> program
       (reduce
         (fn [state char]
           (case char
             \space (finish-word state)
             \[ (-> state
                    (update :depth inc)
                    (update :stack
                            (partial insert-at (:depth state) [])))
             \] (-> state finish-word (update :depth dec))
             (update state :chars #(cons char %))))
         {:stack [] :chars [] :depth 0})
       ((fn [state]
          (-> state finish-word :stack)))
       reverse-all))

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

(defn run [program]
  (->> program
       parse
       (exec [] words)
       reverse
       (clojure.string/join " ")))

