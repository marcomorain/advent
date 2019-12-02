(ns day2
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]))

(def input (slurp "input/day2.txt"))

(def op-codes (mapv #(Integer/parseInt (str/trim %) 10) (str/split input #",")))

(def ops {1 +
          2 *})


(defn run [program]
  (loop [state program
         [op-code & r] program]
    (case op-code
      99 state
      (1 2)
      (let [[p1 p2 p3 & next-ops] r
            next-state (assoc state p3 ((get ops op-code) (get state p1)
                                                          (get state p2)))]
        (recur next-state next-ops)))))


(defn program [noun verb]
  (assoc op-codes 1 noun 2 verb))

(get
 (into {} (for [noun (range 0 100)
                verb (range 0 100)]
            [(first (run (program noun verb))) (+ verb (* 100 noun))]))
 19690720)
