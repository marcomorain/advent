(ns intcode
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.test :refer :all])
  (:import [java.util.concurrent LinkedBlockingQueue BlockingQueue]))

(def ops {1 +
          2 *})

(defn decode [instruction]
  [(int (mod (/ instruction 1)    100))
   (int (mod (/ instruction 100)   10))
   (int (mod (/ instruction 1000)  10))
   (int (mod (/ instruction 10000) 10))])

(defn fetch [memory base-offset reg mode]
  (let [result (case mode
                 0 (get memory reg 0)
                 1 reg
                 2 (get memory (+ base-offset reg) 0))]
    ;(printf "Reading mode=%d reg=%d res=%d\n" mode reg result)
    result))

(defn store [memory base-offset reg mode val]
  (let [address (case mode
                  0 reg
                  2 (+ base-offset reg))
        growth (- address (count memory))]
    (assoc (if (pos? growth)
             (into [] (concat memory (repeat growth 0)))
             memory) address val)))

(decode 21107)

(defn debug [state]
  (doseq [mem (take 16 state)]
    (printf "%6d" mem))
  (println))

(defn ->int [s]
  (Integer/parseInt s 10))

(defn op-codes [input]
  (mapv (comp ->int str/trim) (str/split input #",")))

(op-codes "1,2,3")

(s/def ::memory (s/coll-of int? :kind sequential?))
(s/def ::pc nat-int?)
(s/def ::base-offset nat-int?)
(s/def ::input (s/coll-of int? :kind sequential?))
(s/def ::output (s/coll-of int? :kind sequential?))
(s/def ::status #{:done :running})
(s/def ::state (s/keys :req-un [::memory ::pc ::input ::output ::status]))

(defn valid-state? [state]
  (if (s/valid? ::state state)
    true
    (throw (ex-info "spec error" (s/explain-data ::state state)))))

(defn run-re-entrant [{:keys [memory pc input output base-offset status] :as state}]
  {:pre  [(valid-state? state)]
   :post [(valid-state? %)]}
  (let [[instruction p1 p2 p3] (drop pc memory)
        [op-code m1 m2 m3] (decode instruction)]
      ;(printf "op=%d m1=%d m2=%d m3=%d\n" op-code m1 m2 m3)
      ;(debug memory)
    (try

      (case op-code
      ;; 99 means that the program is finished and should immediately halt.
        99
        (assoc state :status :done)

      ;; Opcode 1 adds together numbers read from two positions and stores the
      ;; result in a third position. The three integers immediately after the
      ;; opcode tell you these three positions - the first two indicate the
      ;; positions from which you should read the input values, and the third
      ;; indicates the position at which the output should be stored.
      ;;  Opcode 2 works exactly like opcode 1, except it multiplies the two 
      ;; inputs instead of adding them. Again, the three integers after the
      ;; opcode indicate where the inputs and outputs are, not their values.
        (1 2)
        (assoc state
               :memory (store memory base-offset p3 m3 ((get ops op-code)
                                                        (fetch memory base-offset p1 m1)
                                                        (fetch memory base-offset p2 m2)))
               :pc (+ pc 1 3))

      ;; Opcode 3 takes a single integer as input and saves it to the position
      ;; given by its only parameter. For example, the instruction 3,50 would
      ;; take an input value and store it at address 50.
        3
        (do
          (printf "op=%d i=%s p1=%s m1=%d pc=%d offset=%s\n" op-code input p1 m1 pc base-offset)
          (debug memory)
          (if (first input)
            (assoc state
                   :input (rest input)
                   :memory (store memory base-offset p1 m1 (first input))
                   :pc (+ pc 1 1))

            state))

      ;; Opcode 4 outputs the value of its only parameter. For example, the
      ;; instruction 4,50 would output the value at address 50.
        4
        (assoc state
               :pc (+ pc 1 1)
               :output (conj output (fetch memory base-offset p1 m1)))

      ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
      ;; the instruction pointer to the value from the second parameter.
      ;; Otherwise, it does nothing.
        5
        (assoc state
               :pc (if (not (zero? (fetch memory base-offset p1 m1)))
                     (fetch memory base-offset p2 m2)
                     (+ pc 1 2)))

      ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
      ;; instruction pointer to the value from the second parameter. Otherwise,
      ;; it does nothing.
        6
        (assoc state
               :pc (if (zero? (fetch memory base-offset p1 m1))
                     (fetch memory base-offset p2 m2)
                     (+ pc 1 2)))

      ;; Opcode 7 is less than: if the first parameter is less than the second
      ;; parameter, it stores 1 in the position given by the third parameter.
      ;; Otherwise, it stores 0.
        7
        (assoc state ;; todo
               :memory (store memory base-offset p3 m3
                              (if (< (fetch memory base-offset p1 m1)
                                     (fetch memory base-offset p2 m2))
                                1
                                0))
               :pc (+ pc 1 3))

      ;; Opcode 8 is equals: if the first parameter is equal to the second
      ;; parameter, it stores 1 in the position given by the third parameter.
      ;; Otherwise, it stores 0.
        8
        (assoc state
               :memory (store memory base-offset p3 m3
                              (if (= (fetch memory base-offset p1 m1)
                                     (fetch memory base-offset p2 m2))
                                1
                                0))
               :pc (+ pc 1 3))

      ;; Opcode 9 adjusts the relative base by the value of its only parameter.
      ;; The relative base increases (or decreases, if the value is negative)
      ;; by the value of the parameter.
        9
        (do
          (printf "adjusting offset offset=%d p=%d m=%d\n" base-offset p1 m1)
          (assoc state
                 :base-offset (+ base-offset (fetch memory base-offset p1 m1))
                 :pc (+ pc 1 1))))
      (catch Exception e
        (throw (ex-info "error" state e))))))

(defn run*
  "Return the final state."
  [program input]
  (loop [state {:memory program
                :pc 0
                :base-offset 0
                :status :running
                :input input
                :output []}]
    (let [next (run-re-entrant state)]
      (if (= (:status next) :done)
        next
        (recur next)))))

(defn run [program input]
  (:output (run* program input)))

(run-all-tests)
