(ns intcode
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all])
  (:import [java.util.concurrent LinkedBlockingQueue BlockingQueue]))

(def ops {1 +
          2 *})

(defn decode [instruction]
  [(int (mod (/ instruction 1)    100))
   (int (mod (/ instruction 100)   10))
   (int (mod (/ instruction 1000)  10))
   (int (mod (/ instruction 10000) 10))])

(defn fetch [memory reg mode]
  (let [result (case mode
                 0 (get memory reg)
                 1 reg)]
    ;(printf "Reading mode=%d reg=%d res=%d\n" mode reg result)
    result))

(decode 1002)

(defn debug [state]
  (doseq [mem (take 16 state)]
    (printf "%6d" mem))
  (println))

(defn run [program input]
  (loop [state program
         pc 0
         input input
         output []]
    (let [[instruction p1 p2 p3] (drop pc state)
          [op-code m1 m2 _m3] (decode instruction)]
      ;(printf "op=%d m1=%d m2=%d m3=%d\n" op-code m1 m2 m3)
      ;(debug state)
      (case op-code
        99 output
        (1 2)
        (let [result ((get ops op-code)
                      (fetch state p1 m1)
                      (fetch state p2 m2))
              ;_ (printf "addmul op=%s a=%d b=%d c[dest]=%d result=%d\n" (get ops op-code) p1 p2 p3 result)
              next-state (assoc state p3 result)]
          (recur next-state (+ pc 1 3) input output))

        ;; Opcode 3 takes a single integer as input and saves it to the position
        ;; given by its only parameter. For example, the instruction 3,50 would
        ;; take an input value and store it at address 50.
        3
        (recur (assoc state p1 (first input)) (+ pc 1 1) (rest input) output)

        ;; Opcode 4 outputs the value of its only parameter. For example, the
        ;; instruction 4,50 would output the value at address 50.
        4
        (recur state (+ pc 1 1) input (conj output (get state p1)))

        ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
        ;; the instruction pointer to the value from the second parameter.
        ;; Otherwise, it does nothing.
        5
        (recur state (if (not (zero? (fetch state p1 m1)))
                       (fetch state p2 m2)
                       (+ pc 1 2)) input output)

        ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
        ;; instruction pointer to the value from the second parameter. Otherwise,
        ;; it does nothing.
        6
        (recur state (if (zero? (fetch state p1 m1))
                       (fetch state p2 m2)
                       (+ pc 1 2)) input output)

        ;; Opcode 7 is less than: if the first parameter is less than the second
        ;; parameter, it stores 1 in the position given by the third parameter.
        ;; Otherwise, it stores 0.
        7
        (recur (assoc state p3 ;(fetch state p3 m3)
                      (if (< (fetch state p1 m1)
                             (fetch state p2 m2))
                        1
                        0))
               (+ pc 1 3)
               input output)

        ;; Opcode 8 is equals: if the first parameter is equal to the second
        ;; parameter, it stores 1 in the position given by the third parameter.
        ;; Otherwise, it stores 0.
        8
        (recur (assoc state p3
                      (if (= (fetch state p1 m1)
                             (fetch state p2 m2))
                        1
                        0))
               (+ pc 1 3)
               input output)))))

(defn ->int [s]
  (Integer/parseInt s 10))

(defn op-codes [input]
  (mapv (comp ->int str/trim) (str/split input #",")))

(op-codes "1,2,3")

(defn run-re-entrant [{:keys [memory pc input output status] :as foo}]
  (let [[instruction p1 p2 p3] (drop pc memory)
        [op-code m1 m2 _m3] (decode instruction)]
      ;(printf "op=%d m1=%d m2=%d m3=%d\n" op-code m1 m2 m3)
      ;(debug state)
    (case op-code
      99
      (assoc foo :status :done)

      (1 2)
      (let [result ((get ops op-code)
                    (fetch memory p1 m1)
                    (fetch memory p2 m2))]
        (assoc foo
               :memory (assoc memory p3 result)
               :pc (+ pc 1 3)))

      ;; Opcode 3 takes a single integer as input and saves it to the position
      ;; given by its only parameter. For example, the instruction 3,50 would
      ;; take an input value and store it at address 50.
      3
      (if (first input)
        (assoc foo
               :input (rest input)
               :memory (assoc memory p1 (first input))
               :pc (+ pc 1 1))

        foo)

      ;; Opcode 4 outputs the value of its only parameter. For example, the
      ;; instruction 4,50 would output the value at address 50.
      4
      (assoc foo
             :pc (+ pc 1 1)
             :output (conj output (get memory p1)))

        ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
        ;; the instruction pointer to the value from the second parameter.
        ;; Otherwise, it does nothing.
      5
      (assoc foo
             :pc (if (not (zero? (fetch memory p1 m1)))
                   (fetch memory p2 m2)
                   (+ pc 1 2)))

        ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
        ;; instruction pointer to the value from the second parameter. Otherwise,
        ;; it does nothing.
      6
      (assoc foo
             :pc (if (zero? (fetch memory p1 m1))
                   (fetch memory p2 m2)
                   (+ pc 1 2)))

        ;; Opcode 7 is less than: if the first parameter is less than the second
        ;; parameter, it stores 1 in the position given by the third parameter.
        ;; Otherwise, it stores 0.
      7
      (assoc foo
             :memory (assoc memory p3
                            (if (< (fetch memory p1 m1)
                                   (fetch memory p2 m2))
                              1
                              0))
             :pc (+ pc 1 3))

        ;; Opcode 8 is equals: if the first parameter is equal to the second
        ;; parameter, it stores 1 in the position given by the third parameter.
        ;; Otherwise, it stores 0.
      8
      (assoc foo
             :memory (assoc memory p3
                            (if (= (fetch memory p1 m1)
                                   (fetch memory p2 m2))
                              1
                              0))
             :pc (+ pc 1 3)))))

(defn run2 [program input]
  (loop [state {:memory program
                :pc 0
                :status :running
                :input input
                :output []}]
    (let [next (run-re-entrant state)]
      (if (= (:status next) :done)
        (:output next)
        (recur next)))))
