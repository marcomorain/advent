(ns day11
  (:require
   [core]
   [clojure.repl :refer :all]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [intcode :as intcode])
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.awt Color]))

(def up    [0  -1])
(def down  [0   1])
(def left  [-1  0])
(def right [ 1  0])

(def turn-left 0)
(def turn-right 1)
(def black 0)
(def white 1)

(def movement {turn-left {up left
                          left down
                          down right
                          right up}
               turn-right {up right
                           right down
                           down left
                           left up}})

(get-in movement [turn-right up])

(defn next-steps [[color turn] {:keys [position panels direction] :as _brain}]
  (let [next-direction (get-in movement [turn direction])
        next-position (mapv + position next-direction)]
    {:position next-position
     :panels (assoc panels position color)
     :direction next-direction
     :color (get panels next-position black)}))

(def initial-brain {:panels {}
                    :position [0 0]
                    :direction up
                    :color black})

(defn robot [program]
  (loop [state (intcode/init-state program [0])
         brain initial-brain
         output-read 0]
    (let [next-state (intcode/run-re-entrant state)
          output-available (drop output-read (:ouput state))]
      ;(prn (select-keys next [:pc :input :ouput]) position direction output (count panels))
      ;(clojure.pprint/pprint state)
      (case (:status next-state)
        :done (:panels brain)
        :running (if (< 1 (count output-available))
                   (recur next-state
                          (next-steps output-available brain)
                          (+ 2 output-read))
                   (recur next-state
                          brain
                          output-read))))))


(def input (slurp "input/day11.txt"))

(count (robot (intcode/op-codes input)))
;(map + [1 1] [3 5])

(deftest part1

  (let [step-0 (assoc initial-brain
                      :position [3 3])
        step-1 {:position [2 3]
                :panels {[3 3] white}
                :direction left
                :color black}
        step-2 {:position [2 4]
                :panels {[3 3] white
                         [2 3] black}
                :direction down
                :color black}
        step-3 {:position [3 3]
                :panels {[3 3] white
                         [2 3] black
                         [2 4] white
                         [3 4] white}
                :direction up
                :color white}
        step-4 {:position [3 2]
                :panels {[3 3] black
                         [4 3] white
                         [4 2] white
                         [2 3] black
                         [2 4] white
                         [3 4] white}
                :direction left
                :color black}]

    (is (= step-1
           (next-steps [white turn-left] step-0)))

    (is (= step-2
           (next-steps [black turn-left] step-1)))
    
    (is (= step-3
           (->> step-2
                (next-steps [white turn-left])
                (next-steps [white turn-left]))))
    
    (is (= step-4
           (->> step-3
                (next-steps [black turn-right])
                (next-steps [white turn-left])
                (next-steps [white turn-left]))))))

(run-tests)
