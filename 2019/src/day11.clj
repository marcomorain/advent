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
(def right [1  0])

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

(defn robot [program brain]
  (loop [state (intcode/init-state program [0])
         brain brain
         output-read 0]
    (let [input [(:color brain)]
          next-state (intcode/run-re-entrant (assoc state :input input))
          output-available (drop output-read (:output state))]
      (case (:status next-state)
        :done brain
        :running (if (< 1 (count output-available))
                   (recur next-state
                          (next-steps output-available brain)
                          (+ 2 output-read))
                   (recur next-state
                          brain
                          output-read))))))

(< 1 1)

(def input (slurp "input/day11.txt"))



(def result (robot (intcode/op-codes input)
                   {:panels {[0 0] white}
                    :position [0 0]
                    :direction up
                    :color white}))
                   

(count (:panels result))
;(map + [1 1] [3 5])

(defn inputs [steps]
  (into []
        (concat (mapcat vector (repeat 104) steps)
                [99])))

(is (= [104 1 104 2 104 3 99]
       (inputs [1 2 3])))

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
                (next-steps [white turn-left]))))


    (is (= step-4
           (robot 
            [104 1 104 0 3 0
             104 0 104 0 3 1
             104 1 104 0 3 2
             104 1 104 0 3 3
             104 0 104 1 3 4
             104 1 104 0 3 5
             104 1 104 0 3 6
             99]
            step-0)))))


(run-tests)


       
       (apply max (map second (keys (:panels result))))


(let [img (BufferedImage. 100 100 BufferedImage/TYPE_INT_RGB)]
  (doseq [[[x y] col] (:panels result)]
    (.setRGB img x y (.getRGB (case col
                                0 Color/BLACK
                                1 Color/WHITE))))
  
  (ImageIO/write img "png", (io/file "img.png")))
