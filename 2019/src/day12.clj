(ns day12
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

(defn parse [^String s]
  (mapv (fn [pos]
          {:pos pos :vel [0 0 0]})
        (for [line (->> s
                        (str/split-lines)
                        (map str/trim))]
          (->> line
               (re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
               (rest)
               (mapv #(Long/parseLong %))))))

(def input
  (parse "<x=1, y=2, z=-9>
<x=-1, y=-9, z=-4>
<x=17, y=6, z=8>
<x=12, y=4, z=2>
"))


(defn gravity* [this other]
  (cond
    (< this other) -1
    (> this other) 1
    :else 0))

;; To apply gravity, consider every pair of moons. On each axis (x, y, and z), the
;; velocity of each moon changes by exactly +1 or -1 to pull the moons together. For
;; example, if Ganymede has an x position of 3, and Callisto has a x position of 5,
;; then Ganymede's x velocity changes by +1 (because 5 > 3) and Callisto's x velocity
;; changes by -1 (because 3 < 5) . However, if the positions on a given axis are the
;; same, the velocity on that axis does not change for that pair of moons.
;; Once all gravity has been applied, apply velocity: simply add the velocity of
;; each moon to its own position. For example, if Europa has a position of x=1, y=2, z=3
;; and a velocity of x=-2, y=0,z=3, then its new position would be x=-1, y=2, z=6.
;; This process does not modify the velocity of any moon.

(defn simulate [moons {:keys [pos vel] :as this}]
  (let [gravity (apply mapv + (for [other moons]
                                (mapv gravity*
                                      (:pos other)
                                      pos)))
        new-vel (mapv + gravity vel)
        new-pos (mapv + pos new-vel)]
    (assoc this
           :pos new-pos
           :vel new-vel)))

(simulate [{:pos [0 0 0] :vel [0 0 0]} {:pos [2 2 2] :vel [0 0 0]}]
          {:pos [0 0 0] :vel [0 0 0]})

(simulate input (first input))

(is (= 1 (gravity* 5 3)))
(is (= -1 (gravity* 3 5)))
(is (= 0 (gravity* 5 5)))

(defn simulate-all [moons]
  (mapv (partial simulate moons) moons))

(def europa {:pos [1 2 3]
             :vel [-2 0 3]})

(is (= [{:vel [-2 0 3]
         :pos [-1 2 6]}]
       (simulate-all [europa])))

(defn example->data [example]
  (str/join "\n"
            (for [line (re-seq
                        #"pos=<x=( *-?\d+), y=( *-?\d+), z=( *-?\d+)>, vel=<x=( *-?\d+), y=( *-?\d+), z=( *-?\d+)>"
                        example)]
              (apply format "{:pos [%3s %3s %3s] :vel [%3s %3s %3s]}" (rest line)))))

(println (example->data
          "
pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>
pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>
pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>
pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>
pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>
pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>
pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2>
pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>
pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>
pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
"))

(def example-1
  (partition
   4
   [{:pos [-1   0  2] :vel [0  0  0]}
    {:pos [2 -10 -7] :vel [0  0  0]}
    {:pos [4  -8  8] :vel [0  0  0]}
    {:pos [3   5 -1] :vel [0  0  0]}
    {:pos [2 -1  1] :vel [3 -1 -1]}
    {:pos [3 -7 -4] :vel [1  3  3]}
    {:pos [1 -7  5] :vel [-3  1 -3]}
    {:pos [2  2  0] :vel [-1 -3  1]}
    {:pos [5 -3 -1] :vel [3 -2 -2]}
    {:pos [1 -2  2] :vel [-2  5  6]}
    {:pos [1 -4 -1] :vel [0  3 -6]}
    {:pos [1 -4  2] :vel [-1 -6  2]}
    {:pos [5 -6 -1] :vel [0 -3  0]}
    {:pos [0  0  6] :vel [-1  2  4]}
    {:pos [2  1 -5] :vel [1  5 -4]}
    {:pos [1 -8  2] :vel [0 -4  0]}
    {:pos [2 -8  0] :vel [-3 -2  1]}
    {:pos [2  1  7] :vel [2  1  1]}
    {:pos [2  3 -6] :vel [0  2 -1]}
    {:pos [2 -9  1] :vel [1 -1 -1]}
    {:pos [-1 -9  2] :vel [-3 -1  2]}
    {:pos [4  1  5] :vel [2  0 -2]}
    {:pos [2  2 -4] :vel [0 -1  2]}
    {:pos [3 -7 -1] :vel [1  2 -2]}
    {:pos [-1 -7  3] :vel [0  2  1]}
    {:pos [3  0  0] :vel [-1 -1 -5]}
    {:pos [3 -2  1] :vel [1 -4  5]}
    {:pos [3 -4 -2] :vel [0  3 -1]}
    {:pos [2 -2  1] :vel [3  5 -2]}
    {:pos [1 -4 -4] :vel [-2 -4 -4]}
    {:pos [3 -7  5] :vel [0 -5  4]}
    {:pos [2  0  0] :vel [-1  4  2]}
    {:pos [5  2 -2] :vel [3  4 -3]}
    {:pos [2 -7 -5] :vel [1 -3 -1]}
    {:pos [0 -9  6] :vel [-3 -2  1]}
    {:pos [1  1  3] :vel [-1  1  3]}
    {:pos [5  3 -4] :vel [0  1 -2]}
    {:pos [2 -9 -3] :vel [0 -2  2]}
    {:pos [0 -8  4] :vel [0  1 -2]}
    {:pos [1  1  5] :vel [0  0  2]}
    {:pos [2  1 -3] :vel [-3 -2  1]}
    {:pos [1 -8  0] :vel [-1  1  3]}
    {:pos [3 -6  1] :vel [3  2 -3]}
    {:pos [2  0  4] :vel [1 -1 -1]}]))


(println (example->data

          "After 0 steps:
pos=<x= -8, y=-10, z=  0>, vel=<x=  0, y=  0, z=  0>
pos=<x=  5, y=  5, z= 10>, vel=<x=  0, y=  0, z=  0>
pos=<x=  2, y= -7, z=  3>, vel=<x=  0, y=  0, z=  0>
pos=<x=  9, y= -8, z= -3>, vel=<x=  0, y=  0, z=  0>

After 10 steps:
pos=<x= -9, y=-10, z=  1>, vel=<x= -2, y= -2, z= -1>
pos=<x=  4, y= 10, z=  9>, vel=<x= -3, y=  7, z= -2>
pos=<x=  8, y=-10, z= -3>, vel=<x=  5, y= -1, z= -2>
pos=<x=  5, y=-10, z=  3>, vel=<x=  0, y= -4, z=  5>

After 20 steps:
pos=<x=-10, y=  3, z= -4>, vel=<x= -5, y=  2, z=  0>
pos=<x=  5, y=-25, z=  6>, vel=<x=  1, y=  1, z= -4>
pos=<x= 13, y=  1, z=  1>, vel=<x=  5, y= -2, z=  2>
pos=<x=  0, y=  1, z=  7>, vel=<x= -1, y= -1, z=  2>

After 30 steps:
pos=<x= 15, y= -6, z= -9>, vel=<x= -5, y=  4, z=  0>
pos=<x= -4, y=-11, z=  3>, vel=<x= -3, y=-10, z=  0>
pos=<x=  0, y= -1, z= 11>, vel=<x=  7, y=  4, z=  3>
pos=<x= -3, y= -2, z=  5>, vel=<x=  1, y=  2, z= -3>

After 40 steps:
pos=<x= 14, y=-12, z= -4>, vel=<x= 11, y=  3, z=  0>
pos=<x= -1, y= 18, z=  8>, vel=<x= -5, y=  2, z=  3>
pos=<x= -5, y=-14, z=  8>, vel=<x=  1, y= -2, z=  0>
pos=<x=  0, y=-12, z= -2>, vel=<x= -7, y= -3, z= -3>

After 50 steps:
pos=<x=-23, y=  4, z=  1>, vel=<x= -7, y= -1, z=  2>
pos=<x= 20, y=-31, z= 13>, vel=<x=  5, y=  3, z=  4>
pos=<x= -4, y=  6, z=  1>, vel=<x= -1, y=  1, z= -3>
pos=<x= 15, y=  1, z= -5>, vel=<x=  3, y= -3, z= -3>

After 60 steps:
pos=<x= 36, y=-10, z=  6>, vel=<x=  5, y=  0, z=  3>
pos=<x=-18, y= 10, z=  9>, vel=<x= -3, y= -7, z=  5>
pos=<x=  8, y=-12, z= -3>, vel=<x= -2, y=  1, z= -7>
pos=<x=-18, y= -8, z= -2>, vel=<x=  0, y=  6, z= -1>

After 70 steps:
pos=<x=-33, y= -6, z=  5>, vel=<x= -5, y= -4, z=  7>
pos=<x= 13, y= -9, z=  2>, vel=<x= -2, y= 11, z=  3>
pos=<x= 11, y= -8, z=  2>, vel=<x=  8, y= -6, z= -7>
pos=<x= 17, y=  3, z=  1>, vel=<x= -1, y= -1, z= -3>

After 80 steps:
pos=<x= 30, y= -8, z=  3>, vel=<x=  3, y=  3, z=  0>
pos=<x= -2, y= -4, z=  0>, vel=<x=  4, y=-13, z=  2>
pos=<x=-18, y= -7, z= 15>, vel=<x= -8, y=  2, z= -2>
pos=<x= -2, y= -1, z= -8>, vel=<x=  1, y=  8, z=  0>

After 90 steps:
pos=<x=-25, y= -1, z=  4>, vel=<x=  1, y= -3, z=  4>
pos=<x=  2, y= -9, z=  0>, vel=<x= -3, y= 13, z= -1>
pos=<x= 32, y= -8, z= 14>, vel=<x=  5, y= -4, z=  6>
pos=<x= -1, y= -2, z= -8>, vel=<x= -3, y= -6, z= -9>

After 100 steps:
pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>
pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>
pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>
pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>"))

(def example-2
  (partition
   4
   [{:pos [-8 -10   0] :vel [0   0   0]}
    {:pos [5   5  10] :vel [0   0   0]}
    {:pos [2  -7   3] :vel [0   0   0]}
    {:pos [9  -8  -3] :vel [0   0   0]}
    {:pos [-9 -10   1] :vel [-2  -2  -1]}
    {:pos [4  10   9] :vel [-3   7  -2]}
    {:pos [8 -10  -3] :vel [5  -1  -2]}
    {:pos [5 -10   3] :vel [0  -4   5]}
    {:pos [-10   3  -4] :vel [-5   2   0]}
    {:pos [5 -25   6] :vel [1   1  -4]}
    {:pos [13   1   1] :vel [5  -2   2]}
    {:pos [0   1   7] :vel [-1  -1   2]}
    {:pos [15  -6  -9] :vel [-5   4   0]}
    {:pos [-4 -11   3] :vel [-3 -10   0]}
    {:pos [0  -1  11] :vel [7   4   3]}
    {:pos [-3  -2   5] :vel [1   2  -3]}
    {:pos [14 -12  -4] :vel [11   3   0]}
    {:pos [-1  18   8] :vel [-5   2   3]}
    {:pos [-5 -14   8] :vel [1  -2   0]}
    {:pos [0 -12  -2] :vel [-7  -3  -3]}
    {:pos [-23   4   1] :vel [-7  -1   2]}
    {:pos [20 -31  13] :vel [5   3   4]}
    {:pos [-4   6   1] :vel [-1   1  -3]}
    {:pos [15   1  -5] :vel [3  -3  -3]}
    {:pos [36 -10   6] :vel [5   0   3]}
    {:pos [-18  10   9] :vel [-3  -7   5]}
    {:pos [8 -12  -3] :vel [-2   1  -7]}
    {:pos [-18  -8  -2] :vel [0   6  -1]}
    {:pos [-33  -6   5] :vel [-5  -4   7]}
    {:pos [13  -9   2] :vel [-2  11   3]}
    {:pos [11  -8   2] :vel [8  -6  -7]}
    {:pos [17   3   1] :vel [-1  -1  -3]}
    {:pos [30  -8   3] :vel [3   3   0]}
    {:pos [-2  -4   0] :vel [4 -13   2]}
    {:pos [-18  -7  15] :vel [-8   2  -2]}
    {:pos [-2  -1  -8] :vel [1   8   0]}
    {:pos [-25  -1   4] :vel [1  -3   4]}
    {:pos [2  -9   0] :vel [-3  13  -1]}
    {:pos [32  -8  14] :vel [5  -4   6]}
    {:pos [-1  -2  -8] :vel [-3  -6  -9]}
    {:pos [8 -12  -9] :vel [-7   3   0]}
    {:pos [13  16  -3] :vel [3 -11  -5]}
    {:pos [-29 -11  -1] :vel [-3   7   4]}
    {:pos [16 -13  23] :vel [7   1   1]}]))

(defn compute-energy [system]
  (apply +
         (mapv (fn [{:keys [pos vel]}]
                 (* (apply + (map #(Math/abs %) pos))
                    (apply + (map #(Math/abs %) vel))))
               system)))

(deftest part-1
  (is (= example-1
         (take 11
               (iterate simulate-all
                        (parse "  <x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>  
<x=3, y=5, z=-1>")))))



  (is (= (last example-2)
         (first (drop 100
                      (iterate simulate-all (parse
                                             "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>"))))))


  (is (= 1940
         (compute-energy (first (drop 100
                                      (iterate simulate-all (parse
                                                             "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>")))))))


  (is (= 7471
         (compute-energy (first (drop 1000
                                      (iterate simulate-all input)))))))

(run-tests)
