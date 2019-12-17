(ns day10
  (:require
   [core]
   [clojure.repl :refer :all]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [intcode :as intcode]))

(defn find-asteroids [reader]
  (->>
   (map-indexed
    (fn [y line]
      (map-indexed (fn [x c]
                     (when (not= c \.)
                       [x y c]))
                   (str/trim line)))
    (line-seq reader))
   (apply concat)
   (filter identity)))

(defn gcd [a b]
  (if (zero? b)
    (Math/abs a)
    (recur b (mod a b))))

(defn angle-from-top [x y]
  (if (and (zero? x)
           (neg? y))
    (- Math/PI)
    (Math/atan2 (- x) y)))

(defn compute [[sx sy :as station] [ax ay name :as asteroid]]
  (when (not= [ax ay] [sx sy])
    (let [i (- ax sx)
          j (- ay sy)
          d (gcd i j)]
      {:angle [(/ i d) (/ j d)]
       :location [ax ay]
       :name name
       :i i
       :j j
       :degrees (angle-from-top i j)
       :gcd d
       :distance (+ (Math/abs i) (Math/abs j))})))

(defn degress [radians]
  (* 57.2958 radians))

(degress (Math/atan 2))
(degress (Math/atan 0.5))

(defn evaluate* [asteroids [sx sy :as station]]
  (let [angles (filter identity
                       (mapv (fn [[ax ay name :as asteroid]]
                               (compute station asteroid))
                             asteroids))]
    (group-by :angle angles)))

(defn evaluate [asteroids n]
  (evaluate* asteroids (nth asteroids n)))



(try
  (let [asteroids (find-asteroids (core/string-reader
                                   "#.........
                  ...A......
                  ...B..a...
                  .EDCG....a
                  ..F.c.b...
                  .....c....
                  ..efd.c.gb
                  .......c..
                  ....f...c.
                  ...e..d..c"))]
    (evaluate asteroids 0))

  (catch Exception e e))

(defn best-location [input]
  (let [asteroids (find-asteroids input)]
    (apply max (mapv (comp count (partial evaluate* asteroids)) asteroids))))


(defn debug-location [input [x y]]
  (let [asteroids (find-asteroids input)]
    (evaluate* asteroids [x y])))


(debug-location
 (core/string-reader
  "......a.b.
   c..d.e....
   ..fghijkl.
   .m.n.opq..
   .r..s.....
   ..t....u.v
   w..x....y.
   .zA.B..CDE
   FG...H..I.
   .J....KLMN")
 [5 8])


(defn print-angle [x y]
  (printf "[%d, %d] %f\n" x y (angle-from-top x y)))

(do
  (print-angle  1 -1)
  (print-angle  1  1)
  (print-angle -1  1)
  (print-angle -1 -1))



(deftest part1
  (is (= 8
         (best-location (core/string-reader
                         ".#..#
                          .....
                          #####
                          ....#
                          ...##"))))


    ;; Best is 5,8 with 33 other asteroids detected:
  (is (= 33
         (best-location
          (core/string-reader
           "......a.b.
            c..d.e....
            ..fghijkl.
            .m.n.opq..
            .r..s.....
            ..t....u.v
            w..x....y.
            .zA.B..CDE
            FG...H..I.
            .J....KLMN"))))



  ;   Best is 1,2 with 35 other asteroids detected:
  (is (= 35

         (best-location (core/string-reader

                         "   #.#...#.#.
   .###....#.
   .#....#...
   ##.#.#.#.#
   ....#.#.#.
   .##..###.#
   ..#...##..
   ..##....##
   ......#...
   .####.###."))))



  ;   Best is 6,3 with 41 other asteroids detected:
  (is (= 41

         (best-location (core/string-reader

                         "   .#..#..###
     ####.###.#
     ....###.#.
     ..###.##.#
     ##.##.#.#.
     ....###..#
     ..#.#..#.#
     #..#.#.###
     .##...##.#
     .....#.#.."))))


  ;   Best is 11,13 with 210 other asteroids detected:
  (is (= 210

         (best-location (core/string-reader
                         "    .#..##.###...#######
     ##.############..##.
     .#.######.########.#
     .###.#######.####.#.
     #####.##.#.##.###.##
     ..#####..#.#########
     ####################
     #.####....###.#.#.##
     ##.#################
     #####.##.###..####..
     ..######..##.#######
     ####.##.####...##..#
     .#####..#.######.###
     ##...#.##########...
     #.##########.#######
     .####.#.###.###.#.##
     ....##.##.###..#####
     .#.#.###########.###
     #.#.#.#####.####.###
     ###.##.####.##.#..##"))))

  (is (= 260
         (best-location (io/reader "input/day10.txt")))))

(defn map-longest
  ([fn _missing-value-fn c1]
   (map fn c1))
  ([fn missing-value-fn c1 & colls]
   (lazy-seq
    (when (not-every? empty? (conj colls c1))
      (let [firsts (map first (conj colls c1))]
        (cons
         (apply fn (map #(if (nil? %) (missing-value-fn) %) firsts))
         (apply map-longest
                (conj (map rest colls) (rest c1) missing-value-fn fn))))))))

(defn lazer-order [asteroids [sx sy :as station]]
  (let [angles (filter identity
                       (mapv (fn [[ax ay name :as asteroid]]
                               (compute station asteroid))
                             asteroids))]
    (->> angles
         (group-by :degrees)
         (into [])
         (sort-by first)
         (mapv second)
         (mapv (partial sort-by :distance)))))


(defn lazer-input [input x y]
  (let [asteroids (find-asteroids input)]
    (->> (lazer-order asteroids [x y])
         (apply map-longest
                vector
                (constantly nil))
         (apply concat)
         (filter identity))))


(lazer-input (core/string-reader
              ".#....###24...#..
               ##...##.13#67..9#
               ##...#...5.8####.
               ..#.....X...###..
               ..#.#.....#....##")

             8 3)



(defn find-best-location* [asteroids [sx sy :as station]]
  (let [angles (filter identity
                       (mapv (fn [[ax ay name :as asteroid]]
                               (compute station asteroid))
                             asteroids))]
    {:station station
     :count (count
             (group-by :angle angles))}))

(defn find-best-location [input]
  (let [asteroids (find-asteroids input)]
    (->> asteroids
         (mapv (partial find-best-location* asteroids))
         (sort-by :count)
         (reverse))))


(deftest part-2

  (let [results (concat [:dummy] (map :location
                                      (lazer-input
                                       (core/string-reader
                                        "    .#..##.###...#######
     ##.############..##.
     .#.######.########.#
     .###.#######.####.#.
     #####.##.#.##.###.##
     ..#####..#.#########
     ####################
     #.####....###.#.#.##
     ##.#################
     #####.##.###..####..
     ..######..##.#######
     ####.##.####...##..#
     .#####..#.######.###
     ##...#.##########...
     #.##########.#######
     .####.#.###.###.#.##
     ....##.##.###..#####
     .#.#.###########.###
     #.#.#.#####.####.###
     ###.##.####.##.#..##") 11 13)))]

    (is (= [11,12] (nth results 1)))
    (is (= [12,1] (nth results 2)))
    (is (= [12,2] (nth results 3)))
    (is (= [12,8] (nth results 10)))
    (is (= [16,0] (nth results 20)))
    (is (= [16,9] (nth results 50)))
    (is (= [10,16] (nth results 100)))
    (is (= [9,6] (nth results 199)))
    (is (= [8,2] (nth results 200)))
    (is (= [10,9] (nth results 201)))
    (is (= [11,1] (nth results 299))))

  (let [{:as target
         [x y] :location} (nth (lazer-input (io/reader "input/day10.txt") 14 17)
                               199)]

    (is (= 608 (+ y (* 100 x))))

    (is (= {:angle [-8 -9], :location [6 8], :name \#, :i -8, :j -9, :degrees 2.4149503129080676, :gcd 1, :distance 17}
           target))))
