(ns day8
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]
   [intcode :as intcode])
  (:import [java.io StringReader BufferedReader]
           [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.awt Color]))

(def input
  (slurp "input/day8.txt"))

(defn pixels [s]
  (->> s
       (seq)
       (mapv str)
       (mapv keyword)))

(defn layers [w h data]
  (partition (* w h) (pixels data)))

(defn rows [w h data]
  (mapv (partial partition w)
        (layers w h data)))

(defn fewest-zeros [layers]
  (->> layers
       (mapv frequencies)
       (sort-by :0)
       (first)))


(deftest part1
  (is (= [[[:1 :2 :3]
           [:4 :5 :6]]
          [[:7 :8 :9]
           [:0 :1 :2]]]
         (rows 3 2 "123456789012")))

  (is (= 1320
         (apply * (-> (fewest-zeros (layers 25 6 input))
                      (select-keys [:1 :2])
                      (vals))))))

(defn flatten-image [w h data]
  (let [row-data (layers w h data)]
    (apply map (fn [& pixels]
                 (->> pixels
                      (remove #{:2})
                      (first))) row-data)))

(deftest part2
  (is (= [:0 :1 :1 :0]
         (flatten-image 2 2 "0222112222120000"))))

(let [img (BufferedImage. 25 6 BufferedImage/TYPE_INT_RGB)]
  (doall
   (map-indexed
    (fn [i row]
      (doall
       (map-indexed
        (fn [j p]
          (prn p)
          (.setRGB img j i (.getRGB (case p
                                      :0 Color/BLACK
                                      :1 Color/WHITE)))) row)))
    (partition 25 (flatten-image 25 6 input))))
  (ImageIO/write img "png", (io/file "img.png")))


(run-tests)
