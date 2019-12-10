(ns day9
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
  (slurp "input/day9.txt"))
