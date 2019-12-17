(ns core
  (:require [pjstadig.humane-test-output])
  (:import [java.io StringReader BufferedReader]))

(pjstadig.humane-test-output/activate!)

(defn string-reader [^String s]
  (BufferedReader. (StringReader. s)))
