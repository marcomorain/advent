(ns core
  (:import [java.io StringReader BufferedReader]))

(defn string-reader [^String s]
  (BufferedReader. (StringReader. s)))
