(ns advent.core-test
  (:require [clojure.test :refer :all]
            [advent.core :refer :all]))

(deftest day-1-part-1
  (is (= 3 (captcha1 "1122")))
  (is (= 4 (captcha1 "1111")))
  (is (= 0 (captcha1 "1234")))
  (is (= 9 (captcha1 "91212129"))))

(deftest day-1-part-2
  (is (=  6 (captcha2 "1212")))
  (is (=  0 (captcha2 "1221")))
  (is (=  4 (captcha2 "123425")))
  (is (= 12 (captcha2 "123123")))
  (is (=  4 (captcha2 "12131415"))))

(run-tests)
