(ns carbon-grid.core-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [carbon-grid.core :refer [init-grid]]))

(deftest init-grid-test
  (is (= (init-grid 2 2) [[[0 0] [0 1]] [[1 0] [1 1]]])))
