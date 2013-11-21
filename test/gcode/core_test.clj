(ns gcode.core-test
  (:require [clojure.test :refer :all]
            [gcode.core :refer :all]))


(defn end-location
  [gcodes]
  (let [st (process gcodes)]
    (location st)))


(deftest distance-modes
  (testing "G90 - Absolute distance mode"
    (let [st (process ["G90"])]
      (is (= (get-in st [:modal :distance-mode]) :absolute))))
  (testing "G91 - Relative distance mode"
    (let [st (process ["G91"])]
      (is (= (get-in st [:modal :distance-mode]) :relative))))
  (testing "G90 -> G91 -> G90 - swapping distance mode"
    (let [st (process ["G90" "G91" "G90"])]
      (is (= (get-in st [:modal :distance-mode]) :absolute)))))

(deftest machine-move
  (testing "G53 - Machine Coordinates move"
    (is (= (end-location ["G0 G53 X50 Y50 Z50"])
           {:x 50.0 :y 50.0 :z 50.0})))
  (testing "G53 G91 G0 - Machine Coordinates move"
    (is (= (end-location ["G0 G53 G91 X50 Y50 Z50" "X1 Y1 Z1"])
           {:x 51.0 :y 51.0 :z 51.0}))))

(deftest relative-move
  (testing "G91 - relative move"
    (is (= (end-location ["G0 G53 G91 X0 Y0 Z0" "X1"])
           {:x 1.0 :y 0.0 :z 0.0}))
    (is (= (end-location ["G0 G53 G91 X0 Y0 Z0" "X1" "X1"])
           {:x 2.0 :y 0.0 :z 0.0}))
    (is (= (end-location ["G0 G53 G91 X0 Y0 Z0" "X1" "Y1" "Z1"])
           {:x 1.0 :y 1.0 :z 1.0}))
    (is (= (end-location ["G0 G53 G91 X0 Y0 Z0" "X1" "Y1" "Z1" "X-1 Y-1 Z-1"])
           {:x 0.0 :y 0.0 :z 0.0}))
    (is (= (end-location ["G0 G53 G91 X0 Y0 Z0" "X.001" "Y.001" "Z.001"])
           {:x 0.001 :y 0.001 :z 0.001}))))

(deftest rapid-move
  (testing "G0 - rapid move"
    (let [st (process ["G0 G53 X0 Y0 Z0"])]
      (is (= (get-in st [:modal :move-type]) :rapid)))))

(deftest feed-move
  (testing "G1 - feed"
    (let [st (process ["G1 G53 F5 X0 Y0 Z0"])]
      (is (= (get-in st [:modal :feed-rate]) 5.0))
      (is (= (get-in st [:modal :move-type]) :feed))
      (is (= (:feed-rate (first (:moves st))) 5.0)))))

(deftest arcs
  (testing "G2 - CW arg"
    (let [st (process ["G0 G53" "G2 F1 X2 Y2 I2 J2"])
          mv (second (:moves st))]
      (is (= (:feed-rate mv) 1.0))
      (is (= (:move-type mv) :cw-arc))
      (is (= (get-in mv [:from-coord :x]) 0.0))
      (is (= (get-in mv [:from-coord :y]) 0.0))
      (is (= (get-in mv [:from-coord :z]) 0.0))
      (is (= (get-in mv [:to-coord :x]) 2.0))
      (is (= (get-in mv [:to-coord :y]) 2.0))
      (is (= (get-in mv [:to-coord :z]) 0.0))
      (is (= (get-in mv [:center :i]) 2.0))
      (is (= (get-in mv [:center :j]) 2.0))
      (is (= (get-in mv [:center :k]) 0.0))))
  (testing "G3 - CCW arg"
    (let [st (process ["G0 G53" "G3 F1 X2 Y2 I2 J2"])
          mv (second (:moves st))]
      (is (= (:feed-rate mv) 1.0))
      (is (= (:move-type mv) :ccw-arc))
      (is (= (get-in mv [:from-coord :x]) 0.0))
      (is (= (get-in mv [:from-coord :y]) 0.0))
      (is (= (get-in mv [:from-coord :z]) 0.0))
      (is (= (get-in mv [:to-coord :x]) 2.0))
      (is (= (get-in mv [:to-coord :y]) 2.0))
      (is (= (get-in mv [:to-coord :z]) 0.0))
      (is (= (get-in mv [:center :i]) 2.0))
      (is (= (get-in mv [:center :j]) 2.0))
      (is (= (get-in mv [:center :k]) 0.0)))))

