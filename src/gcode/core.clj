(ns gcode.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn read-double
  [s]
  (Double/parseDouble s))

(defn read-int
  [s]
  (Integer/parseInt s))

(def codes
  {:g
   {"0" [[:modal :move-type] :rapid]
    "1" [[:modal :move-type] :feed]
    "2" [[:modal :move-type] :cw-arc]
    "3" [[:modal :move-type] :ccw-arc]
    "4" [[:temporary :move-type] :dwell]
    "53" [[:temporary :move-coordsys] :machine]
    "90" [[:modal :distance-mode] :absolute]
    "91" [[:modal :distance-mode] :relative]}
   :x [[:temporary :coord :x] read-double]
   :y [[:temporary :coord :y] read-double]
   :z [[:temporary :coord :z] read-double]
   :i [[:modal :center :i] read-double]
   :j [[:modal :center :j] read-double]
   :k [[:modal :center :k] read-double]
   :f [[:modal :feed-rate] read-double]
   :p [[:temporary :parameter] read-int]})


(defn create-state
  []
  {:modal 
   {:move-type
    :rapid
    :coord {:x 0.0 :y 0.0 :z 0.0}
    :center {:i 0.0 :j 0.0 :k 0.0}
    :move-coordsys :local}
   :temporary {}
   :moves []
   :errors []
   :line 1})

(defn lookup-path
  [codekw value]
  (let [entry (codekw codes)]
    (cond
      (map? entry) (let [subentry (get entry value)]
                     [(first subentry) (second subentry)])
      (vector? entry) [(first entry) ((second entry) value)]
      (nil? entry) [nil nil])))

(defn apply-code
  [s c]
  (let [gcode (keyword (.toLowerCase (.substring c 0 1)))
        grest (.substring c 1)
        [path value] (lookup-path gcode grest)]
    (if path
      (assoc-in s path value)
      (assoc s :errors (conj (:errors s) (str "line " (:line s) ": Unhandled code " c))))))

(defn mutate-state
  [mach-state gcode-seq]
  (reduce (fn [s c] (apply-code s c)) mach-state gcode-seq))

(defn get-state-var
  [st k defv]
  (get-in st [:temporary k] (get-in st [:modal k] defv)))

(defn get-state-coord
  [st]
  (reduce (fn [s c] (assoc s c (get-in st [:temporary :coord c] (get-in st [:modal :coord c] 0.0)))) {} [:x :y :z]))

(defn get-rel-coord
  [st]
  (reduce (fn [s c] (assoc s c (get-in st [:temporary :coord c] 0.0))) {} [:x :y :z]))

(defn add-relative
  [fc tc]
  (let [[fx fy fz] [(:x fc) (:y fc) (:z fc)]
        [tx ty tz] [(:x tc) (:y tc) (:z tc)]]
    {:x (+ fx tx) :y (+ fy ty) :z (+ fz tz)}))

(defn make-move
  [mach-state move-type move-coordsys distance-mode from-coord to-coord]
  (let [base {:move-type move-type :distance-mode distance-mode :from-coord from-coord :to-coord to-coord :line (:line mach-state)}]
    (case move-type
      (:rapid :feed) (if (= from-coord to-coord)
                       nil
                       (-> base
                           (assoc :feed-rate (get-state-var mach-state :feed-rate nil))))
      (:cw-arc :ccw-arc) (-> base
                             (assoc :feed-rate (get-state-var mach-state :feed-rate nil))
                             (assoc :center (get-state-var mach-state :center nil) :turns (get-state-var mach-state :parameter 1)))
      :dwell (-> base
                 (assoc :duration (get-state-var mach-state :parameter 1))
                 (dissoc :distance-mode :from-coord :to-coord :move-coordsys))
      base)))


(defn advance-state
  [old-state mach-state]
  (let [move-type (get-state-var mach-state :move-type :rapid)
        move-coordsys (get-state-var mach-state :move-coordsys :local)
        distance-mode (get-state-var mach-state :distance-mode :absolute)
        new-coord (get-state-coord mach-state)
        rel-coord (get-rel-coord mach-state)
        old-coord (get-state-coord old-state)
        dest-coord (if (= distance-mode :absolute) new-coord (add-relative old-coord rel-coord))
        mv (make-move mach-state move-type move-coordsys distance-mode old-coord dest-coord)]
    (-> mach-state
        (assoc :temporary {})
        (assoc :moves (conj (:moves old-state) mv))
        (assoc-in [:modal :coord] dest-coord)
        (update-in [:line] inc)))) 

(defn extract-feed-lines
  [moves]
  (let [extract-pt (fn [m wh] (let [cd (wh m)] (map #(%1 cd) [:x :y :z])))]
    (:lines (reduce (fn [st mv]
              (let [t (:move-type mv)
                    lt (:move-type st)
                    ln (:line st)]
                (if (= t lt)
                  (if (= :feed t)
                    (assoc st :line (conj ln (extract-pt mv :to-coord)))
                    st)
                  (if (= :feed t)
                    (assoc st :move-type t :line [(extract-pt mv :from-coord) (extract-pt mv :to-coord)])
                    (assoc st :move-type t :lines (conj (:lines st) ln) :line nil)))))
                    {:move-type :rapid :lines nil :line nil} moves))))


(defn process-block
  [mach-state gcode-block]
  (let [cseq (string/split gcode-block #"\s+")
        updated-state (mutate-state mach-state cseq)]
    (advance-state mach-state updated-state)))

(defn process
  [gcode-seq]
  (reduce process-block (create-state) gcode-seq))

(defn process-file
  [f]
  (with-open [r (io/reader f)]
    (process (line-seq r))))

(defn location
  [st]
  (get-in st [:modal :coord]))

