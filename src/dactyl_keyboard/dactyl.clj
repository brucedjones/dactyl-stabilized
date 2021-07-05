(ns dactyl-keyboard.dactyl
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]
              [unicode-math.core :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(defn mcube [dims]
  (cube (nth dims 0) (nth dims 1) (nth dims 2) :center false)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 7)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 12))            ; or, change this for more precise tenting control

(def pinky-15u true)                   ; controls whether the outer column uses 1.5u keys
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 3)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row false)                   ; adds an extra bottom row to the outer columns
(def inner-column true)                ; adds an extra inner column (two less rows than nrows)

(def column-style :standard)

(defn column-offset [column]
  (if inner-column
    (cond (<= column 1) [0 -2 0]
          (= column 3) [0 2.82 -4.5]
          (>= column 5) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])
    (cond (= column 2) [0 2.82 -4.5]
          (>= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0])))

(def thumb-offsets [6 -3 7])

(def keyboard-z-offset 10)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -2)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 0.5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;; http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? true)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(def extra-cornerrow (if extra-row lastrow cornerrow))
(def innercol-offset (if inner-column 1 0))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.15)
(def keyswitch-width 14.15)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3.2))
(def mount-height (+ keyswitch-height 2.7))

(defn make-plate-1u [height-offset]
  (let [
        keyswitch-height (+ keyswitch-height height-offset)
        top-wall (->> (cube (+ keyswitch-width 3) 1.5 (+ plate-thickness 0.5))
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- (/ plate-thickness 2) 0.25)]))
        left-wall (->> (cube 1.8 (+ keyswitch-height 3) (+ plate-thickness 0.5))
                       (translate [(+ (/ 1.8 2) (/ keyswitch-width 2))
                                   0
                                   (- (/ plate-thickness 2) 0.25)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2.5)) 0 (- (/ retention-tab-hole-thickness 2) 0.5)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))

(def plate-1u (make-plate-1u 0))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;; Fill the keyholes instead of placing a a keycap over them
(def keyhole-fill (->> (cube keyswitch-height keyswitch-width plate-thickness)
                       (translate [0 0 (/ plate-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range (+ innercol-offset 0) ncols))
(def rows (range 0 nrows))

(def innercolumn 0)
(def innerrows (range 0 (- nrows 2)))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col, row]
  (if (and pinky-15u
           (= col lastcol)
           (<= row last-15u-row)
           (>= row first-15u-row))
    4.7625
    0))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column, row) 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
               :orthographic placed-shape-ortho
               :fixed        placed-shape-fixed
               placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow)(= column 0))
                         (not= row lastrow))]
           (->> plate-1u
                ;                (rotate (/ π 2) [0 0 1])
                (key-place column row)))))
(def caps
  (apply union
         (conj (for [column columns
               row rows
               :when (or (and (= column 0) (< row 3))
                         (and (.contains [1 2] column) (< row 4))
                         (.contains [3 4 5 6] column))]
               (->> (sa-cap (if (and pinky-15u (= column lastcol) (not= row lastrow)) 1.5 1))
                    (key-place column row)))
               (list (key-place 0 0 (sa-cap 1))
                 (key-place 0 1 (sa-cap 1))
                 (key-place 0 2 (sa-cap 1))))))

(def caps-fill
  (apply union
         (conj (for [column columns
               row rows
               :when (or (and (= column 0) (< row 3))
                         (and (.contains [1 2] column) (< row 4))
                         (.contains [3 4 5 6] column))]
                 (key-place column row keyhole-fill))
               (list (key-place 0 0 keyhole-fill)
                 (key-place 0 1 keyhole-fill)
                 (key-place 0 2 keyhole-fill)))))

;placement for the innermost column
(def key-holes-inner
  (if inner-column
    (apply union
           (for [row innerrows]
             (->> plate-1u
                  ;               (rotate (/ π 2) [0 0 1])
                  (key-place 0 row))))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 4.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

; wide posts for 1.5u keys in the main cluster
(if pinky-15u
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
    (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
    (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
    (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
    (def wide-post-tl web-post-tl)
    (def wide-post-bl web-post-bl)
    (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range (+ innercol-offset 0) (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

(def inner-connectors
  (if inner-column
    (apply union
           (concat
            ;; Row connections
            (for [column (range 0 1)
                  row (range 0 (- nrows 2))]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))

            ;; Column connections
            (for [row (range 0 (dec cornerrow))]
              (triangle-hulls
               (key-place innercolumn row web-post-bl)
               (key-place innercolumn row web-post-br)
               (key-place innercolumn (inc row) web-post-tl)
               (key-place innercolumn (inc row) web-post-tr)))

            ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 2)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))))))

(def extra-connectors
  (if extra-row
    (apply union
           (concat
            (for [column (range 3 ncols)
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

            (for [column (range 3 (dec ncols))
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))

            (for [column (range 4 (dec ncols))
                  row (range lastrow nrows)]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))))))

;;;;;;;;;;;;;;;;;;;
;; Thumb Cluster ;;
;;;;;;;;;;;;;;;;;;;

(def thumborigin
  (map + (key-position (+ innercol-offset 1) cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(defn thumb-0-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-9 -16 3])
       ))
(defn thumb-1-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -2) [0 1 0])
       (rotate (deg2rad  12.5) [0 0 1])
       (translate thumborigin)
       (translate [-30.5 -25 -2])))
(defn thumb-2-place [shape]
  (->> shape
       (translate [0 0 1.5])
       (rotate (deg2rad   10) [1 0 0])
       (rotate (deg2rad 15) [0 1 0])
       (rotate (deg2rad  15.5) [0 0 1])
       (translate thumborigin)
       (translate [-52.75 -26.4 3])))
(defn thumb-3-place [shape]
  (->> shape
       (translate [0 0 -1.5])
       (rotate (deg2rad   10) [1 0 0])
       (rotate (deg2rad 15) [0 1 0])
       (rotate (deg2rad  15.5) [0 0 1])
       (translate thumborigin)
       (translate [-48.7 -45.25 -1.5])))

(defn thumb-1u-layout [shape]
  (union
   (thumb-2-place shape)
   (thumb-3-place shape)
  )
)

(defn thumb-2u-layout [shape]
  (union
   (thumb-0-place shape)
   (thumb-1-place shape)
  )
)

(def plate-2u-keyswitch-height-offset (- 0.2))
(def plate-2u
  (let [
        keyswitch-height-offset (- 0.2)
        plate-height (/ (- sa-double-length mount-height) 2)
        plate-width mount-width
        top-plate (->> (cube plate-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        side-plate-width (/ (- plate-width (+ keyswitch-height 3 plate-2u-keyswitch-height-offset)) 2)
        side-plate-height mount-height
        side-plate (->> (cube side-plate-width side-plate-height web-thickness)
                       (translate [(-(/ plate-width 2) (/ side-plate-width 2)) 0
                                   (- plate-thickness (/ web-thickness 2))]))
        ]
    (union (rotate (/ π 2) [0 0 1] (make-plate-1u plate-2u-keyswitch-height-offset)) top-plate (mirror [0 1 0] top-plate side-plate (mirror [1 0 0] side-plate)))
  )
)

; Cherry MX 2u stabilizer cutout
; Reference: https://cdn.sparkfun.com/datasheets/Components/Switches/MX%20Series.pdf
; https://geekhack.org/index.php?action=dlattach;topic=93285.0;attach=185472;image
(defn inches-to-mm [val] (* val 25.4))
; Suitable for 2u, 2.25u, and 2.75u cherry stabilizers
(defn small-stabilizer [A]
  (let 
    [
      main-stab-cutout-height (inches-to-mm 0.484)
      main-stab-cutout-x (/ A 2)
      main-stab-cutout-y (- main-stab-cutout-height (inches-to-mm 0.26) (/ main-stab-cutout-height 2))
      main-stab-cutout-width (inches-to-mm 0.262)
      main-stab-cutout (->>
        (cube main-stab-cutout-height main-stab-cutout-width web-thickness)
        (translate [main-stab-cutout-y main-stab-cutout-x (- plate-thickness (/ web-thickness 2))])
      )

      secondary-stab-cutout-width (inches-to-mm 0.12)
      secondary-stab-cutout-height (+ (inches-to-mm 0.26) (- (inches-to-mm 0.53) main-stab-cutout-height))
      secondary-stab-cutout-x (/ A 2)
      secondary-stab-cutout-y (/ (- 0 secondary-stab-cutout-height) 2)
      secondary-stab-cutout (->>
        (cube secondary-stab-cutout-height secondary-stab-cutout-width web-thickness)
        (translate [secondary-stab-cutout-y secondary-stab-cutout-x (- plate-thickness (/ web-thickness 2))])
      )

      side-stab-cutout-width (+ A (* 0.8 2) main-stab-cutout-width)
      side-stab-cutout-height 2.8
      side-stab-cutout-x 0
      side-stab-cutout-y 0.9
      side-stab-cutout (->>
        (cube side-stab-cutout-height side-stab-cutout-width web-thickness)
        (translate [side-stab-cutout-y side-stab-cutout-x (- plate-thickness (/ web-thickness 2))])
      )

      connector-stab-cutout-width A
      connector-stab-cutout-height 10.7
      connector-stab-cutout-x 0
      connector-stab-cutout-y (- (/ connector-stab-cutout-height 2 ) 5.97)
      connector-stab-cutout (->>
        (cube connector-stab-cutout-height connector-stab-cutout-width web-thickness)
        (translate [connector-stab-cutout-y connector-stab-cutout-x (- plate-thickness (/ web-thickness 2))])
      )

      stabilizer-bar-clearance-width (+ A main-stab-cutout-width)
      stabilizer-bar-clearance-height (/ (+ mount-width 3) 2)
      stabilizer-bar-clearance-thickness retention-tab-hole-thickness
      stabilizer-bar-clearance-y (/ (- stabilizer-bar-clearance-height) 2)
      stabilizer-bar-clearance (->>
        (cube stabilizer-bar-clearance-height stabilizer-bar-clearance-width web-thickness)
        (translate [stabilizer-bar-clearance-y 0 (- (/ stabilizer-bar-clearance-thickness 2) 0.5)])
      )

      main-retention-tab-width (- A main-stab-cutout-width)
      main-retention-tab-depth (/ (- mount-width (+ keyswitch-height plate-2u-keyswitch-height-offset)) 2)
      main-retention-tab-y (-(/ (+ (+ keyswitch-height plate-2u-keyswitch-height-offset) main-retention-tab-depth) 2))
      main-retention-tab-x 0
      main-retention-tab-thickness retention-tab-hole-thickness
      main-retention-tab (->>
        (cube main-retention-tab-depth main-retention-tab-width (- plate-thickness main-retention-tab-thickness))
        (translate [main-retention-tab-y main-retention-tab-x  (- web-thickness (/ main-retention-tab-thickness 2))])
      )

      retention-tab-y 5.53
      retention-tab-x secondary-stab-cutout-x
      retention-tab-width 3
      retention-tab-depth 3.2
      retention-tab (->>
        (cube retention-tab-depth retention-tab-width retention-tab-hole-thickness)
        (translate [retention-tab-y retention-tab-x  (- (/ retention-tab-hole-thickness 2) 0.5)])
      )

      cutout (union
        ; Plate cutout
        main-stab-cutout
        secondary-stab-cutout
        side-stab-cutout
        connector-stab-cutout
        (difference stabilizer-bar-clearance main-retention-tab)
        retention-tab
      )
    ]
    (union cutout (mirror [0 1 0] cutout))
  )
)
(def stabilizer-cutout-2u (small-stabilizer (inches-to-mm 0.94)))

(def thumbcaps
  (union
   (thumb-1u-layout (sa-cap 1))
   (thumb-2u-layout (rotate (/ π 2) [0 0 1] (sa-cap 2)))))

(def thumbcaps-fill
  (union
   (thumb-1u-layout keyhole-fill)
   (thumb-2u-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def thumb
  (union
    (thumb-1u-layout (rotate (/ π 2) [0 0 0] plate-1u))
    (thumb-2u-layout plate-2u)
  )
)

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  0.9) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  0.9) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -0.9) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -0.9) post-adj) 0] web-post))

(def thumb-connectors
  (union
   (triangle-hulls    ; First two
    (thumb-1-place thumb-post-tr)
    (thumb-1-place thumb-post-br)
    (thumb-0-place thumb-post-tl)
    (thumb-0-place thumb-post-bl))
   (triangle-hulls    ; Second thumb to end two
    (thumb-3-place web-post-br)
    (thumb-3-place web-post-tr)
    (thumb-1-place thumb-post-bl)

    (thumb-1-place thumb-post-bl)
    (thumb-1-place thumb-post-tl)
    (thumb-3-place web-post-tr)

    (thumb-1-place thumb-post-tl)
    (thumb-2-place web-post-br)
    (thumb-3-place web-post-tr)

    (thumb-2-place web-post-br)
    (thumb-2-place web-post-tr)
    (thumb-1-place thumb-post-tl)

    (thumb-1-place thumb-post-tl)
    (thumb-2-place web-post-tr)
    (thumb-3-place web-post-tr)
    )
   (triangle-hulls    ; end two
    (thumb-3-place web-post-tr)
    (thumb-3-place web-post-tl)
    (thumb-2-place web-post-br)
    (thumb-2-place web-post-bl))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (thumb-1-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (thumb-1-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (thumb-0-place thumb-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (thumb-0-place thumb-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-0-place thumb-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-0-place thumb-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl)
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))
  ))


(def thumb-type thumb)
(def thumb-connector-type thumb-connectors)
(def thumbcaps-type thumbcaps)
(def thumbcaps-fill-type thumbcaps-fill)

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 0)
(def left-wall-z-offset 0.5)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) 0])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def right-wall
  (if pinky-15u
    (union
     ; corner between the right wall and back wall
     (if (> first-15u-row 0)
       (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
       (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)
              (key-wall-brace lastcol 0 0 1 wide-post-tr lastcol 0 1 0 wide-post-tr)))
     ; corner between the right wall and front wall
     (if (= last-15u-row extra-cornerrow)
       (union (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 0 -1 wide-post-br)
              (key-wall-brace lastcol extra-cornerrow 0 -1 wide-post-br lastcol extra-cornerrow 1 0 wide-post-br))
       (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br))

     (if (>= first-15u-row 2)
       (for [y (range 0 (dec first-15u-row))]
         (union (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br)
                (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr))))

     (if (>= first-15u-row 1)
       (for [y (range (dec first-15u-row) first-15u-row)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol (inc y) 1 0 wide-post-tr)))

     (for [y (range first-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-tr lastcol y 1 0 wide-post-br))
     (for [y (range first-15u-row last-15u-row)] (key-wall-brace lastcol (inc y) 1 0 wide-post-tr lastcol y 1 0 wide-post-br))

     (if (<= last-15u-row (- extra-cornerrow 1))
       (for [y (range last-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-br lastcol (inc y) 1 0 web-post-br)))

     (if (<= last-15u-row (- extra-cornerrow 2))
       (for [y (range (inc last-15u-row) extra-cornerrow)]
         (union (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr)
                (key-wall-brace lastcol (inc y) 1 0 web-post-tr lastcol (inc y) 1 0 web-post-br))))
     )
    (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
           (if extra-row
             (union (for [y (range 0 (inc lastrow))] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 (inc lastrow))] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             (union (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
             )
           (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br)
           )))

(def default-thumb-wall
  (union
    ; thumb walls in clockwise order
    (wall-brace thumb-0-place  0 -1 thumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
    (wall-brace thumb-0-place  0 -1 thumb-post-bl thumb-0-place  0 -1 thumb-post-br)
    (wall-brace thumb-0-place  0 -1 thumb-post-bl thumb-1-place  0 -1 thumb-post-br)
    (wall-brace thumb-1-place  0 -1 thumb-post-bl thumb-1-place  0 -1 thumb-post-br)
    (wall-brace thumb-1-place  0 -1 thumb-post-bl thumb-3-place  0 -1 web-post-br)
    (wall-brace thumb-3-place  0 -1 web-post-bl thumb-3-place  0 -1 web-post-br)
    (wall-brace thumb-3-place  -1 0 web-post-bl thumb-3-place  -1 0 web-post-tl)
    (wall-brace thumb-3-place  -1 0 web-post-tl thumb-2-place  -1 0 web-post-bl)
    (wall-brace thumb-2-place  -1 0 web-post-tl thumb-2-place  -1 0 web-post-bl)
    (wall-brace thumb-2-place  0 1 web-post-tr thumb-2-place  0 1 web-post-tl)
    (wall-brace thumb-2-place  0 1 web-post-tr thumb-1-place  -1 0 thumb-post-tl)
    ; thumb corners
    (wall-brace thumb-3-place -1  0 web-post-bl thumb-3-place  0 -1 web-post-bl)
    (wall-brace thumb-2-place -1  0 web-post-tl thumb-2-place  0  1 web-post-tl)
    ; clunky bit on the top left thumb connection  (normal connectors don't work well)
    (bottom-hull
      (key-place 0 (- lastrow innercol-offset 1) (translate (wall-locate1 -1 0) web-post-bl))
      (key-place 0 (- lastrow innercol-offset 1) (translate (wall-locate2 -1 0) web-post-bl))
      (key-place 0 (- lastrow innercol-offset 1) (translate (wall-locate3 -1 0) web-post-bl))
      (thumb-1-place thumb-post-tl)
      (thumb-1-place (translate (wall-locate1 -1 0) thumb-post-tl))
      (thumb-1-place (translate (wall-locate2 -1 0) thumb-post-tl))
      (thumb-1-place (translate (wall-locate3 -1 0) thumb-post-tl))
    )
   ; connectors below the inner column to the thumb & second column
    (if inner-column
      (union
        (hull
          (key-place 0 (dec cornerrow) web-post-bl)
          (key-place 0 (dec cornerrow) web-post-br)
          (key-place 0 cornerrow web-post-tr)
        )
        (hull
          (key-place 0 cornerrow web-post-tr)
          (key-place 1 cornerrow web-post-tl)
          (key-place 1 cornerrow web-post-bl)
        )
        (hull
          (key-place 0 (dec cornerrow) web-post-bl)
          (key-place 0 cornerrow web-post-tr)
          (key-place 1 cornerrow web-post-bl)
        )
        (hull
          (key-place 0 (- lastrow innercol-offset 1) (translate (wall-locate1 -1 0) web-post-bl))
          (key-place 0 (dec cornerrow) web-post-bl)
          (key-place 1 cornerrow web-post-bl)
          (thumb-1-place thumb-post-tl)
        )
      )
    )
  )
)

(def case-walls
  (union
    default-thumb-wall
    right-wall
    ; back wall
    (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
    (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
    ; left wall
    (for [y (range 0 (- lastrow innercol-offset))] (key-wall-brace 0 y -1 0 web-post-tl 0 y        -1 0 web-post-bl))
    (for [y (range 1 (- lastrow innercol-offset))] (key-wall-brace 0 y -1 0 web-post-tl 0 (dec y)  -1 0 web-post-bl))
    (key-wall-brace 0 0 0 1 web-post-tl 0 0 -1 0 web-post-tl)
    ; front wall
    (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 3) lastrow   0 -1 web-post-br)
    (key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-br (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-bl)
    (for [x (range (+ innercol-offset 4) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl x       extra-cornerrow 0 -1 web-post-br))
    (for [x (range (+ innercol-offset 5) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl (dec x) extra-cornerrow 0 -1 web-post-br))
   ))

; Connectors between outer column and right wall when 1.5u keys are used
(def pinky-connectors
  (if pinky-15u
    (apply union
           (concat
            ;; Row connections
            (for [row (range first-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol row web-post-tr)
               (key-place lastcol row wide-post-tr)
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol (inc row) web-post-tr)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-br))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-tr)
               (key-place lastcol (inc row) wide-post-tr)
               (key-place lastcol row web-post-br))))

            ;; Column connections
            (for [row (range first-15u-row last-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-tr)
               (key-place lastcol (inc row) wide-post-tr)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-tr))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol (inc row) wide-post-tr)
               (key-place lastcol (inc row) web-post-tr))))
))))


(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0.2  -1.13) [0 (/ mount-height 2) 0])))

(def usb-holder-position (map + [22.75 19.8 0] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-jack-width 9.525)
(def usb-jack-height 4.175)
(def usb-jack-radius (/ usb-jack-height 2))
(def usb-board-thickness 1.5875)
(def usb-cube-thickness (+ usb-board-thickness usb-jack-radius))
(def usb-holder-cube   (cube 22.3 12 usb-cube-thickness))
(def usb-holder-space  (translate (map + usb-holder-position [0 (* -1 wall-thickness) (/ usb-cube-thickness 2)]) usb-holder-cube))
(def usb-holder-holder (translate usb-holder-position (cube 26.3 12 4)))
(def usb-jack-position (map + usb-holder-position [-0.25 10 (+ usb-board-thickness usb-jack-radius)]))

(def usb-jack-left-side
   (->>
    (->> (binding [*fn* 30] (cylinder usb-jack-radius 20)))
    (rotate (deg2rad  90) [1 0 0])
    (translate (map +  usb-jack-position [(* -1 (- (/ usb-jack-width 2) usb-jack-radius)) 0 0]))
  )
)

(def usb-jack-right-side
   (->>
    (->> (binding [*fn* 30] (cylinder usb-jack-radius 20)))
    (rotate (deg2rad  90) [1 0 0])
    (translate (map +  usb-jack-position [(- (/ usb-jack-width 2) usb-jack-radius) 0 0]))
  )
)

(def usb-jack-cube (translate usb-jack-position (cube (- usb-jack-width usb-jack-height) 20 usb-jack-height)))
(def usb-jack-back (translate (map + usb-jack-position [0 -16 -1.5]) (cube usb-jack-width 20 3)))
(def usb-jack (union usb-jack-left-side usb-jack-right-side usb-jack-cube usb-jack-back))

;;;;;;;;;;;;;;;;;;;;;;
;; Control Switches ;;
;;;;;;;;;;;;;;;;;;;;;;
(def control-switch-radius 6.1)
(defn make-control-switch-hole [position]
   (->>
    (->> (binding [*fn* 30] (cylinder control-switch-radius 20)))
    (rotate (deg2rad  90) [1 0 0])
    (translate position)
  )
)
(def control-switches (union
  (make-control-switch-hole [(first (key-position 2 0 [1.75 0 0])) (second (key-position 2 0 [0 2 0])) 8.75])
  (make-control-switch-hole [(first (key-position 3 0 [1.75 0 0])) (second (key-position 3 0 [0 2 0])) 8.75])
))

;;;;;;;;;;;;;;;;;;;;
;; Screw Inserts  ;;
;;;;;;;;;;;;;;;;;;;;
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
                 (cylinder [bottom-radius top-radius] height)))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                        (if shift-down  (key-position column row (map - (wall-locate2  0 -2.5) [0 (/ mount-height 2) 0]))
                          (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                            (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

    

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union
    (screw-insert lastcol 0 bottom-radius top-radius height [4.5 5 0])
    (screw-insert lastcol lastrow bottom-radius top-radius height [7.5 14.5 0])
    (screw-insert 0 lastrow bottom-radius top-radius height [3 -42 0])
    (screw-insert 0 2 bottom-radius top-radius height [4 -7 0])
    (screw-insert 0 0 bottom-radius top-radius height [7 5 0])
  )
)

; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

;;;;;;;;;;;;;;;;;;;;;
;; Nicenano Holder ;;
;;;;;;;;;;;;;;;;;;;;;
(defn make-nicenano-holder [position orientation]
  ; position is the position of the middle of the USB-C opening in XY, with the holder directly on the bottom plate
  ; orientation is degrees rotated about the z axis
  (let
    [
      nicenano-dims [18.4 33.7 2]
      pin-clearance [4 (second nicenano-dims) 2.5]
      support-extent 25
      retainer-extent 1
      wall-thickness 2
      wall-height 2.1 ; height above nicenano
      bounding-box (map + nicenano-dims [(* wall-thickness 2) wall-thickness (+ (nth pin-clearance 2) wall-height)])
      transform (fn [shape] (->> shape
        (translate (map - (map / bounding-box [2 2 2])))
        (rotate (deg2rad  orientation) [0 0 1])
        (translate (map / [0 (- (nth bounding-box 1)) (nth bounding-box 2)] [2 2 2]))
        (translate position)
      ))
    ]
    [
      ; HOLDER
      (transform (union
        (difference
          (mcube bounding-box)
          (translate
            [wall-thickness 0 0]
            (mcube (map + nicenano-dims [0 0 (nth bounding-box 2)]))
          )
        )
        (translate
          [(+ wall-thickness (nth pin-clearance 0)) 0 0]
          (mcube [(- (nth nicenano-dims 0) (* 2 (nth pin-clearance 0))) support-extent (nth pin-clearance 2)])
        )
        (translate
          [(+ wall-thickness (nth pin-clearance 0)) (- (nth nicenano-dims 1) retainer-extent) (+ (nth pin-clearance 2) (nth nicenano-dims 2))]
          (mcube [(- (nth nicenano-dims 0) (* 2 (nth pin-clearance 0))) retainer-extent retainer-extent])
        )
      ))
      ; USB CUTOUT
      (let
        [
          jack-width 9.525
          jack-height 3.5
          jack-radius (/ jack-height 2)
          jack-wall-thickness 1
          clearance-buffer 2.75
          clearance-width (+ jack-width clearance-buffer clearance-buffer)
          clearance-height (+ jack-height clearance-buffer clearance-buffer)
          clearance-radius (/ clearance-height 2)
        ]
        [
          (transform (union
            (mirror
              [0 -1 0]
              (translate
                [(/ (- (nth bounding-box 0) jack-width) 2) (- 0.1) (nth pin-clearance 2)]
                (union
                  (translate [jack-radius wall-thickness jack-radius]
                    (rotate (deg2rad  90) [1 0 0]
                      (binding [*fn* 30] (cylinder jack-radius (* 2 wall-thickness)))
                    )
                  )
                  (translate [(- jack-width jack-radius) wall-thickness jack-radius]
                    (rotate (deg2rad  90) [1 0 0]
                      (binding [*fn* 30] (cylinder jack-radius (* 2 wall-thickness)))
                    )
                  )
                  (translate [jack-radius 0 0]
                    (mcube [(- jack-width jack-radius jack-radius) (* 2 wall-thickness) jack-height])
                  )
                )
              )
            )
            (mirror [0 -1 0]
              (translate
                [(/ (- (nth bounding-box 0) clearance-width) 2) jack-wall-thickness (+ (nth pin-clearance 2) (- (/ jack-height 2) (/ clearance-height 2)))]
                (mcube [clearance-width (* 2 wall-thickness) clearance-height])
              )
            )
          ))
          (transform (translate [(/ (- (nth bounding-box 0) jack-width) 2) 0 (+ (nth pin-clearance 2) jack-height)]
            (mcube [jack-width retainer-extent retainer-extent])
          ))
        ]
      )
    ]
  )
)
(def nicenano-reference-key-position (key-position 1 0 [0 0 0]))
(def nice-nano-reference-position [
  (first nicenano-reference-key-position)
  (+ (second nicenano-reference-key-position) (+ (/ mount-height 2) post-adj))
  0
])
(def nicenano-holder (make-nicenano-holder (map + nice-nano-reference-position [(- 2) -1.7375 0]) 180))

;;;;;;;;;;;;;;;;;;;
;; Complete Case ;;
;;;;;;;;;;;;;;;;;;;
(def model-right (difference
  (difference
    (union
      key-holes
      key-holes-inner
      ;  thumbcaps
      pinky-connectors
      extra-connectors
      connectors
      inner-connectors
      thumb-type
      thumb-connector-type
      (difference
        (union
          case-walls
          screw-insert-outers
          (nth (nth nicenano-holder 1) 1)
        )
        control-switches
        screw-insert-holes
        (nth (nth nicenano-holder 1) 0)
        
      )
    )
    ; Stabilizer cutouts have to happen here as the required stabilizer bar clearance extends beyond the plate
    (thumb-2u-layout stabilizer-cutout-2u)
  )
  (translate [0 0 -20] (cube 350 350 40))
))

(spit "things/right.scad"
      (write-scad model-right))

; (spit "things/left.scad"
;       (write-scad (mirror [-1 0 0] model-right)))

;;;;;;;;;;;;;;;
;; Palm Rest ;;
;;;;;;;;;;;;;;;
; these positions should really be derived from the model
(def outside-case-contact [61.5737 -51.3762])
(def thumb-case-contact [-51.5458 -103.431])
(def internal-corner [-51.5458, -46.3762])
(def rest-length 63.5)
(def lower-right-one (map + outside-case-contact [0 (* -1 rest-length)]))
(def lower-right-two (map + lower-right-one [(* 25.4 (* -1 (√ 0.1))) (* 25.4 (* -2 (√ 0.1)))]))
(def lower-left-one (map + thumb-case-contact [(√ (/ (* rest-length rest-length) 2)) (* -1 (√ (/ (* rest-length rest-length) 2)))] [7.5 10]))
(def lower-left-two (map + lower-left-one [(* 25.4 (* (√ 0.5) (Math/cos (- (/ π 4) (Math/atan 0.5))))) (* -25.4 (* (√ 0.5) (Math/sin (- (/ π 4) (Math/atan 0.5)))))]))
(def palm-screw-one (map + outside-case-contact [-15 -25.4]))
(def palm-screw-two (map + outside-case-contact [-83 -53]))
(def palm-screw-three (map + outside-case-contact [-34.1 -67.15]))
(def palm-rest
  (difference
    (extrude-linear
      {:height 2.6 :center false}
      (polygon [
        lower-left-one
        lower-left-two
        lower-right-two
        lower-right-one
        (map + outside-case-contact [4.0963, 9.8962])
        internal-corner
        (map + thumb-case-contact [-30.8642, 14.441])
      ])
    )
  )
)

(defn plate-screw-holes [plate-thickness]
  (let [
          screw-head-diameter 5.7
          screw-head-radius (/ screw-head-diameter 2.0)
          screw-head-base-diameter 3.2
          screw-head-base-radius (/ screw-head-base-diameter 2.0)
          screw-head-height 2.3
    ]
    (union
      (screw-insert-all-shapes screw-head-radius screw-head-base-radius screw-head-height)
      (screw-insert-all-shapes screw-head-base-radius screw-head-base-radius plate-thickness)
  (->>
        (->> (binding [*fn* 30] (cylinder [screw-head-base-radius screw-head-base-radius] 10 :center false)))
    (translate palm-screw-one)
  )
  (->>
        (->> (binding [*fn* 30] (cylinder [screw-head-radius screw-head-base-radius] screw-head-height :center false))) 
        (translate palm-screw-one)
      )
      (->>
        (->> (binding [*fn* 30] (cylinder [screw-head-base-radius screw-head-base-radius] 10 :center false)))
    (translate palm-screw-two)
  )
  (->>
        (->> (binding [*fn* 30] (cylinder [screw-head-radius screw-head-base-radius] screw-head-height :center false)))
        (translate palm-screw-two)
      )
      (->>
        (->> (binding [*fn* 30] (cylinder [screw-head-radius screw-head-base-radius] screw-head-height :center false)))
    (translate palm-screw-three)
  )
      (->>
        (->> (binding [*fn* 30] (cylinder [screw-head-base-radius screw-head-base-radius] 10 :center false)))
        (translate palm-screw-three)
      )
    )
  )
)

(def plate-right
  (let [
      plate-thickness 2.6
    ]
  (union
    (difference
      (extrude-linear
          {:height plate-thickness :center false}
        (project
          (union
            key-holes
            key-holes-inner
            pinky-connectors
            extra-connectors
            connectors
            inner-connectors
            thumb-type
            thumb-connector-type
            case-walls
            thumbcaps-fill-type
            caps-fill
            screw-insert-outers
            palm-rest
          )
        )
      )
        (plate-screw-holes plate-thickness)
    )
      (translate [0 0 plate-thickness] (nth nicenano-holder 0))
  )
)
)

(spit "things/right-plate.scad"
      (write-scad plate-right))

; (spit "things/left-plate.scad"
;       (write-scad (mirror [-1 0 0] plate-right)))

; (spit "things/right-plate-laser.scad"
;       (write-scad
;        (cut
;         (translate [0 0 -0.1]
;                    (difference (union case-walls
;                                       screw-insert-outers)
;                                (translate [0 0 -10] screw-insert-screw-holes))))))

;;;;;;;;;;;;;;;;
;; Unit Tests ;;
;;;;;;;;;;;;;;;;

; Cherry stabilizer
(def test-plate (let [plate-height sa-double-length]
  (->> (cube mount-width plate-height web-thickness)
                       (translate [0 0
                                   (- plate-thickness (/ web-thickness 2))]))))
(def stabilizer-unit-test
  (difference 
    (union
      plate-2u
      (translate [mount-width 0 0] test-plate)
      (translate [(- mount-width) 0 0] test-plate)
    )
    stabilizer-cutout-2u
  )
)
(spit "things/stabilizer-unit-test.scad"
      (write-scad stabilizer-unit-test))

; Countersink
(spit "things/countersink-unit-test.scad"
  (write-scad 
    (intersection plate-right (translate palm-screw-two (cube 12, 12, 12)))
  )
)

; Nicenano holder unit test
; (def nicenano-holder-test
;   (let [
;     holder (make-nicenano-holder [0 0 0] 0)
;   ]
;     (union
;       ;dummy floor
;       (translate
;         [0  (- 2) (- 1)]
;         (mcube [22.4 37.7 1])
;       )
;       (first holder)
;       (second (second holder))
;       (difference
;         ; dummy front wall
;         (translate
;           [0 (- 2) 0]
;           (mcube [22.4 2 10])
;         )
;         (first (second holder))
;       )
;     )
;   )
; )
; (spit "things/nicenano-holder.scad"
;   (write-scad nicenano-holder-test)
; )

; Nicenano holder integration test
(def unit-test-position [-78 60 0])
(def unit-test-cube   (cube 150 120 40))
(def unit-test-space  (translate unit-test-position unit-test-cube))

(spit "things/nicenano-integration-test-plate.scad" 
      (write-scad (intersection (translate [0 0 (- 2.6)] plate-right) unit-test-space)))
(spit "things/nicenano-integration-test-wall.scad" 
      (write-scad (intersection model-right unit-test-space)))


; Thumb cluster unit test
; (def unit-test-position [-55 -60 0])
; (def unit-test-cube   (cube 100 150 200))
; (def unit-test-space  (translate unit-test-position unit-test-cube))
; (def unit-test (intersection model-right unit-test-space))
; (spit "things/thumb-cluster-unit-test.scad" 
;       (write-scad unit-test))

(defn -main [dum] 1)  ; dummy to make it easier to batch
