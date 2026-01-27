(ns scratch.core)

(def frame-path
  "/home/arduino/ArduinoApps/ledmatrixtest/uno_matrix_frame.txt")

;; Matrix geometry (from datasheet): 13 columns × 8 rows = 104 pixels.
(def matrix-w 13)
(def matrix-h 8)

;; Physical orientation correction (set these once and forget about it)
(def ^:private flip-x? false)
(def ^:private flip-y? true)
(def font
  "5x7 ASCII font. Each char is 5 columns. Bit 0 is the TOP pixel of the column."
  {
   \space [0 0 0 0 0]
   \@ [62 65 93 89 78]
   \` [0 3 7 8 0]
   \! [0 0 95 0 0]
   \A [124 18 17 18 124]
   \a [32 84 84 120 64]
   \B [127 73 73 73 54]
   \b [127 40 68 68 56]
   \# [20 127 20 127 20]
   \C [62 65 65 65 34]
   \c [56 68 68 68 40]
   \$ [36 42 127 42 18]
   \D [127 65 65 65 62]
   \d [56 68 68 40 127]
   \% [35 19 8 100 98]
   \E [127 73 73 73 65]
   \e [56 84 84 84 24]
   \& [54 73 86 32 80]
   \F [127 9 9 9 1]
   \f [0 8 126 9 2]
   \' [0 8 7 3 0]
   \G [62 65 65 81 115]
   \g [24 164 164 156 120]
   \( [0 28 34 65 0]
   \H [127 8 8 8 127]
   \h [127 8 4 4 120]
   \) [0 65 34 28 0]
   \I [0 65 127 65 0]
   \i [0 68 125 64 0]
   \* [42 28 127 28 42]
   \J [32 64 65 63 1]
   \j [32 64 64 61 0]
   \+ [8 8 62 8 8]
   \K [127 8 20 34 65]
   \k [127 16 40 68 0]
   \, [0 128 112 48 0]
   \L [127 64 64 64 64]
   \l [0 65 127 64 0]
   \- [8 8 8 8 8]
   \M [127 2 28 2 127]
   \m [124 4 120 4 120]
   \. [0 0 96 96 0]
   \N [127 4 8 16 127]
   \n [124 8 4 4 120]
   \/ [32 16 8 4 2]
   \O [62 65 65 65 62]
   \o [56 68 68 68 56]
   \0 [62 81 73 69 62]
   \P [127 9 9 9 6]
   \p [252 24 36 36 24]
   \1 [0 66 127 64 0]
   \Q [62 65 81 33 94]
   \q [24 36 36 24 252]
   \2 [114 73 73 73 70]
   \R [127 9 25 41 70]
   \r [124 8 4 4 8]
   \3 [33 65 73 77 51]
   \S [38 73 73 73 50]
   \s [72 84 84 84 36]
   \4 [24 20 18 127 16]
   \T [3 1 127 1 3]
   \t [4 4 63 68 36]
   \5 [39 69 69 69 57]
   \U [63 64 64 64 63]
   \u [60 64 64 32 124]
   \6 [60 74 73 73 49]
   \V [31 32 64 32 31]
   \v [28 32 64 32 28]
   \7 [65 33 17 9 7]
   \W [63 64 56 64 63]
   \w [60 64 48 64 60]
   \8 [54 73 73 73 54]
   \X [99 20 8 20 99]
   \x [68 40 16 40 68]
   \9 [70 73 73 41 30]
   \Y [3 4 120 4 3]
   \y [76 144 144 144 124]
   \: [0 0 20 0 0]
   \Z [97 89 73 77 67]
   \z [68 100 84 76 68]
   \; [0 64 52 0 0]
   \[ [0 127 65 65 65]
   \{ [0 8 54 65 0]
   \< [0 8 20 34 65]
   \| [0 0 119 0 0]
   \= [20 20 20 20 20]
   \] [0 65 65 65 127]
   \} [0 65 54 8 0]
   \> [0 65 34 20 8]
   \^ [4 2 1 2 4]
   \~ [2 1 2 4 2]
   \? [2 1 89 9 6]
   \_ [64 64 64 64 64]
  })

(defn set-frame!
  "Write 4x32-bit frame words to the bridge file."
  [a b c d]
  (spit frame-path
        (format "0x%08X 0x%08X 0x%08X 0x%08X\n"
                (bit-and (long a) 0xFFFFFFFF)
                (bit-and (long b) 0xFFFFFFFF)
                (bit-and (long c) 0xFFFFFFFF)
                (bit-and (long d) 0xFFFFFFFF))))

(defn clear!
  []
  (set-frame! 0 0 0 0))

(defn xy->idx
  "Convert (x,y) into the matrix's linear pixel index.
  x: 0..12 left→right
  y: 0..7  top→bottom

  flip-x?/flip-y? correct for physical mirroring on the UnoQ matrix."
  [x y]
  (let [x (long x)
        y (long y)
        x (if flip-x? (- (dec matrix-w) x) x)
        y (if flip-y? (- (dec matrix-h) y) y)]
    (+ (* y matrix-w) x)))

(defn frame-from-on-indices
  "Given pixel indices (0..103), return [w0 w1 w2 w3].

  IMPORTANT: UnoQ matrix frames are packed MSB-first within each 32-bit word.
  The last 8 pixels (indices 96..103) live in word3 bits 31..24."
  [idxs]
  (reduce
    (fn [[w0 w1 w2 w3] i]
      (let [i    (long i)
            word (quot i 32)
            k    (rem i 32)
            bit  (bit-shift-left 1 (- 31 k))] ; MSB-first packing
        (case word
          0 [(bit-or w0 bit) w1 w2 w3]
          1 [w0 (bit-or w1 bit) w2 w3]
          2 [w0 w1 (bit-or w2 bit) w3]
          3 [w0 w1 w2 (bit-or w3 bit)])))
    [0 0 0 0]
    idxs))

(defn set-pixel!
  "Set a single pixel, clearing everything else (useful for testing)."
  [x y]
  (let [[a b c d] (frame-from-on-indices [(xy->idx x y)])]
    (set-frame! a b c d)))

(defn show-row!
  "Light an entire row y (0..7)."
  [y]
  (let [idxs (for [x (range matrix-w)] (xy->idx x y))
        [a b c d] (frame-from-on-indices idxs)]
    (set-frame! a b c d)))

(defn show-col!
  "Light an entire column x (0..12)."
  [x]
  (let [idxs (for [y (range matrix-h)] (xy->idx x y))
        [a b c d] (frame-from-on-indices idxs)]
    (set-frame! a b c d)))

(defn- text-columns
  "String -> seq of 5x7 column bitmasks (5 cols per char + 1 blank spacer).
  Normalizes to lowercase so \"I\" uses the \\i glyph unless you add \\I explicitly."
  [s]
  (mapcat (fn [ch]
            (let [ch (Character/toLowerCase ^char ch)]
              (concat (get font ch (get font \space)) [0])))
          s))

(defn- window->frame
  "Render a 13×8 window starting at horizontal offset x0 over the column stream."
  [cols x0]
  (let [cols (vec (concat cols (repeat matrix-w 0)))
        idxs (for [x (range matrix-w)
                   y (range 7) ; font is 7px tall
                   :let [col (nth cols (+ x0 x) 0)
                         on? (not (zero? (bit-and col (bit-shift-left 1 y))))]
                   :when on?]
               (xy->idx x y))]
    (frame-from-on-indices idxs)))

(defn scroll-text!
  "Scroll text left across the 13×8 matrix by writing frames to the bridge file."
  ([s] (scroll-text! s 90))
  ([s delay-ms]
   (let [cols (concat (text-columns s) (repeat matrix-w 0))
         n    (count cols)]
     (doseq [x0 (range n)]
       (let [[a b c d] (window->frame cols x0)]
         (set-frame! a b c d)
         (Thread/sleep delay-ms))))))

(defn- string->cols
  "Convert a string into a vector of column bitmasks (5 cols/char + 1 spacer)."
  [s]
  (vec (text-columns s)))

(defn- cols->frame
  "Render exactly one frame from a column vector, with optional alignment.
  align: :left | :center | :right
  yoff: vertical offset (0 or 1; font is 7 tall, matrix is 8)."
  ([cols] (cols->frame cols :left 1))
  ([cols align yoff]
   (let [W matrix-w
         H matrix-h
         x0 (case align
              :center (max 0 (quot (- (count cols) W) 2))
              :right  (max 0 (- (count cols) W))
              :left   0
              0)
         idxs (for [x (range W)
                    y (range 7)
                    :let [col (nth cols (+ x0 x) 0)
                          on? (not (zero? (bit-and col (bit-shift-left 1 y))))
                          yy (+ yoff y)]
                    :when (and on? (<= 0 yy (dec H)))]
                (xy->idx x yy))]
     (frame-from-on-indices idxs))))

(defn show-text!
  "Display a string as a single static frame (no scrolling).
  Options:
    align: :left (default) | :center | :right
    yoff: 1 (default) to visually center 7px text in 8px height; 0 to pin to top."
  ([s] (show-text! s :left 1))
  ([s align yoff]
   (let [cols (string->cols s)
         [a b c d] (cols->frame cols align yoff)]
     (set-frame! a b c d))))
