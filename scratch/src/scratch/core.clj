(ns scratch.core)

(def frame-path
  "/home/arduino/ArduinoApps/ledmatrixtest/uno_matrix_frame.txt")

;; Matrix geometry (from datasheet): 13 columns × 8 rows = 104 pixels.
(def matrix-w 13)
(def matrix-h 8)

;; Physical orientation correction (set these once and forget about it)
(def ^:private flip-x? true)
(def ^:private flip-y? true)

(def font
  "5x7 font. Each char is 5 columns. Bit 0 is the TOP pixel of the column.
  (We normalize input to lowercase when rendering.)"
  {\h [0x7F 0x08 0x08 0x08 0x7F]
   \e [0x7F 0x49 0x49 0x49 0x41]
   \l [0x7F 0x01 0x01 0x01 0x01]
   \o [0x3E 0x41 0x41 0x41 0x3E]
   \f [0x7F 0x48 0x48 0x48 0x40]
   \r [0x7F 0x08 0x08 0x08 0x10]
   \m [0x7F 0x20 0x10 0x20 0x7F]
   \c [0x3E 0x41 0x41 0x41 0x22]
   \j [0x02 0x01 0x01 0x01 0x7E]
   \u [0x3E 0x01 0x01 0x01 0x3F]
   \i [0x00 0x41 0x7D 0x01 0x00]
   \space [0 0 0 0 0]})

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
