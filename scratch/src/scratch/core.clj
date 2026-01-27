(ns scratch.core)

(def frame-path
  "/home/arduino/ArduinoApps/ledmatrixtest/uno_matrix_frame.txt")

(def font
  "5x7 font. Each char is 5 columns. Bit 0 is the TOP pixel of the column."
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

(defn clear! []
  (set-frame! 0 0 0 0))

(defn- text-columns
  "String -> seq of column bitmasks (5 cols per char + 1 blank spacer)."
  [s]
  (mapcat (fn [ch]
            (concat (get font ch (get font \space)) [0]))
          s))

(defn- rows->frame
  "Pack 13 row-bytes (each 8 bits wide) into 4x32-bit words.
  Row 0 is the BOTTOM row; increasing row index goes UP."
  [rows]
  (let [rows (take 13 (concat rows (repeat 0)))]
    (reduce (fn [[w0 w1 w2 w3] [i row-byte]]
              (let [rb (bit-and (long row-byte) 0xFF)
                    word (quot i 4)
                    shift (* (rem i 4) 8)
                    v (bit-shift-left rb shift)]
                (case word
                  0 [(bit-or w0 v) w1 w2 w3]
                  1 [w0 (bit-or w1 v) w2 w3]
                  2 [w0 w1 (bit-or w2 v) w3]
                  3 [w0 w1 w2 (bit-or w3 v)])))
            [0 0 0 0]
            (map-indexed vector rows))))

(defn- window-rows
  "Given full text as columns, produce 13 row-bytes for an 8x13 window
  at horizontal offset x0.

  Options:
  - base-y: where the 7px-tall font sits within 13 rows (default 3 centers it)
  - mirror-x?: if text is mirrored, set true
  - flip-y?: if text is upside-down, set true"
  ([cols x0] (window-rows cols x0 {:base-y 3 :mirror-x? false :flip-y? false}))
  ([cols x0 {:keys [base-y mirror-x? flip-y?] :or {base-y 3 mirror-x? false flip-y? false}}]
   (let [W 8
         H 13
         ;; pad so we can read past the end safely
         cols (vec (concat cols (repeat W 0)))]
     (mapv
      (fn [row] ; row 0 = bottom
        (let [y (if flip-y? (- (dec H) row) row)]
          (reduce
           (fn [acc x]
             (let [xwin (if mirror-x? (- (dec W) x) x)
                   col  (nth cols (+ x0 xwin) 0)
                   ;; font bits: bit 0 = top of 7px glyph
                   ;; map glyph y (0..6, top->bottom) into display rows (base-y..base-y+6).
                   ;; display row index increases upward; row 0 is bottom.
                   ;; so target display y = base-y + (6 - glyph-y)
                   acc (if (and (<= base-y y (+ base-y 6)))
                         (let [glyph-y (- 6 (- y base-y))
                               on? (not (zero? (bit-and col (bit-shift-left 1 glyph-y))))]
                           (if on? (bit-or acc (bit-shift-left 1 x)) acc))
                         acc)]
               acc))
           0
           (range W))))
      (range H)))))

(defn scroll-text!
  "Scroll text across the 8x13 matrix by writing frames to the bridge file."
  ([s] (scroll-text! s 90))
  ([s delay-ms]
   (let [cols (concat (text-columns s) (repeat 8 0))
         n (count cols)]
     (doseq [x0 (range n)]
       (let [rows (window-rows cols x0 {:base-y 3 :mirror-x? false :flip-y? false})
             [a b c d] (rows->frame rows)]
         (set-frame! a b c d)
         (Thread/sleep delay-ms))))))
