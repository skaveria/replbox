(ns scratch.core)

(def frame-path
  "/home/arduino/ArduinoApps/ledmatrixtest/uno_matrix_frame.txt")

;; 5x7 font, each char = 5 columns, LSB = top pixel (we can flip later if needed)
(def font
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

(defn text-columns
  "Turn a string into an infinite-ish column stream (5 cols per char + 1 spacer col)."
  [s]
  (mapcat (fn [ch]
            (concat (get font ch (get font \space)) [0]))
          s))

(defn frame-from-cols
  "Pack 13 8-bit columns into 4x32-bit words.
  Columns 0-3 -> word0, 4-7 -> word1, 8-11 -> word2, 12 -> word3."
  [cols]
  (let [cols (take 13 (concat cols (repeat 0)))]
    (reduce (fn [[w0 w1 w2 w3] [i col]]
              (let [col (bit-and (long col) 0xFF)
                    word (quot i 4)
                    shift (* (rem i 4) 8)
                    v (bit-shift-left col shift)]
                (case word
                  0 [(bit-or w0 v) w1 w2 w3]
                  1 [w0 (bit-or w1 v) w2 w3]
                  2 [w0 w1 (bit-or w2 v) w3]
                  3 [w0 w1 w2 (bit-or w3 v)])))
            [0 0 0 0]
            (map-indexed vector cols))))

(defn set-frame!
  [[a b c d]]
  (spit frame-path
        (format "0x%08X 0x%08X 0x%08X 0x%08X\n"
                (bit-and (long a) 0xFFFFFFFF)
                (bit-and (long b) 0xFFFFFFFF)
                (bit-and (long c) 0xFFFFFFFF)
                (bit-and (long d) 0xFFFFFFFF))))

(defn clear! []
  (set-frame! [0 0 0 0]))

(defn scroll-text!
  ([s] (scroll-text! s 120))
  ([s delay-ms]
   (let [cols (text-columns s)
         ;; add padding so it fully scrolls off
         cols (concat cols (repeat 13 0))
         n (count cols)]
     (doseq [i (range n)]
       (set-frame! (frame-from-cols (drop i cols)))
       (Thread/sleep delay-ms)))))
