(ns scratch.core
  (:gen-class))


(def frame-path
  "/home/arduino/ArduinoApps/ledmatrixtest/uno_matrix_frame.txt")

;; 5x7 font, each char = 5 columns, LSB = top pixel
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
  [s]
  (mapcat
   #(concat (get font % (get font \space)) [0]) ; spacing column
   s))

(defn frame-from-cols
  "Take 13 columns and pack into 4x32-bit frame"
  [cols]
  (let [cols (take 13 (concat cols (repeat 0)))]
    [(reduce bit-or
             (map-indexed
              (fn [i col] (bit-shift-left col (* i 8)))
              cols))
     0 0 0]))

(defn set-frame!
  [[a b c d]]
  (spit frame-path
        (format "0x%08X 0x%08X 0x%08X 0x%08X\n" a b c d)))

(defn scroll-text!
  [s]
  (let [cols (text-columns s)]
    (doseq [i (range (+ (count cols) 13))]
      (set-frame!
       (frame-from-cols (drop i cols)))
      (Thread/sleep 120))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
