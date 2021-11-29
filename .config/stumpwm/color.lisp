(defvar phundrak-nord0 "#2e3440")
(defvar phundrak-nord1 "#3b4252")
(defvar phundrak-nord2 "#434c5e")
(defvar phundrak-nord3 "#4c566a")
(defvar phundrak-nord4 "#d8dee9")
(defvar phundrak-nord5 "#e5e9f0")
(defvar phundrak-nord6 "#eceff4")
(defvar phundrak-nord7 "#8fbcbb")
(defvar phundrak-nord8 "#88c0d0")
(defvar phundrak-nord9 "#81a1c1")
(defvar phundrak-nord10 "#5e81ac")
(defvar phundrak-nord11 "#bf616a")
(defvar phundrak-nord12 "#d08770")
(defvar phundrak-nord13 "#ebcb8b")
(defvar phundrak-nord14 "#a3be8c")
(defvar phundrak-nord15 "#b48ead")

(setq *colors*
      `(,phundrak-nord1   ;; 0 black
        ,phundrak-nord11  ;; 1 red
        ,phundrak-nord14  ;; 2 green
        ,phundrak-nord13  ;; 3 yellow
        ,phundrak-nord10  ;; 4 blue
        ,phundrak-nord14  ;; 5 magenta
        ,phundrak-nord8   ;; 6 cyan
        ,phundrak-nord5)) ;; 7 white

(when *initializing*
  (update-color-map (current-screen)))
