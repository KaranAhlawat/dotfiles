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

(defvar *phundrak-nord-list* `(,phundrak-nord1   ;; 0 black
                               ,phundrak-nord11  ;; 1 red
                               ,phundrak-nord14  ;; 2 green
                               ,phundrak-nord13  ;; 3 yellow
                               ,phundrak-nord10  ;; 4 blue
                               ,phundrak-nord14  ;; 5 magenta
                               ,phundrak-nord8   ;; 6 cyan
                               ,phundrak-nord5)) ;; 7 white

(defvar k-tnight00 "#f7768e")
(defvar k-tnight01 "#ff9e64")
(defvar k-tnight02 "#e0af68")
(defvar k-tnight03 "#9ece6a")
(defvar k-tnight04 "#73daca")
(defvar k-tnight05 "#b4f9f8")
(defvar k-tnight06 "#2ac3de")
(defvar k-tnight07 "#7dcfff")
(defvar k-tnight08 "#7aa2f7")
(defvar k-tnight09 "#bb9af7")
(defvar k-tnight10 "#c0caf5")
(defvar k-tnight11 "#a9b1d6")
(defvar k-tnight12 "#9aa5ce")
(defvar k-tnight13 "#cfc9c2")
(defvar k-tnight14 "#565f89")
(defvar k-tnight15 "#414868")
(defvar k-tnight16 "#24283b")
(defvar k-tnight17 "#1a1b26")

(defvar *k-tokyonight-list* `(,k-tnight17   ;; 0 black
                              ,k-tnight00   ;; 1 red
                              ,k-tnight04   ;; 2 green
                              ,k-tnight02   ;; 3 yellow
                              ,k-tnight08   ;; 4 blue
                              ,k-tnight09   ;; 5 magenta
                              ,k-tnight07   ;; 6 cyan
                              ,k-tnight10)) ;; 7 white
                              
                              

(setq *colors* *k-tokyonight-list*)

(when *initializing*
  (update-color-map (current-screen)))
