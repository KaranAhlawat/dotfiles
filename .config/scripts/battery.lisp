#!/usr/bin/env sbcl

(defun notify-idiom ()
  (let* ((parsed-val (with-open-file (in "/sys/class/power_supply/BAT0/capacity")
                       (read-line in nil nil)))
         (bat-level (parse-integer parsed-val)))
    (if (>= bat-level 90)
        (uiop:run-program "notify-send --urgency=normal 'Battery full' 'Battery has been charged to 90%. Please switch off.'"))
    (if (<= bat-level 10)
        (uiop:run-program "notify-send --urgency=normal 'Battery low!' 'Battery is below 10%. Please plug in.'"))))
