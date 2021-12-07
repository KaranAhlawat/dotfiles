#!/usr/bin/env -S sbcl --script

(require 'uiop)

(defun notify-battery ()
  (let* ((parsed-bat (with-open-file (in "/sys/class/power_supply/BAT0/capacity")
                                     (read-line in nil nil)))
         (parsed-status (with-open-file (in "/sys/class/power_supply/AC0/online")
                                        (read-line in nil nil)))
         (bat-level (parse-integer parsed-bat))
         (ac-status (parse-integer parsed-status)))
    (if (and (>= bat-level 90) (= 1 ac-status))
        (uiop:run-program "notify-send --urgency=normal 'Battery full' 'Battery has been charged to 90%. Please switch off.'"))
    (if (and (<= bat-level 10) (= 0 ac-status))
        (uiop:run-program "notify-send --urgency=normal 'Battery low!' 'Battery is below 10%. Please plug in.'"))))

(notify-battery)
