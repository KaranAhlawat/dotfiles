;; slynk.lisp -*- Mode: Lisp -*-
(in-package :stumpwm)

;;load slynk
(ql:quickload :slynk)

(let ((server-running nil)
      (first-time t))

  (defcommand slynk() ()
    "Toggle slynk on/off"
    (if server-running
        (progn
          (slynk:stop-server 4005)
          (echo-string
           (current-screen)
           "Stopping slynk.")
          (setf server-running nil))
        (progn
          (slynk:create-server :port 4005
                               :style slynk:*communication-style*
                               :dont-close t)
          (if first-time
              (echo-string
               (current-screen)
               "Re-starting slynk")
              (setf first-time nil))
          (setf server-running t)))))

(slynk)
         
      
