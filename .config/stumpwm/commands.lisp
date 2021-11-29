(defcommand delete-window-and-frame () ()
            "Delete the current frame with it's window."
            (delete-window)
            (remove-split))

(defcommand hsplit-and-focus () ()
            "Create a new frame on the right and focus it."
            (hsplit)
            (move-focus :right))

(defcommand vsplit-and-focus () ()
            "Create a new frame below and focus it."
            (vsplit)
            (move-focus :down))

(defcommand term (&optional program) ()
            "Invoke a terminal, possible with a @arg{program}."
            (run-shell-command (if program
                                 (format nil "kitty ~A" program)
                                 "kitty")))
