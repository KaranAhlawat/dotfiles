(in-package :stumpwm)

(run-shell-command "xmodmap -e 'clear mod4'" t)
(run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
(set-prefix-key (kbd "F20"))

(which-key-mode)

(define-key *root-map* (kbd "SPC") "exec rofi -show drun")

(defvar *k-screenshot-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") "exec flameshot gui")
    (define-key m (kbd "s") "exec flameshot screen -c")
    (define-key m (kbd "f") "exec flameshot full -c")
    m))

(defvar *k-apps-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "w") "exec microsoft-edge-beta")
    (define-key m (kbd "d") "exec discord")
    (define-key m (kbd "e") "exec emacsclient -c")
    m))

(define-key *root-map* (kbd "a") '*k-apps-map*)

(define-key *top-map* (kbd "M-RET") "term")
(define-key *top-map* (kbd "Print") '*k-screenshot-map*)

(defvar *k-session-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") "end-session")
    (define-key m (kbd "l") "logout")
    (define-key m (kbd "s") "suspend-computer")
    (define-key m (kbd "S") "shutdown-computer")
    (define-key m (kbd "r") "loadrc")
    (define-key m (kbd "R") "restart-hard")
    (define-key m (kbd "C-r") "restart-computer")
    m))

(define-key *root-map* (kbd "q") '*k-session-map*)

(defvar *korv/workspaces* (list "I" "II" "III" "IV" "V"))
(stumpwm:grename (nth 0 *korv/workspaces*))
(dolist (workspace (cdr *korv/workspaces*))
  (stumpwm:gnewbg workspace))

(defvar *move-to-keybinds* (list "!" "@" "#" "$" "%" "^" "&" "*" "("))
(dotimes (y (length *korv/workspaces*))
  (let ((workspace (write-to-string (+ y 1))))
    (define-key *root-map* (kbd workspace) (concat "gselect " workspace))
    (define-key *root-map* (kbd (nth y *move-to-keybinds*)) (concat "gmove-and-follow " workspace))))

(define-key *root-map* (kbd "g") '*groups-map*)
(define-key *groups-map* (kbd "G") "vgroups")

(defvar *k-frames-float-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "float-this")
    (define-key m (kbd "u") "unfloat-this")
    (define-key m (kbd "C-f") "flatten-floats")
    m))

(defvar *k-frames-management-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "h") "move-focus left")
    (define-key m (kbd "j") "move-focus down")
    (define-key m (kbd "k") "move-focus up")
    (define-key m (kbd "l") "move-focus right")
    (define-key m (kbd "H") "move-window left")
    (define-key m (kbd "J") "move-window down")
    (define-key m (kbd "K") "move-window up")
    (define-key m (kbd "L") "move-window right")
    (define-key m (kbd "/") "hsplit-and-focus")
    (define-key m (kbd "-") "vsplit-and-focus")
    (define-key m (kbd "s") "hsplit")
    (define-key m (kbd "v") "vsplit")
    (define-key m (kbd "S") "hsplit-equally")
    (define-key m (kbd "V") "vsplit-equally")
    (define-key m (kbd ".") "iresize")
    (define-key m (kbd "+") "balance-frames")
    (define-key m (kbd "d") "remove-split")
    (define-key m (kbd "D") "only")
    (define-key m (kbd "f") "fullscreen")
    (define-key m (kbd "F") '*k-frames-float-map*)
    (define-key m (kbd "U") "unmaximize")
    m))

(define-key *root-map* (kbd "w") '*k-frames-management-map*)

(defvar *k-buffers-management-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b") "windowlist")
    (define-key m (kbd "d") "delete-window")
    (define-key m (kbd "D") "delete-window-and-frame")
    (define-key m (kbd "k") "kill-window")
    (define-key m (kbd "o") "other-window")
    m))

(define-key *root-map* (kbd "b") '*k-buffers-management-map*)
(define-key *top-map* (kbd "M-TAB") "next")
(define-key *top-map* (kbd "M-S-TAB") "prev")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer -q set Master 2%+ unmute")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer -q set Master 2%- unmute")
(define-key *top-map* (kbd "XF86AudioMute") "exec amixer -q set Master 1+ toggle")

(define-key *root-map* (kbd "B") "beckon")
(define-key *root-map* (kbd "C-b") "banish")
(define-key *root-map* (kbd "r") "reload")
