;;; exwm/config.el -*- lexical-binding: t; -*-

(defun korv/set-wallpaper (file)
  "Set the DE Wallpaper to be $FILE"
  (interactive)
  (start-process-shell-command "feh" nil (format "feh --bg-scale %s" file)))

(defun korv/exwm-get-index (index)
    (- index 1))

(defun korv/reload-tray ()
  (interactive)
  (exwm-systemtray--exit)
  (exwm-systemtray--init))

(defun korv/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun korv/configure-window-by-class ()
  (interactive)
  (pcase (downcase (or exwm-class-name ""))
    ("discord" (progn
                 (exwm-workspace-move-window (korv/exwm-get-index 1))
                 (korv/reload-tray)))
    ("microsoft-edge-beta" (exwm-workspace-rename-buffer "Edge"))
    ("brave-browser" (exwm-workspace-rename-buffer "Brave"))))

(defun korv/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun korv/exwm-init-hook ()

  ;; modeline
  (display-battery-mode t)
  (setq display-time-format "[  %a %b %Y | %I:%M %p ]")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  (korv/run-in-background "nm-applet")
  (korv/run-in-background "pasystray"))


(use-package desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-mode))

(use-package exwm
  :config
  (setq exwm-workspace-number 4)

  (setq exwm-workspace-show-all-buffers t)

  (setq exwm-layout-show-all-buffers nil)

  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string (+ 1 index))))

  (add-hook 'exwm-update-class-hook #'korv/exwm-update-class)

  (add-hook 'exwm-init-hook #'korv/exwm-init-hook)

  (add-hook 'exwm-manage-finish-hook #'korv/configure-window-by-class)

  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")

  (korv/set-wallpaper "/usr/share/backgrounds/archlinux/awesome.png")

  (setq exwm-input-prefix-keys
        '(?\C-z
          ?\C-x
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))


  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (exwm-input-set-key (kbd "<M-return>") 'term)

  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 20)
  (setq exwm-systemtray-icon-gap 5)
  (exwm-systemtray-enable)

  (setq exwm-input-global-keys
        '(([?\M- ] . app-launcher-run-app)
          ([?\M-r] . exwm-reset)
          ([M-left] . windmove-left)
          ([M-right] . windmove-right)
          ([M-up] . windmove-up)
          ([M-down] . windmove-down)
          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ([?\M-e] . (lambda () (interactive) (dired "~")))

          ([?\s-w] . exwm-workspace-switch)

          ([?\M-q] . (lambda () (interactive) (kill-buffer)))

          ([?\M-1] . (lambda () (interactive)
                       (exwm-workspace-switch-create (korv/exwm-get-index 1))))
          ([?\M-2] . (lambda () (interactive)
                       (exwm-workspace-switch-create (korv/exwm-get-index 2))))
          ([?\M-3] . (lambda () (interactive)
                       (exwm-workspace-switch-create (korv/exwm-get-index 3))))
          ([?\M-4] . (lambda () (interactive)
                       (exwm-workspace-switch-create (korv/exwm-get-index 4))))))
 (exwm-enable))
