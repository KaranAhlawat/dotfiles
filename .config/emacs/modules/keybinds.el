;;; keybinds.el -*- leixcal-binding: t; -*-

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "M-i") 'consult-imenu)
(global-set-key (kbd "C-h a") 'consult-apropos)
(global-set-key (kbd "C-c q f") 'delete-frame)
(global-set-key (kbd "C-c q e") 'kill-emacs)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-\\") 'backward-delete-ws-till-char)

(provide 'keybinds)
