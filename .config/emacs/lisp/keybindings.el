;;; keybindings.el --- Convenient keybindings for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Keybindings I had setup but forgot to add to yadm lmao
;;; Code:

(use-package god-mode
  :straight t
  :bind (("<escape>" . god-mode-all)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("." . repeat))
  :config
  (god-mode))

(defun conf/delete-ws-backward-till-char ()
  "Delete whitespace backwards till it encounters a character."
  (interactive)
  (save-excursion
    (delete-region
     (point)
     (re-search-backward (rx (not space))))))

(keymap-global-set "M-\\" #'conf/delete-ws-backward-till-char)

(provide 'keybindings)
;;; keybindings.el ends here
