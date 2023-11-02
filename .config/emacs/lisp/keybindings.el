;; keybindings.el --- Convenient keybindings for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Keybindings I had setup but forgot to add to yadm lmao
;;; Code:

;; (use-package god-mode
;;   :straight t
;;   :bind (("<escape>" . god-mode-all)
;;          :map god-local-mode-map
;;          ("i" . god-local-mode)
;;          ("." . repeat))
;;   :config
;;   (add-hook 'after-init-hook #'god-mode-all))

(use-package evil
  :straight t
  :demand t
  :bind (( "<escape>" . keyboard-escape-quit )
         :map evil-normal-state-map
         ( "U" . evil-redo )
         ( "M-." . nil )
         ( "C-." . nil ))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-search-module 'isearch)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-want-fine-undo t)
  (setq evil-symbol-word-search t)
  :config
  (evil-mode t))

(use-package evil-collection
  :straight t
  :after evil
  :bind ( :map evil-normal-state-map
          ( "f" . evil-avy-goto-char-in-line)
          ( "gl" . evil-avy-goto-line))
  :init
  (setq evil-want-integration t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-leader
  :straight t
  :after evil
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "," #'execute-extended-command
    "b" #'beframe-switch-buffer
    "s" #'save-buffer
    "k" #'kill-buffer
    "f" #'find-file
    "r" #'recentf-open
    "qe" #'kill-emacs
    "qq" #'delete-frame
    "pp" #'project-switch-project
    "pb" #'project-switch-to-buffer
    "pk" #'project-kill-buffers
    "pe" #'project-eshell
    "pg" #'consult-ripgrep
    "pf" #'project-find-file)
  (global-evil-leader-mode))

(use-package evil-cleverparens
  :after (evil smartparens)
  :straight t
  :hook (smartparens-mode . evil-cleverparens-mode)
  :bind ( :map evil-insert-state-map
          ( "M-<" . #'evil-cp-<)
          ( "M->" . #'evil-cp->))
  :init
  (setq evil-cleverparens-use-regular-insert t)
  :config
  (push 'evil-cp-change evil-change-commands)
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-w") nil))

(use-package evil-cleverparens-text-objects
  :straight nil
  :after evil-cleverparens)

(use-package avy
  :straight t)

(defun conf/delete-ws-backward-till-char ()
  "Delete whitespace backwards till it encounters a character."
  (interactive)
  (save-excursion
    (delete-region
     (point)
     (re-search-backward (rx (not space))))))

(keymap-global-set "M-\\" #'conf/delete-ws-backward-till-char)

(defvar test/mylist
  '(1 2 3))

(provide 'keybindings)
;;; keybindings.el ends here
