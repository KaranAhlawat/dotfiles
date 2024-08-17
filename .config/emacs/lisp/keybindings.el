;;; keybindings.el --- Convenient keybindings for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Keybindings I had setup but forgot to add to yadm lmao
;;; Code:

(use-package evil
  :straight t
  :demand t
  :bind (( "<escape>" . keyboard-escape-quit )
         :map evil-normal-state-map
         ( "U" . evil-redo )
         ( "M-." . nil )
         ( "C-." . nil )
         ( "<SPC>" . nil)
         ( "/" . ctrlf-forward-literal )
         ( "?" . ctrlf-backward-literal )
         ( "*"  . ctrlf-forward-symbol-at-point )
         ( "C-n" . evil-ex-nohighlight )
         :map evil-visual-state-map
         ( "/" . ctrlf-forward-literal )
         ( "?" . ctrlf-backward-literal )
         ( "*"  . ctrlf-forward-symbol-at-point )
         :map evil-insert-state-map
         ( "C-y" . nil ))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-want-fine-undo t)
  (setq evil-symbol-word-search nil)
  (setq evil-default-cursor 't)
  (setq evil-insert-state-cursor 't)
  :config
  (evil-mode t))

(use-package evil-collection
  :straight t
  :after evil
  :bind ( :map evil-normal-state-map
          ( "f"  . #'evil-avy-goto-char-in-line )
          ( "gl" . #'evil-avy-goto-line )
          ( "gs"  . #'evil-avy-goto-char-timer )
          :map evil-visual-state-map
          ( "f" . #'evil-avy-goto-char-in-line ))
  :init
  (setq evil-want-integration t)
  :config
  (evil-collection-init))

(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode))

(use-package evil-leader
  :straight t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" #'execute-extended-command
    "b" #'consult-buffer
    "s" #'save-buffer
    "k" #'kill-buffer
    "f" #'find-file
    "r" #'consult-recent-file
    "w" evil-window-map
    "qe" #'kill-emacs
    "qq" #'delete-frame
    "pp" #'project-switch-project
    "pb" #'project-switch-to-buffer
    "pk" #'project-kill-buffers
    "pf" #'project-find-file
    "pg" #'consult-ripgrep
    "af" #'apheleia-format-buffer
    "hv" #'helpful-variable
    "hf" #'helpful-callable
    "hk" #'helpful-key
    "hi" #'info
    "gi" #'consult-imenu
    "xe" #'eval-last-sexp
    "xp" #'pp-eval-last-sexp)
  (global-evil-leader-mode))

(use-package evil-cleverparens
  :straight t
  :after (evil smartparens)
  :hook ((clojure-mode emacs-lisp-mode lisp-mode) . evil-cleverparens-mode)
  :config
  (push 'evil-cp-change evil-change-commands))

(use-package avy
  :straight t)

(defun conf/delete-ws-backward-till-char ()
  "Delete whitespace backwards till it encounters a character."
  (declare (indent defun))
  (interactive)
  (save-excursion
    (delete-region
     (point)
     (1+ (re-search-backward (rx (not (in blank space control))))))))

(keymap-global-set "M-\\" #'conf/delete-ws-backward-till-char)

(provide 'keybindings)
;;; keybindings.el ends here
