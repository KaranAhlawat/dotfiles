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
			  :map evil-insert-state-map
			  ( "C-y" . nil ))
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
	(setq evil-default-cursor 't)
	(setq evil-insert-state-cursor 't)
	:config
	(evil-set-initial-state 'eat-mode 'insert)
	(evil-mode t))

(use-package evil-collection
	:straight t
	:after evil
	:bind ( :map evil-normal-state-map
			  ( "f"	 . #'evil-avy-goto-char-in-line )
			  ( "gl" . #'evil-avy-goto-line )
			  ( "gs"	. #'evil-avy-goto-char-timer ))
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
		"b" #'consult-buffer
		"s" #'save-buffer
		"k" #'kill-buffer
		"f" #'find-file
		"r" #'recentf-open
		"w" evil-window-map
		"qe" #'kill-emacs
		"qq" #'delete-frame
		"pp" #'project-switch-project
		"pb" #'project-switch-to-buffer
		"pk" #'project-kill-buffers
		"pf" #'project-find-file
		"pg" #'consult-ripgrep
		"pe" #'eat-project-other-window
		"af" #'apheleia-format-buffer
		"hv" #'describe-variable
		"hf" #'describe-function
		"hk" #' describe-key
		"hi" #'info
		"gi" #'consult-imenu
		"xe" #'eval-last-sexp
		"xp" #'pp-eval-last-sexp)
	(global-evil-leader-mode))

(use-package evil-cleverparens
	:after (evil smartparens)
	:straight t
	:hook (smartparens-mode . evil-cleverparens-mode)
	:bind ( :map evil-insert-state-map
			  ( "M-<" . #'evil-cp-< )
			  ( "M->" . #'evil-cp-> ))
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
	(declare (indent defun))
	(interactive)
	(save-excursion
		(delete-region
			(point)
			(1+ (re-search-backward (rx (not (in blank space control))))))))

(keymap-global-set "M-\\" #'conf/delete-ws-backward-till-char)

(provide 'keybindings)
;;; keybindings.el ends here
