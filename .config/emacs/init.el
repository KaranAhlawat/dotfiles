;;; init.el --- Initial file read by Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; This file ties all the modules together
;;; Code:

;; Add ~/.config/emacs/lisp to the load-path
(eval-and-compile
	(add-to-list
	 'load-path
	 (concat (file-name-as-directory user-emacs-directory) "lisp/")))

(set-default-coding-systems 'utf-8)
(setq load-suffixes (list ".so" ".eln" ".elc" ".el"))
(setq load-prefer-newer t)

;; Straight.el is my current package manager of choice, even with the
;; recent improvements to package.el in Emacs >=29. Also, since
;; use-package is now built into Emacs itself, I'll be opting to use
;; that and it's straight.el intergration.

;; Here we're just bootstraping straight.el as noted in their github README.
(defvar bootstrap-version)
(defvar straight-repository-branch nil)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
			 (expand-file-name "straight/repos/straight.el/bootstrap.el"
												 user-emacs-directory))
			(bootstrap-version 6))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
				(url-retrieve-synchronously
				 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
				 'silent
				 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

(declare-function straight-use-package "straight")
(straight-use-package 'org)

;; Setting the custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
	(load custom-file nil 'nomessage))

;; Enable some disabled commands
(put 'dired-find-alternate-file 'disabled nil)

(require 'setup-org)
(require 'windows)

(use-package popper
	:straight t
  :demand t
	:bind
	(("C-`" . popper-toggle)
	 ("C-M-`" . popper-cycle)
	 ("C-c p t" . popper-toggle-type)
	 ("C-M-q" . popper-kill-latest-popup))
	:init
	(setq popper-group-function #'popper-group-by-project)
	(setq popper-reference-buffers
				(append
				 conf/help-modes-list
				 conf/man-modes-list
				 conf/repl-modes-list
				 conf/repl-names-list
				 conf/occur-grep-modes-list
				 '(Custom-mode (compilation-mode . hide) messages-buffer-mode)
				 '(("^\\*Warnings\\*$" . hide)
					 ("^\\*Compile-Log\\*$" . hide)
					 "^\\*Backtrace\\*"
					 "^\\*Apropos"
					 "^Calc:"
					 "^\\*eldoc\\(.*\\)\\*"
					 "^\\*TeX errors\\*"
					 "^\\*ielm\\*"
					 "^\\*TeX Help\\*"
					 "\\*Shell Command Output\\*"
					 ("\\*Async Shell Command\\*" . hide)
					 "\\*Completions\\*"
					 "[Oo]utput\\*"
					 "\\*EGLOT\\(.*\\)\\*"
           "^\\*tree-sitter explorer for \\(.+\\)\\*"
					 ("^\\*straight-process\\*" . hide)
					 ("^\\*straight-byte-compilation\\*" . hide)
					 ("^\\*Async-native-compile-log" . hide))))

	(setq popper-display-function
				(defun conf/popper-select-below (buffer &optional _alist)
					(funcall (if (> (frame-width) 170)
											 #'popper-select-popup-at-bottom
										 #'display-buffer-at-bottom)
									 buffer
									 `((window-height . ,popper-window-height)
										 (direction . below)
										 (body-function . ,#'select-window)))))
  (setq popper-display-control t)
  
  :config
	(advice-add
	 'popper-cycle
	 :after
	 (defun conf/popper-cycle-repeated (&rest _)
		 "Continue to cycle popups with the grave key."
		 (set-transient-map
			(let ((map (make-sparse-keymap)))
				(define-key map (kbd "`") #'popper-cycle)
				map))))

	(use-package
		popper-echo
		:defer 3
		:init
		(defun popper-message-shorten (name)
			(cond
			 ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
				(concat (match-string 1 name) "(H)"))
			 ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
				(concat (match-string 1 name) "(H)"))
			 ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
				(concat
				 (match-string 1 name)
				 (if (string-empty-p (match-string 1 name))
						 "shell(E)"
					 "(E)")))
			 ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
				(concat (match-string 1 name) "(O)"))
			 ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
				(concat (match-string 1 name) "(L)"))
			 ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
				(concat (match-string 1 name) "(C)"))
			 (t
				name)))
		(setq popper-mode-line
					'(:eval (propertize " POP " 'face 'mode-line-highlight)))
		(setq popper-echo-transform-function #'popper-message-shorten)
		(setq
		 popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
		 popper-echo-dispatch-actions t)
    :config
		(advice-add
		 'popper-echo
		 :around
		 (defun conf/popper-echo-no-which-key (orig-fn)
			 (let ((which-key-show-transient-maps nil))
				 (funcall orig-fn))))
		(popper-echo-mode))

	(popper-mode))

(use-package custom
	:init
	(defvar after-enable-theme-hook nil)
	(defun run-after-enable-theme-hook (&rest _args)
		(run-hooks 'after-enable-theme-hook))
	:config
	(declare-function run-after-enable-theme-hook "init")
	(advice-add 'enable-theme :after #'run-after-enable-theme-hook))

(require 'general)
(require 'keybindings)
(require 'ui)
(require 'ligate)
(require 'defaults)
(require 'completions)
(require 'languages)
(require 'development)
(require 'shells)
(require 'setup-latex)
(require 'reader)

(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
