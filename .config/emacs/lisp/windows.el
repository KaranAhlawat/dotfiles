;;; windows.el --- Windows, buffers, popus -*- lexical-binding: t -*-
;;; Commentary:
;;; Setup some variables for now.
;;; Most credit goes to Karthink for this.
;;; Code:

(setq fit-window-to-buffer-horizontally nil
      fit-frame-to-buffer t)

(use-package beframe
  :straight t
  :init
  (setq beframe-global-buffers
        '("*Messages*" "*Org Agenda*" "*scratch*"))
  (setq beframe-functions-in-frames '(project-prompt-project-dir))
  (setq beframe-create-frame-scratch-buffer nil)
  (setq beframe-kill-frame-scratch-buffer nil)
  (setq beframe-rename-function #'beframe-rename-frame)

  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe--consult-source
      `(:name
        "Frame-specific buffers (current frame)"
        :narrow ?F
        :category buffer
        :face beframe-buffer
        :history beframe-history
        :items ,#'beframe--buffer-names
        :action ,#'switch-to-buffer
        :state ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe--consult-source))
  :config
  (beframe-mode +1))

(defvar conf/occur-grep-modes-list
  '(occur-mode
    grep-mode
    xref--xref-buffer-mode
    locate-mode
    flymake-diagnostics-buffer-mode
    flycheck-error-list-mode)
  "List of major-modes used in occur-type buffers.")
(defvar conf/occur-grep-shackle-rules
  (mapcar
   (lambda (regex)
     `(,regex :select t :popup t :align right :size 0.4))
   conf/occur-grep-modes-list)
  "Shackle rules for repl-names-list.")

;; This does not work at buffer creation since the major-mode for
;; REPLs is not yet set when `display-buffer' is called, but is
;; useful afterwards
(defvar conf/repl-modes-list
  '(eshell-mode shell-mode inferior-python-mode cider-repl-mode eat-mode eat-eshell-mode)
  "List of major-modes used in REPL buffers.")

(defvar conf/repl-modes-shackle-rules
  (mapcar
   (lambda (regex)
     `(,regex :select t :popup t :align t :size 0.4))
   conf/repl-modes-list)
  "Shackle rules for repl-modes-list.")

(defvar conf/repl-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Python\\*"
    "\\*Inferior .*\\*$"
    "^\\*cider-repl.*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*"
    "\\*Flutter\\*")
  "List of buffer names used in REPL buffers.")
(defvar conf/repl-names-shackle-rules
  (mapcar
   (lambda (regex)
     `(,regex :select t :popup t :align t :size 0.4 :regexp t))
   conf/repl-names-list)
  "Shackle rules for repl-names-list.")

(defvar conf/help-modes-list
  '(helpful-mode help-mode pydoc-mode TeX-special-mode)
  "List of major-modes used in documentation buffers.")
(defvar conf/help-modes-shackle-rules
  (mapcar
   (lambda (mode)
     `(,mode :select t :popup t :align right :size 0.35))
   conf/help-modes-list)
  "Shackle rules for help-modes-list.")

(defvar conf/man-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")
(defvar conf/man-modes-shackle-rules
  (mapcar
   (lambda (mode) `(,mode :select nil :same t)) conf/man-modes-list)
  "Shackle rules for man-modes-list.")

(defvar conf/message-modes-list '(compilation-mode edebug-eval-mode)
  "List of major-modes used in message buffers.")
(defvar conf/message-modes-shackle-rules
  (mapcar
   (lambda (mode)
     `(,mode :select nil :popup t :align t :size 0.25))
   conf/message-modes-list)
  "Shackle rules for message-modes-list.")

(use-package shackle
  :straight t
  :config
  (setq shackle-select-reused-windows nil)
  (setq shackle-default-size 0.33)
  (setq shackle-default-alignment 'below)
  (setq shackle-inhibit-window-quit-on-same-windows t)
  ;; Available keys are :select, :inhibit-window-quit, :ignore, :other, :same, :popup, :align, :size, :frame
  ;; We can use :align with popped up windows (:other if it creates a new one, :popup)
  ;; We can use :size with :align
  ;; We can use :frame with :other
  (setq shackle-rules
        (append
         conf/help-modes-shackle-rules
         conf/man-modes-shackle-rules
         conf/occur-grep-shackle-rules
         conf/repl-modes-shackle-rules
         conf/repl-names-shackle-rules
         '(("^\\*Customize"
            :select t
            :popup t
            :align right
            :size 0.4
            :regexp t)
           (compilation-mode
            :select nil
            :popup t
            :align t
            :size 0.25)
           ("^\\*Messages\\*$"
            :select nil
            :other t
            :align left
            :size 0.33
            :regexp t)
           (messages-buffer-mode
            :select nil
            :other t
            :align left
            :size 0.33)
           ("^\\*Warnings\\*$"
            :select nil
            :ignore t
            :align t
            :size 0.25
            :regexp t)
           ("^\\*Compile-Log\\*$"
            :select nil
            :popup t
            :align t
            :size 0.25
            :regexp t)
           ("^\\*Backtrace\\*$"
            :select t
            :align left
            :size 0.4
            :regexp t)
           ("^\\*Apropos"
            :select t
            :popup t
            :align right
            :size 0.33
            :regexp t)
           ("^Calc:"
            :select t
            :popup t
            :align right
            :size 0.25
            :regexp t)
           ("^\\*eldoc\\(.*\\)\\*"
            :select nil
            :popup t
            :align t
            :size 0.33
            :regexp t)
           ("^\\*TeX errors\\*"
            :select nil
            :popup t
            :align t
            :size 0.25
            :regexp t)
           ("^\\*TeX Help\\*"
            :select t
            :popup t
            :align right
            :size 0.33
            :regexp t)
           ("\\*Shell Command Output\\*"
            :select nil
            :popup t
            :align t
            :size 0.25
            :regexp t)
           ("\\*Async Shell Command\\*"
            :select nil
            :popup t
            :align t
            :size 0.25
            :regexp t)
           ("\\*Completions\\*"
            :select t
            :popup t
            :align bottom
            :size 0.4
            :regexp t)
           ("[Oo]utput\\*"
            :select nil
            :popup t
            :align t
            :size 0.25
            :regexp t)
           ("\\*EGLOT\\(.*\\)\\*"
            :select nil
            :same t
            :regexp t)
           ("^\\*straight-process\\*"
            :select nil
            :ignore t
            :regexp t)
           ("^\\*straight-byte-compilation\\*"
            :select nil
            :ignore t
            :regexp t)
           ("^\\*Async-native-compile-log\\*"
            :select nil
            :ignore t
            :regexp t)
           ("^\\*tree-sitter explorer for \\(.+\\)\\*"
            :select nil
            :popup t
            :align right
            :size 0.4
            :regexp t))))

  (shackle-mode +1))

(provide 'windows)
;;; windows.el ends here
