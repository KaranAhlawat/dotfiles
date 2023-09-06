;;; shells.el --- Shell configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configure shells I rarely use
;;; Code:

(use-package comint
  :straight (:type built-in)
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package xterm-color
  :straight t
  :init
  (setq xterm-color-use-bold-for-bright t)
  (setq xterm-color-names ["#3B4252" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#E5E9F0"])
  (setq xterm-color-names-bright ["#4C566A" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#8FBCBB" "#ECEFF4"]))

(use-package pcmpl-args
  :straight t
  :hook
  ((eshell-mode . conf/pcmpl-args-pcomplete-settings))
  :config
  (defun conf/pcmpl-args-pcomplete-settings ()
    (setq-local pcomplete-try-first-hook
                '(eshell-complete-host-reference
                  eshell-complete-history-reference
                  eshell-complete-user-reference
                  ;;eshell-complete-variable-assignment
                  eshell-complete-variable-reference
                  eshell-complete-lisp-symbols
                  t))))

(use-package eshell
  :straight (:type built-in)
  :hook
  ((eshell-mode . conf/eshell-setup-modes)
   (eshell-mode . conf/setup-remote-aliases)
   (eshell-first-time-mode . conf/eshell-first-load-settings)
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t)))
   (eshell-mode . (lambda ()
                    (setenv "TERM" "xterm-256color")))
   (eshell-mode . (lambda ()
                    (setenv "PAGER" "cat"))))
  :init
  (require 'esh-mode)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (defun conf/eshell-first-load-settings ()
    (setq
     eshell-hist-ignoredups t
     eshell-history-size nil
     eshell-scroll-to-bottom-on-input t
     eshell-scroll-to-bottom-on-output t
     eshell-glob-case-insensitive t
     eshell-error-if-no-glob t)
    (if (and (file-readable-p eshell-aliases-file))
        (eshell-read-aliases-list)
      (progn
        (eshell/alias "clear" "clear 1")
        (eshell/alias "ls" "exa $*")
        (eshell/alias "l" "exa -la $*"))))

  (defun conf/eshell-setup-modes ()
    "Setup various modes for eshell."
    (display-line-numbers-mode -1)
    (corfu-mode -1))

  (defun conf/setup-remote-aliases ()
    "Setup remote aliases."
    (when (file-remote-p default-directory)
      (eshell/alias "ls")
      (eshell/alias "l" "ls -la $*"))))

;; Eshell appearance
(use-package eshell
  :straight (:type built-in)
  :config
  (setq
   eshell-prompt-regexp "^.* λ "
   eshell-prompt-function #'conf/eshell-default-prompt-fn)

  ;; From the Doom emacs config
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s "
                                     (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face
                             'font-lock-keyword-face)))

  (defun conf/eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (concat
     (if (bobp)
         ""
       "\n")
     (when (bound-and-true-p conda-env-current-name)
       (propertize (concat "(" conda-env-current-name ") ")
                   'face
                   'conf/eshell-prompt-git-branch))
     (let ((pwd (eshell/pwd)))
       (propertize (if (equal pwd "~")
                       pwd
                     (abbreviate-file-name pwd))
                   'face 'conf/eshell-prompt-pwd))
     (propertize (conf/eshell--current-git-branch)
                 'face
                 'conf/eshell-prompt-git-branch)
     (propertize " λ"
                 'face
                 (if (zerop eshell-last-command-status)
                     'success
                   'error))
     " "))

  (defsubst conf/eshell--current-git-branch ()
    ;; TODO Refactor me
    (cl-destructuring-bind
        (status . output)
        (with-temp-buffer
          (cons
           (or (call-process "git"
                             nil
                             t
                             nil
                             "symbolic-ref"
                             "-q"
                             "--short"
                             "HEAD")
               (call-process "git"
                             nil
                             t
                             nil
                             "describe"
                             "--all"
                             "--always"
                             "HEAD")
               -1)
           (string-trim (buffer-string))))
      (if (equal status 0)
          (format " [%s]" output)
        "")))

  (defface conf/eshell-prompt-pwd
    '((t (:inherit font-lock-keyword-face)))
    "TODO"
    :group 'eshell)

  (defface conf/eshell-prompt-git-branch
    '((t (:inherit font-lock-builtin-face)))
    "TODO"
    :group 'eshell))



(use-package shell
  :straight (:type built-in)
  :requires xterm-color
  :hook
  ((shell-mode . (lambda () (corfu-mode -1)))
   (shell-mode . (lambda () (display-line-numbers-mode -1)))
   (shell-mode . (lambda ()
                   (font-lock-mode -1)
                   (make-local-variable 'font-lock-function)
                   (setq font-lock-function (lambda (_) nil))
                   (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
   (shell-mode . (lambda () (setenv "TERM" "xterm-256color")))))

(provide 'shells)
;;; shells.el ends here
