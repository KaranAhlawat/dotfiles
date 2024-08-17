;;; shells.el --- Shell configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configure shells I rarely use
;;; Code:

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
                  eshell-complete-variable-reference
                  eshell-complete-lisp-symbols
                  t))))

(use-package eshell
  :straight (:type built-in)
  :hook
  ((eshell-mode . conf/eshell-setup-modes)
   (eshell-first-time-mode . conf/eshell-first-load-settings)
   (eshell-mode . (lambda ()
                    (setq-local completion-styles '(basic))
                    (setq-local corfu-count 5)
                    (setq-local completion-at-point-functions
                                '(pcomplete-completions-at-point cape-file cape-history)))))
  :config
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
        (eshell/alias "clear" "clear t"))))

  (defun conf/eshell-setup-modes ()
    "Setup various modes for eshell."
    (display-line-numbers-mode -1)))

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
  :hook
  (;; (shell-mode . (lambda () (corfu-mode -1)))
   (shell-mode . (lambda () (display-line-numbers-mode -1)))
   (shell-mode . (lambda ()
                   (font-lock-mode -1)
                   (make-local-variable 'font-lock-function)
                   (setq font-lock-function (lambda (_) nil))))))

(provide 'shells)
;;; shells.el ends here
