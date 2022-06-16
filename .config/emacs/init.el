(add-to-list 'load-path
             (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(set-default-coding-systems 'utf-8)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
	                        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	                        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar k-config-file (expand-file-name "config.el" k-config-path))

(when (file-exists-p k-config-file)
  (load k-config-file nil 'nomessage))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil 'nomessage))

(setq gc-cons-threshold (* 2 1000 1000))

(put 'dired-find-alternate-file 'disabled nil)
