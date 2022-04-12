;;; eshell.el -*- lexical-binding: t -*-

(straight-use-package '(eshell :type built-in))
(straight-use-package 'eshell-z)
(straight-use-package 'eshell-syntax-highlighting)
                      
(require 'eshell)

(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-destroy-buffer-when-process-dies t
      eshell-highlight-prompt t
      eshell-scroll-to-bottom-on-output t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "vi" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")
            (eshell/alias "ls" "exa --icons --color=always")
            (eshell/alias "l" "exa -la --icons --color=always")
            (eshell/alias "clear" "clear 1")))
          
                          
(defun eshell/gst ()
  (magit-status (pop args) nil)
  (eshell/echo))

(defun curr-dir-git-branch-string (pwd)
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (s-trim git-url)))
           (git-output (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))
           (git-branch (s-trim git-output))
           (git-icon "\xe0a0"))
      (concat git-repo " " git-icon " " git-branch))))

(defun pwd-replace-home (pwd)
  "Replace $HOME in PWD with ~"
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all the directory namesin PWD except the last two"
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                    (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))

(defun split-dir-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(defun eshell/eshell-local-prompt-function ()
  (interactive)
  (let* ((pwd (eshell/pwd))
         (host (s-trim (shell-command-to-string "hostname")))
         (directory (split-dir-prompt
                     (pwd-shorten-dirs
                      (pwd-replace-home pwd))))
         (parent (car directory))
         (name (cadr directory))
         (branch (curr-dir-git-branch-string pwd))
         (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
         (for-host (if dark-env `(:foreground "#00c06f") `(:foreground "#a60000")))
         (for-bars `(:weight bold))
         (for-parent (if dark-env `(:foreground "#00bcff") `(:foreground "#2544bb")))
         (for-dir (if dark-env `(:foreground "#2fafff" :weight bold)
                    `(:foreground "#0000c0" :weight bold)))
         (for-git (if dark-env `(:foreground "#00d3d0") `(:foreground "#145c33")))
         (for-prompt (if dark-env `(:foreground "#f78fe7" :weight ultra-bold) `
                       (:foreground "#8f0075" :weight ultra-bold))))
    (concat
     "\n"
     (propertize (concat host " ") 'face for-host)
     (propertize "in " 'face for-bars)
     (propertize parent 'face for-parent)
     (propertize name 'face for-dir)
     (when branch
       (concat (propertize " ── " 'face for-bars)
               (propertize branch 'face for-git)))
     (propertize "\n" 'face for-bars)
     (propertize (if (= (user-uid) 0) " #" " ➜") 'face for-prompt)
     (propertize " " 'face `(:weight normal)))))
     

(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)

(setq eshell-highlight-prompt t
      eshell-prompt-regexp "^ ➜")

(require 'eshell-z)

(setenv "PAGER" "cat")

(define-key global-map (kbd "C-!") #'eshell)

(require 'eshell-syntax-highlighting)
(add-hook 'eshell-mode-hook (lambda ()
                              (eshell-syntax-highlighting-mode)))
(provide 'eshell-conf)
