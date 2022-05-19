;;; funcs.el -*- lexical-binding: t; -*-

(defun backward-delete-ws-till-char ()
  (interactive)
  (let ((start (point))
        (end (search-backward-regexp "[^[:space:]]")))
    (delete-region start end)))

(defun cp-compile-current-file ()
  (interactive)
  (when (string= major-mode "c++-mode")
   (let* ((current-buffer (buffer-name))
          (exec-name (car (split-string current-buffer "\\."))))
     (compile (format "clang++ -Wall -O2 %s -o %s" current-buffer exec-name)))))

(defun cp-comint-run-exec ()
  (interactive)
  (when (string= major-mode "c++-mode")
    (let ((exec-name (car (split-string (buffer-name) "\\."))))
      (comint-run (format "./%s" exec-name)))))
    

(provide 'funcs)
