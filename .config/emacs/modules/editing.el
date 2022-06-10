;;; editing.el -*- lexical-binding: t; -*-

(show-paren-mode 1)

(straight-use-package 'rainbow-delimiters)
(straight-use-package 'reformatter)
(straight-use-package 'whole-line-or-region)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'reformatter)

(defun k-conf/get-scalafmt-conf-relative ()
  "This function returns the path to the .scalafmt.conf file relative to the current directory/file"
  (concat
   (file-relative-name (cdr (last (project-current))))
   ".scalafmt.conf"))

(reformatter-define scala-format
  :program "scalafmt"
  :args `("--config"
          ,(k-conf/get-scalafmt-conf-relative)
          "--stdout"
          ,(file-relative-name (buffer-file-name)))
  :lighter " scalafmt")

(require 'whole-line-or-region)
(whole-line-or-region-global-mode)

(provide 'editing)
