;;; web.el -*- lexical-binding: t; -*-

(straight-use-package 'web-mode)
(straight-use-package 'emmet-mode)
(straight-use-package 'typescript-mode)

(require 'web-mode)
(require 'emmet-mode)
(require 'typescript-mode)

(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-enable-current-column-highlight t
      web-mode-enable-current-element-highlight t)

(setq auto-mode-alist
      (append
       '(("\\.html\\'" . web-mode)
         ("\\.css\\'" . css-mode)
         ("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . web-mode))
       auto-mode-alist))

(setq emmet-self-closing-tag-style " /" emmet-indentation 2)
(add-hook 'web-mode-hook 'emmet-mode)
(define-key emmet-mode-keymap (kbd "C-M-l") #'emmet-next-edit-point)


(provide 'web)
;;; web.el ends here
