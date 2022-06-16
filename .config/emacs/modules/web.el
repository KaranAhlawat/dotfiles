;;; web.el -*- lexical-binding: t; -*-

(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package 'rjsx-mode)

;; web mode configuration
(require 'web-mode)
(require 'rjsx-mode)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(setq auto-mode-alist
      (append
       '(("\\.html\\'" . web-mode)
         ("\\.css\\'" . css-mode)
         ("\\.js\\'" . rjsx-mode)
         ("\\.ts\\'" . rjsx-mode)
         ("\\.cjs\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode)
         ("\\.vue\\'" . web-mode))
       auto-mode-alist))

;; setup emmet
(setq emmet-self-closing-tag-style " /")
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'rjsx-mode-hook 'emmet-mode)

;; Vue setup ig?
(setq k-web-mode-lsp-extensions `(,(rx ".vue" eos)))

(defun k-web-mode-lsp ()
  (when (seq-some
         (lambda (regex) (string-match-p regex (buffer-name)))
         k-web-mode-lsp-extensions)
    (lsp-deferred)))

(add-hook 'web-mode-hook #'k-web-mode-lsp)

(defun k-vue-setup (&rest _)
  (when (string-match-p (rx ".vue" eos) (buffer-name))
    (setq-local web-mode-script-padding 0)
    (setq-local web-mode-style-padding 0)
    (setq-local create-lockfiles nil)))

(add-hook 'web-mode-hook 'k-vue-setup)

(provide 'web)
;;; web.el ends here
