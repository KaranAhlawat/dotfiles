;;; languages.el --- Setup programming specific stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; This is just mostly language related stuff.
;;; Code:

(use-package treesit
  :straight nil
  :demand t
  :custom
  (treesit-font-lock-level 4)
  :config
  (dolist (pair
           '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src" nil nil))
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src" nil nil))
             (css . ("https://github.com/tree-sitter/tree-sitter-css" nil nil nil nil))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" nil nil nil nil))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" nil nil nil nil))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" nil nil nil nil))
             (go . ("https://github.com/tree-sitter/tree-sitter-go"  nil nil nil))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" nil nil  nil))
             (scala . ("https://github.com/tree-sitter/tree-sitter-scala" nil nil nil))))
    (push pair treesit-language-source-alist))

  (dolist (pair
           '((python-mode . python-ts-mode)
             (c-mode . c-ts-mode)
             (csharp-mode . csharp-ts-mode)
             (c++-mode . c++-ts-mode)
             (javascript-mode . js-ts-mode)
             (java-mode . java-ts-mode)
             (css-mode . css-ts-mode)
             (sh-mode . bash-ts-mode)
             (shell-script-mode . bash-ts-mode)))
    (push pair major-mode-remap-alist)))

;; Cider for clojure
(use-package cider
  :straight t
  :custom
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-help-banner nil)
  (cider-shadow-cljs-command "npx shadow-cljs"))

;; PHP
(use-package php-mode
  :straight t
  :mode "\\.php\\'")

;; Web Stuff
(use-package web-mode
  :straight t
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  :config
  (setq web-mode-engines-alist '(("blade" . "\\.blade\\.")))
  (add-hook 'web-mode-hook (lambda ()
                             (smartparens-mode -1))))

;; Flutter and dart
(use-package flutter
  :straight t
  :custom
  (flutter-sdk-path "/opt/flutter"))

(use-package dart-mode
  :straight t)

(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2))

(use-package python
  :straight (:type built-in)
  :config
  (setq
   python-indent-offset 4
   python-indent-guess-indent-offset nil
   python-indent-guess-indent-offset-verbose nil))

(use-package css-mode
  :straight (:type built-in)
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :straight (:type built-in)
  :mode ("\\.scss\\'" . scss-mode)
  :custom (css-indent-offset 2))

;; Scala TS (local)
(use-package scala-ts-mode
  :straight (:local-repo "/home/karan/repos/scala-ts-mode"))

(provide 'languages)
;;; languages.el ends here
