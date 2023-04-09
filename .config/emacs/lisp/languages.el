;;; languages.el --- Setup programming specific stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; This is just mostly language related stuff.
;;; Code:

;; Cider for clojure
(use-package cider
  :straight t
  :mode "\\.clj\\'"
  :custom
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-help-banner nil)
  (cider-shadow-cljs-command "npx shadow-cljs"))

;; Elixir TS
(use-package heex-ts-mode
  :straight
  (:type git :host github :repo "wkirschbaum/heex-ts-mode"))

(use-package elixir-ts-mode
  :straight
  (:type git :host github :repo "wkirschbaum/elixir-ts-mode"))

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
  :straight t
  :hook (dart-mode . (lambda ()
                       (add-hook 'after-save-hook #'flutter-run-or-hot-reload nil t))))

;; Associate extensions with the correct tree-sitter mode and others
;; Keep this as the last operation in this file
(dolist (pair
         '(("\\.py\\'" . python-ts-mode)
           ("\\.c\\'" . c-ts-mode)
           ("\\.h\\'" . c-ts-mode)
           ("\\.cs\\'" . csharp-ts-mode)
           ("\\.cpp\\'" . c++-ts-mode)
           ("\\.hpp\\'" . c++-ts-mode)
           ("\\.sh\\'" . bash-ts-mode)
           ("\\.js\\'" . js-ts-mode)
           ("\\.ts\\'" . typescript-ts-mode)
           ("\\.tsx\\'" . tsx-ts-mode)
           ("\\.cs\\'" . csharp-ts-mode)
           ("\\.java\\'" . java-ts-mode)
           ("\\.json\\'" . json-ts-mode)
           ("\\.css\\'" . css-ts-mode)
           ("\\.rs\\'" . rust-ts-mode)
           ("mix\\.lock" . elixir-ts-mode)
           ("\\.exs\\'" . elixir-ts-mode)
           ("\\.ex\\'" . elixir-ts-mode)
           ("\\.elixir\\'" . elixir-ts-mode)
           ("\\.[hl]?eex\\'" . heex-ts-mode)))
  (push pair auto-mode-alist))

;; indentations
(with-eval-after-load 'js-ts-mode
  (setq js-indent-level 2))

(with-eval-after-load "python"
  (require 'python)
  (setq
   python-indent-offset 4
   python-indent-guess-indent-offset nil
   python-indent-guess-indent-offset-verbose nil))


(provide 'languages)
;;; languages.el ends here
