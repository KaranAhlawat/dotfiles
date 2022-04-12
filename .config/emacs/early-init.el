;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 50 1000 1000))

(setq load-prefer-newer noninteractive)

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-earrors nil)
  (setq native-comp-deferred-compilation t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(setq inhibit-startup-message t)

(setq default-frame-alist `((vertical-scroll-bars . nil)
			    (horizontal-scroll-bars . nil)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)))

(setq initial-major-mode 'fundamental-mode)
(setq backup-directory-alist '(("." . "~/.cache/emacssaves")))
(setq create-lockfiles nil)
(setq initial-scratch-message nil)

(defvar k-config-path
  (expand-file-name "~/.config/emacs/")
  "The configuration path")
