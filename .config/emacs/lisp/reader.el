;;; general.el --- Reading ebooks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for reading ebooks (EPUBs and PDFs, mostly)
;;; Code:

(use-package doc-view
  :straight (:type built-in)
  :init
  (setq doc-view-mupdf-use-svg t))

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . conf/setup-nov-mode)
         (nov-mode . visual-line-mode))
  :bind (:map nov-mode-map
              ("C-q" . #'conf/nov-toggle-cursor))
  :init
  (setq nov-variable-pitch t
        nov-text-width 120)
  (defvar nov-cursor nil "Whether the cursor is enabled.")
  :config
  (defun conf/nov-toggle-cursor ()
    "Toggle cursor in nov-mode."
    (interactive)
    (if nov-cursor
        (progn
          (setq cursor-type nil)
          (setq nov-cursor nil)
          (scroll-lock-mode 1))
      (progn
        (setq cursor-type '(hbar . 5))
        (setq nov-cursor t)
        (scroll-lock-mode -1))))

  (defun conf/setup-nov-mode ()
    "Sets up nov-mode."
    (face-remap-add-relative 'default :height 1.1)
    (font-lock-mode -1)
    (display-line-numbers-mode -1)
    (toggle-scroll-bar -1)
    (scroll-lock-mode 1)
    (setq cursor-type nil)
    (setq-local next-screen-context-lines 4
                shr-use-colors t
                shr-use-fonts t
                mode-line-format nil
                nov-header-line-format "")))

(use-package calibredb
  :straight t
  :init
  (setq calibredb-root-dir (concat (getenv "HOME") "/Secondary/Media/Calibre"))
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Secondary/Media/Calibre"))))

(provide 'reader)
;;; reader.el ends here
