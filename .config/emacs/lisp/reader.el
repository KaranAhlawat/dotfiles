;;; general.el --- Reading ebooks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for reading ebooks (EPUBs and PDFs, mostly)
;;; Code:

(use-package doc-view
  :straight (:type built-in)
  :init
  (setq doc-view-mupdf-use-svg t))

(provide 'reader)
;;; reader.el ends here
