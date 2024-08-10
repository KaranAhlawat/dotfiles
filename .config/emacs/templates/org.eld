org-mode ;; only in org mode

(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(quote "#+begin_quote" n> r> n> "#+end_quote")
(example "#+begin_example" n> r> n> "#+end_example")
(center "#+begin_center" n> r> n> "#+end_center")
(latex "#+begin_export latex" n> r> n> "#+end_export")
(comment "#+begin_comment" n> r> n> "#+end_comment")
(verse "#+begin_verse" n> r> n> "#+end_verse")
(src "#+begin_src " p n> r> n> "#+end_src")
(inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+date: " p n "#+author: Karan Ahlawat" n)
(par "\\paragraph{" q "}")
