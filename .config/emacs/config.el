(require 'ui)
(require 'defaults)
(require 'completion)
(require 'editing)
(require 'text)
(require 'dev)
(require 'programming)
(require 'keybinds)
(require 'eshell-conf)

(straight-use-package 'project)

(require 'project)

(defun k-conf/current-project-root ()
  "Return the absolute path to the current project root"
  (let ((path (cdr (project-current))))
    (expand-file-name path)))

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "g") 'magit-status)
  (define-key project-prefix-map (kbd "G") 'project-find-regexp))

(defvar project-root-markers '("mix.exs" "build.sbt" ".project" "Cargo.toml" "project.clj" "go.mod"))

(defun k-conf/project-find-root (path)
  (let* ((this-dir (file-name-as-directory (file-truename path)))
         (parent-dir (expand-file-name (concat this-dir "../")))
         (system-root-dir (expand-file-name "/")))
      (cond
       ((k-conf/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (k-conf/project-find-root parent-dir)))))

(defun k-conf/project-root-p (path)
  (let ((results (mapcar (lambda (marker)
                           (file-exists-p (concat path marker)))
                         project-root-markers)))
    (eval `(or ,@ results))))

(add-to-list 'project-find-functions #'k-conf/project-find-root)
