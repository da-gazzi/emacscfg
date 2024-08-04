; project.el

(use-package compile
  :custom
  (compilation-read-command nil "Don't prompt every time.")
  (compilation-scroll-output 'first-error))

(use-package project
  :pin gnu
  :bind (("C-c k" . #'project-kill-buffers)
         ("C-c m" . #'project-compile)
         ("C-x f" . #'find-file)
         ("C-c F" . #'project-switch-project)
         ("C-c R" . #'pt/recentf-in-project)
         ("C-c f" . #'project-find-file))
  :custom
  ;; This is one of my favorite things: you can customize
  ;; the options shown upon switching projects.
  (project-switch-commands
   '((project-find-file "Find file")
     (magit-project-status "Magit" ?g)
     (deadgrep "Grep" ?h)
     (pt/project-run-vterm "vterm" ?t)
     (project-dired "Dired" ?d)
     (pt/recentf-in-project "Recently opened" ?r)))
  (compilation-always-kill t)
  (project-vc-merge-submodules nil)
  )

(defun pt/recentf-in-project ()
  "As `recentf', but filtering based on the current project root."
  (interactive)
  (let* ((proj (project-current))
         (root (if proj (project-root proj) (user-error "Not in a project"))))
    (cl-flet ((ok (fpath) (string-prefix-p root fpath)))
      (find-file (completing-read "Find recent file:" recentf-list #'ok)))))
