;;; packages.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(use-package yaml-mode)

 (use-package tree-sitter
    :config (global-tree-sitter-mode))

  (use-package tree-sitter-langs)

(let ((installed (package-installed-p 'all-the-icons)))
  (use-package all-the-icons)
  (unless installed (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Let the OS determine what monospace means...
(set-face-attribute 'default nil :font "monospace")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package diminish
  :config
  (diminish 'visual-line-mode))

(use-package rainbow-delimiters
  :disabled
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package centered-window
  :custom
  (cwm-centered-window-width 180))

(use-package undo-tree)
(global-undo-tree-mode)

(use-package multiple-cursors
  :defer 1
  )

(use-package magit)

(straight-use-package 'helm)
(helm-mode 1)
(setq helm-move-to-line-cycle-in-source nil)

(use-package helm-file-preview
  :ensure t
  :config (helm-file-preview-mode 1))

(use-package markdown-mode
  :ensure t
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))


(use-package apheleia
  :custom (apheleia-remote-algorithm 'local)
  )

(use-package cmake-mode)

;; (use-package direnv
;;   :config (direnv-mode)
;;   :custom (direnv-always-show-summary nil))

(use-package dracula-theme)
(defun dracula()
  (interactive)
  (load-theme 'dracula t))

(add-hook 'after-init-hook 'dracula)

(use-package helm-ag
  :ensure t)
(setq helm-ag-use-fuzzy-match t)
(use-package helm-swoop
  :ensure t)

(setq helm-swoop-split-direction 'split-window-vertically)
(setq helm-swoop-split-with-multiple-windows t)
(setq helm-swoop-use-line-number-face t)
(setq helm-swoop-use-fuzzy-match t)
(setq helm-multi-swoop-edit-save t)
