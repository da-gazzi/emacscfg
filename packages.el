;;; packages.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(use-package verilog-mode)

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

(use-package markdown-mode
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package cmake-mode)

(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

(use-package dracula-theme)
(defun dracula()
  (interactive)
  (load-theme 'dracula t))

(add-hook 'after-init-hook 'dracula)
