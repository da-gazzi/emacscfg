;;; packages.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

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
;; Multi-Cursor keybinds
(global-set-key (kbd "C-,") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/mark-previous-like-this)

(use-package magit)
;; Magit keybinds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c m") 'smerge-keep-mine)
(global-set-key (kbd "C-c n") 'smerge-next)
(global-set-key (kbd "C-c p") 'smerge-previous)
(global-set-key (kbd "C-c o") 'smerge-keep-other)

(use-package markdown-mode
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

(use-package dracula-theme)
(defun dracula()
  (interactive)
  (load-theme 'dracula t))
(add-hook 'after-init-hook 'dracula)
