;;; packages.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

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
  (load-theme 'dracula))
(add-hook 'after-init-hook 'dracula)


