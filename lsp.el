;;; lsp.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(use-package lsp-mode
  :hook (python-mode . lsp)
  (c-mode . lsp)
  (verilog-mode . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "C-c q") 'lsp-find-definition)
(global-set-key (kbd "C-c w") 'lsp-find-references)

;;; PYTHON

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp))))  ; or lsp-deferred

(setq lsp-pylsp-plugins-flake8-max-line-length 120)
(setq lsp-pylsp-plugins-flake8-ignore "E251")
