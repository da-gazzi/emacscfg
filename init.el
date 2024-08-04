;;; init.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(when (eq system-type 'darwin)
  (setq ns-auto-hide-menu-bar t))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(setq package-native-compile t)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  )

(setq max-lisp-eval-depth 2000)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "config.el")
(load-user-file "snippets.el")
(load-user-file "packages.el")
(load-user-file "lsp.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(lsp-clients-svlangserver-formatCommand "verible-verilog-format")
 '(lsp-clients-svlangserver-launchConfiguration "verilator verilator -sv --lint-only -Wall")
 '(package-selected-packages
   '(direnv lsp-verilog lsp-pyright yasnippet-snippets use-package undo-tree sphinx-doc rustic pylint multiple-cursors magit lsp-ui latex-preview-pane impatient-mode flycheck-pycheckers flycheck-mypy exec-path-from-shell dracula-theme company cmake-mode clang-format async anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
