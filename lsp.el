;;; lsp.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(use-package xref
  :pin gnu
  :custom (xref-auto-jump-to-first-xref t)
  :bind (("C-c q" . #'xref-find-definitions)
         ("C-c w" . #'xref-find-references)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("s-d" . #'eldoc)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (verilog-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         )
  :bind (:map eglot-mode-map
         ("C-c q" . #'xref-find-definitions)
         ("C-c w" . #'xref-find-references)
         ("C-c a" . #'eglot-rename)
         ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  )

(use-package flymake
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)))

(use-package numpydoc
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

;; Provide drop-down completion.
(use-package company
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))

;;; VERILOG

(use-package verilog-ext
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET verilog-ext':
  ;; Comment out/remove the ones you do not need
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          beautify
          navigation
          template
          formatter
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          block-end-comments
          ports))
  :config
  (verilog-ext-mode-setup)
  (verilog-ext-eglot-set-server 've-verible-ls))

(setq verilog-ext-formatter-column-limit 100)

(defun verilog-formatter-hook ()
  (when (eq major-mode 'verilog-mode)
    (verilog-ext-formatter-run))
  )

(add-hook 'before-save-hook #'verilog-formatter-hook)

(setq verilog-ext-tags-backend 'builtin)

;; (treesit-install-language-grammar ('verilog))

;; (unless (treesit-language-available-p 'verilog)
;;   (treesit-install-language-grammar 'verilog)
;;   )
;; (use-package verilog-ts-mode)
;; (add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
;; (setq verilog-ext-tags-backend 'tree-sitter)
;; ;;; todo: add seave hook to format!
