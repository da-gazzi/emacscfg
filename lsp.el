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
  (verilog-ext-eglot-set-server 've-svlangserver))

(setq verible-formatter-indentation-spaces 2)
(setq verible-formatter-wrap-spaces 4)
(setq verible-formatter-column-limit 100)
(setq verible-formatter-line-break-penalty 2)
(setq verible-formatter-over-column-limit-penalty 10000)
(setq verible-formatter-assignment_statement_alignment "align")
(setq verible-formatter-case_items_alignment "align")
(setq verible-formatter-class_member_variable_alignment "align")
(setq verible-formatter-compact_indexing_and_selections 1)
(setq verible-formatter-distribution_items_alignment "align")
(setq verible-formatter-enum_assignment_statement_alignment "align")
(setq verible-formatter-formal_parameters_alignment "align")
(setq verible-formatter-formal_parameters_indentation "indent")
(setq verible-formatter-module_net_variable_alignment "align")
(setq verible-formatter-named_parameter_alignment "align")
(setq verible-formatter-named_parameter_indentation "indent")
(setq verible-formatter-named_port_alignment "align")
(setq verible-formatter-named_port_indentation "indent")
(setq verible-formatter-port_declarations_alignment "align")
(setq verible-formatter-port_declarations_indentation "indent")
(setq verible-formatter-struct_union_members_alignment "align")


(push '(verible . ("verible-verilog-format"
                   "--column_limit" (number-to-string verible-formatter-column-limit)
                   "--indentation_spaces" (number-to-string verible-formatter-indentation-spaces)
                   "--line_break_penalty" (number-to-string verible-formatter-line-break-penalty)
                   "--over_column_limit_penalty" (number-to-string verible-formatter-over-column-limit-penalty)
                   "--wrap_spaces" (number-to-string verible-formatter-wrap-spaces)

                   "--assignment_statement_alignment"  verible-formatter-assignment_statement_alignment
                   "--case_items_alignment"  verible-formatter-case_items_alignment
                   "--class_member_variable_alignment"  verible-formatter-class_member_variable_alignment
                   "--distribution_items_alignment"  verible-formatter-distribution_items_alignment
                   "--enum_assignment_statement_alignment"  verible-formatter-enum_assignment_statement_alignment
                   "--formal_parameters_alignment"  verible-formatter-formal_parameters_alignment
                   "--formal_parameters_indentation"  verible-formatter-formal_parameters_indentation
                   "--module_net_variable_alignment"  verible-formatter-module_net_variable_alignment
                   "--named_parameter_alignment"  verible-formatter-named_parameter_alignment
                   "--named_parameter_indentation"  verible-formatter-named_parameter_indentation
                   "--named_port_alignment"  verible-formatter-named_port_alignment
                   "--named_port_indentation"  verible-formatter-named_port_indentation
                   "--port_declarations_alignment"  verible-formatter-port_declarations_alignment
                   "--port_declarations_indentation"  verible-formatter-port_declarations_indentation
                   "--struct_union_members_alignment"  verible-formatter-struct_union_members_alignment
                   ;; "--compact_indexing_and_selections"  (number-to-string verible-formatter-compact_indexing_and_selections)
                   ;; "--expand_coverpoints"  verible-formatter-expand_coverpoints
                   ;; "--try_wrap_long_lines"  verible-formatter-try_wrap_long_lines
                   ;; "--wrap_end_else_clauses"  verible-formatter-wrap_end_else_clauses
                   ;; "--port_declarations_right_align_packed_dimensions"  verible-formatter-port_declarations_right_align_packed_dimensions
                   ;; "--port_declarations_right_align_unpacked_dimensions"  verible-formatter-port_declarations_right_align_unpacked_dimensions
                   "-"))
      apheleia-formatters)


(defun verilog-formatter-hook ()
  (when (eq major-mode 'verilog-mode)
    (verilog-ext-formatter-run))
  )

(add-hook 'before-save-hook #'verilog-formatter-hook)

;; (use-package apheleia)

(setq verilog-ext-tags-backend 'builtin)

;; (treesit-install-language-grammar ('verilog))

;; (unless (treesit-language-available-p 'verilog)
;;   (treesit-install-language-grammar 'verilog)
;;   )
;; (use-package verilog-ts-mode)
;; (add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
;; (setq verilog-ext-tags-backend 'tree-sitter)
;; ;;; todo: add seave hook to format!
