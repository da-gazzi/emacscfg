;;; verilog.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; VERILOG


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

(defun override-verilog-ext-formatter ()
  (interactive)
  (setq apheleia-formatters (assq-delete-all 'verible apheleia-formatters))
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
  )

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
  (verilog-ext-eglot-set-server 've-svlangserver)
  (override-verilog-ext-formatter))


;; (treesit-install-language-grammar ('verilog))

;; (unless (treesit-language-available-p 'verilog)
;;   (treesit-install-language-grammar 'verilog)
;;   )
;; (use-package verilog-ts-mode)
;; (add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
;; (setq verilog-ext-tags-backend 'tree-sitter)
;; ;;; todo: add seave hook to format!
