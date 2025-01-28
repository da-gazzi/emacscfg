;;; verilog.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; VERILOG

(straight-use-package 'use-package)
(use-package verilog-mode
  :straight (:repo "veripool/verilog-mode"))
(use-package verilog-ext)
(use-package verilog-ts-mode)

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

(use-package verilog-ts-mode)
(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
(unless (treesit-language-available-p 'verilog)
  (verilog-ts-install-grammar)
  )

;; (use-package verilog-ext
;;   :hook ((verilog-ts-mode . verilog-ext-mode))
;;   :init
;;   ;; Can also be set through `M-x RET customize-group RET verilog-ext':
;;   ;; Comment out/remove the ones you do not need
;;   (setq verilog-ext-feature-list
;;         '(font-lock
;;           xref
;;           capf
;;           hierarchy
;;           eglot
;;           beautify
;;           navigation
;;           template
;;           formatter
;;           compilation
;;           imenu
;;           which-func
;;           hideshow
;;           typedefs
;;           time-stamp
;;           block-end-comments
;;           ports))
;;   :config
;;   (verilog-ext-mode-setup)
;;   (verilog-ext-eglot-set-server 've-verible-ls)
;;   (override-verilog-ext-formatter))

;; (setq verilog-ext-tags-backend 'builtin)

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '((c-mode c++-mode)
;;                  . ("clangd"
;;                     "-j=8"
;;                     "--log=error"
;;                     "--malloc-trim"
;;                     "--background-index"
;;                     "--clang-tidy"
;;                     "--cross-file-rename"
;;                     "--completion-style=detailed"
;;                     "--pch-storage=memory"
;;                     "--header-insertion=never"
;;                     "--header-insertion-decorators=0"))))

(defun bender-get-top-level-project-root ()
  "Determine the top-level project root for the current buffer.
If the path contains `.bender` and the current buffer's file is within its hierarchy,
the top-level is the parent directory of `.bender`.
Otherwise, use the output of `git rev-parse --show-toplevel`."
  (let* ((buffer-dir (or (buffer-file-name) default-directory))
         (bender-dir (expand-file-name ".bender" (locate-dominating-file buffer-dir ".bender"))))

    (message "bender-dir: %s" bender-dir)
    (message "in path? %s" (file-exists-p bender-dir))
    (message "str prefix? %s" (string-prefix-p (expand-file-name bender-dir) (expand-file-name buffer-dir)))

    (if (and bender-dir
             (file-exists-p bender-dir)
             (string-prefix-p (expand-file-name bender-dir) (expand-file-name buffer-dir)))
        (file-name-directory (directory-file-name bender-dir)) ;; Parent directory of `.bender`
      (let ((git-top-level (string-trim
                            (shell-command-to-string "git rev-parse --show-toplevel"))))
        (if (string-empty-p git-top-level)
            (user-error "No Git repository found for the current buffer")
          git-top-level)))))

(defun bender-generate-file-list (file-path)
  "Generate the file list at FILE-PATH using a shell command.
The file is created in the top-level project directory containing FILE-PATH."
  (let ((directory (file-name-directory file-path)))
    (message "Generating file list in directory: %s" directory)
    (let ((exit-code
           (call-process-shell-command
            (format "cd %s && bender script flist > %s"
                    (shell-quote-argument directory)
                    (shell-quote-argument file-path)))))
      (if (zerop exit-code)
          (message "File list generated successfully: %s" file-path)
        (message "Error generating file list with exit code: %d" exit-code)))))

(defun bender-verilog-file-list-path ()
  "Get the path to the Verilog file list in the top-level project directory.
Generate the file list if it doesn't exist."
  (let* ((top-level-root (bender-get-top-level-project-root))
         (file-path (expand-file-name ".verible.f" top-level-root)))
    (message "Resolved file list path: %s" file-path)
    (unless (file-exists-p file-path)
      (message ".verible.f does not exist. Generating...")
      (bender-generate-file-list file-path))
    file-path))

(defun bender-setup-verilog-eglot ()
  "Setup `eglot` for Verilog mode dynamically.
This ensures the Verible language server gets the correct `--file_list_path`."
  (let ((file-list-path (bender-verilog-file-list-path)))
    (message "Setting up eglot for Verilog with file list path: %s" file-list-path)
    (setq-local eglot-server-programs
                `((verilog-mode . ("verible-verilog-ls"
                                   "--file_list_path"
                                   ,file-list-path))))))

;; Add a hook to reset eglot-server-programs and run bender-verilog-file-list-path
(add-hook 'verilog-mode-hook #'bender-setup-verilog-eglot)
