;;; verilog.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; VERILOG

(require 'eglot)

(straight-use-package 'use-package)
(use-package verilog-mode
  :straight (:repo "veripool/verilog-mode"))
(use-package verilog-ts-mode)
;; indentation level: 2
(setq verilog-indent-level 2)
(setq verilog-indent-level-module 2)
(setq verilog-indent-level-declaration 2)

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

 (use-package verilog-ts-mode)
(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
 (unless (treesit-language-available-p 'verilog)
   (verilog-ts-install-grammar)
   )

(defun bender-get-top-level-project-root ()
  "Determine the top-level project root for the current buffer.
If the path contains `.bender` and the current buffer's file is within its hierarchy,
the top-level is the parent directory of `.bender`.
Otherwise, use the output of `git rev-parse --show-toplevel`."
  (let* ((buffer-dir (or (buffer-file-name) default-directory))
         (bender-dir (expand-file-name ".bender" (locate-dominating-file buffer-dir ".bender"))))

    (message "bender-dir: %s" bender-dir)
    (message "in path? %s" (file-exists-p bender-dir))

    (if (and bender-dir
             (file-exists-p bender-dir))
        (file-name-directory (directory-file-name bender-dir)) ;; Parent directory of `.bender`
      (let ((git-top-level (string-trim
                            (shell-command-to-string "git rev-parse --show-toplevel"))))
        (if (string-empty-p git-top-level)
            (user-error "No Git repository found for the current buffer")
          git-top-level)))))

(defun bender-generate-file-list (file-path)
  "Generate the file list at FILE-PATH using a shell command.
The file is created in the top-level project directory containing FILE-PATH.
Handles remote TRAMP paths by executing the command remotely."
  (let ((directory (file-name-directory file-path)))
    (message "Generating file list in directory: %s" directory)
    (if (file-remote-p directory)
        (let* ((vec (tramp-dissect-file-name directory))
               (local-dir (tramp-file-name-localname vec))
               (remote-cmd (format "bash -c 'cd %s && ~/.cargo/bin/bender script flist > %s'"
                                   (shell-quote-argument local-dir)
                                   (shell-quote-argument (file-local-name file-path)))))
          (message "Executing remote command: %s" remote-cmd)
          (let ((output (with-temp-buffer
                          (tramp-handle-shell-command remote-cmd nil (current-buffer))
                          (buffer-string))))
            (if (string-match-p "error\\|failed" output)
                (message "Error generating file list: %s" output)
              (message "File list generated successfully: %s" output))))
      ;; Local execution
      (let ((exit-code
             (call-process-shell-command
              (format "cd %s && bender script flist > %s"
                      (shell-quote-argument directory)
                      (shell-quote-argument file-path)))))
        (if (zerop exit-code)
            (message "File list generated successfully: %s" file-path)
          (message "Error generating file list with exit code: %d" exit-code))))))

(defun bender-verilog-file-list-path ()
  "Get the path to the Verilog file list in the top-level project directory.
Generate the file list if it doesn't exist or if `Bender.yml` or `Bender.lock` have changed."
  (let* ((top-level-root (bender-get-top-level-project-root))
         (file-path (expand-file-name ".verible.f" top-level-root))
         (bender-files `((,(expand-file-name "Bender.yml" top-level-root) . ,(file-attribute-modification-time (file-attributes (expand-file-name "Bender.yml" top-level-root))))
                         (,(expand-file-name "Bender.lock" top-level-root) . ,(file-attribute-modification-time (file-attributes (expand-file-name "Bender.lock" top-level-root))))))
         (verible-f-modtime (and (file-exists-p file-path)
                                 (file-attribute-modification-time (file-attributes file-path)))))
    ;; Check if `.verible.f` needs to be regenerated
    (when (or (not (file-exists-p file-path)) ;; `.verible.f` does not exist
              (cl-some (lambda (bender-file)
                         (time-less-p verible-f-modtime (cdr bender-file))) ;; Bender files are newer
                       bender-files))
      (message "Bender.yml or Bender.lock has changed, regenerating .verible.f...")
      (bender-generate-file-list file-path))
    file-path))

(defun bender-setup-verilog-eglot ()
  "Setup `eglot` for Verilog mode dynamically."
(let* ((file-list-path (bender-verilog-file-list-path))
         (is-remote (file-remote-p default-directory))
         (remote-prefix (when is-remote (file-remote-p default-directory 'method-user-host)))
         (local-file-list-path (if is-remote
                                   (tramp-file-name-localname (tramp-dissect-file-name file-list-path))
                                 file-list-path))
         (server-command (if is-remote
                             ;; Run the language server remotely
                             `(,remote-prefix "verible-verilog-ls")
                           ;; Run locally
                           "verible-verilog-ls")))
    ;; Debug messages
    (message "[DEBUG] bender-setup-verilog-eglot: Setting up Eglot for Verilog")
    (message "[DEBUG] bender-setup-verilog-eglot: File list path: %s" file-list-path)
    (message "[DEBUG] bender-setup-verilog-eglot: Remote editing detected? %s" (if is-remote "YES" "NO"))
    (when is-remote
      (message "[DEBUG] bender-setup-verilog-eglot: TRAMP remote prefix: %s" remote-prefix))
    (message "[DEBUG] bender-setup-verilog-eglot: Server command: %s" (format "%s" server-command))

    (message "Setting up eglot for Verilog with file list path: %s" file-list-path)
    (add-to-list 'eglot-server-programs
                 `(verilog-mode .
                   ("verible-verilog-ls"
                    "--file_list_path" , local-file-list-path
                    "--column_limit" ,(number-to-string (or verible-formatter-column-limit 100))
                    "--indentation_spaces" ,(number-to-string (or verible-formatter-indentation-spaces 2))
                    "--line_break_penalty" ,(number-to-string (or verible-formatter-line-break-penalty 10))
                    "--over_column_limit_penalty" ,(number-to-string (or verible-formatter-over-column-limit-penalty 20))
                    "--wrap_spaces" ,(number-to-string (or verible-formatter-wrap-spaces 1))

                    "--assignment_statement_alignment"  ,(format "%s" (or verible-formatter-assignment_statement_alignment "flush-left"))
                    "--case_items_alignment"  ,(format "%s" (or verible-formatter-case_items_alignment "flush-left"))
                    "--class_member_variable_alignment"  ,(format "%s" (or verible-formatter-class_member_variable_alignment "flush-left"))
                    "--distribution_items_alignment"  ,(format "%s" (or verible-formatter-distribution_items_alignment "flush-left"))
                    "--enum_assignment_statement_alignment"  ,(format "%s" (or verible-formatter-enum_assignment_statement_alignment "flush-left"))
                    "--formal_parameters_alignment"  ,(format "%s" (or verible-formatter-formal_parameters_alignment "flush-left"))
                    "--formal_parameters_indentation"  ,(format "%s" (or verible-formatter-formal_parameters_indentation "flush-left"))
                    "--module_net_variable_alignment"  ,(format "%s" (or verible-formatter-module_net_variable_alignment "flush-left"))
                    "--named_parameter_alignment"  ,(format "%s" (or verible-formatter-named_parameter_alignment "flush-left"))
                    "--named_parameter_indentation"  ,(format "%s" (or verible-formatter-named_parameter_indentation "flush-left"))
                    "--named_port_alignment"  ,(format "%s" (or verible-formatter-named_port_alignment "flush-left"))
                    "--named_port_indentation"  ,(format "%s" (or verible-formatter-named_port_indentation "indent"))
                    "--port_declarations_alignment"  ,(format "%s" (or verible-formatter-port_declarations_alignment "flush-left"))
                    "--port_declarations_indentation"  ,(format "%s" (or verible-formatter-port_declarations_indentation "flush-left"))
                    "--struct_union_members_alignment"  ,(format "%s" (or verible-formatter-struct_union_members_alignment "flush-left"))
                    )))))

(defun my-run-verilog-formatter ()
  "Run `eglot-format-buffer` on the current buffer safely."
  (when (and (derived-mode-p 'verilog-mode 'verilog-ts-mode) ;; Ensure in Verilog mode
             (eglot-managed-p)) ;; Check if Eglot is active
    (let ((original-content (buffer-string)) ;; Save buffer content
          (original-point (point)))          ;; Save cursor position
      (eglot-format-buffer) ;; Apply formatting via LSP

      ;; Ensure formatting was applied
      (unless (string= original-content (buffer-string))
        (message "Verilog formatter applied changes"))

      (goto-char original-point)))) ;; Restore cursor position

(defun my-setup-verilog-formatting ()
  "Set up automatic formatting for Verilog."
  (add-hook 'before-save-hook #'my-run-verilog-formatter nil t))

;; Enable for Verilog modes
;(add-hook 'verilog-mode-hook 'my-setup-verilog-formatting)
(add-hook 'verilog-mode-hook #'bender-setup-verilog-eglot)
