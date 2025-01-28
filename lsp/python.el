;; Install eglot if not already installed
(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))

;; Enable eglot for Python
(add-hook 'python-mode-hook 'eglot-ensure)

;; Specify the server to use for Python (e.g., pyright)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pyright-langserver" "--stdio"))))

;; Optional: Install pyright easily if it's not installed
(defun ensure-pyright-installed ()
  "Ensure the pyright language server is installed."
  (unless (executable-find "pyright-langserver")
    (message "Pyright is not installed. Installing via npm...")
    (call-process-shell-command "npm install -g pyright")))

(defun my-run-formatter (command args)
  "Run a formatter COMMAND with ARGS on the current buffer content.
The formatter is run in a temporary buffer, and the result is applied back to the current buffer
only if no conflicts with unsaved changes are detected."
  (let ((original-content (buffer-string)) ;; Save current content
        (formatted-content nil))
    ;; Run the formatter in a temporary buffer
    (with-temp-buffer
      (insert original-content)
      (let ((exit-code (apply #'call-process-region (point-min) (point-max)
                              command t t nil args)))
        (if (zerop exit-code)
            (setq formatted-content (buffer-string))
          (message "Formatter %s failed with exit code %d" command exit-code))))
    ;; Only apply changes if the content has not been modified
    (when (and formatted-content
               (string= original-content (buffer-string)))
      (erase-buffer)
      (insert formatted-content))))

(defun my-python-formatting-passes ()
  "Run autoflake, yapf, and isort on the current buffer safely."
  (when (eq major-mode 'python-mode)
    (let ((original-point (point))) ;; Save the cursor position
      ;; Apply autoflake
      (my-run-formatter "autoflake" '("--remove-all-unused-imports"
                                       "--ignore-init-module-imports"
                                       "--stdin" "--stdout"))
      ;; Apply yapf
      (my-run-formatter "yapf" '("--quiet"))
      ;; Apply isort
      (my-run-formatter "isort" '("--profile" "black" "--stdout"))
      (goto-char original-point)))) ;; Restore the cursor position

(defun my-setup-python-formatting ()
  "Set up custom formatting passes for Python."
  (add-hook 'before-save-hook #'my-python-formatting-passes nil t))

;; Add the setup to Python mode
(add-hook 'python-mode-hook 'my-setup-python-formatting)
(add-hook 'python-mode-hook 'ensure-pyright-installed)
