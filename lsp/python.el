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

(defun my-python-formatting-passes ()
  "Run autoflake, yapf, and isort on the current buffer safely."
  (when (eq major-mode 'python-mode)
    (let ((original-point (point))) ;; Save the cursor position
      ;; Apply autoflake
      (run-formatter-on-buffer "autoflake" '("--remove-all-unused-imports"
                                       "--ignore-init-module-imports"
                                       "--stdin" "--stdout"))
      ;; Apply yapf
      (run-formatter-on-buffer "yapf" '("--quiet"))
      ;; Apply isort
      (run-formatter-on-buffer "isort" '("--profile" "black" "--stdout"))
      (goto-char original-point)))) ;; Restore the cursor position

(defun my-setup-python-formatting ()
  "Set up custom formatting passes for Python."
  (add-hook 'before-save-hook #'my-python-formatting-passes nil t))

;; Add the setup to Python mode
(add-hook 'python-mode-hook 'my-setup-python-formatting)
(add-hook 'python-mode-hook 'ensure-pyright-installed)
