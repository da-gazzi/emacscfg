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
  "Run autoflake, yapf, and isort on the current buffer."
  (when (eq major-mode 'python-mode)
    (let ((buffer-file (buffer-file-name)))
      (when buffer-file
        ;; Apply autoflake
        (call-process "autoflake" nil "*autoflake-output*" nil
                      "--remove-all-unused-imports" "--ignore-init-module-imports" buffer-file)
        ;; Apply yapf
        (call-process "yapf" nil "*yapf-output*" nil buffer-file)
        ;; Apply isort
        (call-process "isort" nil "*isort-output*" nil buffer-file)
        ;; Reload the buffer to reflect changes
        (revert-buffer t t t)))))

(defun my-setup-python-formatting ()
  "Set up custom formatting passes for Python."
  (add-hook 'before-save-hook #'my-python-formatting-passes nil t))

;; Add the setup to Python mode
(add-hook 'python-mode-hook 'my-setup-python-formatting)

(add-hook 'python-mode-hook 'ensure-pyright-installed)
