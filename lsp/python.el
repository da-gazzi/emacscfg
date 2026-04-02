;; Install eglot if not already installed
(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))

;; Enable eglot for Python
(add-hook 'python-mode-hook 'eglot-ensure)

;; Specify the server to use for Python (e.g., pyright)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ("rass" "basedruff"))))
