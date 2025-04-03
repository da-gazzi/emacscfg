;; Ensure eglot is available
(require 'eglot)

;; Install cmake-mode if it's not already there
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Register cmake-language-server with Eglot
(add-to-list 'eglot-server-programs
             '(cmake-mode . ("cmake-language-server")))

;; Enable Eglot automatically for cmake-mode
(add-hook 'cmake-mode-hook #'eglot-ensure)
