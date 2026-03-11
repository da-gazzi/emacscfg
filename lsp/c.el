;;; c.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(require 'eglot)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-ts-mode)
                 . ("clangd"
                    "-j=8"
                    "--query-driver=/opt/riscv/bin/riscv32-unknown-elf-gcc"
                    "--log=verbose"
                    "--background-index"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))
