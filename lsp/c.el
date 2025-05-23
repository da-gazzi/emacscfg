;;; c.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(require 'eglot)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=8"
                    "--query-driver=/opt/riscv/bin/riscv32-unknown-elf-gcc"
                    "--log=error"
                    "--background-index"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))))
