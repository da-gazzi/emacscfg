;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(c-default-style
   '((c-mode . "google") (c++-mode . "google") (java-mode . "java") (awk-mode . "awk")
     (csharp-mode . "csharp") (other . "gnu")))
 '(c-offsets-alist '((access-label . -) (innamespace . +)))
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(default-frame-alist
   '((font . "DejaVu Sans Mono 14") (fullscreen . maximized) (vertical-scroll-bars)))
 '(eglot-events-buffer-config '(:size 2000000 :format lisp))
 '(global-treesit-auto-mode nil)
 '(helm-buffer-max-length 50)
 '(helm-ff-rsync-progress-bar-style 'text)
 '(helm-swoop-split-with-multiple-windows t)
 '(markdown-command "multimarkdown")
 '(projectile-mode t nil (projectile))
 '(projectile-project-root-files
   '(".project.el" "Package.swift" "build.zig.zon" "dune-project" "Project.toml" "elm.json"
     "pubspec.yaml" "info.rkt" "Cargo.toml" "stack.yaml" "DESCRIPTION" "Eldev" "Eask" "Cask"
     "shard.yml" "Gemfile" "deps.edn" "build.boot" "project.clj" "build.mill" "build.sc" "build.sbt"
     "application.yml" "gradlew" "build.gradle" "pom.xml" "pyproject.toml" "poetry.lock" "Pipfile"
     "tox.ini" "setup.py" "requirements.txt" "manage.py" "angular.json" "package.json" "gulpfile.js"
     "Gruntfile.js" "mix.exs" "rebar.config" "composer.json" "Taskfile.yml" "CMakeLists.txt"
     "GNUMakefile" "Makefile" "debian/control" "WORKSPACE" "flake.nix" "default.nix" "meson.build"
     "SConstruct" "xmake.lua" "?*.nimble" "?*.sln" "?*.fsproj" "?*.csproj" "GTAGS" "TAGS"
     "configure.ac" "configure.in" "cscope.out"))
 '(projectile-project-root-files-top-down-recurring '(".project.el" ".svn" "CVS" "Makefile"))
 '(projectile-require-project-root 'prompt)
 '(safe-local-variable-values
   '((eval add-hook 'c++-mode-hook (lambda nil (c-set-offset 'access-label '/)) nil t)
     (eval add-hook 'c++-mode-hook (lambda nil (c-set-offset 'access-label /)) nil t)
     (eval add-hook 'c++-mode-hook (lambda nil (c-set-offset 'access-label *)) nil t)
     (eval add-hook 'c++-mode-hook (lambda nil (c-set-offset 'access-label (lambda (_langelem) *)))
           nil t)
     (eval add-hook 'c++-mode-hook (lambda nil (c-set-offset 'access-label (lambda (_langelem) 1)))
           nil t)
     (eval add-hook 'c++-mode-hook
           (lambda nil (c-set-offset 'access-label #'google-access-label-indent)) nil t)
     (c-ts-mode-indent-offset . 2)))
 '(save-some-buffers-default-predicate 'save-some-buffers-root)
 '(split-height-threshold nil)
 '(switch-window-background t)
 '(tree-sitter-major-mode-language-alist
   '((c-ts-mode . c) (c++-ts-mode . cpp) (verilog-ts-mode . systemverilog)
     (actionscript-mode . actionscript) (ada-mode . ada) (agda-mode . agda) (agda2-mode . agda)
     (arduino-mode . arduino) (astro-mode . astro) (fish-mode . fish) (asm-mode . asm)
     (fasm-mode . asm) (masm-mode . asm) (nasm-mode . asm) (gas-mode . asm) (sh-mode . bash)
     (beancount-mode . beancount) (bibtex-mode . bibtex) (c-mode . c) (caml-mode . ocaml)
     (clojure-mode . clojure) (lisp-mode . commonlisp) (lisp-interaction-mode . commonlisp)
     (csharp-mode . c-sharp) (c++-mode . cpp) (cmake-mode . cmake) (d-mode . d) (dart-mode . dart)
     (dockerfile-mode . dockerfile) (css-mode . css) (csv-mode . csv)
     (editorconfig-conf-mode . editorconfig) (elm-mode . elm) (elixir-mode . elixir)
     (emacs-lisp-mode . elisp) (erlang-mode . erlang) (ess-r-mode . r) (fennel-mode . fennel)
     (f90-mode . fortran) (fortran-mode . fortran) (gdscript-mode . gdscript)
     (git-commit-mode . gitcommit) (git-rebase-mode . git-rebase)
     (gitattributes-mode . gitattributes) (gitignore-mode . gitignore) (gleam-mode . gleam)
     (glsl-mode . glsl) (go-mode . go) (gpr-mode . gpr) (groovy-mode . groovy)
     (jenkinsfile-mode . groovy) (haskell-mode . haskell) (haxe-mode . haxe) (hcl-mode . hcl)
     (terraform-mode . hcl) (heex-mode . heex) (hlsl-mode . hlsl) (html-mode . html)
     (markdown-mode . markdown) (mhtml-mode . html) (nix-mode . nix) (jai-mode . jai)
     (janet-mode . janet-simple) (java-mode . java) (javascript-mode . javascript)
     (js-mode . javascript) (js2-mode . javascript) (js3-mode . javascript) (json-mode . json)
     (jsonc-mode . json) (jsonnet-mode . jsonnet) (julia-mode . julia) (kotlin-mode . kotlin)
     (latex-mode . latex) (LaTeX-mode . latex) (llvm-mode . llvm) (llvm-mir-mode . llvm-mir)
     (lua-mode . lua) (magik-mode . magik) (makefile-mode . make) (makefile-automake-mode . make)
     (makefile-gmake-mode . make) (makefile-makepp-mode . make) (makefile-bsdmake-mode . make)
     (makefile-imake-mode . make) (matlab-mode . matlab) (mermaid-mode . mermaid)
     (meson-mode . meson) (ninja-mode . ninja) (noir-mode . noir) (ocaml-mode . ocaml)
     (org-mode . org) (pascal-mode . pascal) (perl-mode . perl) (purescript-mode . purescript)
     (cperl-mode . perl) (php-mode . php) (qss-mode . css) (prisma-mode . prisma)
     (python-mode . python) (pygn-mode . pgn) (racket-mode . racket) (rjsx-mode . javascript)
     (rst-mode . rst) (ruby-mode . ruby) (rust-mode . rust) (rustic-mode . rust)
     (scala-mode . scala) (scheme-mode . scheme) (solidity-mode . solidity) (smithy-mode . smithy)
     (sql-mode . sql) (svelte-mode . svelte) (swift-mode . swift) (tablegen-mode . tablegen)
     (toml-mode . toml) (conf-toml-mode . toml) (tcl-mode . tcl) (tuareg-mode . ocaml)
     (twig-mode . twig) (typescript-mode . typescript) (typescript-tsx-mode . tsx)
     (typst-mode . typst) (verilog-mode . verilog) (vhdl-mode . vhdl) (nxml-mode . xml)
     (yaml-mode . yaml) (k8s-mode . yaml) (zig-mode . zig)))
 '(verilog-ts-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inherit bold))))
 '(eglot-semantic-definition ((t (:inherit font-lock-variable-use-face))))
 '(eglot-semantic-namespace ((t (:inherit (bold font-lock-misc-punctuation-face)))))
 '(font-lock-constant-face ((t (:foreground "gray82"))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-punctuation-face ((t (:foreground "light green" :weight normal))))
 '(fringe ((t (:background "#282a36")))))
