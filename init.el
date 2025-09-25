;;; init.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:

;; don't display the hideous toolbars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(require 'package)
;; Workaround the TLS problems with ELPA in older versions
(unless '(version> emacs-version "29.0")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq package-check-signature nil))
(unless '(version> emacs-version "29.0")
  (package-install 'gnu-elpa-keyring-update))


(setq package-install-upgrade-built-in t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t) ; Org-mode's repository
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
(package-initialize)

(setq package-native-compile t)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  )

(setq max-lisp-eval-depth 2000)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "bootstrap-straight.el")
(load-user-file "config.el")
(load-user-file "snippets.el")
(load-user-file "packages.el")
(load-user-file "project.el")
(load-user-file "lsp.el")
(load-user-file "keybindings.el")
(load-user-file "gpt.el")

(defun my/set-frame-params (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)
                             (menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (font . "monospace 13"))))
(add-hook 'after-make-frame-functions 'my/set-frame-params)


(provide 'init)
(put 'upcase-region 'disabled nil)



(defun my/copy-grammars-to-emacs-tree-sitter-dir ()
  "Copy tree-sitter grammar files to native Emacs dir."
  (interactive)
  (let* ((files (directory-files (tree-sitter-langs--bin-dir) nil "\\.so$")))
    (dolist (grammar-file files)
      (copy-file (concat (tree-sitter-langs--bin-dir) grammar-file) (concat (expand-file-name user-emacs-directory) "tree-sitter/" "libtree-sitter-" grammar-file) t)
      (message "%s grammar files copied" (length files)))))

(defcustom my-scroll-window-factor 0.5
  "Fraction of window height to scroll with custom scroll commands.
Should be a float between 0.1 and 1.0."
  :type 'float
  :group 'convenience)


(defcustom my-overscroll-window-factor 0.5
  "Fraction of window height to scroll past the end of the buffer with custom scroll commands.
Should be a float between 0.1 and 1.0."
  :type 'float
  :group 'convenience)

(defun last-fully-visible-line ()
  "Return the line number of the last fully visible line in the current window."
  (save-excursion
    (let ((start (window-start))
          (end (window-end nil t)))
      (goto-char end)
      ;; Check if current line is fully visible
      (when (not (pos-visible-in-window-p (line-beginning-position) nil t))
        (forward-line -1))
      (line-number-at-pos))))

(defun my--scroll-lines (down move)
  "Compute number of lines to scroll..."
  (let* ((orig-lines (max 1 (truncate (* my-scroll-window-factor (window-body-height)))))
         ;; true last line number; no +1 fudge
         (buf-lines (save-excursion (goto-char (point-max)) (line-number-at-pos)))
         (overscroll-lines (- (truncate (* my-overscroll-window-factor (window-body-height))))))
    (if move
        (if down
            (min (- buf-lines (line-number-at-pos)) orig-lines)
          (min (- (line-number-at-pos) 1) orig-lines))
      (if down
          ;; clamp the “how many lines remain below” at 0 to be extra safe
          (max overscroll-lines
               (min (max 0 (- buf-lines (last-fully-visible-line))) orig-lines))
        (min (- (line-number-at-pos) 1) orig-lines)))))


(defun my-scroll-down-custom ()
  "Scroll window down by a fraction of its height, move point accordingly."
  (interactive)
  (let* ((move-lines (my--scroll-lines t t))
         (scroll-lines (my--scroll-lines t nil))
         (orig-line (line-number-at-pos))
         (scroll-fn (if (bound-and-true-p pixel-scroll-precision-mode)
         #'(lambda (l) (pixel-scroll-precision-interpolate  (* (pixel-line-height (point)) (- l))))
         #'scroll-up)))
;    (message "calling scroll-fn with %d lines" scroll-lines)
    (unwind-protect
        (when (not (equal scroll-lines 0))
          (funcall scroll-fn scroll-lines))
      (progn
 ;       (message "moving %d lines after going to beginning" (+ orig-line move-lines -1))
        (goto-char (point-min)) ;; start safe
        (forward-line (+ orig-line move-lines -1)))))) ;; -1 since line numbers are 1-based

(defun my-scroll-up-custom ()
  "Scroll window up by a fraction of its height, move point accordingly."
  (interactive)
  (let* ((move-lines (my--scroll-lines nil t))
         (scroll-lines (my--scroll-lines nil nil))
         (orig-line (line-number-at-pos))
         (scroll-fn (if (bound-and-true-p pixel-scroll-precision-mode)
         #'(lambda (l) (pixel-scroll-precision-interpolate  (* (pixel-line-height (point)) l)))
       #'scroll-down)))
    (unwind-protect
        (when (not (equal scroll-lines 0))
          (funcall scroll-fn scroll-lines))
      (progn
        (goto-char (point-min)) ;; start safe
        (forward-line (max 0 (- orig-line move-lines 1))))))) ;; clamp at beginning



(setq custom-file "~/.emacs.d/autogen_customize.el")
(load custom-file)

;; overwrite helm-ag's project root finding so it doesn't go out of submodules
(eval-after-load "helm-ag"
  '(defun helm-ag--project-root ()
  "Find the root directory of the current project."
  (cl-loop for dir in '(".git" ".git/" ".hg/" ".svn/")
           when (locate-dominating-file default-directory dir)
           return it)))
