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
;; save sessions
(desktop-save-mode 1)

(require 'package)
;; Workaround the TLS problems with ELPA in older versions
(unless '(version> emacs-version "29.0")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq package-check-signature nil))
(unless '(version> emacs-version "29.0")
  (package-install 'gnu-elpa-keyring-update))

(defun my/setup-font (frame)
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (set-face-attribute 'default frame
                          :family "DejaVu Sans Mono"
                          :height 110
                          :weight 'medium))))

(when (display-graphic-p)
  (my/setup-font (selected-frame)))

(add-hook 'after-make-frame-functions #'my/setup-font)
;(defun my/set-frame-params (frame)
;  (modify-frame-parameters frame
;                           '((vertical-scroll-bars . nil)
;                             (horizontal-scroll-bars . nil)
;                             (menu-bar-lines . 0)
;                             (tool-bar-lines . 0)
;                             (font . "DejaVu Sans Mono 11"))))
;(add-hook 'after-make-frame-functions 'my/set-frame-params)
;
;(set-frame-font "DejaVu Sans Mono 11")
;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 11"))

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
(load-user-file "google-c-style.el")

;; stop emacs from warning about overriding projectile-root-functions
(put 'projectile-project-root-functions 'safe-local-variable
     (lambda (v)
       (and (listp v)
            (cl-every #'symbolp v))))


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

(defvar-local my--overscroll-down-count 0)
(defvar-local my--overscroll-up-count 0)

(defun my--scroll-step-lines ()
  "Return the normal scroll step in screen lines."
  (max 1
       (truncate (* my-scroll-window-factor
                    (window-text-height nil t)))))

(defun my--overscroll-limit ()
  "Return the overscroll limit in screen lines."
  (max 0
       (truncate (* my-overscroll-window-factor
                    (window-text-height nil t)))))

(defun my--pixel-scroll-active-p ()
  "Return non-nil when precision pixel scrolling is active."
  (and (bound-and-true-p pixel-scroll-precision-mode)
       (fboundp 'pixel-scroll-precision-interpolate)))

(defun my--point-window-row ()
  "Return point's visual row within the selected window."
  (let ((pos (posn-at-point)))
    (or (cdr (posn-col-row pos)) 0)))

(defun my--restore-point-window-row (row col)
  "Move point back to ROW within the selected window and restore COL."
  (move-to-window-line row)
  (move-to-column col))

(defun my--move-point-visual-lines (n)
  "Move point by N visual lines, saturating at buffer boundaries."
  (let ((line-move-visual t))
    (condition-case nil
        (line-move n t)
      (beginning-of-buffer
       (goto-char (point-min)))
      (end-of-buffer
       (goto-char (point-max))))))

(defun my--window-at-bottom-p ()
  "Return non-nil if the selected window is at bottom."
  (pos-visible-in-window-p (point-max) nil t))

(defun my--window-at-top-p ()
  "Return non-nil if the selected window is at top."
  (pos-visible-in-window-p (point-min) nil t))

(defun my--scroll-window-forward (n)
  "Scroll forward by N screen lines."
  (if (my--pixel-scroll-active-p)
      ;; pixel-scroll page-down uses negative delta
      (pixel-scroll-precision-interpolate (- n) nil 1)
    (scroll-up n)))

(defun my--scroll-window-backward (n)
  "Scroll backward by N screen lines."
  (if (my--pixel-scroll-active-p)
      ;; pixel-scroll page-up uses positive delta
      (pixel-scroll-precision-interpolate n nil 1)
    (scroll-down n)))

(defun my-scroll-down-custom ()
  "Scroll down by a fraction of the window height.
Keeps point at roughly the same window row while scrolling.
At bottom, allows limited overscroll, then moves to point-max."
  (interactive)
  (let ((n (my--scroll-step-lines))
        (limit (my--overscroll-limit)))
    ;; changing direction resets opposite-edge overscroll
    (setq my--overscroll-up-count 0)
    (if (my--window-at-bottom-p)
        (let ((room (- limit my--overscroll-down-count)))
          (if (> room 0)
              (let ((step (min n room)))
                (setq my--overscroll-down-count (+ my--overscroll-down-count step))
                (my--move-point-visual-lines step))
            (goto-char (point-max))))
      (let ((row (my--point-window-row))
            (col (current-column)))
        (condition-case nil
            (my--scroll-window-forward n)
          (end-of-buffer
           (goto-char (point-max))))
        (my--restore-point-window-row row col)
        (setq my--overscroll-down-count 0)))))

(defun my-scroll-up-custom ()
  "Scroll up by a fraction of the window height.
Keeps point at roughly the same window row while scrolling.
At top, allows limited overscroll, then moves to point-min."
  (interactive)
  (let ((n (my--scroll-step-lines))
        (limit (my--overscroll-limit)))
    ;; changing direction resets opposite-edge overscroll
    (setq my--overscroll-down-count 0)
    (if (my--window-at-top-p)
        (let ((room (- limit my--overscroll-up-count)))
          (if (> room 0)
              (let ((step (min n room)))
                (setq my--overscroll-up-count (+ my--overscroll-up-count step))
                (my--move-point-visual-lines (- step)))
            (goto-char (point-min))))
      (let ((row (my--point-window-row))
            (col (current-column)))
        (condition-case nil
            (my--scroll-window-backward n)
          (beginning-of-buffer
           (goto-char (point-min))))
        (my--restore-point-window-row row col)
        (setq my--overscroll-up-count 0)))))


(setq custom-file "~/.emacs.d/autogen_customize.el")
(load custom-file)

;; overwrite helm-ag's project root finding so it doesn't go out of submodules
(eval-after-load "helm-ag"
  '(defun helm-ag--project-root ()
  "Find the root directory of the current project."
  (cl-loop for dir in '(".git" ".git/" ".hg/" ".svn/")
           when (locate-dominating-file default-directory dir)
           return it)))
(put 'downcase-region 'disabled nil)

;; Fix compatibility of (e.g.) dired-do-rename and helm
(defun my-helm-mode--default-filename (orig-fun default dir &optional must-match)
  ;; Emacs 30+ sometimes passes a list (DIR DEFAULT-FILE).
  (if  (listp default)
      (if (and
             ;; very defensive, in case Helm starts using lists itself later
             (= (length default) 2)
             (stringp (cadr default)))
          (setq default (cadr default))
    (if (and (= (length default) 1)
             (stringp (car default)))
        (setq default (car default)))))
  (funcall orig-fun default dir must-match))

(with-eval-after-load 'helm-mode
  (advice-add 'helm-mode--default-filename
              :around #'my-helm-mode--default-filename))


;; helper to describe face at point
(defun describe-face-at-point (face)
  "Describe the typeface properties of FACE."

  (interactive
   (list
    (let* ((fap (face-at-point))
           (cseq (append (when fap (list fap)) '("mode-line-inactive" "mode-line")) ))
      (completing-read "Face: " cseq nil t))))

  (describe-face face))
