;;; keybindings.el --- Moritz Scherer's emacs setup

(global-unset-key (kbd "M-u"))

;; Multi-Cursor keybinds
(global-set-key (kbd "C-,") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/mark-previous-like-this)

;; Magit keybinds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'smerge-keep-all)
(global-set-key (kbd "C-c m") 'smerge-keep-mine)
(global-set-key (kbd "C-c n") 'smerge-next)
(global-set-key (kbd "C-c p") 'smerge-previous)
(global-set-key (kbd "C-c o") 'smerge-keep-other)

(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-c s" #'isearch-forward-symbol)


(defun pt/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))

(bind-key "C-c I" #'pt/indent-just-yanked)

;; YAS keybinds
(global-set-key (kbd "C-x C-a") 'scheremo/insert_author)
(global-set-key (kbd "C-x y") 'yas-insert-snippet)

(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "s-u") 'revert-buffer)

;; Helm bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; BEGIN CHATGPT SLOP
(defvar my/helm-ff-tab-preview-close-buffer t
  "If non-nil, close preview buffers opened via TAB after exiting helm.")

(defvar my/helm-ff-previewed-buffers nil
  "Buffers opened during preview that may be cleaned up.")

(defvar my/helm-ff-last-inserted-dir nil
  "The last directory inserted into the minibuffer, to detect second TAB.")


(defun my/helm-ff-smart-tab ()
  "Custom TAB behavior in `helm-find-files`:
- Preview file on first press.
- Complete dir on first, open on second."
  (interactive)
  (let* ((sel (helm-get-selection))
         (fullpath (and sel (expand-file-name sel)))
         (is-dir (and fullpath (file-directory-p fullpath)))
         (is-file (and fullpath (file-regular-p fullpath)))
         (pattern (file-name-as-directory (expand-file-name helm-pattern))))
    (cond
     ;; 📁 Directory logic
     (is-dir
      (let ((already-inserted (string= pattern (file-name-as-directory fullpath))))
        (if (and already-inserted
                 (string= my/helm-ff-last-inserted-dir fullpath))
            ;; Second press — open dir
            (progn
              (setq my/helm-ff-last-inserted-dir nil)
              (helm-execute-persistent-action))
          ;; First press — insert
          (setq my/helm-ff-last-inserted-dir fullpath)
          (helm-set-pattern (directory-file-name fullpath)))))

     ;; 📄 File preview
     (is-file
      ;; make sure the pattern matches the file

      ;; track previewed buffer
      (unless (get-file-buffer fullpath)
        (push (find-file-noselect fullpath) my/helm-ff-previewed-buffers))
      ;; now call persistent-action with correct pattern
      (helm-execute-persistent-action))

     ;; Fallback
     (t
      (helm-execute-persistent-action)))))

(defun my/helm-ff-cleanup-preview-buffers ()
  "Cleanup previewed buffers on exit."
  (when my/helm-ff-tab-preview-close-buffer
    (dolist (buf my/helm-ff-previewed-buffers)
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (setq my/helm-ff-previewed-buffers nil)
    (setq my/helm-ff-last-inserted-dir nil)))

(add-hook 'helm-cleanup-hook #'my/helm-ff-cleanup-preview-buffers)

(with-eval-after-load 'helm-files
  (define-key helm-find-files-map (kbd "TAB") #'my/helm-ff-smart-tab)
  (define-key helm-find-files-map (kbd "<tab>") #'my/helm-ff-smart-tab))
;; END CHATGPT SLOP

(global-set-key (kbd "M-s s") 'helm-do-ag-project-root)
;; replace ordinary isearch with swoop
(global-set-key (kbd "C-s") 'helm-swoop-without-pre-input)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
;; lol we don't use isearch
;(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Undo-tree stuff
(define-key undo-tree-map (kbd "C-x u") 'undo-tree-undo)
(define-key undo-tree-map (kbd "C-x U") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-x M-u") 'undo-tree-visualize)

;; Verilog-mode - disable the ultra annoying semicolon
(define-key verilog-mode-map (kbd ";") nil)

(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows))
(defadvice delete-window (after rebalance-windows activate)
  (balance-windows))
;(ad-activate 'split-window-horizontally)
