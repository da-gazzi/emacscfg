;;; snippets.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(defun scheremo/insert_author ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "author_header"))
  )


(use-package yasnippet
  :defer 15 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))
;; YAS keybinds
(global-set-key (kbd "C-x C-a") 'scheremo/insert_author)
(global-set-key (kbd "C-x y") 'yas-insert-snippet)
(eval-after-load 'yasnippet
  '(progn
     (define-key yas-keymap (kbd "TAB") 'yas-next-field)))
