;;===================================== KEYBINDINGS =================================================

(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-k] [C-k])


(global-unset-key (kbd "C-x C-z"))

(use-package prog-mode
  :ensure nil
  :bind ("C-q" . 'move-beginning-of-line)
  :bind ("C-e" . 'move-end-of-line)
  :bind ("C-." . 'next-buffer)
  :bind ("C-," . 'previous-buffer)
  :bind ("M-/" . 'delete-other-windows)
  :bind ("M-u" . 'compile)
  :bind ("M-m" . 'man)
  :bind ("M-s" . 'shell)
  :bind ("C-x f" . 'dired-ibuffer-sidebar-toggle)
  :bind ("C-x h" . 'hs-toggle-hiding)
  :bind ("C-x k" . 'kill-buffer-and-window)
  :bind ("C-w" . 'kill-line)
  :bind ("C-z" . 'kill-region)
  :bind ("M-z" . 'easy-kill)
  :bind ("<C-backspace>" . 'delete-forward-char)
  :bind ("M-q" . 'undo-tree-undo)
  :bind ("M-e" . 'undo-tree-redo)
  :bind ("M-w" . 'undo-tree-visualize)
  :bind ("M-i" . 'windmove-up)
  :bind ("M-k" . 'windmove-down)
  :bind ("M-l" . 'windmove-right)
  :bind ("M-j" . 'windmove-left)
  :bind ("<C-i>" . 'previous-line)
                                        ;:bind("C-i" . 'indent-for-tab-command)
  :bind ("C-k" . 'next-line)
  :bind ("C-l" . 'right-char)
  :bind ("C-j" . 'left-char)
  :bind ("C-i" . 'indent-region)
  )




(use-package ibuffer
  :ensure nil
  :bind ("M-i" . 'windmove-up)
  :bind ("M-k" . 'windmove-down)
  :bind ("M-l" . 'windmove-right)
  :bind ("M-j" . 'windmove-left)
  :bind ("<C-i>" . 'previous-line)
  :bind ("<C-k>" . 'next-line)
  :bind ("C-l" . 'right-char)
  :bind ("C-j" . 'left-char)
  :bind ("<C-return>" . 'ibuffer-visit-buffer-other-window)
  )


(use-package dired
  :ensure nil
  :bind ("M-i" . 'windmove-up)
  :bind ("M-k" . 'windmove-down)
  :bind ("M-l" . 'windmove-right)
  :bind ("M-j" . 'windmove-left)
  :bind ("<C-i>" . 'previous-line)
  :bind ("C-k" . 'next-line)
  :bind ("C-l" . 'right-char)
  :bind ("C-j" . 'left-char)
  :bind ("<C-return>" . 'dired-find-file)
  )



(use-package shell
  :ensure nil
  :bind ("C-M-i" . 'comint-previous-input)
  :bind ("C-M-k" . 'comint-next-input)
  )


;; (use-package lsp
;;   :ensure nil
;;   :bind ("M-q" . 'undo-tree-undo)
;;   :bind ("M-e" . 'undo-tree-redo)
;;   :bind ("M-w" . 'undo-tree-visualize)
;;   :bind ("C-x h" . 'hs-hide-block-at-point)
;;   )

;; (use-package org
;;   :ensure nill
;;   :bind )


;;===================================== ALIASES =================================================

(defalias 'ti 'describe-text-properties)
(defalias 'ki 'describe-key-briefly)
(defalias 'ev 'eval-buffer)
(defalias 'ma 'mark-whole-buffer)

(defalias 'tch 'make-empty-file)
(defalias 'rn 'crux-rename-buffer-and-file)

(defalias 'cr 'comment-region)
(defalias 'uc 'uncomment-region)

(defalias 'top 'beginning-of-buffer)
(defalias 'gtl 'goto-line)
(defalias 'btm 'end-of-buffer)


(defalias 'pl 'list-packages)

(defalias 'dsh (lambda ()
                 "switch to dashboard"
                 (interactive)
                 (switch-to-buffer "*dashboard*")))

(defalias 'hb 'hs-hide-block)
(defalias 'sb 'hs-show-block)
(defalias 'ha 'hs-hide-all)
(defalias 'hs 'hs-show-all)
(defalias 'sc 'shell-command)

;;https://github.com/june3474 thanks dad :D
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'my-keybinds-aliases)












