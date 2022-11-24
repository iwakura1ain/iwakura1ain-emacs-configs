;;===================================== KEYBINDINGS =================================================

(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-k] [C-k])

;;define my-keys-minor-mode
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'windmove-left)
    (define-key map (kbd "M-l") 'windmove-right)
    (define-key map (kbd "M-i") 'windmove-up)
    (define-key map (kbd "M-k") 'windmove-down)
    (define-key map (kbd "C-q") 'crux-move-beginning-of-line)
    (define-key map (kbd "C-e") 'move-end-of-line)
    (define-key map (kbd "C-.") 'next-buffer)
    (define-key map (kbd "C-,") 'previous-buffer)
    (define-key map (kbd "M-/") 'delete-other-windows)
    (define-key map (kbd "M-u") 'compile)
    (define-key map (kbd "M-m") 'man)
    (define-key map (kbd "M-s") 'shell)
    (define-key map (kbd "C-x f") 'dired-ibuffer-sidebar-toggle)
    (define-key map (kbd "C-x h") 'hs-toggle-hiding)
    (define-key map (kbd "C-x k") 'kill-buffer-and-window)
    (define-key map (kbd "C-M-z") 'kill-line)
    (define-key map (kbd "C-z") 'kill-region)
    (define-key map (kbd "M-z") 'easy-kill)
    (define-key map (kbd "<C-backspace>") 'delete-forward-char)
    (define-key map (kbd "M-q") 'undo-tree-undo)
    (define-key map (kbd "M-e") 'undo-tree-redo)
    (define-key map (kbd "M-w") 'undo-tree-visualize)
    (define-key map (kbd "<C-i>") 'previous-line)
    (define-key map (kbd "<C-k>") 'next-line)
    (define-key map (kbd "C-l") 'right-char)
    (define-key map (kbd "C-j") 'left-char)
    (define-key map (kbd "C-M-<backspace>") 'crux-kill-line-backwards)
    (define-key map (kbd "C-M-<delete>") 'kill-line)
    (define-key map (kbd "C-S-I") 'backward-paragraph)
    (define-key map (kbd "C-S-K") 'forward-paragraph)
    (define-key map (kbd "C-S-J") 'left-word)
    (define-key map (kbd "C-S-L") 'right-word)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

;;load my-keys-minor-mode after everything else
(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.

Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))


(use-package org
  :ensure nil
  :bind ("C-M-l" . 'org-metaright)
  :bind ("C-M-j" . 'org-metaleft)
  :bind ("C-M-i" . 'org-metaup)
  :bind ("C-M-k" . 'org-metadown)
  )



(use-package ein
  :ensure nil
  :bind ("C-M-k" . 'ein:worksheet-kill-cell)
  :bind ("C-M-n" . 'ein:worksheet-insert-cell-below)
  )




;; (global-unset-key (kbd "M-j"))
;; (global-unset-key (kbd "M-k"))
;; (global-unset-key (kbd "M-l"))
;; (global-unset-key (kbd "M-i"))


;; (global-set-key (kbd "M-j") 'windmove-left)
;; (global-set-key (kbd "M-k") 'windmove-down)
;; (global-set-key (kbd "M-l") 'windmove-right)
;; (global-set-key (kbd "M-i") 'windmove-up)


;; (use-package prog-mode
;;   :ensure nil
;;   :bind ("C-q" . 'move-beginning-of-line)
;;   :bind ("C-e" . 'move-end-of-line)
;;   :bind ("C-." . 'next-buffer)
;;   :bind ("C-," . 'previous-buffer)
;;   :bind ("M-/" . 'delete-other-windows)
;;   :bind ("M-u" . 'compile)
;;   :bind ("M-m" . 'man)
;;   :bind ("M-s" . 'shell)
;;   :bind ("C-x f" . 'dired-ibuffer-sidebar-toggle)
;;   :bind ("C-x h" . 'hs-toggle-hiding)
;;   :bind ("C-x k" . 'kill-buffer-and-window)
;;   :bind ("C-w" . 'kill-line)
;;   :bind ("C-z" . 'kill-region)
;;   :bind ("M-z" . 'easy-kill)
;;   :bind ("<C-backspace>" . 'delete-forward-char)
;;   :bind ("M-q" . 'undo-tree-undo)
;;   :bind ("M-e" . 'undo-tree-redo)
;;   :bind ("M-w" . 'undo-tree-visualize)
;;   :bind ("M-i" . 'windmove-up)
;;   :bind ("M-k" . 'windmove-down)
;;   :bind ("M-l" . 'windmove-right)
;;   :bind ("M-j" . 'windmove-left)
;;   :bind ("<C-i>" . 'previous-line)
;;                                         ;:bind("C-i" . 'indent-for-tab-command)
;;   :bind ("C-k" . 'next-line)
;;   :bind ("C-l" . 'right-char)
;;   :bind ("C-j" . 'left-char)
;;   :bind ("C-i" . 'indent-region)
;;   )




;; (use-package ibuffer
;;   :ensure nil
;;   :bind ("M-i" . 'windmove-up)
;;   :bind ("M-k" . 'windmove-down)
;;   :bind ("M-l" . 'windmove-right)
;;   :bind ("M-j" . 'windmove-left)
;;   :bind ("<C-i>" . 'previous-line)
;;   :bind ("<C-k>" . 'next-line)
;;   :bind ("C-l" . 'right-char)
;;   :bind ("C-j" . 'left-char)
;;   :bind ("<C-return>" . 'ibuffer-visit-buffer-other-window)
;;   )


;; (use-package dired
;;   :ensure nil
;;   :bind ("M-i" . 'windmove-up)
;;   :bind ("M-k" . 'windmove-down)
;;   :bind ("M-l" . 'windmove-right)
;;   :bind ("M-j" . 'windmove-left)
;;   :bind ("<C-i>" . 'previous-line)
;;   :bind ("C-k" . 'next-line)
;;   :bind ("C-l" . 'right-char)
;;   :bind ("C-j" . 'left-char)
;;   :bind ("<C-return>" . 'dired-find-file)
;;   )



;; (use-package shell
;;   :ensure nil
;;   :bind ("C-M-i" . 'comint-previous-input)
;;   :bind ("C-M-k" . 'comint-next-input)
;;   )



;; (use-package lsp
;;   :ensure nil
;;   :bind ("M-j" . 'windmove-left)
;;   :bind ("M-k" . 'windmove-down)
;;   :bind ("M-l" . 'windmove-right)
;;   :bind ("M-i" . 'windmove-up)
;;   )



;;===================================== ALIASES =================================================

(defalias 'ti 'describe-text-properties)
(defalias 'ki 'describe-key-briefly)
(defalias 'ev 'eval-buffer)
(defalias 'ma 'mark-whole-buffer)

(defalias 'touch 'make-empty-file)
(defalias 'rn 'crux-rename-buffer-and-file)
(defalias 'rm 'crux-delete-buffer-and-file)

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

(defalias 'enb (lambda ()
                 "switch to ein notebooklist"
                 (interactive)
                 (switch-to-buffer "*ein:notebooklist http://127.0.0.1:8888*")))

(defalias 'hb 'hs-hide-block)
(defalias 'sb 'hs-show-block)
(defalias 'ha 'hs-hide-all)
(defalias 'hs 'hs-show-all)
(defalias 'sc 'shell-command)

(defalias 'swp 'crux-transpose-windows)
(defalias 'sudo 'crux-sudo-edit)
(defalias 'his 'crux-recentf-find-file)

(defalias 'ein 'ein:run)
(defalias 'ein-ins 'ein:worksheet-insert-cell-below)
(defalias 'ein-del 'ein:worksheet-delete-cell)
(defalias 'ein-hs 'ein:worksheet-toggle-output)
(defalias 'ein-rn 'ein:notebook-rename-command-km)

;;===================================== FUNCTIONS =================================================

;;https://github.com/june3474 thanks dad :D
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun ppt2pdf ()
  "convert pptx to pdf files in directory using libreoffice"
  (interactive)
  (setq dir (read-directory-name "Directory: "))
  (shell-command (format "ppt2pdf --outdir %s %s*.pptx &" dir dir))
  (shell-command (format "rm %s*.pptx &" dir)))

(provide 'my-keybinds-aliases)












