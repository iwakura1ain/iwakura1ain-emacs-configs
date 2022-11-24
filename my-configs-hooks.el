;;=========================== PACKAGE CONFIGURATION + CUSTOM HOOKS  =================================
(add-hook 'global-mode-hook 'global-whitespace-mode -1)
(add-hook 'global-mode-hook 'prettify-symbols-mode)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-operators-mode)
(add-hook 'prog-mode-hook 'highlight-function-calls-mode)

(add-hook 'prog-mode-hook 'eldoc-mode)

(add-hook 'prog-mode-hook 'company-mode)
(provide 'company)

(add-hook 'prog-mode-hook 'flymake-mode-off)


;;c eldoc hooks
;;(Autoload 'tal-mode "tal-mode" "A major mode for editing TAL files." t nil)
;;(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
;;(load "c-eldoc")

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)

(defvar c-eldoc-cpp-command "/usr/bin/cpp ")
(defvar c-eldoc-cpp-macro-arguments "-dP -w -P")
(defvar c-eldoc-cpp-normal-arguments "-w -P")
(defvar c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")


;;js rjsx-mode + ac-js2-company for js only
(add-hook 'js2-mode-hook (company-mode -1))
(add-hook 'js2-mode-hook 'auto-complete-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;js2 indent styles
;;(setq js2-bounce-indent-p t)


;;rjsx-mode configuration
(add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))

;;different face for js
(defun js-buffer-face-mode ()
  "custom faces for js"
  (face-remap-add-relative 'font-lock-function-name-face '(:foreground "turquoise3" :overline nil :height 0.9))
  (face-remap-add-relative 'font-lock-variable-name-face '(:overline t :weight ultra-bold ))
  (face-remap-add-relative 'font-lock-type-face '(:foreground "turquoise1" :height 1.0))
  (face-remap-add-relative 'font-lock-keyword-face '(:foreground "turquoise3" :weight bold :height 0.9)))


(add-hook 'tide-mode-hook 'js-buffer-face-mode)
(add-hook 'js2-mode-hook 'js-buffer-face-mode)


;;tide-mode for typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-installXF
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;format config options 
;;https://github.com/Microsoft/TypeScript/blob/v3.3.1/src/server/protocol.ts#L2858-L2890
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                                                                  :placeOpenBraceOnNewLineForFunctions nil))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;enable python elpy, pyenv for python 
(elpy-enable)

(setq python-flymake-command '("/home/dks/.local/bin/flake8" "--config=/home/dks/.config/flake8" "-"))
(setq python-check-command "/home/dks/.local/bin/flake8  --config=/home/dks/.config/flake8")
(setq elpy-syntax-check-command python-check-command)



;;web-mode hook for html + css + js
(add-hook 'web-mode-hook (company-mode -1))
(add-hook 'web-mode-hook 'auto-complete-mode)

(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-css-indent-offset 2)))

;;web-mode for web development
(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))


                                        ;(eval-after-load 'flycheck
                                        ;  '(flycheck-add-mode 'html-tidy 'web-mode))


;;yasnippet configuration
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;;company backend configuration
                                        ;web mode
(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)


;;org mode
(add-hook 'org-mode-hook 'org-indent-mode)

                                        ;tabnine AI autocomplete
                                        ;(require 'company-tabnine)
                                        ;(add-to-list 'company-backends 'company-tabnine)


                                        ;auto-complete configuration
                                        ;(global-auto-complete-mode nil)


;;dashboard
(require 'dashboard)
;;(dashboard-setup-startup-hook)
;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))




;;dired auto update
(setq global-auto-revert-non-file-buffers t)
;;(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; ;;dired omit mode
(add-hook 'dired-mode-hook ( lambda ()
                             (dired-hide-dotfiles-mode)
                             (all-the-icons-dired-mode)
                             (dired-buffer-face-mode)
                             (local-set-key (kbd "z") 'dired-hide-dotfiles-mode)))

;;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(defun dired-buffer-face-mode ()
  "custom faces for dired"
  (interactive)
  (face-remap-add-relative 'default '(:height 0.8)))




;; ;;dired omit mode toggle dotfiles
;; ;; (setq dired-omit-files
;; ;;       (rx (or (seq bol (? ".") "#")
;; ;;               (seq bol "." eol)
;; ;;               )))

;; (defun dired-dotfiles-toggle ()
;;   "Show/hide dot-files"
;;   (interactive)
;;   (when (equal major-mode 'dired-mode)
;;     (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
;; 	(progn 
;; 	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
;; 	  (message "h")
;; 	  (dired-mark-files-regexp "^\\\.")
;; 	  (dired-do-kill-lines))
;;       (progn (revert-buffer) ; otherwise just revert to re-show
;; 	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))


;; ibuffer: smaller font and all-the-icons 
(add-hook 'ibuffer-sidebar-mode-hook
          ( lambda ()
            (face-remap-add-relative 'default '(:height 0.8))
            (local-unset-key (kbd "C-k")))
          (all-the-icons-ibuffer-mode))


;;toggle both dired, ibuffer sidebars at the same time
(defun dired-ibuffer-sidebar-toggle ()
  "toggle both dired, ibuffer sidebars at the same time"
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar)
  (windmove-up))


;;??
(setq font-lock-maximum-decoration t)


;;flymake for elisp
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'emacs-lisp-mode-hook (flycheck-mode -1))
;;(add-hook 'emacs-lisp-mode-hook (smartparens-strict-mode -1))



;; tab widths
(setq tab-stop-list (number-sequence 4 200 4))
(setq tab-width 4)


;; java jdee 
;; (add-hook 'jdee-mode-hook 'highlight-numbers-mode)
;; (add-hook 'jdee-mode-hook 'highlight-operators-mode)
;; (add-hook 'jdee-mode-hook 'highlight-function-calls-mode)
;; (add-hook 'jdee-mode-hook 'hs-minor-mode)

;; (add-hook 'jdee-mode-hook ( lambda ()
;;                             (local-unset-key (kbd "M-q"))
;;                             (local-unset-key (kbd "M-e"))
;;                             (face-remap-add-relative 'font-lock-type-face '(:height 1.2))))

;;java lsp
(require 'lsp-java)
(add-hook 'java-mode-hook ( lambda ()
                            (local-unset-key (kbd "M-q"))
                            (local-unset-key (kbd "M-e"))
                            (hs-minor-mode)
                            (dap-mode -1)
                            (highlight-numbers-mode)
                            (highlight-operators-mode)
                            (highlight-function-calls-mode)
                            (face-remap-add-relative 'font-lock-type-face '(:height 1.2))))

(add-hook 'java-mode-hook #'lsp)
;;(setenv "JAVA_HOME"  "path_to_java_folder/Contents/Home/")
(setq lsp-java-java-path "/usr/bin/java")


;;lsp-java and spring with sts4
(require 'lsp-java-boot)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)


;;java lsp-javacomp
;; (require 'lsp-javacomp)
;; (add-hook 'java-mode-hook #'lsp-javacomp-enable)


(require 'ibuffer)
(add-hook 'ibuffer-mode-hook ( lambda ()
                               (local-unset-key (kbd "M-j"))
                               (local-unset-key (kbd "M-l"))
                               (local-unset-key (kbd "M-k"))
                               (local-unset-key (kbd "M-i"))))


(provide 'my-configs-hooks)

