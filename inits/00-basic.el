;;------------------------------------------------------------------------------
;; Basic Settings
;;------------------------------------------------------------------------------
(setq require-final-newline nil)
(setq make-backup-files nil)
(setq truncate-partial-width-windows t)
;; (tool-bar-mode 0)
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default case-fold-search t)
(show-paren-mode t)
(setq dabbrev-case-replace nil)
(ffap-bindings)
(setq transient-mark-mode t)
(setq set-mark-command-repeat-pop t)
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(setq indent-line-function 'indent-relative-maybe)
;; (load-theme 'misterioso)

;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;;------------------------------------------------------------------------------
;; Key Bindings
;;------------------------------------------------------------------------------
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key (kbd "M-h")   'help-for-help)
(global-set-key (kbd "M-g")   'grep)
(global-set-key (kbd "M-o")   'next-error)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-;")   'other-window-or-split)
(global-set-key (kbd "C-.")   'ff-find-other-file)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c s") 'sort-lines)

;;--------------------------------------------------------------------------
;; bs-cycle
;;-------------------------------------------------------------------------
(global-set-key (kbd "C-<")   'bs-cycle-next)
(global-set-key (kbd "C->")   'bs-cycle-previous)
(global-set-key (kbd "C-x C-b") 'bs-show)

(add-to-list 'bs-configurations
             '("mycustom"
               "^\\*shell.*\\*$\\|^\\*magit.*\\*$\\|^\\*scratch\\*$"
               nil nil bs-visits-non-file bs-sort-buffer-interns-are-last))

(setq bs-cycle-configuration-name "mycustom")

;;--------------------------------------------------------------------------
;; dired-x
;;-------------------------------------------------------------------------
(require 'dired-x)

;;--------------------------------------------------------------------------
;; uniquify
;;-------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;--------------------------------------------------------------------------
;; ruby-mode
;;-------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.rb$\\|\\.ru$\\|Rakefile$\\|Gemfile\\|\\.irbrc" . ruby-mode))

;; inf-ruby
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;--------------------------------------------------------------------------
;; cua-mode
;;-------------------------------------------------------------------------
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;------------------------------------------------------------------------------
;; recentf
;;------------------------------------------------------------------------------
(setq recentf-max-saved-items 5000)
(setq recentf-save-file (concat user-emacs-directory "/" ".recentf"))

;;------------------------------------------------------------------------------
;; Recommended Settings
;;------------------------------------------------------------------------------
;; comment!
;; (put 'narrow-to-region 'disabled nil)
;; (setq ring-bell-function 'ignore)

