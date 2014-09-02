;;--------------------------------------------------------------------------
;; smartrep
;;--------------------------------------------------------------------------
(require 'smartrep)

;;--------------------------------------------------------------------------
;; popwin
;;--------------------------------------------------------------------------
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)
(push '("*grep*" :height 20 :noselect t :stick t) popwin:special-display-config)

;;------------------------------------------------------------------------------
;; helm
;;------------------------------------------------------------------------------
(require 'helm-config)
(helm-mode 1)
;; Function that doesn't use helm
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point))
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(push '("*helm mini*" :height 20) popwin:special-display-config)
(push '("*helm-mode-execute-extended-command*" :height 20) popwin:special-display-config)
(global-set-key (kbd "C-,") 'helm-mini)

;;------------------------------------------------------------------------------
;; anzu
;;------------------------------------------------------------------------------
(require 'anzu)
(global-anzu-mode +1)

(defun anzu-query-replace-or (n)
  (interactive "P")
  (if (consp n)
      (anzu-query-replace-at-cursor)
    (anzu-query-replace)))

(global-set-key (kbd "M-%") 'anzu-query-replace-or)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;;--------------------------------------------------------------------------
;; auto-complete
;;--------------------------------------------------------------------------
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "/" "ac-dict"))
(ac-config-default)
(global-auto-complete-mode t)

;;--------------------------------------------------------------------------
;; smartparens
;;--------------------------------------------------------------------------
(require 'smartparens-config)
(smartparens-global-mode 1)

;;--------------------------------------------------------------------------
;; wdired
;;--------------------------------------------------------------------------
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;--------------------------------------------------------------------------
;; open-junk-file
;;--------------------------------------------------------------------------
(require 'open-junk-file)
;; (setq open-junk-file-format "~/Documents/junk/%Y-%m%d-%H%M%S.")
(global-set-key "\C-xj" 'open-junk-file)

;;--------------------------------------------------------------------------
;;highlight-symbol
;;--------------------------------------------------------------------------
(require 'highlight-symbol)

(global-unset-key (kbd "M-s"))

(smartrep-define-key global-map "M-s"
  '(("M-s"        . 'highlight-symbol-next)
    ("h"          . 'highlight-symbol-at-point)
    ("n"          . 'highlight-symbol-next)
    ("p"          . 'highlight-symbol-prev)
    ("N"          . 'highlight-symbol-prev)
    ("l"          . 'highlight-symbol-list-all)
    ("a"          . 'highlight-symbol-remove-all)
    ("o"          . 'highlight-symbol-occur)
    ("j"          . 'highlight-symbol-next-in-defun)
    ("k"          . 'highlight-symbol-prev-in-defun)
    ("r"          . 'highlight-symbol-query-replace)))

;;--------------------------------------------------------------------------
;; vim-region
;;--------------------------------------------------------------------------
(require 'vim-region)
(global-set-key (kbd "C-@") 'vim-region-mode)

;;--------------------------------------------------------------------------
;; quickrun
;;--------------------------------------------------------------------------
(require 'quickrun)
(push '("*quickrun*" :stick t) popwin:special-display-config)
(global-set-key (kbd "<f5>") 'quickrun)

;;--------------------------------------------------------------------------
;; multiple-cursors
;;--------------------------------------------------------------------------
(require 'multiple-cursors)

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")
(smartrep-define-key global-map "C-t"
  '(("C-t"        . 'mc/mark-next-like-this)
    ("C-n"        . 'mc/mark-next-like-this)
    ("C-p"        . 'mc/mark-previous-like-this)
    ("C-m"        . 'mc/mark-more-like-this-extended)
    ("C-u"        . 'mc/unmark-next-like-this)
    ("C-U"        . 'mc/unmark-previous-like-this)
    ("C-s"        . 'mc/skip-to-next-like-this)
    ("C-S"        . 'mc/skip-to-previous-like-this)
    ("C-*"        . 'mc/mark-all-like-this)
    ("C-d"        . 'mc/mark-all-like-this-dwim)
    ("C-i"        . 'mc/insert-numbers)
    ("C-o"        . 'mc/sort-regions)
    ("C-O"        . 'mc/reverse-regions)))

;;--------------------------------------------------------------------------
;; ace-jump-mode
;;--------------------------------------------------------------------------
(require 'ace-jump-mode)
(global-set-key (kbd "M-j") 'ace-jump-mode)
(global-set-key (kbd "M-J") 'ace-jump-mode-pop-mark)

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (local-set-key (kbd "M-j") 'ace-jump-mode)
	     ))

;;--------------------------------------------------------------------------
;; yasnippet
;;--------------------------------------------------------------------------
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "/" "snippets"))
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "C-x y y") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x y n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x y v") 'yas-visit-snippet-file)

;; helm-interface
(eval-after-load "helm-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (helm-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*helm yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key helm-command-map (kbd "y") 'yas/insert-snippet)))

(push '("*helm-mode-execute-extended-command*" :height 20) popwin:special-display-config)

;;------------------------------------------------------------------------------
;; org-mode
;;------------------------------------------------------------------------------
(require 'org)
(require 'ob-C)
(require 'ob-ruby)

(setq org-directory "~/Documents/junk")
(setq org-agenda-files (list org-directory))

(setq org-src-fontify-natively t)

(smartrep-define-key
    org-mode-map "C-c"
  '(("n" . (lambda () (outline-next-visible-heading 1)))
    ("p" . (lambda () (outline-previous-visible-heading 1)))
    ("i"   . (org-clock-in))
    ("O"   . (org-clock-out))
    ("o"   . (org-open-at-point))
    ))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "emacs-lisp")
           (string= lang "ruby")
           (string= lang "C")
           (string= lang "cpp")
           )))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;;--------------------------------------------------------------------------
;; overriding-minor-mode
;;--------------------------------------------------------------------------
;; (makunbound 'overriding-minor-mode-map) ; For reset minor-mode
(define-minor-mode overriding-minor-mode
  "Define force override keybindings"
  t                                     ; Default enable
  ""                                    ; Not display mode-line
  `(
    (,(kbd "C-,") . helm-mini)
    (,(kbd "M-h") . help-for-help)
    ))

;;--------------------------------------------------------------------------
;; ring-transparency
;;--------------------------------------------------------------------------
(defun ring-transparency (arg)
  (interactive "P")
  ; (let* ((ring '(100 50 25 0))
  (let* ((ring '(100 90 80))
         (current (frame-parameter nil 'alpha))
         (last (car (last ring)))
         (next (if arg
                   (if (equal current (car ring)) last (car ring))
                 (or (cadr (member current ring)) (car ring)))))
    (set-frame-parameter nil 'alpha next)))

(global-set-key (kbd "C--") 'ring-transparency)
