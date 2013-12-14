;;------------------------------------------------------------------------------
;; load-path
;;------------------------------------------------------------------------------
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

;;------------------------------------------------------------------------------
;; package.el
;;------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;------------------------------------------------------------------------------
;; init-loader
;;------------------------------------------------------------------------------
(require 'init-loader)
;; (setq init-loader-show-log-after-init nil)
(init-loader-load (locate-user-emacs-file "inits"))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Your setting area.
;; emacs-brave don't write here.

(add-to-list 'load-path "~/.emacs.d/auto-install")

(require 'platform-p)
(require 'my-functions)

; 画面や、フレームの幅に満たないウィンドウでも、テキストを折り返して表示する
;; (setq truncate-partial-width-windows nil)
;; ;(setq truncate-partial-width-windows t)
;; (global-set-key (kbd "C-c l") 'toggle-truncate-lines)

;;; ツールバーを表示しない
(tool-bar-mode 0)

;; 起動時のカレントディレクトリの設定
(cd "~")

;; ffap
(ffap-bindings)

;; ナロイングを有効に
(put 'narrow-to-region 'disabled nil)

;; 警告音もフラッシュも全て無効(警告音が完全にならなくなるので注意)
(setq ring-bell-function 'ignore)

;; 段落を移動
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;;--------------------------------------------------------------------------
;; Emacsに必要なパスを通す 
;;-------------------------------------------------------------------------
;; http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます
(when platform-darwin-p
  (dolist (dir (list
                "/usr/X11/bin"
                "/usr/local/bin"
                "/sbin"
                "/usr/sbin"
                "/bin"
                "/usr/bin"
                "/usr/local/mysql/bin"
                "/Developer/Tools"
                "/usr/local/sbin"
                "/opt/local/sbin"
                "/usr/local/bin"
                "/opt/local/bin" ;; これが/usr/binよりも下に書いてあればよい
                (expand-file-name "~/bin")
                (expand-file-name "~/bin/gnuplot")
                ))
    ;; PATH と exec-path に同じ物を追加します
    (when ;; (and 
        (file-exists-p dir) ;; (not (member dir exec-path)))
      (setenv "PATH" (concat dir ":" (getenv "PATH")))
      (setq exec-path (append (list dir) exec-path))))
  )

;;--------------------------------------------------------------------------
;; shell-mode
;;-------------------------------------------------------------------------
; .bashrcでcdやpushd,popdにエイリアスを貼る場合は、Emacs側にも伝えておく必要がある
; http://www.geocities.co.jp/SiliconValley-Bay/9285/EMACS-JA/emacs_412.html
;(setq shell-pushd-regexp "\\(cd\\|pushd\\)")
;(setq shell-popd-regexp "\\(bd\\|popd\\)")

;; shell-mode でエスケープを綺麗に表示
(when platform-darwin-p
  (autoload 'ansi-color-for-comint-mode-on "ansi-color"
    "Set `ansi-color-for-comint-mode' to t." t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;--------------------------------------------------------------------------
;; Font & Color
;;-------------------------------------------------------------------------
;; (require 'color-setting)

; 行間を開ける量、これを調整することでかなり見え方が変わる
;; (setq-default line-spacing 1)

;; テーマを設定
;; (load-theme 'whiteboard)

;;--------------------------------------------------------------------------
;; grep
;;-------------------------------------------------------------------------
;; M-g gmilk
(global-set-key "\M-g" 'grep)
(setq grep-command "gmilk ")
(setq grep-use-null-device nil)

;; C-x C-g gren
(global-set-key (kbd "C-x C-g") 'grep-find)
(setq grep-find-command "gren ")

;;--------------------------------------------------------------------------
;; C, C++
;;-------------------------------------------------------------------------
;; ヒント
;; (defface hint-face '((t (:foreground "#c0c0c0"))) nil)

;; ヒントの表示(コピペ用:???)
 ;; (defun display-hint ()
 ;;  (font-lock-add-keywords nil '(
 ;;    ("\n" 0 'hint-face prepend)
 ;;    ("\t" 0 'hint-face prepend)
 ;;    ("　" 0 'hint-face prepend)
 ;;  ))
 ;;  (setq buffer-display-table (make-display-table))
 ;;  (aset buffer-display-table ?\n (vconcat "?\n"))
 ;;  (aset buffer-display-table ?\t (vconcat "^\t"))
 ;;  (aset buffer-display-table ?　 (vconcat "□")))

;; マジックナンバー
(defface danger-magic-number-face '((t (:foreground "#e00000"))) nil)

;; マジックナンバーの表示
(defun font-lock-add-magic-number ()
  (font-lock-add-keywords nil '(
    ("[^]A-Za-z0-9_\)]\\([-\\+]?0x[0-9a-fA-F]+\\|[-\\+]?[0-9.]+[fF]?\\)" 1 'danger-magic-number-face append)
  ))
)

;;; C++モードではマジックナンバーとヒントを表示
(add-hook 'c++-mode-hook
	  '(lambda ()
             ;; (display-hint)
             (font-lock-add-magic-number)
	     ))

;; c++-modeキーバインディング
(add-hook 'c++-mode-hook
	  (function (lambda () (setq comment-column 40))))

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (local-set-key "\C-cc" 'c-insert-function-comment)
	     (local-set-key "\C-cn" 'c-insert-name-comment)
	     (local-set-key "\C-ci" 'c-insert-if0-region)
	     ))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (c-set-style "java")
;;             (setq c-basic-offset 4)))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "java")
             (setq c-basic-offset 4)
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'statement-cont 4)
             (c-set-offset 'arglist-intro 4)
             (c-set-offset 'arglist-cont-nonempty 4)
             (c-set-offset 'innamespace 0)
             (c-set-offset 'topmost-intro-cont 4)
             (c-set-offset 'member-init-intro 0)
             (c-set-offset 'member-init-cont -4)
             (c-set-offset 'access-label -4)
             (c-set-offset 'arglist-close 4)
             (c-set-offset 'case-label 4)
             (c-set-offset 'statement-case-open 0)
             (c-set-offset 'inher-cont 0)
             ))

;;--------------------------------------------------------------------------
;; sdic
;;-------------------------------------------------------------------------
(setq load-path (cons "~/.emacs.d/site-lisp/sdic" load-path))
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;; 辞書ファイルの置き場所
(setq sdic-eiwa-dictionary-list '((sdicf-client "~/.emacs.d/site-lisp/sdic/gene.sdic")))
(setq sdic-waei-dictionary-list '((sdicf-client "~/.emacs.d/site-lisp/sdic/jedict.sdic")))

;;--------------------------------------------------------------------------
;; recentf
;;-------------------------------------------------------------------------
;; (require 'recentf-ext)
;; (setq recentf-exclude '(".recentf"))
;; (setq recentf-auto-cleanup 10)
;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
;; (recentf-mode 1)

;;--------------------------------------------------------------------------
;; helm
;;-------------------------------------------------------------------------
;; C-x a をanytingのプレフィックスに置き換える、というのは迷わないでよさそう
(global-set-key (kbd "C-x a a") 'helm-apropos)
(global-set-key (kbd "C-x a c") 'helm-colors)
;; (global-set-key (kbd "C-x a g") 'helm-google-suggest)
(global-set-key (kbd "C-x a y") 'helm-show-kill-ring)

;;--------------------------------------------------------------------------
;; emacsclientサーバーの起動
;;-------------------------------------------------------------------------
(server-start)

;;--------------------------------------------------------------------------
;; dired-details
;;--------------------------------------------------------------------------
;; (require 'dired-details)
;; (dired-details-install)
;; (setq dired-details-hidden-string "")

;;--------------------------------------------------------------------------
;; joseph-single-dired
;;--------------------------------------------------------------------------
;; (require 'joseph-single-dired)
;; (eval-after-load 'dired '(require 'joseph-single-dired))

;;--------------------------------------------------------------------------
;; anzu
;;-------------------------------------------------------------------------
(global-set-key (kbd "C-q") 'anzu-query-replace-or)
(global-set-key (kbd "M-q") 'anzu-query-replace-regexp)

