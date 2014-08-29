;;------------------------------------------------------------------------------
;; load-path
;;------------------------------------------------------------------------------
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

;;--------------------------------------------------------------------------
;; windows.el
;;   Do not load windows.el after loading popwin.el. Load windows.el first.
;;     https://github.com/m2ym/popwin-el#windowsel
;;--------------------------------------------------------------------------

;; キーバインドを変更(デフォルトは C-c C-w)
;; (setq win:switch-prefix "\C-z")
;; (define-key global-map win:switch-prefix nil)
;; (define-key global-map "\C-z1" 'win-switch-to-window)

(require 'windows)

;; 新規にフレームを作らない
(setq win:use-frame nil)
(win:startup-with-window)

;; C-x C-c で終了するとそのときのウィンドウの状態を保存する
;; C-x C なら保存しない
(define-key ctl-x-map "\C-c" 'see-you-again)
(define-key ctl-x-map "C" 'save-buffers-kill-emacs)

;; M-0   .. toggle
;; M-1-9 .. switch
(defun win-switch-to-window-1 () (interactive) (win-switch-to-window 0 1))
(defun win-switch-to-window-2 () (interactive) (win-switch-to-window 0 2))
(defun win-switch-to-window-3 () (interactive) (win-switch-to-window 0 3))
(defun win-switch-to-window-4 () (interactive) (win-switch-to-window 0 4))
(defun win-switch-to-window-5 () (interactive) (win-switch-to-window 0 5))
(defun win-switch-to-window-6 () (interactive) (win-switch-to-window 0 6))
(defun win-switch-to-window-7 () (interactive) (win-switch-to-window 0 7))
(defun win-switch-to-window-8 () (interactive) (win-switch-to-window 0 8))
(defun win-switch-to-window-9 () (interactive) (win-switch-to-window 0 9))

(global-set-key (kbd "M-0") 'win-toggle-window)
(global-set-key (kbd "M-1") 'win-switch-to-window-1)
(global-set-key (kbd "M-2") 'win-switch-to-window-2)
(global-set-key (kbd "M-3") 'win-switch-to-window-3)
(global-set-key (kbd "M-4") 'win-switch-to-window-4)
(global-set-key (kbd "M-5") 'win-switch-to-window-5)
(global-set-key (kbd "M-6") 'win-switch-to-window-6)
(global-set-key (kbd "M-7") 'win-switch-to-window-7)
(global-set-key (kbd "M-8") 'win-switch-to-window-8)
(global-set-key (kbd "M-9") 'win-switch-to-window-9)

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

;;--------------------------------------------------------------------------
;; auto-install
;; (install-elisp-from-emacswiki "auto-install.el")
;;--------------------------------------------------------------------------
(require 'auto-install)
;; (auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;--------------------------------------------------------------------------
;; basic setting
;;--------------------------------------------------------------------------
; 画面や、フレームの幅に満たないウィンドウでも、テキストを折り返して表示する
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

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

;; カーソルの点滅を止める
;; (blink-cursor-mode 0)

;;--------------------------------------------------------------------------
;; Font & Color
;;-------------------------------------------------------------------------
(require 'color-setting)

; 行間を開ける量、これを調整することでかなり見え方が変わる
(setq-default line-spacing 1)

;; テーマを設定
;; (load-theme 'whiteboard)

;; IMEの日本語対応
(setq default-input-method "MacOSX")

;;--------------------------------------------------------------------------
;; auto-save-buffers-enhanced (ファイルの自動セーブ)
;;--------------------------------------------------------------------------
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1) ; 指定のアイドル秒で保存
(auto-save-buffers-enhanced t)

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

;; sdic-display-buffer 書き換え 
(defadvice sdic-display-buffer (around sdic-display-buffer-normalize activate)
  "sdic のバッファ表示を普通にする。"
  (setq ad-return-value (buffer-size))
  (let ((p (or (ad-get-arg 0)
               (point))))
    (and sdic-warning-hidden-entry
         (> p (point-min))
         (message "この前にもエントリがあります。"))
    (goto-char p)
    (display-buffer (get-buffer sdic-buffer-name))
    (set-window-start (get-buffer-window sdic-buffer-name) p)))

(defadvice sdic-other-window (around sdic-other-normalize activate)
  "sdic のバッファ移動を普通にする。"
  (other-window 1))

(defadvice sdic-close-window (around sdic-close-normalize activate)
  "sdic のバッファクローズを普通にする。"
  (bury-buffer sdic-buffer-name))

(push '("*sdic*") popwin:special-display-config)

;;--------------------------------------------------------------------------
;; recentf
;;-------------------------------------------------------------------------
(require 'recentf-ext)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)

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
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")

;;--------------------------------------------------------------------------
;; joseph-single-dired
;;--------------------------------------------------------------------------
(require 'joseph-single-dired)
(eval-after-load 'dired '(require 'joseph-single-dired))

;;--------------------------------------------------------------------------
;;milkode
;;--------------------------------------------------------------------------
(require 'moz)
(require 'milkode)
(setq gmilk-command "gomilk --nogroup --nocolor --smart-case")
(global-set-key (kbd "M-g") 'milkode:search)
(global-set-key (kbd "M-s") 'milkode:search-at-point)

;;--------------------------------------------------------------------------
;; anything-milkode
;;--------------------------------------------------------------------------
;; (require 'anything-milkode)
;; (setq anything-grep-multiline nil)                                            ; Use anything-grep single line mode

;; ;; Shortcut setting
;; (global-set-key (kbd "M-g")     'anything-milkode)                                
;; (global-set-key (kbd "C-x a f") 'anything-milkode-files)

;; ;; For popwin
;; (push '("*milkode*"                :height 20) popwin:special-display-config)
;; (push '("*anything milkode*"       :height 20) popwin:special-display-config) 
;; (push '("*anything milkode files*" :height 20) popwin:special-display-config)

;;--------------------------------------------------------------------------
;; anzu
;;-------------------------------------------------------------------------
(global-set-key (kbd "C-q") 'anzu-query-replace-or)
(global-set-key (kbd "M-q") 'anzu-query-replace-regexp)

;;--------------------------------------------------------------------------
;; yank-other
;;--------------------------------------------------------------------------
(defface yank-other-highlight-face '((t (:background "#CCCC33" :underline t)))
  "Face for yank-other-point." :group 'yani-other)

(defvar yank-other-overlay nil)

(defun yank-other-highlight-start ()
  (let ((args (list (point) (1+ (point)) nil)))
    (if (not yank-other-overlay)
        (setq yank-other-overlay (apply 'make-overlay args))
      (apply 'move-overlay yank-other-overlay args))
    (overlay-put yank-other-overlay 'face 'yank-other-highlight-face)))

(defun yank-other-highlight-end ()
  (when yank-other-overlay
    (delete-overlay yank-other-overlay)
    (setq yank-other-overlay nil)))

(defun yank-other-init ()
  "Init yank-other-point"
  (interactive)
  (window-configuration-to-register 121) ; C-x r w y
  (yank-other-highlight-start)
  (message "Set yank-other-point"))

(defun yank-other ()
  "Back to yank-other-point & yank"
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning) (region-end)))
  (jump-to-register 121) ; C-x r j y
  (yank)
  (yank-other-init)
  (yank-other-highlight-end))

;; Key binding
(global-set-key (kbd "C-M-SPC") `yank-other-init)
(global-set-key (kbd "C-M-y")   `yank-other)

;; with ace-jump-mode
;; (add-hook 'ace-jump-mode-before-jump-hook
;;           (lambda ()
;;             (yank-other-init)))

;;--------------------------------------------------------------------------
;; duplicate-thing
;;   カーソル行を複製する、範囲選択時は範囲を複製
;;--------------------------------------------------------------------------
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing) ; 元のキーはcapitalize-word

;;--------------------------------------------------------------------------
;; open-junk-file
;;--------------------------------------------------------------------------
(setq open-junk-file-format "~/Documents/junk/%Y-%m%d-%H%M%S.")

;;--------------------------------------------------------------------------
;; auto-shell-command
;;--------------------------------------------------------------------------
;; (load-file "~/Documents/auto-shell-command/auto-shell-command.el")
;; (require 'auto-shell-command)

;; キーバインドの設定
(global-set-key (kbd "C-c C-m") 'ascmd:toggle)      ; Temporarily on/off auto-shell-command run
(global-set-key (kbd "C-c C-,") 'ascmd:popup)  ; Pop up '*Auto Shell Command*'
(global-set-key (kbd "C-c C-.") 'ascmd:exec)   ; Exec-command specify file name

;; 結果の通知をGrowlで行う
(when platform-darwin-p
  (defun ascmd:notify (msg) (deferred:process-shell (format "growlnotify -m %s -t emacs" msg))))

;; エラー時のポップアップを見やすくする。 ※ (require 'popwin) が必要です。
(push '("*Auto Shell Command*" :height 20 :noselect t) popwin:special-display-config)

;; コマンドリストの設定 (下が優先高)
(ascmd:add '("Documents/milkode/test/.*\.rb$" "ruby -I../lib -I../test ./rake_test_loader.rb $FILE"))
(ascmd:add '("Documents/milkode/test/runner.rb" "rake test"))
(ascmd:add '("Documents/milkode/lib/milkode/cdstk/package.rb" "(cd ~/Documents/milkode/test/ && ruby -I../lib -I../test ./test_package.rb)"))

(ascmd:add '("Documents/rubykokuban-osx/.*\.cpp" "cd ~/Documents/rubykokuban-osx/apps/RubyKokubanWorkspace/RubyKokuban && xcodebuild -project RubyKokuban.xcodeproj -configuration Release build"))
(ascmd:add '("Documents/rubykokuban-gem/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/qiita_mail/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/ltsvr/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/mygithub/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/gren/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/grn_mini/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/honyomi/test/.*\.rb" "ruby -I../lib -I../test $FILE"))
(ascmd:add '("Documents/mrubin/test/.*\.rb" "ruby -I../lib -I../test $FILE"))

(ascmd:add '("Resources/" "wget -O /dev/null http://0.0.0.0:9090/run"))
;;(ascmd:add '("junk/.*\.rb" "ruby $FILE"))

;;--------------------------------------------------------------------------
;; 捨てコマンド
;;--------------------------------------------------------------------------
(defun run-kokuban ()
  (interactive)
  (shell-command "open ~/Documents/rubykokuban-osx/apps/RubyKokubanWorkspace/RubyKokuban/bin/RubyKokuban.app --new --args /Users/ongaeshi/Documents/rubykokuban-osx/apps/RubyKokubanWorkspace/RubyKokuban/src/sample.rb"))

;;--------------------------------------------------------------------------
;;url-retrieve-synchronously
;;--------------------------------------------------------------------------
;(setq url-proxy-services
;      '(("http"     . "proxy.co.jp:8080")
;        ("no_proxy" . "proxy.co.jp\\|proxy2.co.jp")))


;;--------------------------------------------------------------------------
;; popwin:close-popup-window-timer: error: (error Variable binding depth exceeds max-specpdl-size)
;; https://github.com/emacs-jp/issues/issues/2
;;--------------------------------------------------------------------------
;; (setq popwin:close-popup-window-timer-interval 0.05)
;; (setq popwin:close-popup-window-timer-interval 0.1)
(setq popwin:close-popup-window-timer-interval 1.0)

;;--------------------------------------------------------------------------
;; po-mode
;;--------------------------------------------------------------------------
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))
(put 'upcase-region 'disabled nil)

;;--------------------------------------------------------------------------
;; toggle-letter-case
;;--------------------------------------------------------------------------
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]")            (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]")            (put this-command 'state "all caps") )
         (t                                     (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ))

(global-set-key (kbd "M-t") 'toggle-letter-case)

;;--------------------------------------------------------------------------
;; tab-width
;;--------------------------------------------------------------------------
(setq default-tab-width 4)

;;--------------------------------------------------------------------------
;; direx
;;--------------------------------------------------------------------------
;; ;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
;; ;; (push '(direx:direx-mode :position left :width 25 :dedicated t) popwin:special-display-config)
;; (push '(direx:direx-mode :position left :width 30 :dedicated t) popwin:special-display-config)

;;--------------------------------------------------------------------------
;; scss-mode
;;--------------------------------------------------------------------------
(add-hook 'scss-mode-hook
  '(lambda ()
     (and
      (set (make-local-variable 'css-indent-offset) 2)      ; インデント2
      (set (make-local-variable 'scss-compile-at-save) nil) ; セーブ時にコンパイルしない
      )))

;;--------------------------------------------------------------------------
;; window-resizer
;;--------------------------------------------------------------------------
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key "\C-c\C-r" 'window-resizer)

;;--------------------------------------------------------------------------
;; theme-setting
;;--------------------------------------------------------------------------
;; (load-theme 'misterioso)
;; (set-cursor-color "White")

;;--------------------------------------------------------------------------
;; windmove
;;--------------------------------------------------------------------------
(windmove-default-keybindings) ; 引数なしの場合は Shift
;; Alt + 矢印でウィンドウを移動する
;; (windmove-default-keybindings 'meta) ; Alt の場合は meta を指定
;; Mac の Command + 矢印でウィンドウを移動する
;; (windmove-default-keybindings 'super) ; Macの人はこちらをオススメ

;;--------------------------------------------------------------------------
;; magit
;;--------------------------------------------------------------------------
(global-set-key (kbd "C-c m") 'magit-status)

(add-hook 'magit-mode-hook
          '(lambda()
             (define-key magit-mode-map (kbd "M-1") nil)
             (define-key magit-mode-map (kbd "M-2") nil)
             (define-key magit-mode-map (kbd "M-3") nil)
             (define-key magit-mode-map (kbd "M-4") nil)
          ))

;;--------------------------------------------------------------------------
;; objc-mode
;;--------------------------------------------------------------------------
;; (add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
