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

