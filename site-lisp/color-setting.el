;;; color-setting.el
;;
;; (require 'color-setting)

; フェイスカスタマイズのために先行して読み込む必要があるもの
(require 'dired)
(require 'compile)

;;; font-lock-modeを有効に
(global-font-lock-mode t)

;; 背景色
(set-background-color "#ffffff")

;; 画面端の縁の色
(set-face-background 'fringe "#d0d0d0")

;; モードライン(アクティブな時)
(set-face-attribute 'mode-line nil :foreground "#ffffff" :background "#026AFE")

;; モードライン(アクティブじゃない時)
(set-face-attribute 'mode-line-inactive nil :foreground "#000000" :background "#CCC7BA")

;; モードラインid(バッファ名の部分)
(set-face-attribute 'mode-line-buffer-id nil :bold t)

;; 文字の基本色
(set-foreground-color "#000000")

;; コメント
(set-face-foreground 'font-lock-comment-face "#008000")

;; 文字列
(set-face-foreground 'font-lock-string-face "#A31515")

;; キーワード(Cのforやifのように構文的に重要な名前に使われる)
(set-face-foreground 'font-lock-keyword-face "#1518FF")

;; プリプロセッサ(#if, #endif)の名前に使われる
(set-face-foreground 'font-lock-preprocessor-face "#1518FF")

;; 組み込み関数の名前に使われる
(set-face-foreground 'font-lock-builtin-face "#1518FF")

;; 関数名(関数定義／宣言内において、定義／宣言されている関数の名前に使われる)
(set-face-foreground 'font-lock-function-name-face "#008080")

;; 変数名(変数定義／宣言内において、定義／宣言されている変数の名前に使われる)
(set-face-foreground 'font-lock-variable-name-face "#008080")

;; 要調査
(set-face-foreground 'font-lock-doc-face nil)

;; ハイライト(現在行の表示等)
(set-face-attribute 'highlight      nil :foreground "#000000" :background "#e7f2fe")

;; isearchの検索対象
(set-face-attribute 'isearch        nil :foreground "#000000" :background "#b5d5ff")

;; ゆっくりとしたハイライト(isearch候補等)
(set-face-attribute 'lazy-highlight        nil :foreground "#000000" :background "#b5d5ff")

;; 選択領域(transient-mark-modeを有効にしなければ意味が無い)
(set-face-attribute 'region         nil :foreground "#000000" :background "#b5d5ff")

;; 対応する括弧を強調表示(マッチ部分)
(set-face-attribute 'show-paren-match    nil :foreground "#000000" :background "#CDCDFF" :bold t)

;; 対応する括弧を、強調表示(非マッチ部分)
(set-face-attribute 'show-paren-mismatch nil :foreground "#4a1616" :background "#fa8989" :bold t)

;; diredモードにおけるディレクトリ
(set-face-attribute 'dired-directory nil :foreground "#000000" :background "#FEDF7B" :bold t)

(provide 'color-setting)
;;; color-setting.el ends here.



