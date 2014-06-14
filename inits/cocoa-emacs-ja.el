;; 英語フォント(Monaco)
(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 140)

;; 日本語フォント(ヒラギノ角ゴProN)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Maru Gothic ProN"))

;; Yen -> Backslash
(define-key global-map [?\¥] [?\\])
(define-key global-map [?\C-¥] [?\C-\\])
(define-key global-map [?\M-¥] [?\M-\\])
(define-key global-map [?\C-\M-¥] [?\C-\M-\\])
