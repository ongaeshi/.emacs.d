;;; color-setting.el
;;
;; (require 'color-setting)

; �t�F�C�X�J�X�^�}�C�Y�̂��߂ɐ�s���ēǂݍ��ޕK�v���������
(require 'dired)
(require 'compile)

;;; font-lock-mode��L����
(global-font-lock-mode t)

;; �w�i�F
(set-background-color "#ffffff")

;; ��ʒ[�̉��̐F
(set-face-background 'fringe "#d0d0d0")

;; ���[�h���C��(�A�N�e�B�u�Ȏ�)
(set-face-attribute 'mode-line nil :foreground "#ffffff" :background "#026AFE")

;; ���[�h���C��(�A�N�e�B�u����Ȃ���)
(set-face-attribute 'mode-line-inactive nil :foreground "#000000" :background "#CCC7BA")

;; ���[�h���C��id(�o�b�t�@���̕���)
(set-face-attribute 'mode-line-buffer-id nil :bold t)

;; �����̊�{�F
(set-foreground-color "#000000")

;; �R�����g
(set-face-foreground 'font-lock-comment-face "#008000")

;; ������
(set-face-foreground 'font-lock-string-face "#A31515")

;; �L�[���[�h(C��for��if�̂悤�ɍ\���I�ɏd�v�Ȗ��O�Ɏg����)
(set-face-foreground 'font-lock-keyword-face "#1518FF")

;; �v���v���Z�b�T(#if, #endif)�̖��O�Ɏg����
(set-face-foreground 'font-lock-preprocessor-face "#1518FF")

;; �g�ݍ��݊֐��̖��O�Ɏg����
(set-face-foreground 'font-lock-builtin-face "#1518FF")

;; �֐���(�֐���`�^�錾���ɂ����āA��`�^�錾����Ă���֐��̖��O�Ɏg����)
(set-face-foreground 'font-lock-function-name-face "#008080")

;; �ϐ���(�ϐ���`�^�錾���ɂ����āA��`�^�錾����Ă���ϐ��̖��O�Ɏg����)
(set-face-foreground 'font-lock-variable-name-face "#008080")

;; �v����
(set-face-foreground 'font-lock-doc-face nil)

;; �n�C���C�g(���ݍs�̕\����)
(set-face-attribute 'highlight      nil :foreground "#000000" :background "#e7f2fe")

;; isearch�̌����Ώ�
(set-face-attribute 'isearch        nil :foreground "#000000" :background "#b5d5ff")

;; �������Ƃ����n�C���C�g(isearch��ⓙ)
(set-face-attribute 'lazy-highlight        nil :foreground "#000000" :background "#b5d5ff")

;; �I��̈�(transient-mark-mode��L���ɂ��Ȃ���ΈӖ�������)
(set-face-attribute 'region         nil :foreground "#000000" :background "#b5d5ff")

;; �Ή����銇�ʂ������\��(�}�b�`����)
(set-face-attribute 'show-paren-match    nil :foreground "#000000" :background "#CDCDFF" :bold t)

;; �Ή����銇�ʂ��A�����\��(��}�b�`����)
(set-face-attribute 'show-paren-mismatch nil :foreground "#4a1616" :background "#fa8989" :bold t)

;; dired���[�h�ɂ�����f�B���N�g��
(set-face-attribute 'dired-directory nil :foreground "#000000" :background "#FEDF7B" :bold t)

(provide 'color-setting)
;;; color-setting.el ends here.



