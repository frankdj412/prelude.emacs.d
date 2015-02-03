;;; package -- Summary

;;; Commentary:
;; This package is for initialization of CLI (Command Line Interface) of Emacs

;;; Code:

(prelude-require-packages '(powerline
                            powerline-evil))

;; The codes are copied from spacemacs configuration
(defun dj/set-font (font size &optional options)
  (let* ((fontstr (if options
                      (format "%s-%s:%s" font size options)
                    (format "%s-%s" font size))))
    (add-to-list 'default-frame-alist (cons 'font fontstr))
    (set-frame-font fontstr)))

(dj/set-font "Source Code Pro for Powerline" 13)

(setq-default line-spacing 2)
(set-face-attribute 'region nil :foreground "white" :background "#Aa7941")

(add-to-list 'default-frame-alist '(width . 86))
(add-to-list 'default-frame-alist '(height . 36))

(set-fringe-mode 8)
(setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

(require 'powerline)
(require 'powerline-evil)
;; (powerline-evil-vim-color-theme)
(powerline-center-evil-theme)

(provide 'dj-gui)
;;; dj-gui.el ends here
