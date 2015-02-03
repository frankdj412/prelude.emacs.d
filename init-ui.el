;;; package --- Summary

;;; Commentary:

;;; Code:

(prelude-require-packages '(face-remap+
                            zoom-frm))

(require 'zoom-frm)
(global-set-key (kbd "C-+") 'zoom-in)
(global-set-key (kbd "C--") 'zoom-out)

(scroll-bar-mode -1)

;; Smooth Scrolling effect
;; Copied from http://stackoverflow.com/questions/445873/how-can-i-make-emacs-mouse-scrolling-slower-and-smoother
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(custom-set-faces
 '(font-lock-builtin-face ((t (:inherit font-lock-type-face :foreground nil))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse4"))))
 ;; '(org-level-1 ((t (:height 1))))
 ;; '(org-level-2 ((t (:height 1))))
 ;; '(org-level-3 ((t (:height 1))))
 ;; '(org-level-4 ((t (:height 1))))
 '(org-quote ((t (:foreground "SkyBlue3"))) t))
 ;; '(org-code ((t (:foreground "#b58900")))))

;;; init-gui.el ends here
