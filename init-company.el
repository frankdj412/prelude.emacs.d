;;; package --- Summary

;;; Commentary:
;; Company Mode Setting

;;; Code:

(global-unset-key (kbd "C-SPC"))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map [escape] 'company-abort)
     (dotimes (i 10)
       (define-key company-active-map
         (read-kbd-macro (format "s-%d" i)) 'company-complete-number))

     ;; Using Helm Interface
     (define-key company-mode-map (kbd "C-SPC") 'helm-company)
     (define-key company-active-map (kbd "C-SPC") 'helm-company)

     (setq company-idle-delay 0.5)))

;; Key Setting
(global-set-key (kbd "s-i") 'company-complete)
(global-set-key (kbd "M-<tab>") 'company-complete-common)


;;; init-company.el ends here
