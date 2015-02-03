;;; package -- Summary

;;; Commentary:


;;; Code:

(eval-after-load 'org
  (progn
    (setq org-adapt-indentation nil)
    (setq org-src-fontify-natively t)
    (setq org-startup-indented t)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-use-speed-commands t)))

;; solve the conflict with helm-describe-key
;; It's originally binded to 'org-table-info
(define-key org-mode-map (kbd "C-c ?") nil)
(evil-declare-key 'normal org-mode-map
  "gk" 'outline-previous-visible-heading
  "gh" 'outline-up-heading)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((c          . t)
;;    (css        . t)
;;    (dot        . t)
;;    (emacs-lisp . t)
;;    (js         . t)
;;    (perl       . t)
;;    (python     . t)
;;    (ruby       . t)
;;    (sh         . t)))

(defun dj-org-mode-defaults ()
  "DJ's hook for org-mode"
  (turn-on-auto-fill)
  (turn-on-org-cdlatex))

(require 'dj-funcs)
(defun org-text-bold ()
  (interactive)
  (surround-text "*"))

(defun org-text-italics ()
  (interactive)
  (surround-text "/"))

(defun org-text-code ()
  (interactive)
  (surround-text "="))

(setq org-format-latex-options (plist-put org-format-latex-options
                                          :scale 1.2))
(setq dj-org-mode-hook 'dj-org-mode-defaults)

(add-hook 'org-mode-hook
          (lambda ()
            (run-hooks 'dj-org-mode-hook)
            (local-set-key (kbd "A-b") 'org-text-bold)
            (local-set-key (kbd "s-b") 'org-text-bold)    ;; For Linux
            (local-set-key (kbd "A-i") 'org-text-italics)
            (local-set-key (kbd "s-i") 'org-text-italics)
            (local-set-key (kbd "A-=") 'org-text-code)
            (local-set-key (kbd "s-=") 'org-text-code)))

;; Exporting the source code in black background in default
;; (defun my/org-inline-css-hook (exporter)
;;   "Insert custom inline css to automatically set the background
;; of code to whatever theme I'm using's background"
;;   (when (eq exporter 'html)
;;     (let* ((my-pre-bg (face-background 'default))
;;            (my-pre-fg (face-foreground 'default)))
;;       (setq org-html-head-extra
;;             (concat org-html-head-extra
;;                     (format "<style type=\"text/css\">\n pre.src
;;                             {background-color: %s; color:
;;                             %s;}</style>\n" my-pre-bg my-pre-fg))))))

;; (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

;;; init-org-mode.el ends here
