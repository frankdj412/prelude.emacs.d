;;; package --- Summary

;;; Commentary:
;; My custom setting for emacs.

;;; Code:
(prelude-require-package 'names)
(prelude-require-package 'multi-term)

(require 'names-dev)
(require 'multi-term)

(define-namespace dj/

;; Open cheat sheet in evernote
(defun open-emacs-cheatsheet (arg)
  (interactive "P")
  (let* ((filename "https://www.evernote.com/shard/s257/nl/34264237/6f524890-22a8-4439-9598-778f86a1d180/")
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (start-process "open-emacs-cheatsheet" nil program filename))))


;; Zshell setting
(setq multi-term-program "/bin/zsh")

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive
  (term-send-raw-string "\t")))
(add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))

(evil-leader/set-key
  "ast" 'multi-term)
(global-set-key (kbd "C-c m") 'multi-term)

;;; init-util.el ends here
