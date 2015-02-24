;;; package --- Summary

;;; Commentary:

;;; Code:
(prelude-require-package 'multi-term)
(require 'multi-term)

(setq multi-term-program "/bin/zsh")
(setq multi-term-buffer-name "zshell")
(setq multi-term-scroll-to-bottom-on-output 't)

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive
  (term-send-raw-string "\t")))

(add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))

(evil-define-key 'insert term-raw-map
  "\C-w" 'term-send-backward-kill-word)

(evil-leader/set-key
  "ast" 'multi-term
  "asn" 'multi-term-next)

(global-set-key (kbd "s-t") 'multi-term)
(global-set-key (kbd "s-}") 'multi-term-next)
(global-set-key (kbd "s-{") 'multi-term-prev)


;;; init-util.el ends here
