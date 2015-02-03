;;; package --- Summary

;;; Commentary:

;;; Code:

(prelude-require-package 'evil-leader)
(eval-after-load 'evil-leader
  (evil-leader/set-key-for-mode 'web-mode
    ;; General
    "cf" 'web-mode-toggle-comments
    "cm" 'web-mode-mark-and-expand
    "cdd" 'web-mode-dom-errors-show))


;;;dj-web.el ends here
