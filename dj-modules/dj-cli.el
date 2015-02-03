;;; package -- Summary

;;; Commentary:
;; This package is for initialization of CLI (Command Line Interface) of Emacs

;;; Code:

(prelude-require-package 'color-theme-solarized)

(unless (display-graphic-p)
  (disable-theme 'zenburn)
  (load-theme 'solarized-dark t))

(provide 'dj-cli)
;;; dj-cli.el ends here
