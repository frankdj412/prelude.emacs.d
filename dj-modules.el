;;; package -- Summary

;;; Commentary:
;; Selectively include packages according to the system

;;; Code:
(add-to-list 'load-path "~/.emacs.d/personal/dj-modules/")

;;(require 'dj-cli)
(require 'dj-gui)
(require 'dj-osx)
(setq gdb-many-windows 't)
;;; dj-modules.el ends here
