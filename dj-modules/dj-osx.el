;;; package --- Summary

;;; Commentary:

;;; Code:

(when (equal system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

(provide 'dj-osx)

;;; dj-osx.el ends here
