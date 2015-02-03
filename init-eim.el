;;; package -- Summary

;;; Commentary:
;; This package is for eim (Emacs Input Method) which is a Chinese
;; input method support


;;; Code:

(add-to-list 'load-path "~/.emacs.d/personal/packages/emacs-eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
(setq eim-use-tooltip nil)

(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")


;;; init-eim.el ends here
