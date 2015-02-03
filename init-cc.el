;;; package -- Summary

;;; Commentary:
;;; For ease of c++ programming
;;
;;; Code

(defun cc/get-file-name-with-ext (name)
  (car (last (split-string name "/"))))

(defun cc/get-file-name-without-ext (name)
  (car (split-string (cc/get-file-name-with-ext name)"\\.")))

(defun cc/build-current-file ()
  "Runs the compilation of the current file.
Assumes it has the same name, but without an extension"
  (interactive)
  (compile (concat "make -k " (cc/get-file-name-without-ext buffer-file-name))))

(defun cc/run-executable ()
  (interactive)
  (shell-command (concat "./" (cc/get-file-name-without-ext buffer-file-name))))

(define-key c-mode-base-map (kbd "<f8>") 'cc/build-current-file)
(define-key c-mode-base-map (kbd "<f9>") 'cc/run-executable)

;;; init-cc-mode.el ends here
