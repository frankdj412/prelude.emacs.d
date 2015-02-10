;;; package -- Summary

;;; Commentary:
;; Remove anaconda backend from company backends.
;; Add elpy support
;; Note that this configuration has conflict with gdb command.

;;; Code:

(require 'prelude-programming)

;; Copy pasted from python-mode.el
(defun prelude-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun prelude-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun prelude-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun prelude-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (prelude-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (prelude-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (prelude-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (eldoc-mode)
  (semantic-mode 1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))
(setq prelude-python-mode-hook 'prelude-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))

;; The default is automatic, which seems to point to Rope.
;; Jedi is much more lightweight and its parsing function is more robust
(prelude-require-package 'elpy)
(setq elpy-rpc-backend "jedi")
;; (setq elpy-use-ipython t)
;; (eval-after-load 'python '(semantic-mode 1))

(elpy-enable)

(define-key python-mode-map (kbd "s-.") 'elpy-goto-definition)
(define-key python-mode-map (kbd "s-*") 'pop-tag-mark)

(evil-leader/set-key-for-mode 'python-mode-map
  "cf" 'elpy-find-file
  "cs" 'elpy-rgrep-symbol
  "cz" 'elpy-shell-switch-to-shell
  "cc" 'elpy-shell-send-region-or-buffer
  "cd" 'python-shell-send-defun)

;; TODO: Go to definition: if two frames are available, switch to it.
;;; init-python.el ends here
