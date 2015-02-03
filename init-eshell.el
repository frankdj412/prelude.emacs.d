;;; Commentary:
;; 1. Ignore Capital case of the file
;; 2. Shrink the path length
;; 3. Clear the screen.

;;; Code:

;; eshell ignore cases
(setq eshell-cmpl-ignore-case t)

;; eshell shorten path name
(setq eshell-prompt-function
      (lambda ()
        (propertize
         (concat (if (getenv "HOSTNAME") (concat (getenv "HOSTNAME") ":"))
                 (if (string= (eshell/pwd) (getenv "HOME"))
                     "~"
                   (car (last (split-string (eshell/pwd) "[/]"))))
                 " $ ")
         'face
         `(:foreground "yellowgreen"))))
;; (setq eshell-highlight-prompt nil)


;; eshell clear screen
(defun eshell/clear ()
  "Clear the eshell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;; eshell autojump
;; stole from: http://www.emacswiki.org/emacs/eshell-autojump.el
(defun eshell/j (&rest args)
  "Jump to a directory you often cd to.
This compares the argument with the list of directories you usually jump to.
Without an argument, list the ten most common directories.
With a positive integer argument, list the n most common directories.
Otherwise, call `eshell/cd' with the result."
  (setq args (eshell-flatten-list args))
  (let ((arg (or (car args) 50))
        (map (make-hash-table :test 'equal))
        (case-fold-search (eshell-under-windows-p))
        candidates
        result)
    ;; count paths in the ring and produce a map
    (dolist (dir (ring-elements eshell-last-dir-ring))
      (if (gethash dir map)
          (puthash dir (1+ (gethash dir map)) map)
        (puthash dir 1 map)))
    ;; use the map to build a sorted list of candidates
    (maphash (lambda (key value)
               (setq candidates (cons key candidates)))
             map)
    (setq candidates (sort candidates
                           (lambda (a b)
                             (> (gethash a map)
                                (gethash b map)))))
    ;; list n candidates or jump to most popular candidate
    (if (and (integerp arg) (> arg 0))
        (progn
          (let ((n (nthcdr (1- arg) candidates)))
            (when n
              (setcdr n nil)))
          (eshell-lisp-command
           (mapconcat (lambda (s)
                        (format "%4d %s" (gethash s map) s))
                      candidates "\n")))
      (while (and candidates (not result))
        (if (string-match arg (car candidates))
            (setq result (car candidates))
          (setq candidates (cdr candidates))))
      (eshell/cd result))))


(setq eshell-lasts-dir-rint-size 500)
;; Emacs use .bashrc file
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))
