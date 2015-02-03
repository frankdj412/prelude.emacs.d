(prelude-require-package 'yasnippet)

;; Add snippet path
;; Warning:
;; No need to load snippet again
;; (add-to-list 'load-path "~/.emacs.d/snippets")

(require 'yasnippet)
(yas/global-mode t)

;; Reload all the snippets at startup
(yas/reload-all)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Use Helm to select the snippet
(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (if (require 'helm-config)
        (let (tmpsource cands result rmap)
          (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
          (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
          (setq tmpsource
                (list
                 (cons 'name prompt)
                 (cons 'candidates cands)
                 '(action . (("Expand" . (lambda (selection) selection))))
                 ))
          (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
          (if (null result)
              (signal 'quit "user quit!")
            (cdr (assoc result rmap))))
      nil))

;; (setq yas/prompt-functions nil)
(add-hook 'yas/prompt-functions 'shk-yas/helm-prompt)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-yasnippet))

(global-set-key (kbd "<backtab>") 'company-yasnippet)
;; (add-to-list 'company-backends '(company-clang company-yasnippet))
