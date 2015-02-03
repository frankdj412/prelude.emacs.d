;;; package --- Summary

;;; Commentary:
;; The commands related to the editing.

;;; Code:

(prelude-require-packages '(discover
                            evil-leader
                            evil-matchit
                            evil-nerd-commenter
                            ido-vertical-mode
                            use-package))

(require 'use-package)
(require 'dj-funcs)
(require 'ido-vertical-mode)

(global-discover-mode)
(global-evil-leader-mode)
(global-evil-matchit-mode)
(ido-vertical-mode 1)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  ":" 'helm-M-x)

;;; Copied from spacemacs config: https://github.com/syl20bnr/spacemacs/blob/0e4dc90d5da35c419efb45acb5c46f1560446aa8/spacemacs/keybindings.el
;; Universal argument ---------------------------------------------------------
(evil-leader/set-key "u" 'universal-argument)

;; shell command  -------------------------------------------------------------
(evil-leader/set-key "!" 'shell-command)

;; ace-jump-mode --------------------------------------------------------------
(evil-leader/set-key
  "SPC" 'evil-ace-jump-word-mode
  "l" 'evil-ace-jump-line-mode)

;; applications ---------------------------------------------------------------
(evil-leader/set-key
  "ac"  'calc-dispatch
  "ad"  'dired
  "ai"  'irc
  "ap"  'proced
  "ase" 'eshell
  "asi" 'shell
  "au"  'undo-tree-visualize)

;; buffers --------------------------------------------------------------------
(evil-leader/set-key
  "b0"  'beginning-of-buffer
  "b$"  'end-of-buffer
  "bb"  'spacemacs/alternate-buffer ;; switch back and forth between two last buffers
  "TAB" 'spacemacs/alternate-buffer
  ;; "be"  'spacemacs/safe-erase-buffer
  "bK"  'kill-other-buffers
  "bk"  'ido-kill-buffer
  "b C-k" 'kill-matching-buffers-rudely
  "bn"  'switch-to-next-buffer
  "bp"  'switch-to-prev-buffer
  ;; "bR"  'spacemacs/safe-revert-buffer
  "br"  'rename-current-buffer-file
  "bw"  'toggle-read-only)

;; describe functions ---------------------------------------------------------
(evil-leader/set-key
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdp" 'describe-package
  "hdt" 'describe-theme
  "hdv" 'describe-variable)

;; errors ---------------------------------------------------------------------
(evil-leader/set-key
  "en" 'next-error
  "ep" 'previous-error)

;; find -----------------------------------------------------------------------
(evil-leader/set-key
  "ff" 'ido-find-file
  "fg" 'rgrep)

;; file -----------------------------------------------------------------------
(evil-leader/set-key
  "fd"  'delete-current-buffer-file
  "fei" 'prelude-find-user-init-file
  "fed" 'find-dotfile
  ;; "fev" 'spacemacs/display-and-copy-version
  "fj" 'dired-jump
  "fo" 'prelude-open-with
  ;; "fo" 'spacemacs/open-in-external-app
  "fS" 'evil-write-all
  "fs" 'evil-write
  "fy" 'show-and-copy-buffer-filename)

;; flycheck stuff -------------------------------------------------------------
(evil-leader/set-key
  "ec" 'flycheck-clear ;; error clear
  "ef" 'flycheck-mode
  "el" 'flycheck-list-errors
  "en" 'spacemacs/next-error
  "eN" 'spacemacs/previous-error)

;; helm stuff -----------------------------------------------------------------
(evil-leader/set-key
  "bs"  'helm-mini
  "sl"  'helm-semantic-or-imenu
  "hb"  'helm-bookmarks
  "hl"  'helm-resume
  "ry"  'helm-show-kill-ring
  "rr"  'helm-register
  "rm"  'helm-all-mark-rings
  "fh"  'helm-find-files
  "fr"  'helm-recentf
  "<f1>" 'helm-apropos
  )

;; insert stuff ---------------------------------------------------------------
(evil-leader/set-key
  "ij"  (lambda (count)
          "Insert a new line below with no identation."
          (interactive "p")
          (save-excursion
            (evil-move-end-of-line)
            (while (> count 0)
              (insert "\n")
              (setq count (1- count)))))
  "ik" 'evil-insert-line-above)

;; format ---------------------------------------------------------------------
;; <SPC> j k key binding for a frequent action: go and indent line below the point
;; <SPC> J split the current line at point and indent it
(evil-leader/set-key
  "J"  'sp-split-sexp
  ;; "jJ" 'spacemacs/split-and-new-line
  "jj" 'sp-newline
  "jk" 'evil-goto-next-line-and-indent)

;; compilation ----------------------------------------------------------------
(evil-leader/set-key "cc" 'helm-make-projectile)
(evil-leader/set-key "cC" 'compile)
(evil-leader/set-key "cr" 'recompile)

;; magit ----------------------------------------------------------------------
(evil-leader/set-key
  "mb" 'magit-blame-mode
  "mf" 'magit-file-log
  "ml" 'magit-log
  "mm" 'magit-status)

;; narrow & widen -------------------------------------------------------------
(use-package fancy-narrow
  :defer t
  :init
  (evil-leader/set-key
    "nr" 'narrow-to-region
    "np" 'narrow-to-page
    "nf" 'narrow-to-defun
    "nw" 'widen))

;; spell check  ---------------------------------------------------------------
(evil-leader/set-key
  "Sc" 'cofi/helm-flyspell-correct
  "Sn" 'flyspell-goto-next-error)

;; toggle ---------------------------------------------------------------------
(evil-leader/set-key
  "t8" 'toggle-fill-column-indicator
  "tF" 'spacemacs/toggle-frame-fullscreen
  "tf" 'fringe-mode
  "th" 'global-hl-line-mode
  "tl" 'toggle-truncate-lines
  "tL" 'visual-line-mode
  "tM" 'toggle-frame-maximized
  "tn" 'global-linum-mode
  "tt" 'toggle-transparency
  "t SPC" 'whitespace-mode)

;; window ---------------------------------------------------------------------
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(evil-leader/set-key
  "w2"  'layout-double-columns
  "w3"  'layout-triple-columns
  "wc"  'delete-window
  "wC"  'delete-other-windows
  "wd"  'toggle-current-window-dedication
  "wH"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "wM"  'toggle-maximize-centered-buffer
  "wm"  'toggle-maximize-buffer
  "wo"  'other-frame
  ;; "wr"  'spacemacs/resize-window-overlay-map
  "wR"  'rotate-windows
  ;; "wv"  'evenly-split-window-below)
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "w/"  'split-window-right
  "ww"  'other-window)

;; text -----------------------------------------------------------------------
(evil-leader/set-key
  ;; "zx="  'spacemacs/reset-font-size
  ;; "zx+"  'spacemacs/scale-up-font
  ;; "zx-"  'spacemacs/scale-down-font
  "xdw" 'delete-trailing-whitespace
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwC" 'count-words-analysis
  "xwc" 'count-words-region)

;; google translate -----------------------------------------------------------
(evil-leader/set-key
  "xgl" 'set-google-translate-languages)

;; artist mode:
;; Note: It seems that the rubber banding functionality fails
(setq artist-rubber-banding nil)

(global-unset-key [C-backspace])

(global-set-key (kbd "C-c /") 'anzu-replace-at-cursor-thing)
(global-set-key [C-backspace] 'c-hungry-delete)
(global-set-key (kbd "s->") 'ace-jump-buffer)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; evil mode
(eval-after-load 'evil
  '(progn
     (define-key evil-ex-map "e" 'helm-find-files)
     (evil-ex-define-cmd "b" 'helm-buffers-list)
     (evil-ex-define-cmd "q" 'ido-kill-buffer)

     (define-key evil-normal-state-map (kbd "C-'") 'evil-scroll-up)
     (define-key evil-visual-state-map (kbd "C-'") 'evil-scroll-up)))

;; evil Nerd Commenter:
;; (defun dj/evilnc-default-hotkeys ()
;;   "Set the hotkeys of evil-nerd-comment"
;;   (interactive)
;;   (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;;   (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
;;   (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;;   (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
;;   (eval-after-load 'evil
;;     '(progn
;;        (define-key evil-normal-state-map "gci" 'evilnc-comment-or-uncomment-lines)
;;        (define-key evil-normal-state-map "gcl" 'evilnc-comment-or-uncomment-to-the-line)
;;        (define-key evil-normal-state-map "gcc" 'evilnc-copy-and-comment-lines)
;;        (define-key evil-normal-state-map "gcp" 'evilnc-comment-or-uncomment-paragraphs)
;;        (define-key evil-normal-state-map "gcr" 'comment-or-uncomment-region))))
;; (dj/evilnc-default-hotkeys)
;; (setq evilnc-hotkey-comment-operator "<SPC>,")

(evil-leader/set-key
  "\\" 'evilnc-comment-operator)

;;; init-editor.el ends here
