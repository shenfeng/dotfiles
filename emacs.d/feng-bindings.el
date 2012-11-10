(require 'feng-defuns)

(global-set-key (kbd "M-1") 'feng-select-first-window)
(global-set-key (kbd "M-2") 'feng-select-second-window)
(global-set-key (kbd "M-3") 'feng-select-third-window)
(global-set-key (kbd "M-4") 'feng-select-forth-window)
(global-set-key (kbd "M-q") 'cleanup-buffer)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key [ret] 'newline-and-indent)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-o") 'feng-anthing-for-files)
(global-set-key (kbd "M-o") 'feng-anthing-for-occur)
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f3] 'save-buffer)
(global-set-key [f12] 'smart-split)

(global-set-key (kbd "C--") 'decrement-number-decimal)
(global-set-key (kbd "C-=") 'increment-number-decimal)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "<C-M-backspace>") 'kill-to-beginning-of-line)

(define-key ac-mode-map (kbd "C-c C-n") 'auto-complete)
(define-key undo-tree-map (kbd "C-.") 'undo-tree-redo)

(eval-after-load 'gud
  '(progn
     (define-key gud-mode-map [f9] 'gud-break)
     (define-key gud-mode-map [f5] 'gud-step)    ; step into
     (define-key gud-mode-map [f6] 'gud-next)    ; step
     (define-key gud-mode-map [f7] 'gud-finish)))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c .") 'slime-edit-definition-other-window)
     (require 'clojure-test-mode)
     (define-key clojure-test-mode-map [f9] 'clojure-test-run-tests)))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
(define-key emacs-lisp-mode-map [f9] 'edebug-defun)

(eval-after-load 'go-mode
  '(define-key go-mode-map (kbd "M-q") 'gofmt))

(eval-after-load 'dired
  '(progn
     ;;; http://xahlee.org/emacs/file_management.html
     (define-key dired-mode-map [ret] 'dired-find-alternate-file)
     (define-key dired-mode-map [f2] 'dired-toggle-read-only)
     (define-key dired-mode-map (kbd "r") 'dired-do-rename)
     (define-key dired-mode-map (kbd "c") 'dired-do-copy)
     (define-key dired-mode-map (kbd ".") 'dired-next-dirline)
     (define-key dired-mode-map (kbd ",") 'dired-prev-dirline)
     (define-key dired-mode-map (kbd "M-m") 'dired-mark-files-regexp)
     (define-key dired-mode-map (kbd "^")
       (lambda () (interactive) (find-alternate-file "..")))))

(require 'paredit)
(define-key paredit-mode-map (kbd "M-<up>") 'beginning-of-defun)
(define-key paredit-mode-map (kbd "M-<down>") 'end-of-defun)
(define-key paredit-mode-map (kbd "M-q") 'nil)
;; include forward
(define-key paredit-mode-map (kbd "C-M-0") 'paredit-forward-slurp-sexp)
;; exclude forward
(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
;; include backward
(define-key paredit-mode-map (kbd "C-M-9") 'paredit-backward-slurp-sexp)
;; exclude backward
(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)

(define-key c-mode-map (kbd "C-c C-l") 'copy-line)
(define-key c-mode-map (kbd "M-q") 'cleanup-buffer)
(define-key c-mode-map [f5] 'gud-step)

(eval-after-load 'magit
  '(progn (define-key magit-mode-map (kbd "M-1") nil)
          (define-key magit-mode-map (kbd "M-2") nil)
          (define-key magit-mode-map (kbd "M-3") nil)
          (define-key magit-mode-map (kbd "M-4") nil)
          (set-face-foreground 'magit-diff-add "green3")
          (set-face-foreground 'magit-diff-del "red3")))

(eval-after-load 'html-mode
  '(progn
     (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
     (define-key html-mode-map (kbd "M-r") 'rename-sgml-tag)
     (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)))

(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$" . mustache-mode))

(provide 'feng-bindings)
