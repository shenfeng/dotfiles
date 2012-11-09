(require 'feng-defuns)
(require 'ac-slime)

(defun feng-elisp-mode-hook ()
  (setq mode-name "el")
  (paredit-mode t)
  (hl-line-mode)
  (idle-highlight-mode)
  (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
  (define-key emacs-lisp-mode-map [f9] 'edebug-defun))

(defun feng-clj-mode-hook ()
  (paredit-mode t)
  (idle-highlight-mode)
  (hl-line-mode)
  (make-local-variable 'ac-ignores)
  (setq mode-name "clj")
  (set-up-slime-ac)
  (let ((ns (clojure-find-package)))    ; defined in clojure-mode.el
    (when (search "-test" ns)
      (save-window-excursion
        (clojure-test-mode t)
        (define-key clojure-test-mode-map [f9] 'clojure-test-run-tests))))
  (define-key clojure-mode-map (kbd "C-M-q") 'session-jump-to-last-change)
  (define-key clojure-mode-map (kbd "C-c .")
    'slime-edit-definition-other-window)
  (setq ac-ignores '("ns" "df" "dfp" "dt" "ns" "ss" "resp" "bp" "req"))
  (yas-minor-mode))

(defun feng-repl-hook ()
  (require 'clojure-mode)
  (clojure-mode-font-lock-setup)
  (set-up-slime-ac)
  (paredit-mode t))

(defun feng-css-mode-hook ()
  (make-local-variable 'ac-ignores)
  ;; (define-key css-mode-map (kbd "M-<up>") 'move-text-up)
  ;; (define-key css-mode-map (kbd "M-<down>") 'move-text-down)
  (setq ac-ignores '("bg" "bgc" "ff" "fl" "fr" "fs" "fw" "lh" "pa" "pr" "ta"
                     "td" "va" "hi"))
  (yas-minor-mode)
  (hl-line-mode))

(defun feng-html-mode-hook ()
  (require 'rename-sgml-tag)
  (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
  (define-key html-mode-map (kbd "M-r") 'rename-sgml-tag)
  (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
  (autopair-mode)
  (yas-minor-mode)
  (hl-line-mode))

;;; http://xahlee.org/emacs/file_management.html
(defun feng-dired-mode-hook ()
  ;; was dired-advertised-find-file
  (define-key dired-mode-map [ret] 'dired-find-alternate-file)
  (define-key dired-mode-map [f2] 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "r") 'dired-do-rename)
  (define-key dired-mode-map (kbd "c") 'dired-do-copy)
  (define-key dired-mode-map (kbd ".") 'dired-next-dirline)
  (define-key dired-mode-map (kbd ",") 'dired-prev-dirline)
  (define-key dired-mode-map (kbd "M-m") 'dired-mark-files-regexp)
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file ".."))))

(defun feng-js2-mode-hook ()
  (make-local-variable 'ac-ignores)
  (setq js2-basic-offset 2)
  (hl-line-mode)
  (setq mode-name "js2")
  (setq ac-ignores '("log" "tc" "df" "fc" "el" "ei" "if" "ife" "for"))
  (yas-minor-mode)
  (autopair-mode)
  (require 'js2-highlight-vars)
  (js2-highlight-vars-mode))

(eval-after-load 'paredit
  '(progn
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
     (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)))

(eval-after-load 'magit
  '(progn (define-key magit-mode-map (kbd "M-1") nil)
          (define-key magit-mode-map (kbd "M-2") nil)
          (define-key magit-mode-map (kbd "M-3") nil)
          (define-key magit-mode-map (kbd "M-4") nil)
          ;; (set-face-foreground 'magit-diff-add "green3")
          ;; (set-face-foreground 'magit-diff-del "red3")
          ))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\>\\)"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'js2-mode
  '(font-lock-add-keywords
    'js2-mode `(("\\(function *\\)("
                 (0 (progn (compose-region (match-beginning 1)
                                           (match-end 1) "ƒ")
                           nil))))))

(eval-after-load 'css-mode
  '(font-lock-add-keywords
    'css-mode `(("\$[^\s:;]+" . font-lock-constant-face)
                ("//.*$" . font-lock-comment-face)
                ("#[abcdef[:digit:]]\\{6\\}"
                 (0 (put-text-property
                     (match-beginning 0)
                     (match-end 0)
                     'face (list :background
                                 (match-string-no-properties 0))))))))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook 'feng-elisp-mode-hook)
(add-hook 'write-file-hooks 'delete-trailing-whitespace t)
(add-hook 'clojure-mode-hook 'feng-clj-mode-hook)
(add-hook 'dired-mode-hook 'feng-dired-mode-hook)
(add-hook 'js2-mode-hook 'feng-js2-mode-hook)
(add-hook 'css-mode-hook 'feng-css-mode-hook)
(add-hook 'html-mode-hook 'feng-html-mode-hook)
(add-hook 'js2-mode-hook 'feng-js2-mode-hook)
(add-hook 'slime-repl-mode-hook 'feng-repl-hook)

(provide 'feng-hooks)
