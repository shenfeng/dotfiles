(require 'feng-defuns)

(defun feng-elisp-mode-hook ()
  (setq mode-name "el")
  (paredit-mode t)
  (define-key emacs-lisp-mode-map [f9] 'edebug-defun)
  (define-key emacs-lisp-mode-map (kbd "C-M-q") 'session-jump-to-last-change))

(defun feng-clj-mode-hook ()
  (paredit-mode t)
  (make-local-variable 'ac-ignores)
  (setq mode-name "clj")
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

(defun feng-css-mode-hook ()
  (make-local-variable 'ac-ignores)
  (define-key css-mode-map (kbd "M-<up>") 'move-text-up)
  (define-key css-mode-map (kbd "M-<down>") 'move-text-down)
  (setq ac-ignores '("bg" "bgc" "ff" "fl" "fr" "fs" "fw" "lh" "pa" "pr" "ta"
                     "td" "va" "hi"))
  (yas-minor-mode)
  (font-lock-add-keywords
   nil '(("\$[^\s:;]+" . font-lock-constant-face)
         ("//.*$" . font-lock-comment-face)
         ("#[abcdef[:digit:]]\\{6\\}"
          (0 (put-text-property
              (match-beginning 0)
              (match-end 0)
              'face (list :background (match-string-no-properties 0))))))))

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "M-<up>") 'beginning-of-defun)
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
          (define-key magit-mode-map (kbd "M-4") nil)))

(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook 'feng-elisp-mode-hook)
(add-hook 'write-file-hooks 'delete-trailing-whitespace t)
(add-hook 'clojure-mode-hook 'feng-clj-mode-hook)

(provide 'feng-hooks)
