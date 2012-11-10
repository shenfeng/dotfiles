(require 'feng-defuns)
(require 'ac-slime)
(require 'feng-lastchange)

(defun feng-elisp-mode-hook ()
  (setq mode-name "el")
  (paredit-mode t)
  (hl-line-mode)
  ;; (idle-highlight-mode)
  )

(defun feng-clj-mode-hook ()
  (paredit-mode t)
  ;; (idle-highlight-mode)
  (hl-line-mode)
  (make-local-variable 'ac-ignores)
  (setq mode-name "clj")
  (set-up-slime-ac)
  (let ((ns (clojure-find-package)))    ; defined in clojure-mode.el
    (when (search "-test" ns)
      (save-window-excursion
        (clojure-test-mode t))))
  (setq ac-ignores '("ns" "df" "dfp" "dt" "ns" "ss" "resp" "bp" "req"))
  (yas-minor-mode))

(defun feng-c-mode-hook ()
  (make-local-variable 'ac-ignores)
  (setq ac-ignores '("in" "for" "if" "def" "pr"))
  ;; (idle-highlight-mode t)
  (setq comment-start "// " comment-end "")
  (hl-line-mode)
  (autopair-mode)
  (yas-minor-mode))

(defun feng-repl-hook ()
  (require 'clojure-mode)
  (clojure-mode-font-lock-setup)
  (set-up-slime-ac)
  (paredit-mode t))

(defun feng-go-mode-hook ()
  (autopair-mode)
  (setq ac-ignores '("pr", "df"))
  (yas-minor-mode))

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
  (autopair-mode)
  (yas-minor-mode)
  (hl-line-mode))

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

(defun feng-python-mode-hook ()
  (make-local-variable 'ac-ignores)
  (autopair-mode)
  ;; (idle-highlight-mode t)
  (yas-minor-mode)
  (setq ac-ignores '("if" "for"))
  (setq mode-name "py"))

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

(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  ;; (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
  )

(add-hook 'emacs-lisp-mode-hook 'feng-elisp-mode-hook)
(add-hook 'write-file-hooks 'delete-trailing-whitespace t)
(add-hook 'clojure-mode-hook 'feng-clj-mode-hook)
(add-hook 'js2-mode-hook 'feng-js2-mode-hook)
(add-hook 'css-mode-hook 'feng-css-mode-hook)
(add-hook 'html-mode-hook 'feng-html-mode-hook)
(add-hook 'js2-mode-hook 'feng-js2-mode-hook)
(add-hook 'slime-repl-mode-hook 'feng-repl-hook)
(add-hook 'go-mode-hook 'feng-go-mode-hook)
(add-hook 'python-mode-hook 'feng-python-mode-hook)
(add-hook 'after-change-functions 'feng-buffer-change-hook)
(add-hook 'c-mode-hook 'feng-c-mode-hook)

(provide 'feng-hooks)
