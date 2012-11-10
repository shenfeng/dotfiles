(require 'package)

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(let ((packages '(anything anything-config anything-match-plugin auto-complete
                           clojure-test-mode css-mode yasnippet ack
                           go-mode ;; ac-nrepl nrepl
                           ;; idle-highlight-mode
                           inf-ruby lua-mode magit ac-slime
                           markdown-mode mustache-mode paredit autopair
                           python-mode rainbow-delimiters zenburn-theme
                           ruby-mode swank-clojure undo-tree zencoding-mode)))
  (when (remove-if-not (lambda (p)
                         (not (package-installed-p p))) packages)
    (package-refresh-contents)
    (dolist (package packages)
      (unless (package-installed-p package)
        (message "Installing %s" (symbol-name package))
        (package-install package)))))

(provide 'feng-elpa)
