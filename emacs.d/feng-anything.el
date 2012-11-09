(require 'anything)
(require 'anything-match-plugin)
(require 'anything-config)

(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
(define-key anything-map (kbd "C-e") 'anything-execute-persistent-action)

(defun feng-project-root (&rest args)
  (expand-file-name
   (or (locate-dominating-file default-directory ".git")
       "~/workspace")))

(defvar feng-file-patterns
  '("*.clj" "*.html" "*.el" "*.js" "*.xml"
    "*.py" "*.jinja2"
    "*.tpl" "*.less" "*.scss" "*.sql")
  "List of patterns to look for")

(defvar feng-file-find-options
  (concat "! -regex '.*src/templates.*'"
          " ! -regex '.*bin/.*'"
          " ! -regex '.*templates/jinja.*'"))

(defun feng-join-file-patten ()
  (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
             feng-file-patterns " -or "))

(defun feng-anything-occur-init ()
  (when (get-buffer "*Anything Occor*")
    (kill-buffer "*Anything Occor*"))
  (message (concat "project root: " (feng-project-root)))
  (setq project-root (feng-project-root)))

(defun feng-anything-files-init ()
  (when (get-buffer "*Anything Files*")
    (kill-buffer "*Anything Files*"))
  (setq project-root (feng-project-root)))

(defun feng-files-in-projects ()
  (let* ((project (feng-project-root))
         (files (split-string
                 (shell-command-to-string
                  (format "find %s -type f \\( %s \\) %s"
                          project
                          (feng-join-file-patten)
                          feng-file-find-options)))))
    (mapcar (lambda (file)
              (cons (replace-regexp-in-string project "" file)
                    file)) files)))

(defvar anything-c-source-project-files
  '((name . "Files in project")
    (init . feng-anything-files-init)
    (candidates . feng-files-in-projects)
    (candidate-number-limit . 30000)
    (type . file)
    (requires-pattern . 0)))

(defface feng-grep-file-font
  '((t (:foreground "MediumSpringGreen" :underline t)))
  "Face used to highlight grep results filenames."
  :group 'anything)

(defun feng-anything-c-grep-transformer (candidates sources)
  "Filtered candidate transformer function for `anything-c-source-grep'."
  (loop for i in candidates
        for split  = (and i (anything-c-grep-split-line i))
        for fname  = (car split)
        for lineno = (nth 1 split)
        for str    = (nth 2 split)
        when (and fname lineno str)
        collect
        (cons (concat
               (propertize (file-name-nondirectory fname)
                           'face 'feng-grep-file-font
                           'help-echo fname) ":"
                           (propertize lineno 'face font-lock-function-name-face) ":"
                           (anything-c-grep-highlight-match str)) i)))

(defun feng-anything-c-tags-transformer (candidates)
  (loop for i in candidates
        for f = (first i)
        for ns  = (second i)
        for filename = (third i)
        for path = (fourth i)
        collect
        (cons (concat (propertize f 'face font-lock-variable-name-face)
                      " - " (propertize ns 'face font-lock-builtin-face)
                      " " filename)
              path)))

(defun anything-c-tags-action (candidate)
  (let* ((file (first (split-string candidate "\\$")))
         (line (second (split-string candidate "\\$"))))
    (find-file file)
    (goto-line (string-to-int line))))

(defvar anything-c-source-tags
  '((name . "Tags in project")
    (init . feng-anything-occur-init)
    (candidate-transformer feng-anything-c-tags-transformer)
    (candidates . (lambda ()
                    (let ((file (concat project-root "tags")))
                      (when (file-readable-p file)
                        (read (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))))
    (action . anything-c-tags-action)
    (requires-pattern . 0)))

;; TODO  need to run anything-do-grep before invoke it
(defvar anything-c-source-grep
  `((name . "Grep in project")
    (init . (lambda ()
              (anything-c-occur-init)
              (setq grepip-include-files
                    (mapconcat (lambda (p)
                                 (concat "--include="
                                         (shell-quote-argument p)))
                               feng-file-patterns " "))))
    (candidates
     . (lambda ()
         (let* ((anything-c-grep-default-command "grep -nirH -e %s %s %s")
                (anything-mp-highlight-delay nil)
                (anything-compile-source-functions
                 (delq 'anything-compile-source--match-plugin
                       (copy-sequence anything-compile-source-functions))))
           (funcall anything-c-grep-default-function
                    (list project-root) grepip-include-files))))
    (filtered-candidate-transformer feng-anything-c-grep-transformer)
    (candidate-number-limit . 200)
    (action . anything-c-grep-action)
    (requires-pattern . 2)
    (delayed)))

(defcustom feng-anything-for-files-list
  '(anything-c-source-buffers+
    anything-c-source-project-files
    anything-c-source-bookmarks
    anything-c-source-recentf)
  "Prefered sources to find files."
  :type 'list
  :group 'anything-config)

(defcustom feng-anything-for-occurs-list
  '(anything-c-source-imenu
    anything-c-source-occur
    anything-c-source-etags-select
    ;; anything-c-source-tags
    ;; anything-c-source-grep
    )
  "Prefered sources to doing occur."
  :type 'list
  :group 'anything-config)

(defun feng-anthing-for-occur ()
  (interactive)
  (when (eq major-mode 'python-mode)
    (setq imenu-create-index-function 'py-imenu-create-index-new)
    (imenu--make-index-alist t))
  (anything-other-buffer feng-anything-for-occurs-list
                         "*Anything Occor*"))

(defun feng-anthing-for-files ()
  (interactive)
  (anything-other-buffer feng-anything-for-files-list
                         "*Anything Files*"))

(provide 'feng-anything)
