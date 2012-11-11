(require 'thingatpt)

(defface feng-highlight-at-point-face
  `((((class color) (background light))
     (:background "light green"))
    (((class color) (background dark))
     (:background "royal blue")))
  "Face for highlighting variables"
  :group 'feng-highlight)

(defvar current-highlighted nil)
(make-variable-buffer-local 'current-highlighted)

(defun feng-at-point-prev ()
  (interactive)
  (when current-highlighted
    (unless (re-search-backward
             (concat "\\<" (regexp-quote current-highlighted) "\\>") nil t)
      (message "search hit TOP, continue from BOTTOM")
      (goto-char (point-max))
      (feng-at-point-prev))))

(defun feng-at-point-next ()
  (interactive)
  (when current-highlighted
    (forward-char (+ 1 (length current-highlighted))) ; more out of current
    (if (re-search-forward
         (concat "\\<" (regexp-quote current-highlighted) "\\>") nil t)
        (backward-char (length current-highlighted))
      (backward-char (+ 1 (length current-highlighted)))
      (message "search hit BOTTOM, continue from TOP")
      (goto-char (point-min))
      (feng-at-point-next))))

(defun feng-at-point-replace ()
  (interactive)
  (when current-highlighted
    (save-excursion
      (goto-char (point-min))           ;; back to top
      (feng-at-point-next)              ;; find first
      (setq isearch-string current-highlighted)
      (isearch-query-replace))))

(defvar feng-highlight-at-point-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'feng-at-point-next)
    (define-key map (kbd "M-r") 'feng-at-point-replace)
    (define-key map (kbd "M-p") 'feng-at-point-prev)
    map))

(defun feng-highlight-at-point ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'feng-highlight t)
  (let* ((target-symbol (symbol-at-point))
         (target (symbol-name target-symbol)))
    (when target-symbol
      (setq current-highlighted target)
      (save-excursion
        (goto-char (point-min))
        (let* ((regexp (concat "\\<" (regexp-quote target) "\\>"))
               (len (length target))
               (end (re-search-forward regexp nil t)))
          (while end
            (let ((ovl (make-overlay (- end len) end)))
              (overlay-put ovl 'keymap feng-highlight-at-point-keymap)
              (overlay-put ovl 'face 'feng-highlight-at-point-face)
              (overlay-put ovl 'feng-highlight t))
            (setq end (re-search-forward regexp nil t))))))))

(provide 'feng-highlight)
