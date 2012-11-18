(require 'thingatpt)

(defface feng-highlight-at-point-face
  `((((class color) (background light))
     (:background "light green"))
    (((class color) (background dark))
     (:background "royal blue")))
  "Face for highlighting variables"
  :group 'feng-highlight)

(defvar current-highlighted nil)
(defvar highlighted-history-length 2)
(defvar highlighted-history '())

(make-variable-buffer-local 'current-highlighted)
(make-variable-buffer-local 'highlighted-history)

(defun feng-highlight-regex (target)
  (if (and (eq major-mode 'clojure-mode) ; clojure-mode
           (eq (search ":" target) 0))
      (concat (regexp-quote target) "\\>")
    (concat "\\<" (regexp-quote target) "\\>")))

(defun feng-at-point-prev ()
  (interactive)
  (when current-highlighted
    (unless (re-search-backward
             (feng-highlight-regex current-highlighted) nil t)
      (message "search hit TOP, continue from BOTTOM")
      (goto-char (point-max))
      (feng-at-point-prev))))

(defun feng-at-point-next ()
  (interactive)
  (when current-highlighted
    (forward-char (+ 1 (length current-highlighted))) ; more out of current
    (if (re-search-forward
         (feng-highlight-regex current-highlighted) nil t)
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

(defun feng-highlight-add-to-hisotry (target)
  (let ((l (remove-if (lambda (h)
                        (string= target (second h))) highlighted-history))
        (n (list (point) target)))
    (setq highlighted-history (subseq (cons n l) 0 highlighted-history-length))))

(defun feng-highlight-at-point (&optional arg)
  (interactive "p")
  (remove-overlays (point-min) (point-max) 'feng-highlight t)
  (let* ((target-symbol (symbol-at-point))
         (target (symbol-name target-symbol)))
    (when target-symbol
      (setq current-highlighted target)
      (if (= arg 1)
          (feng-highlight-add-to-hisotry target))
      (save-excursion
        (goto-char (point-min))
        (let* ((regexp (feng-highlight-regex target))
               (len (length target))
               (end (re-search-forward regexp nil t)))
          (while end
            (let ((ovl (make-overlay (- end len) end)))
              (overlay-put ovl 'keymap feng-highlight-at-point-keymap)
              (overlay-put ovl 'face 'feng-highlight-at-point-face)
              (overlay-put ovl 'feng-highlight t))
            (setq end (re-search-forward regexp nil t))))))))

;;; cycle highlight-hisoty ring
(defun feng-highlight-history-prev ()
  (interactive)
  (if highlighted-history
      (let ((f (car highlighted-history)))
        (setq highlighted-history
              (append (remove-if-not 'identity (rest highlighted-history)) (list f)))
        (goto-char (car f))
        (feng-highlight-at-point 10))
    (message "No history yet")))

(provide 'feng-highlight)
