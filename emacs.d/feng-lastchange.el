;;; record two different file's last change
(defvar feng-last-change-pos1 nil)

(defvar feng-last-change-pos2 nil)

(defun feng-swap-last-changes ()
  (when feng-last-change-pos2
    (let ((tmp feng-last-change-pos2))
      (setf feng-last-change-pos2 feng-last-change-pos1
            feng-last-change-pos1 tmp))))

(defun feng-goto-last-change ()
  (interactive)
  (when feng-last-change-pos1
    (let* ((file (car feng-last-change-pos1))
           (buffer (find-file-noselect file))
           (win (get-buffer-window buffer)))
      (if win
          (select-window win)
        (if (not (equal file (buffer-file-name)))
            (switch-to-buffer-other-window buffer)))
      (goto-char (cdr feng-last-change-pos1))
      (feng-swap-last-changes))))

(defun feng-buffer-change-hook (beg end len)
  (let ((bfn (buffer-file-name))
        (file (car feng-last-change-pos1)))
    (when bfn
      (if (or (not file) (equal bfn file)) ;; change the same file
          (setq feng-last-change-pos1 (cons bfn end))
        (progn (setq feng-last-change-pos2 (cons bfn end))
               (feng-swap-last-changes))))))

(provide 'feng-lastchange)
