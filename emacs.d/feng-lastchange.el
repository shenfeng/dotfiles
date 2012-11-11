;; ;;; record two different file's last change
;; (defvar feng-last-change-pos1 nil)

;; (defvar feng-last-change-pos2 nil)

;; (defun feng-swap-last-changes ()
;;   (when feng-last-change-pos2
;;     (let ((tmp feng-last-change-pos2))
;;       (setf feng-last-change-pos2 feng-last-change-pos1
;;             feng-last-change-pos1 tmp))))

;; (defun feng-goto-last-change ()
;;   (interactive)
;;   (when feng-last-change-pos1
;;     (let* ((file (car feng-last-change-pos1)))
;;       (if (eq (buffer-file-name) file)
;;           (goto-char (cdr feng-last-change-pos1))
;;         (let* ((buffer (find-file-noselect file))
;;                (win (get-buffer-window buffer)))
;;           (when win
;;             (select-window win)
;;             (switch-to-buffer-other-window buffer)
;;             (goto-char (cdr feng-last-change-pos1)))))
;;       (feng-swap-last-changes))))

;; (defun feng-buffer-change-hook (beg end len)
;;   (let ((bfn (buffer-file-name))
;;         (file (car feng-last-change-pos1)))
;;     (when bfn
;;       (if (or (not file) (equal bfn file)) ;; change the same file
;;           (setq feng-last-change-pos1 (cons bfn end))
;;         (setq feng-last-change-pos2 (cons bfn end))
;;         (feng-swap-last-changes)))))



;; (defvar feng-last-change-pos nil)

;; (defun feng-swap-last-changes ()
;;   (when feng-last-change-pos2
;;     (let ((tmp feng-last-change-pos2))
;;       (setf feng-last-change-pos2 feng-last-change-pos1
;;             feng-last-change-pos1 tmp))))

;; (defun feng-goto-last-change ()
;;   (interactive)
;;   (if feng-last-change-pos
;;       (let* ((file (car feng-last-change-pos)))
;;         (if (eq (buffer-file-name) file)
;;             (goto-char (cdr feng-last-change-pos))
;;           (let* ((buffer (find-file-noselect file))
;;                  (win (get-buffer-window buffer)))
;;             (when win
;;               (select-window win)
;;               (switch-to-buffer-other-window buffer)
;;               (goto-char (cdr feng-last-change-pos)))))
;;         (message "goto last change"))
;;     (message "no change")))

(defvar feng-last-change-pos nil)

(defun feng-buffer-change-hook (beg end len)
  (when (and (> end beg) (buffer-file-name))
    (setq feng-last-change-pos (cons (buffer-file-name) end))))

(defun feng-goto-last-change ()
  (interactive)
  (if feng-last-change-pos
      (let ((file (car feng-last-change-pos)))
        (unless (eq (buffer-file-name) file)
          (let* ((buffer (find-file-noselect file))
                 (win (get-buffer-window buffer)))
            (if win
                (select-window win)
              (find-file file))))
        (goto-char (cdr feng-last-change-pos))
        (message "Last Edit Location"))
    (message "No Change")))

(provide 'feng-lastchange)
