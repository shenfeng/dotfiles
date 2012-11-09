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

(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$" . mustache-mode))

(provide 'feng-bindings)
