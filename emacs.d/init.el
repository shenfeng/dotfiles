(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(ignore-errors                          ; emacs --daemon error
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Consolas" :height 112)))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta)
  ;; macbook air is tiny
  (set-face-attribute 'default nil :font "Monaco" :height 132))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "vendor"))

(require 'feng-elpa)
(require 'feng-defuns)
(require 'feng-hooks)
(require 'uniquify)
(require 'recentf)
(require 'magit)
(require 'yasnippet)
(require 'auto-complete-config)
(require 'feng-anything)
(require 'autopair)
(require 'js2-mode)

(setq auto-save-default nil             ; Don't want any auto saving
      ;; echo-keystrokes 0.1
      visible-bell t                    ; Prevent noise when C-g is hit
      inhibit-startup-message t
      kill-whole-line 1                 ; C-k kill whole line
      make-backup-files nil)            ; Don't want any backup files

(setq ruby-indent-level 2
      css-indent-level 4
      ack-command "s "
      ack-default-directory-function 'feng-project-root
      uniquify-buffer-name-style 'forward
      js2-auto-indent-p t
      js2-indent-on-enter-key t
      js2-enter-indents-newline t
      slime-net-coding-system 'utf-8-unix
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      yas-also-auto-indent-first-line t)

(setq-default indent-tabs-mode nil
              c-basic-offset 4
              line-spacing 1
              tab-width 2)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; (tooltip-mode -1)
  ;; (blink-cursor-mode 1)
  )

(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'dichromacy t)
;; (load-theme 'tango-dark t)

(when (> emacs-major-version 21)        ; copy from emacs starter kit
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

;; (setq x-select-enable-clipboard t)  ; this is default in emacs 24

(column-number-mode t)
(delete-selection-mode)
(hl-line-mode t)
(idle-highlight-mode t)
(recentf-mode 1)                ; Save a list of recent files visited.
(show-paren-mode t) ; Highlight matching parentheses when the point is on them

(when (require 'bar-cursor nil 'noerror)
  (bar-cursor-mode t))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-undo-tree-mode)
(global-auto-revert-mode)

(yas/load-directory (concat dotfiles-dir "snippets"))
(ac-config-default)
(setq ac-auto-show-menu 0.2
      ac-fuzzy-enable t
      ac-quick-help-height 25
      ac-menu-height 18
      ac-quick-help-delay 0.4           ;show doc quickly
      ac-use-menu-map t)

(require 'feng-bindings)
