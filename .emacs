;; general

;;; CUA
(cua-mode t)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(show-paren-mode 1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq initial-scratch-message "; scratch!\n\n")
(setq initial-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(define-coding-system-alias 'UTF-8 'utf-8)
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")


;; custom bindings
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-r") 'revert-buffer)

;;; window navigation
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)


;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; undisabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; commands
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; mode management

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(defun my-web-mode-hook ()
  (set-face-attribute 'web-mode-html-tag-face nil
                      :foreground "blue")
  (set-face-attribute 'web-mode-html-attr-name-face nil
                      :foreground "#a0522d")
  (set-face-attribute 'web-mode-html-attr-value-face nil
                      :foreground "#8b2252"))
(add-hook 'web-mode-hook 'my-web-mode-hook)


;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
              '("MELPA" . "http://melpa.milkbox.net/packages/" ))
(package-initialize)
