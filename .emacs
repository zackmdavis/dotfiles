(cua-mode t)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(show-paren-mode 1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(define-coding-system-alias 'UTF-8 'utf-8)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
              '("MELPA" . "http://melpa.milkbox.net/packages/" ))

(package-initialize)

(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
