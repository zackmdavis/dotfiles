;; general

;;; CUA
(cua-mode t)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(defun backward-delete-word (multiple)
  "As opposed to backward-kill-word, which modifies the clipboard
   ('kill ring')."
  ;; from http://stackoverflow.com/a/6133921
  (interactive "p")
  (delete-region (point) (progn (backward-word multiple) (point))))
(global-set-key [C-backspace] 'backward-delete-word)

(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)
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


;; undisabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; commands

(defun kill-all-buffers ()
  ;; from http://stackoverflow.com/a/3417472
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(setq python-debug-line-identifier-counter ?A)

(defun python-debug-print ()
  (interactive)
  (insert "print(\"MY DEBUG MARKER ")
  (insert python-debug-line-identifier-counter)
  (insert "\", )")
  (backward-char 1)
  (setq python-debug-line-identifier-counter
        (1+ python-debug-line-identifier-counter)))

(defun python-debug-breakpoint ()
  (interactive)
  (insert "from pudb import set_trace as debug; debug()"))

(defconst github-commmit-url-format-string
  "https://github.com/%s/%s/commit/%s/")

(defun browse-github-sha (owner repository sha)
  (browse-url (format github-commmit-url-format-string
                      owner repository sha)))

(defun browse-github-sha-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (sha (buffer-substring (car bounds) (cdr bounds))))
    (let ((remote
           (shell-command-to-string "git config --get remote.origin.url")))
      (string-match "git@github.com:\\(.+\\)/\\(.+\\).git" remote)
      (let* ((owner (match-string 1 remote))
             (repository (match-string 2 remote)))
        (browse-github-sha owner repository sha)))))

(defun markdown-to-html ()
  (interactive)
  (let* ((basename (file-name-sans-extension (buffer-file-name)))
         (html-filename (format "%s.html" basename)))
    (shell-command (format "pandoc -o %s %s"
                           html-filename (buffer-file-name)))
    (find-file-other-window html-filename)))

(defun fullscreen ()
  ;; from http://www.emacswiki.org/emacs/FullScreen
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;; hooks
(defun delete-trailing-whitespace-in-code ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace-in-code)

(defconst window-count 2)

(defun my-after-init-hook ()
  (when (display-graphic-p)
    (fullscreen)
    (dotimes (i (1- window-count))
      (split-window-horizontally))
    (balance-windows)))

(add-hook 'after-init-hook 'my-after-init-hook)


;; mode management

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
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
