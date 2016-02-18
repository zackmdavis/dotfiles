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

;; backward- and forward- word seem the same as the defaults of left- and
;; right-word, but get remapped by subword mode like you would expect
(global-set-key [C-left] 'backward-word)
(global-set-key [C-right] 'forward-word)

(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq vc-follow-symlinks t)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq initial-scratch-message "; scratch!\n\n")
(setq initial-major-mode 'text-mode)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(define-coding-system-alias 'UTF-8 'utf-8)
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(when (file-exists-p "~/.emacs.d/themes/an-algorithmic-luciditheme-theme.el")
  (load-theme 'an-algorithmic-luciditheme t))
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

;; about halfway between San Francisco and Walnut Creek
(setq calendar-latitude 37.8448)
(setq calendar-longitude -122.2490)

;; enhanced commands

(global-set-key (kbd "M-/") 'hippie-expand)
(delete 'try-expand-list hippie-expand-try-functions-list)
(delete 'try-expand-line hippie-expand-try-functions-list)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; custom bindings
(global-set-key (kbd "M-r") 'revert-buffer)

;;; window navigation
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; undisabled default commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; disabled default commands

;; `suspend-frame` is not so useful and too easy to fat-finger
(global-unset-key (kbd "C-x C-z"))

;; special characters

(defun insert-em-dash ()
  (interactive)
  (insert "—"))
(global-set-key (kbd "M-_") 'insert-em-dash)

(defun insert-en-dash ()
  (interactive)
  (insert "–"))

(defun insert-lambda ()
  (interactive)
  (insert "λ"))
(global-set-key (kbd "M-l") 'insert-lambda)

;; pairings

(setq parens-require-spaces nil)

(defun my-insert-pair (pair-literal)
  (interactive)
  (insert pair-literal)
  (backward-char))

(defun insert-brackets ()
  (interactive)
  (my-insert-pair "[]"))

(defun insert-braces ()
  (interactive)
  (my-insert-pair "{}"))

(defun insert-double-quotes ()
  (interactive)
  (my-insert-pair "\"\""))

(defun insert-single-quotes ()
  (interactive)
  (my-insert-pair "''"))

(defun insert-angle-brackets ()
  (interactive)
  (my-insert-pair "<>"))

(when (display-graphic-p)
  (global-set-key (kbd "M-[") 'insert-brackets)
  (global-set-key (kbd "M-{") 'insert-braces)
  (global-set-key (kbd "M-\"") 'insert-double-quotes)
  (global-set-key (kbd "M-'") 'insert-single-quotes)
  (global-set-key (kbd "M-<") 'insert-angle-brackets))

;; more commands

(defun cd~ ()
  (interactive)
  (cd "~"))

(defun reset-emacs ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (cd~))

(setq debug-line-identifier-counter ?A)

(defun debug-print (opening closing)
  (insert opening debug-line-identifier-counter closing)
  (backward-char)
  (setq debug-line-identifier-counter
        (1+ debug-line-identifier-counter)))

(defun python-debug-print ()
  (interactive)
  (debug-print "print(\"MY DEBUG MARKER " "\", )"))

(defun python-debug-breakpoint ()
  (interactive)
  (insert "from pudb import set_trace as debug; debug()"))

(defun python-debug-stacktrace ()
  (interactive)
  (insert "import traceback; traceback.print_stack()"))

(defun python-string-to-implicitly-joined-for-pep8 ()
  (interactive)
  (let ((paren-maybe (lambda (p) (when (not (equal (char-before) p))
                                   (insert p)))))
    (save-excursion
      (search-backward "\"")
      (funcall paren-maybe 40)  ; "("
      (setq end-of-the-line (move-to-column 78))
      (while (equal end-of-the-line 78)  ; we made it far enough
        (search-backward " ")
        (forward-char)
        (insert "\"\n\"")
        (indent-for-tab-command)
        (setq end-of-the-line (move-to-column 78)))
      (funcall paren-maybe 41))))  ; ")"

(defun clojure-debug-print ()
  (interactive)
  (debug-print "(println \"MY DEBUG MARKER " "\" )"))

(defun rust-debug-print ()
  (interactive)
  (debug-print "println!(\"MY DEBUG MARKER " " {:?}\", );"))

(defun javascript-debug-print ()
  (interactive)
  (debug-print "console.log(\"MY DEBUG MARKER " "\", )"))

(defun go-debug-print ()
  (interactive)
  (debug-print "fmt.Printf(\"MY DEBUG MARKER " " %v\\n\", )"))

(defun go-if-err (preerr posterr)
  (let ((action (format "%serr%s" preerr posterr)))
    (dolist (line `("if err != nil {"
                    ,action
                    "}"))
      (insert line)
      (indent-for-tab-command)
      (newline))
    (delete-backward-char 1))) ; one too many newlines

(defun go-return-if-err ()
  (interactive)
  (go-if-err "return " ""))

(defun go-panic-if-err ()
  (interactive)
  (go-if-err "panic(" ")"))

(defun hy-debug-print ()
  (interactive)
  (debug-print "(print \"MY DEBUG MARKER " "\" )"))

(defun hy-debug-breakpoint ()
  (interactive)
  (insert "(import [pudb [set-trace]]) (set-trace)"))

(defun literalize-hexstream ()
  (interactive)
  (let ((line-bytes-literalized 0))
    (while (string-match "[0-9a-f-A-F]" (char-to-string (following-char)))
      (insert "0x")
      (forward-char 2)
      (insert ", ")
      (setq line-bytes-literalized (1+ line-bytes-literalized))
      (when (>= line-bytes-literalized 16)
        (insert "\n")
        (setq line-bytes-literalized 0)))))

(defun truncate-sha-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (delete-region (+ (car bounds) 8) (cdr bounds))))

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
  (when (or (derived-mode-p 'prog-mode)
            (equal major-mode 'web-mode)
            (equal major-mode 'rst-mode)
            (equal major-mode 'rust-mode))
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

(defun display-startup-echo-area-message ()
  (when (display-graphic-p)
    (message (sunrise-sunset))))

;; mode management

(setq safe-local-variable-values
      '((eval put-clojure-indent 'map-comprehension 1)))

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(setq git-commit-summary-max-length 72)

(defun my-anti-git-rebase-mode-hook ()
  ;; I don't like rebase mode
  (fundamental-mode)
  (setq buffer-read-only nil))
(add-hook 'git-rebase-mode-hook 'my-anti-git-rebase-mode-hook)


;; Go
(defun my-go-mode-hook ()
  (setq tab-width 3)
  (setq gofmt-command "~/Code/go_workspace/bin/goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (subword-mode 1) ; ubiquitousCamelCaseMeritsSubWordMode
  (local-set-key (kbd "M-=") (lambda () (interactive) (insert ":="))))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; C (?!)
(setq c-basic-offset 4)
(setq c-default-style "k&r")

;; dayjob-specific configuration
(defconst dayjob-config "~/.emacs.d/dayjob.el")
(when (file-exists-p dayjob-config)
  (load-file dayjob-config))


;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
              '("MELPA" . "https://melpa.org/packages/"))
(package-initialize)

(defconst my-packages '(clojure-mode racket-mode rust-mode web-mode less-css-mode yaml-mode magit))

;; Using a Git clone instead of package management for the moment
;; because I've hacked on this at least once and might do so again
(defconst hy-mode-path "~/.emacs.d/hy-mode/hy-mode.el")
(when (file-exists-p hy-mode-path)
  (load-file hy-mode-path))


;; I've been writing a programming language lately
(defconst glitteral-mode-path "~/Code/Glitteral/glitteral-mode.el")
(when (file-exists-p glitteral-mode-path)
  (load-file glitteral-mode-path))


(defun install-my-packages ()
  (interactive)
  ;; http://stackoverflow.com/a/10093312
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))
