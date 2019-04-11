;;; dotfile --- Summary
;;; Commentary:
;;; Code:

;;; GUI settings
(setq inhibit-startup-screen 1)
(setq initial-scratch-message "🌈🦄 Welcome to the Magic Rainbow Unicorn Emacs 🌈🦄")
(setq use-dialog-box -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack:pixelsize=14"))
;; Hide borders in terminal mode
;(set-display-table-slot standard-display-table
;					'vertical-border (make-glyph-code 8203))

(set-display-table-slot standard-display-table
			'vertical-border (make-glyph-code 160))

(global-set-key [f11] 'switch-full-screen)
(define-key global-map (kbd "M-t") (lambda () (interactive) (ansi-term explicit-shell-file-name)))

;; Enable melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; Package management

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :init
  (progn
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(use-package anzu
  :ensure t
  :init (global-anzu-mode))

(use-package async
  :ensure t)

(use-package bash-completion
  :ensure t
  :init (bash-completion-setup))

(use-package company
  :ensure t
  :init (global-company-mode))

(use-package company-c-headers
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-c-headers))

(use-package company-flx
  :ensure t
  :after company
  :config (company-flx-mode +1))

(use-package company-quickhelp
  :ensure t
  :after company
  :if window-system
  :hook ((company-mode . company-quickhelp-mode))
  :config (setq company-quickhelp-delay 0.1))

(use-package company-shell
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-shell))

(use-package dired-async
  :after async
  :config
  (dired-async-mode 1))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq display-time-24hr-format 1)
  (setq display-time-day-and-date 1)
  (setq display-time-default-load-average nil)
  (setq display-time-string-forms '((propertize (format-time-string "%A %H:%M" now) 'face 'bold)))
  (setq doom-modeline-buffer-file-name-style 'file-name)
  ;(setq doom-modeline-major-mode-icon t)
  ;(setq doom-modeline-icon t)
  ;(setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count t)
  (display-time-mode 1))

(use-package dracula-theme
  :ensure t
  :init (load-theme 'dracula 1))

(use-package emojify
  :ensure t
  :init
  (global-emojify-mode)
  :config
  (setq emojify-emoji-set "twemoji-v12")
  (progn
    (add-hook 'after-init-hook 'global-emojify-mode-line-mode)))

(use-package eww
  :ensure t
  :config (setq browse-url-browser-function 'eww-browse-url))

(use-package excorporate
  :ensure t
  :config
  (setq excorporate-configuration (quote ("user@domain.com" . "https://outlook.office365.com/EWS/Exchange.asmx"))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-sh-bash-executable "/usr/bin/bash")
  (setq flycheck-sh-checkbashisms-executable "/usr/bin/checkbashisms")
  (setq flycheck-sh-shellcheck-executable "/home/sheikki/.cabal/bin/shellcheck"))


(use-package flycheck-checkbashisms
  :ensure t)

(use-package flycheck-clangcheck
  :ensure t
  :config
  (setq flycheck-clangcheck-analyze 1))

(use-package flycheck-status-emoji
  :ensure t
  :config (flycheck-status-emoji-mode 1))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("C-x g" . helm-google))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
	    (setq helm-display-header-line nil)
	    (setq helm-mode-line-string nil)
	    (setq helm-M-x-fuzzy-match t)
	    (setq helm-buffers-fuzzy-matching t)
	    (setq helm-recentf-fuzzy-match t)
	    (require 'helm-config)
	    (helm-mode 1)))

(use-package helm-google
  :ensure t)

(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode 1)
  (set-face-foreground 'hl-line "#f8f8f2"))

(use-package julia-mode
  :ensure t)

(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "🌈🦄"))

(use-package multi-term
  :ensure t)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1)
  (setq nyan-bar-length 32)
  (setq nyan-minimum-window-width 64)
  (setq nyan-animate-nyancat 1)
  ;(setq nyan-wavy-trail 1)
  )

(use-package org
  :ensure t
  :config
  (setq org-support-shift-select 1)
  (setq org-log-done 1)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
  (setq org-todo-keyword-faces
	'(("IN-PROGRESS" . "#ffb86c") ("WAITING" . "#ff79c6") ("CANCELED" . "$ff5555") ("DONE" . "#50fa7b"))))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package pos-tip
  :ensure t)

(use-package paren
  :ensure t
  :init
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-diff 1))

(use-package which-func
  :ensure t
  :init (which-function-mode 1))

(use-package wiki-summary
  :ensure t)

(use-package xpm
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(electric-pair-mode t)
 '(explicit-shell-file-name "/usr/bin/bash")
 '(fringe-mode 1 nil (fringe))
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (multi-term xpm bash-completion all-the-icons all-the-icons-dired anzu dracula-theme helm helm-google company-shell doom-modeline magit minions wiki-summary vline undo-tree screenshot rainbow-mode rainbow-identifiers rainbow-delimiters popup pdf-tools nyan-mode julia-shell font-lock+ flycheck-status-emoji flycheck-julia flycheck-clangcheck flycheck-checkbashisms excorporate emojify dna-mode company-quickhelp company-flx company-c-headers beacon async use-package)))
 '(select-enable-primary t)
 '(tramp-default-method (quote ssh) nil (tramp))
 '(xterm-mouse-mode t)
 '(xterm-mouse-utf-8 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; https://github.com/vermiculus/sx.el/issues/283
(setq gnutls-min-prime-bits 1024)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;; Functions

(defun switch-full-screen ()
  "Toggle full screen."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
(erase-buffer)))

;(defun org-mode-export-hook()
;  "Auto export html."
;  (when (eq major-mode 'org-mode)
;    (org-export-as-html t)))

(defun my-select-clangcheck-for-checker ()
  "Select clang-check for flycheck's checker."
  (flycheck-set-checker-executable 'c/c++-clangcheck "/usr/bin/clang-check")
  (flycheck-select-checker 'c/c++-clangcheck))


;(setq org-agenda-files '("~/Org/"))

;(add-hook 'after-save-hook 'org-mode-export-hook)

;;; Backup

(setq
 ;; Don't clobber symlinks
 backup-by-copying t
 backup-directory-alist
 ;; Don't litter my fs tree
 '(("." . "~/.backups"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; Use versioned backups
 version-control t)

;; company mode backends
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-shell)
    

;;; Disable Nyan-mode when no X
;(if (display-graphic-p)
;    (nyan-mode t))

;;; Disable hl-line and nyan-mode in some places
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local global-hl-line-mode nil)
			      (setq-local nyan-mode nil)))

(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)
			    (setq-local nyan-mode nil)))

;;; Kill term/ansi-term on exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;;; Kill eshell on exit
(defcustom eshell-kill-on-exit t
  "*If non-nil, kill the Eshell buffer on the `exit' command.
Otherwise, the buffer will simply be buried."
  :type 'boolean
  :group 'eshell-mode)

;;; Kill shell on exit
(defun my-shell-mode-hook ()
  "Custom `shell-mode' behaviours."
  ;; Kill the buffer when the shell process exits.
  (let* ((proc (get-buffer-process (current-buffer)))
         (sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        ;; Call the original process sentinel first.
        (funcall #',sentinel process signal)
        ;; Kill the buffer on an exit signal.
        (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process))
             (kill-buffer (process-buffer process)))))))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)


(provide '.emacs)
;;; .emacs ends here
