;;; dotfile --- Summary
;;; Commentary:
;;; Code:

;; Enable melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Tell Emacs about rainbowdash theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/rainbowdash-theme/")

;; Packages we need
(require 'all-the-icons)
(require 'anzu)
(require 'company)
(require 'company-c-headers)
(require 'company-flx)
(require 'company-quickhelp)
(require 'emojify)
(require 'flycheck)
(require 'flycheck-checkbashisms)
(require 'flycheck-clangcheck)
(require 'julia-mode)
(require 'mode-icons)
(require 'nyan-mode)
(require 'nyan-prompt)
(require 'org)
(require 'pdf-tools)
(require 'pos-tip)
(require 'powerline)
(require 'spaceline)
(require 'spaceline-config)
(require 'undo-tree)
(require 'wiki-summary)
(require 'yahoo-weather)
(require 'helm-config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-quickhelp-mode t)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    (default)))
 '(doc-view-continuous t)
 '(electric-pair-mode t)
 '(excorporate-configuration
   (quote
    ("user@domain.com" . "https://outlook.office365.com/EWS/Exchange.asmx")))
 '(explicit-shell-file-name "/usr/bin/bash")
 '(flycheck-sh-bash-executable "/usr/bin/bash")
 '(flycheck-sh-checkbashisms-executable "/usr/bin/checkbashisms")
 '(flycheck-sh-shellcheck-executable "~/.cabal/bin/shellcheck")
 '(flycheck-status-emoji-mode t nil (flycheck-status-emoji))
 '(fringe-mode 1 nil (fringe))
 '(global-anzu-mode t)
 '(global-company-mode t)
 '(global-emojify-mode t)
 '(global-flycheck-mode t)
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(helm-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mode-icons-mode t)
 '(nyan-animate-nyancat t)
 '(nyan-bar-length 26)
 '(nyan-minimum-window-width 112)
 '(org-startup-truncated nil)
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (helm-google zone-sl helm company-shell magit xkcd wiki-summary vline undo-tree symon spaceline screenshot rich-minority rainbow-mode rainbow-identifiers rainbow-delimiters popup pdf-tools nyan-prompt nyan-mode mode-icons minimap minesweeper julia-shell golden-ratio font-lock+ flycheck-status-emoji flycheck-julia flycheck-clangcheck flycheck-checkbashisms excorporate emojify dracula-theme dna-mode company-quickhelp company-flx company-c-headers beacon async anzu anaphora all-the-icons)))
 '(powerline-default-separator (quote wave))
 '(powerline-height 28)
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tool-bar-mode nil)
 '(tramp-default-method (quote ssh) nil (tramp))
 '(undo-tree-visualizer-diff t)
 '(which-function-mode t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#50fa7b" :foreground "#282a36"))))
 '(flycheck-error ((t (:background "Red1"))))
 '(flycheck-warning ((t (:background "DarkOrange"))))
 '(hl-line ((t (:background "#44475a" :foreground "#50fa7b"))))
 '(mode-line ((t (:background "#BD93f9" :foreground "#282a36" :box (:line-width 1 :color "#282a36")))))
 '(mode-line-inactive ((t (:background "#44475a" :foreground "#282a36" :box (:line-width 1 :color "#282a36")))))
 '(powerline-active1 ((t (:inherit mode-line :background "#BD93f9" :foreground "#282a36"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#BD93f9" :foreground "#282a36"))))
 '(powerline-inactive1 ((t (:inherit mode-line :background "#44475a" :foreground "#282a36"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "#44475a" :foreground "#282a36"))))
 '(show-paren-match ((t (:background "#ff79c6" :foreground "#282a36" :inverse-video nil))))
 '(spaceline-modified ((t (:background "#50fa7b" :foreground "#282a36" :inherit (quote mode-line)))))
 '(spaceline-read-only ((t (:background "#ff79c6" :foreground "#282a36" :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "#BD93f9" :foreground "#282a36" :inherit (quote mode-line)))))
 '(which-func ((t (:inherit font-lock-function-name-face :foreground "#282a36")))))

;; https://github.com/vermiculus/sx.el/issues/283
(setq gnutls-min-prime-bits 1024)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; FlyCheck
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup))

;; enable static analysis
(setq flycheck-clangcheck-analyze t)

;; Spaceline
(spaceline-emacs-theme)
(spaceline-info-mode t)
(spaceline-toggle-buffer-size-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
(spaceline-toggle-flycheck-info-off)
(spaceline-toggle-flycheck-warning-off)
(spaceline-toggle-flycheck-error-off)
(spaceline-toggle-selection-info-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-hud-off)

;; Functions
(defun switch-full-screen ()
  "Toggle full screen."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
(erase-buffer)))

(defun my-select-clangcheck-for-checker ()
  "Select clang-check for flycheck's checker."
  (flycheck-set-checker-executable 'c/c++-clangcheck "/usr/bin/clang-check")
  (flycheck-select-checker 'c/c++-clangcheck))

;; Shortcuts
(global-set-key [f11] 'switch-full-screen)
(global-set-key (kbd "C-x g") 'helm-google)
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file "~/Org/tasks.org")))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; Org-mode settings
(setq org-log-done t)
(setq org-todo-keywords
 ;; This is what we will see in org files
 '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED"))
 )
 
(setq org-agenda-files '("~/Org/"))

(setq org-todo-keyword-faces
 '(("IN-PROGRESS" . "orange") ("WAITING" . "magenta") ("CANCELED" . "red") ("DONE" . "green"))
 )

(add-hook 'after-save-hook 'org-mode-export-hook)

;; Backup
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

;; nyan-prompt for eshell
(add-hook 'eshell-load-hook 'nyan-prompt-enable)

;; pdf-tools
(pdf-tools-install)

;; company mode backends
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-shell)
    
;; Disable Nyan-mode when no X
(if (display-graphic-p)
    (nyan-mode t))

;; Disable hl-line in some places
(add-hook 'eshell-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))
(add-hook 'term-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))
;; eww as default for helm-google
(setq browse-url-browser-function 'eww-browse-url)

(provide '.emacs)
;;; .emacs ends here
