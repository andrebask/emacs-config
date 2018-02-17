(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org:/packages/"))
(package-initialize)

(server-start)
(setq inhibit-startup-screen t)
(setq stack-trace-on-error t)
(delete-selection-mode 1)
(set-default-font "Source Code Pro")
(set-face-attribute 'default nil :height 140)
(setq mouse-drag-copy-region nil)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-linum-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(toggle-frame-maximized)

(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(setq speedbar-directory-unshown-regexp "^$")
(setq speedbar-show-unknown-files t)

(require 'tabbar)
(tabbar-mode)

(require 'pc-bufsw)
(pc-bufsw-default-keybindings)

;; Print left margin
(setq lpr-page-header-switches (quote ("-o6")))

(require 'color-theme)
(load-file "~/.emacs.d/color-theme/color-theme-oblivion.el")
(eval-after-load "color-theme"
  '(progn
     (color-theme-oblivion)))

(require 'smooth-scrolling)

(require 'redo+)
(global-set-key (kbd "C-y") 'redo)

(setq ispell-dictionary "en_GB")
(add-hook 'markdown-mode-hook 'flyspell-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elm-mode tabbar-ruler sr-speedbar smooth-scrolling redo+ pc-bufsw markdown-mode company-quickhelp color-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
