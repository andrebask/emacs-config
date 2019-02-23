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
(setq-default indent-tabs-mode nil)
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

;; ctrl+backspace delete by Aborn Jiang
;; https://stackoverflow.com/questions/28221079
;; changed to avoid use of s library
(require 'subr-x)
(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (cl-search " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (eq "" (string-trim substr)))
                        (cl-search "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))

(global-set-key  [C-backspace]
                 'aborn/backward-kill-word)

;; duplicate line
;; https://stackoverflow.com/questions/88399
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  )

(global-set-key (kbd "C-d") 'kill-line)
(global-set-key (kbd "C-S-d") 'duplicate-line)
