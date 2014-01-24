(setq load-path (cons "~/.emacs.d" load-path))
(load "redoplus/redoplus")
(load "color-theme/color-theme-oblivion")
(load "linum-scale/linum-scale")
(load "buffcycle/buffcycle")
(load "smooth-scroll/smooth-scroll")
(load "pycomplete/pycomplete")
(load "tabbar/tabbar")
(load "scheme/company/company")
(load "scheme/paredit")
(load "scheme/geiser/elisp/geiser")
(load "auto-complete-haskell/auto-complete-haskell")
(load "yesod/hamlet-mode")
;; (load "glsl-mode/glsl-mode")

(server-start)
(delete-selection-mode 1)
(set-default-font "Source Code Pro")
(setq mouse-drag-copy-region nil)

(if window-system
    (set-frame-size (selected-frame) 164 47))

(require 'tabbar)
(tabbar-mode)

(require 'buffcycle)

;; Print left margin
(setq lpr-page-header-switches (quote ("-o6")))

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-oblivion)))

(global-linum-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'smooth-scroll)
(smooth-scroll-mode t)

;;(global-ede-mode 1)                      ; Enable the Project management system
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu

(setq ecb-tip-of-the-day nil)
(setq stack-trace-on-error t)
(ecb-layout-switch "leftright2")
(setq ecb-source-path '("/home/andrebask"))
(require 'ecb)
(setq ecb-auto-activate 1)

(require 'redo+)
(global-set-key (kbd "C-y") 'redo)

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'auto-complete-config)
(ac-config-default)

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
	    (add-to-list 'ac-sources 'ac-source-ropemacs)
	    (local-set-key "\C-c\C-c" 'py-compile)))

(setq auto-mode-alist
  (cons (cons "\\.pl" 'prolog-mode)
     auto-mode-alist))

(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(defun py-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))
(setq compilation-scroll-output t)

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)

;; python-mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))
;; path to the python interpreter, e.g.: ~rw/python27/bin/python2.7
(setq py-python-command "python")
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; pymacs settings
(setq pymacs-python-command py-python-command)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

(require 'pycomplete)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

(require 'company)
(setq company-backends '())
(company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; GLSL Highlighting
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))


;; Haskell
(require 'auto-complete-haskell)
;; Somehow the hook doesn't enable auto-complete-mode for Haskell although it should
;; ac-modes lists all modes with auto-complete enabled
(setq ac-modes
      (append '(scheme-mode haskell-mode literate-haskell-mode tuareg-mode js-mode inferior-haskell-mode)
              ac-modes))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode haskell-mode)))
 '(ecb-layout-window-sizes (quote (("leftright2" (ecb-directories-buffer-name 0.12804878048780488 . 0.5957446808510638) (ecb-sources-buffer-name 0.12804878048780488 . 0.3829787234042553) (ecb-methods-buffer-name 0.12195121951219512 . 0.5957446808510638) (ecb-history-buffer-name 0.12195121951219512 . 0.3829787234042553)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(geiser-guile-binary "guile")
 '(inhibit-startup-screen t) 
 '(haskell-doc-show-global-types t)
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-eldoc-mode turn-on-haskell-doc-mode (lambda nil (ghc-init) (flymake-mode)) turn-on-haskell-indentation turn-on-haskell-decl-scan turn-on-font-lock)))
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(defun ghc-flymake-init ()
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-in-system-tempdir))
             (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ghcflakes" (list temp-file)))))

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)
	    (require 'auto-complete-config)
	    (auto-complete-mode t)
	    (add-to-list 'ac-sources 'ac-source-ghc-mod)))
;; haskell-mode hooks
(add-hook 'haskell-mode-hook 'capitalized-words-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(require 'hamlet-mode)

;; ELPA
(load "package/package")
(require 'package)
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-to-list 'auto-mode-alist
	     '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
