;;; package --- Summary
;;; Emacs dot file
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)
(add-to-list 'package-archives
						 '("marmalade" .
							 "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
						 '("melpa" .
							 "http://melpa.milkbox.net/packages/") t)

;; list of packages
(add-to-list 'load-path "~/.emacs.d")

(dolist (package-path
				 '(
					 ))
  (add-to-list 'load-path (format "%s%s" "~/.emacs.d/" package-path)))

(dolist (package
				 '(
					 slime-autoloads
					 ido
					 auto-complete-config
					 smex
					 tabbar-ruler
					 ))
  (require package))

;; package configs

(setq tabbar-ruler-global-tabbar t) ;tabbar
;(setq tabbar-ruler-popup-scrollbar t)

(yas-global-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-to-list 'ac-dictionary-directories
						 "~/.emacs.d/auto-complete//ac-dict")
(ac-config-default) ;auto-complete

(smex-initialize) ;smex

(define-globalized-minor-mode ;fill column indicator
	global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

(ido-mode t) ;ido-mode

(add-hook 'python-mode-hook 'jedi:setup) ;python jedi
(setq jedi:complete-on-dot t)

(setq inferior-lisp-program
			"/usr/local/Cellar/sbcl/1.1.16/bin/sbcl") ;slime
(setq slime-contribs '(slime-fancy))
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core"
							 "/Users/Wei/.emacs.d/slime/sbcl.core-for-slime"))))

;; apprearance

(x-focus-frame nil)
(column-number-mode 1)
(show-paren-mode 1)
(load-theme 'solarized-light t)     
(setq default-frame-alist '((font . "Inconsolata 18"))) ;font
(global-linum-mode t) ;line number
(setq visible-bell t) ;visual bell
(setq resize-mini-windows nil) ;do not resize mini buffer
(tool-bar-mode -1) ;hide the tool bar
(toggle-scroll-bar -1)
(setq ring-bell-function 'ignore) ;get rid of bells

;; editing

(cua-mode 1)
(setq delete-by-moving-to-trash t) ;trash delete
(setq indent-tabs-mode nil) ;tab setting
(setq tab-width 4)
(setq cash-fold-search t) ;search
(setq ;backup to designated folder
  backup-by-copying t
  backup-directory-alist
  '(("." . "~/Documents/tmp/emacs-tmp"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; coding

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-h C-f") 'find-function)

;; mode hooks

(defun byte-compile-current-buffer ()
	"`byte-compile' current buffer if it's `emacs-lisp-mode' and compiled file exits."
	(interactive)
	(when (and (eq major-mode 'emacs-lisp-mode)
						 (file-exists-p (byte-compile-dest-file buffer-file-name)))
		(byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(add-hook 'emacs-lisp-mode-hook ;emacs lisp mode hook
	  (lambda ()
	    (setq tab-width 2)
	    (paredit-mode 1)))

(add-hook 'lisp-mode-hook ;lisp mode hook
          (lambda ()
            (setq tab-width 2)
            (paredit-mode 1)))

(add-hook 'slime-repl-mode-hook ;slime repl mode
	  (lambda ()
	    (setq tab-width 2)
	    (paredit-mode 1)))

;; evil mode setting
(load "~/.emacs.evil.el")

(custom-set-variables
 ; disable the startup screen
 '(inhibit-startup-screen t))
(custom-set-faces)

(server-start)

(provide '.emacs)

;;; .emacs ends here
