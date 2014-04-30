;;; 

;; apprearance
(require 'color-theme-solarized) ;color theme
(load-theme 'solarized-light t)     
(set-default-font "Inconsolata 18") ;font
(global-linum-mode t) ;line number
(setq visible-bell t) ;visual bell
(setq resize-mini-windows nil) ;do not resize mini buffer

;; editing
(setq delete-by-moving-to-trash t) ;trash delete
(setq indent-tabs-mode nil) ;tab setting
(setq tab-width 4)
(setq cash-fold-search t) ;search
(setq ;backup to designated folder
  backup-by-copying t
  backup-directory-alist
  '(("." . "~/Documents/tmp"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; coding
(electric-indent-mode 1) ;indentation

(add-to-list 'load-path "~/.emacs.d/paredit") ;paredit (slip parathesis)

(add-to-list 'load-path "~/.emacs.d/slime") ;slime
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.1.16/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "/Users/Wei/.emacs.d/slime/sbcl.core-for-slime"))))

(add-hook 'lisp-mode-hook ;lisp mode hook
          (lambda ()
            (setq tab-width 2)
	    (paredit-mode 1)))

(add-hook 'slime-repl-mode-hook ;slime repl mode
	  (lambda ()
	    (setq tab-wide 2)
	    (paredit-mode 1)))

(add-to-list 'load-path "~/.emacs.d/evil") ;evil (vim simulator)
(require 'evil)
(evil-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)

;;; evil key bindings

(define-key evil-insert-state-map "jk" [escape]) 
(define-key evil-insert-state-map "JK" [escape])
(define-key evil-insert-state-map "Jk" [escape])
(define-key evil-insert-state-map "jk" [escape])

(evil-leader/set-leader ",")
(evil-leader/set-key 
  "e" 'find-file
  "w" 'save-buffer
  "b" 'switch-to-buffer
  "k" 'kill-buffer)
