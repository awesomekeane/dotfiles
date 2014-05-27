;;; Evil mode, the vim emulator

(evil-mode 1)

(evilnc-default-hotkeys)

(require 'evil-leader)
(global-evil-leader-mode)

;;; evil key bindings
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(evil-leader/set-leader ",")
(evil-leader/set-key 
  "et" 'find-file
  "ef" 'eval-defun
  "ee" 'eval-last-sexp
  "w" 'save-buffer
  "b" 'switch-to-buffer
  "k" 'kill-buffer
	"m" (lambda () (interactive) (find-file "~/.emacs")))

; State binding for evil mode
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (nrepl-mode . insert)
                              (pylookup-mode . emacs)
                              (comint-mode . normal)
                              (shell-mode . insert)
                              (git-commit-mode . insert)
                              (git-rebase-mode . emacs)
                              (term-mode . emacs)
                              (help-mode . emacs)
                              (helm-grep-mode . emacs)
                              (grep-mode . emacs)
                              (bc-menu-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . emacs)
                              (wdired-mode . normal))
      do (evil-set-initial-state mode state))
