
;; Activate Org Mode:
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; Insert timestamp when TODO state changed into DONE state
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-agenda-repeating-timestamp-show-all nil)

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
;; (setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d)")
	(sequence "WAITING(w)" "INACTIVE(i)" "MEETING(m)" "|" "CANCELLED(c)" )))

(require 'org-mobile)

(when *is-a-mac*
  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/Dropbox/Emacs/Org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/Dropbox/Emacs/Org/inbox.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-mobile-files (list "~/Dropbox/Emacs/Org/home.org"
			       "~/Dropbox/Emacs/Org/work.org"
			       "~/Dropbox/Emacs/Org/personal.org"
			       ))
  (setq org-agenda-files (list "~/Dropbox/Emacs/Org/home.org"
			       "~/Dropbox/Emacs/Org/work.org"
			       "~/Dropbox/Emacs/Org/personal.org"
			       ))

  (setq org-capture-templates
	'(("t" "Todo-Personal" entry (file+headline "~/Dropbox/Emacs/Org/personal.org" "Personal Tasks:") "* TODO %?\n")
	  ("h" "Todo-Home" entry (file+headline "~/Dropbox/Emacs/Org/home.org" "Home Tasks:") "* TODO %?\n")
	  ("w" "Todo-Work" entry (file+headline "~/Dropbox/Emacs/Org/work.org" "Work Tasks:") "* TODO %?\n")
	  ("j" "Todo" entry (file+headline "~/Dropbox/Emacs/Org/inbox.org" "Tasks" ) "* TODO %?\n")
	  ("b" "Book" entry (file+headline "~/Dropbox/Emacs/Org/work.org" "Books")
	   "* read /%^{Title}/ by %^{Author}
             :PROPERTIES:
             :CREATED: %T
             :END:
             %\\2, a very cool writer (I always say the same about all authors)%?" :kill-buffer t)
	  ))

  (setq org-refile-targets '(("~/Dropbox/Emacs/Org/home.org" :maxlevel . 1)
			     ("~/Dropbox/Emacs/Org/work.org" :maxlevel . 1)
			     ("~/Dropbox/Emacs/Org/personal.org" :maxlevel . 1)))
  )

(when *is-a-win*
  ;; Set to the location of your Org files on your local system
  (setq org-directory "C:/Users/Marco.Chiu/Dropbox/Emacs/Org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/inbox.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "C:/Users/Marco.Chiu/Dropbox/Apps/MobileOrg")
  (setq org-mobile-files (list "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/home.org"
			       "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/work.org"
			       "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/personal.org"
			       ))
  (setq org-agenda-files (list "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/home.org"
			       "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/work.org"
			       "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/personal.org"
			       ))

  (setq org-capture-templates
	'(("t" "Todo-Personal" entry (file+headline "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/personal.org" "Personal Tasks:") "* TODO %?\n")
	  ("h" "Todo-Home" entry (file+headline "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/home.org" "Home Tasks:") "* TODO %?\n")
	  ("w" "Todo-Work" entry (file+headline "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/work.org" "Work Tasks:") "* TODO %?\n")
	  ("j" "Todo" entry (file+headline "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/inbox.org" "Tasks" ) "* TODO %?\n")
	  ("b" "Book" entry (file+headline "C:/Users/Marco.Chiu/Dropbox/Emacs/Org/work.org" "Books")
	   "* read /%^{Title}/ by %^{Author}
             :PROPERTIES:
             :CREATED: %T
             :END:
             %\\2, a very cool writer (I always say the same about all authors)%?" :kill-buffer t)
	  ))

  (setq org-refile-targets '(("C:/Users/Marco.Chiu/Dropbox/Emacs/Org/home.org" :maxlevel . 1)
			     ("C:/Users/Marco.Chiu/Dropbox/Emacs/Org/work.org" :maxlevel . 1)
			     ("C:/Users/Marco.Chiu/Dropbox/Emacs/Org/personal.org" :maxlevel . 1)))
  )


(add-hook 'org-mode-hook '(lambda ()
			     ;; make the lines in the buffer wrap around the edges of the screen.
                             (toggle-word-wrap)
                             (toggle-truncate-lines)))


;; (use-package org-bullets
;;   :ensure t
;;   :con-fig
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-Org)
