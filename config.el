
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

(setq inhibit-startup-message nil)

(tool-bar-mode -1)
;;(menu-bar-mode 1)
;;(scroll-bar-mode 1)

(setq ring-bell-function 'ignore)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when window-system (global-hl-line-mode t))

;; (when window-system (global-prettify-symbols-mode t))

;; Set front size, the value is in 1/10pt, so 160=16pt
(when *is-a-mac*
  (set-face-attribute 'default nil :height 160))

(when *is-a-win*
  (set-face-attribute 'default nil :height 120)
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
  )

(setq x-stretch-cursor t)

(use-package pangu-spacing
  :ensure t
  :init
  (global-pangu-spacing-mode 1))

(add-hook 'prog-mode-hook 'linum-mode)

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(server-start)

(setq user-full-name "Marco Chiu")
(setq user-mail-address "chiumarco@gmail.com")

(load-theme 'leuven t)

;; (use-package dracula-theme
;;   :ensure t)

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (load-theme 'leuven t)
  (reapply-themes)
  )

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'leuven-dark t)
  ;;(load-theme 'dracula t)
  (reapply-themes)
  )

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq spaceline-line-column-p nil)
    (setq spaceline-line-p nil)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))

(line-number-mode t)
(column-number-mode t)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")

(display-time-mode 1)

(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  ;(diminish 'linum-relative-mode)
  )

(use-package projectile
  :ensure t
  :init
    (projectile-mode 1))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;     (dashboard-setup-startup-hook)
;;     (setq dashboard-banner-logo-title "Welcome to Emacs!")
;;     (setq dashboard-startup-banner 'official)
;;     (setq dashboard-items '((recents  . 5)
;;                             (projects . 5)
;;                             (bookmark . 5)
;;                             (agenda   . 5)))
;;     (add-to-list 'dashboard-items '(agenda) t))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-width 35
          treemacs-indentation 2
          treemacs-collapse-dirs (if (executable-find "python") 3 0)
          treemacs-silent-refresh nil
          treemacs-change-root-without-asking nil
          treemacs-sorting 'alphabetic-desc
          treemacs-show-hidden-files t
          treemacs-never-persist nil
          treemacs-is-never-other-window nil
          treemacs-goto-tag-strategy 'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8] . treemacs-toggle)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
      (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ([f9] . treemacs-projectile)
              ([f9] . treemacs-projectile-toggle)))

(use-package ivy
  :ensure t)

(use-package counsel
  :ensure t
  :bind
  ;; pullup menu for kill ring
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
)

(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    )

(setq scroll-conservatively 100)

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

(require 'winner)
(winner-mode 1)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(global-set-key (kbd "M-[") 'windmove-up)
(global-set-key (kbd "M-/") 'windmove-down)
(global-set-key (kbd "M-'") 'windmove-right)
(global-set-key (kbd "M-;") 'windmove-left)
(global-set-key (kbd "M-:") 'comment-line)

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
        '(("home"
          ("emacs-config" (or (filename . ".emacs.d")
                              (filename . "emacs-config")))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("code" (filename . "code"))
           ("Web Dev" (or (mode . html-mode)
                          (mode . css-mode)))
           ("Subversion" (name . "\*svn"))
           ("Magit" (name . "\*magit"))
           ("Markdown" (filename . ".md"))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-show-empty-filter-groups nil)

;; (setq ibuffer-expert t)

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'close-all-buffers)

(use-package beacon
  :ensure t
  :config
    (beacon-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ace-popup-menu
  :ensure t
  :init
    (ace-popup-menu-mode 1))

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package company
  :ensure t
  :config
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3)
  :init
    (add-hook 'after-init-hook 'global-company-mode))

(with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))

(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           ))

(electric-pair-mode t)

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Full width comment box                                                 ;;
 ;; from http://irreal.org/blog/?p=374                                     ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bjm-comment-box (b e)
  "Draw a box comment around the region but arrange for the region to extend to at least the fill column. Place the point after the comment box."

 (interactive "r")

 (let ((e (copy-marker e t)))
   (goto-char b)
   (end-of-line)
   (insert-char ?  (- fill-column (current-column)))
   (comment-box b e 1)
   (goto-char e)
   (set-marker e nil)))

 ;; (global-set-key (kbd "C-c b b") 'bjm-comment-box)

(custom-set-variables
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(add-to-list 'load-path (expand-file-name "packages/vmd-mode" user-emacs-directory))
(when *is-a-mac*
  (setenv "PATH" (concat "/usr/local/bin:/usr/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/bin" "/usr/bin") exec-path)))
(require 'vmd-mode)

(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :bind (:map matlab-shell-mode-map
              ("C-c C-c" . term-interrupt-subjob))
  :init
  (setq matlab-shell-command "/Applications/MATLAB_R2017a.app/bin/matlab"
        matlab-indent-function t)
  (eval-after-load 'matlab
    '(add-to-list 'matlab-shell-command-switches "-nosplash")))

(defun mc/matlab-shell-here ()
  "opens up a new matlab shell in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (matlab-shell))

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50))

(when *is-a-mac*
  (require 'epa-file)
  (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
  (epa-file-enable)

  (defun offlineimap-get-password (host port)
    (require 'netrc)
    (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
           (hostentry (netrc-machine netrc host port port)))
      (when hostentry (netrc-get hostentry "password"))))

  (require 'mu4e)                      ; load mu4e
  ;; Use mu4e as default mail agent
  (setq mail-user-agent 'mu4e-user-agent)
  ;; Mail folder set to ~/Maildir
  (setq mu4e-maildir "~/Maildir")         ; NOTE: should not be symbolic link
  ;; Fetch mail by offlineimap
  (setq mu4e-get-mail-command "offlineimap")
  ;; Fetch mail in 300 sec interval
  (setq mu4e-update-interval 300)

  ;; (setq mu4e-hide-index-messages t)


  (setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "Gmail"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-sent-folder . "/Gmail/[Gmail].Sent Mail")
       (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
       (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
       ))
   ,(make-mu4e-context
     :name "Hotmail"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/Hotmail" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-sent-folder . "/Hotmail/Sent")
       (mu4e-trash-folder . "/Hotmail/Deleted")
       (mu4e-drafts-folder . "/Hotmail/Drafts")
       ))
   ,(make-mu4e-context
     :name "Yahoo"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/Yahoo" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-sent-folder . "/Yahoo/Sent")
       (mu4e-trash-folder . "/Yahoo/Deleted Items")
       (mu4e-drafts-folder . "/Yahoo/Draft")
       ))
   ))

  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
          '(("/Gmail/INBOX"               . ?i)
            ("/Gmail/[Gamil].Sent Mail"   . ?s)
            ("/Gmail/[Gmail].Trash"       . ?t)))

  ;; give me ISO(ish) format date-time stamps in the header list
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '( (:date          .  20)    ;; alternatively, use :human-date
           (:flags         .   5)
           (:from          .  25)
           (:subject       .  nil))) ;; alternatively, use :thread-subject

  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; try to emulate some of the eww key-bindings
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  ;; Call EWW to display HTML messages
  (defun jcs-view-in-eww (msg)
    (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
  ;; Arrange to view messages in either the default browser or EWW
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

  ;; use org structures and tables in message mode
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)


  ;; Set format=flowed
  ;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
  ;; each paragraph is a single long line; at sending, emacs will add the
  ;; special line continuation characters.
  (setq mu4e-compose-format-flowed t)

  ;; every new email composition gets its own frame! (window)
  ;;(setq mu4e-compose-in-new-frame t)



  ;; show full addresses in view message (instead of just names)
  ;; toggle per name with M-RET
  (setq mu4e-view-show-addresses t)


  (setq mu4e-view-show-images t)

  ;; SMTP setup
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t)
  ;; Personal info
  (setq user-full-name "Marco Chiu")          ; FIXME: add your info here
  (setq user-mail-address "chiumarco@gmail.com"); FIXME: add your info here
  ;; gmail setup
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "chiumarco@gmail.com") ; FIXME: add your gmail addr here

  (setq mu4e-compose-signature "Sent from my emacs.")

  ;; don't keep message buffers after sent message
  (setq message-kill-buffer-on-exit t)

  (global-set-key (kbd "<f6>") 'mu4e)

  )

(when *is-a-mac*
  (use-package mu4e-alert
    :ensure t
    :after mu4e
    :init
    (setq mu4e-alert-interesting-mail-query
          (concat
           "flag:unread maildir:/Exchange/INBOX "
           "OR "
           "flag:unread maildir:/Gmail/INBOX"
           ))
    (mu4e-alert-enable-mode-line-display)
    (defun gjstein-refresh-mu4e-alert-mode-line ()
      (interactive)
      (mu4e~proc-kill)
      (mu4e-alert-enable-mode-line-display)
      )
    (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)    )
  )

(when *is-a-mac*
  (use-package mu4e-maildirs-extension
    :ensure t
    :after mu4e
    :init (mu4e-maildirs-extension)
    )
  )

(defun eww-render-current-buffer ()
Render HTML in the current buffer with EWW"
interactive)
beginning-of-buffer)
eww-display-html 'utf8 (buffer-name)))
ND_SRC

 Makes eww more pleasant to use. Run it after eww buffer is loaded.
EGIN_SRC emacs-lisp
fun eww-more-readable ()
Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload 'local))                            ;; apply eww-header changes

;(setq org-ellipsis " ")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)

(add-hook 'org-mode-hook
          '(lambda ()
             (visual-line-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(use-package org-bullets
  :ensure t
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

;; Insert timestamp when TODO state changed into DONE state
(setq org-log-done 'time)
;; Uses only one star and indents text to line with the heading:
(setq org-startup-indented t)
;; Only one occurrence is shown, either today or the nearest into the future
(setq org-agenda-repeating-timestamp-show-all nil)
;; Open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;; Warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;; Don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;; Don't give awarning colour to tasks with impending deadlines
;; if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;; Don't show tasks that are scheduled or have deadlines in the
;; Normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d)")
        (sequence "WAITING(w)" "INACTIVE(i)" "MEETING(m)" "|" "CANCELLED(c)" )))

(require 'org-mobile)

(when *is-a-mac*
    (setq org-directory "~/Dropbox/Emacs/Org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"))

  (when *is-a-win*
    (setq org-directory "C:/Users/Marco.Chiu/Dropbox/Emacs/Org")
    (setq org-mobile-directory "C:/Users/Marco.Chiu/Dropbox/Apps/MobileOrg"))

  (defvar path_inbox (concat org-directory "/inbox.org"))
  (defvar path_home (concat org-directory "/home.org"))
  (defvar path_work (concat org-directory "/work.org"))
  (defvar path_personal (concat org-directory "/personal.org"))

  (setq org-default-notes-file path_inbox)
  (setq org-mobile-inbox-for-pull path_inbox)
  (setq org-mobile-files (list path_home path_work path_personal))
  (setq org-agenda-files (list path_home path_work path_personal))

  (setq org-capture-templates
        '(("t" "Todo-Personal" entry (file+headline path_personal "Personal Tasks:") "* TODO %?\n")
          ("h" "Todo-Home" entry (file+headline path_home "Home Tasks:") "* TODO %?\n")
          ("w" "Todo-Work" entry (file+headline path_work "Work Tasks:") "* TODO %?\n")
          ("j" "Todo" entry (file+headline path_inbox "Tasks" ) "* TODO %?\n")
          ))

  (setq org-refile-targets '((path_home :maxlevel . 1)
                             (path_work :maxlevel . 1)
                             (path_personal :maxlevel . 1)))

(when *is-a-win*
  (defvar path_sha1sum (concat user-emacs-directory "packages/sha1sum.exe"))
  (setq org-mobile-checksum-binary path_sha1sum))

(require 'url)

(defun insert-image-from-url (&optional url)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
           (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (calc . t)
   (sh . t)
   (python . t)
   (R . t)
   ))

(when *is-a-mac*
  (add-to-list 'org-latex-classes
               '("bjmarticle"
                 "\\documentclass{article}
                  \\usepackage[utf8]{inputenc}
                  \\usepackage[T1]{fontenc}
                  \\usepackage{graphicx}
                  \\usepackage{longtable}
                  \\usepackage{hyperref}
                  \\usepackage{natbib}
                  \\usepackage{amssymb}
                  \\usepackage{amsmath}
                  \\usepackage{geometry}
                  \\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(defun ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((before (downcase (or (thing-at-point 'word) "")))
        after)
    (call-interactively 'ispell-word)
    (setq after (downcase (or (thing-at-point 'word) "")))
    (unless (string= after before)
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table) before after))
      (message "\"%s\" now expands to \"%s\" %sally."
               before after (if p "loc" "glob"))))

(define-key ctl-x-map (kbd "C-i") 'ispell-word-then-abbrev)
(when *is-a-win*
   (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))

(setq ispell-personal-dictionary "~/.emacs.d/dictionary/")
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq ispell-program-name "aspell")

(use-package youdao-dictionary
  :ensure t
  :bind ("C-c d" . youdao-dictionary-search-at-point)
  :init (setq url-automatic-caching t))

(use-package try
  :ensure t)

(use-package pandoc-mode
  :ensure t)

(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Tsuen Wan"
                                "Tin Shui Wai"
                                "Hong Kong"))
  (setq wttrin-default-accept-language '("Accept-Language" . "zh-TW")))
