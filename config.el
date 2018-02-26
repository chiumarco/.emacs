
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

(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'leuven t)

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

(line-number-mode 1)
(column-number-mode 1)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")

(display-time-mode 1)

(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode))

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
              ([f9] . treemacs-projectile)))

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
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50))

;(setq org-ellipsis " ")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)

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

(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq ispell-program-name "aspell")

(use-package youdao-dictionary
  :ensure t
  :bind ("C-c d" . youdao-dictionary-search-at-point)
  :init (setq url-automatic-caching t))
