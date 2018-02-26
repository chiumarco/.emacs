;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold (* 100 1024 1024))

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 100 1024 1024))

;; User Info
(setq user-full-name "Marco Chiu")
(setq user-mail-address "chiumarco@gmail.com")

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


(use-package try
  :ensure t)

(use-package which-key
  :ensure t 
  :config
  (which-key-mode))


(use-package pandoc-mode
  :ensure t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; ======================================================
;; Make window status undo-able
;; ======================================================
(require 'winner)
(winner-mode 1)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" default)))
 '(package-selected-packages
   (quote
    (dashboard projectile spaceline markdown-mode pandoc-mode magit moe-theme company company-mode compnay-mode leuven-theme color-theme-sanityinc-tomorrow ibuffer-vc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (require 'init-themes)
;; (require 'init-ispell)
;; (require 'init-Org)




;; (use-package moe-theme
;;   :ensure t
;;   )


(use-package counsel
  :ensure t
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

(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode)
  )


;; (require 'whitespace)
;; (setq whitespace-line-column 80) ;; limit line length
;; (setq whitespace-style '(face lines-tail))
;; (add-hook 'prog-mode-hook 'whitespace-mode)


(global-set-key (kbd "M-[") 'windmove-up)
(global-set-key (kbd "M-/") 'windmove-down)
(global-set-key (kbd "M-'") 'windmove-right)
;; (global-set-key (kbd "M-;") 'windmove-left)
(global-set-key (kbd "M-:") 'comment-line)

;; make cursor the width of the character it is under
;; i.e. full width of a TAB
(setq x-stretch-cursor t)

