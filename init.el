;; Warn when opening files bigger than 500MB
(setq large-file-warning-threshold (* 500 1024 1024))

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; ;; Make sure Org is installed
;; (unless (package-installed-p 'org)
;;   (package-refresh-contents)
;;   (package-install 'org))

;; (use-package org
;;   :ensure t
;;   :init
;;   ;; stuff you want done before org loaded
;;   :config
;;   ;; stuff  you want doen after org loaded
;;   )

(use-package org
  :ensure org-plus-contrib
  :init
  ;; stuff you want done before org loaded
  :config
  ;; stuff you want doen after org loaded
  )


(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Garbage collector - decrease threshold to 100 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" default)))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(package-selected-packages
   (quote
    (sx google-this org-plus-contrib atomic-chrome flyspell-correct-ivy dracula-theme yasnippet-snippets yasnippet mu4e-alert pangu-spacing dashboard projectile spaceline markdown-mode pandoc-mode magit moe-theme company company-mode compnay-mode leuven-theme color-theme-sanityinc-tomorrow ibuffer-vc)))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
