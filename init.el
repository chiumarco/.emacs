;; Get obsolete package backtrace when it loads
;; (defun debug-on-load-obsolete (filename)
;;   (when (equal (car (last (split-string filename "[/\\]") 2))
;;                "obsolete")
;;     (debug)))
;; (add-to-list 'after-load-functions #'debug-on-load-obsolete)


;; Warn when opening files bigger than 500MB
(setq large-file-warning-threshold (* 500 1024 1024))

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 100 1024 1024))


;; Initialize package and add Melpa source
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


;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))


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
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" default)))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elfeed-feeds
   (quote
    (("http://feeds2.feedburner.com/unwirelife" tech)
     ("https://sspai.com/feed" tech)
     ("http://pragmaticemacs.com/feed/" emacs)
     ("http://planet.emacsen.org/atom.xml" emacs))))
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(fci-rule-color "#5B6268")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(package-selected-packages
   (quote
    (writeroom-mode elfeed-org elfeed engine-mode minions elpy org-ref esup exec-path-from-shell helpful deft org-mime org-mine doom-themes all-the-icons-ivy ivy-rich typit auto-package-update aggressive-indent tangotango-theme monokai-theme monokai sx google-this org-plus-contrib atomic-chrome flyspell-correct-ivy dracula-theme yasnippet-snippets yasnippet mu4e-alert pangu-spacing dashboard projectile spaceline markdown-mode pandoc-mode magit moe-theme company company-mode compnay-mode leuven-theme color-theme-sanityinc-tomorrow ibuffer-vc)))
 '(pdf-tools-handle-upgrades nil)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
