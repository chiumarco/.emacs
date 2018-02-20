;; This file is used to customize the interface of Emacs

;; Default theme.

(load-theme 'leuven t)

;;(add-hook 'prog-mode-hook 'linum-mode)

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  ;; (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (load-theme 'leuven t)
  (reapply-themes)
  )

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  ;; (setq custom-enabled-themes '(sanityinc-tomorrow-night))
  (load-theme 'leuven-dark t)
  (reapply-themes)
  )

;; Set front size, the value is in 1/10pt, so 160=16pt
(when *is-a-mac*
  (set-face-attribute 'default nil :height 160))

(when *is-a-win*
  (set-face-attribute 'default nil :height 120)
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
  )
  
;; (setq fonts
;;       (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
;;             ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
;;             ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))


;; (defvar emacs-english-font "SF Pro Text"
;;   "The font name of English.")

;; (defvar emacs-cjk-font "Heiti TC"
;;   "The font name for CJK.")

;; (defvar emacs-font-size-pair '(16 . 19)
;;   "Default font size pair for (english . chinese)")

;; (defvar emacs-font-size-pair-list
;;   '(( 5 .  6) (9 . 10) (10 . 12) (12 . 14)
;;     (13 . 16) (15 . 18) (16 . 19) (17 . 20)
;;     (19 . 22) (20 . 24) (21 . 26) (24 . 28)
;;     (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44))
;;   "This list is used to store matching (englis . chinese) font-size.")

;; (defun font-exist-p (fontname)
;;   "Test if this font is exist or not."
;;   (if (or (not fontname) (string= fontname ""))
;;       nil
;;     (if (not (x-list-fonts fontname)) nil t)))

;; (defun set-font (english chinese size-pair)
;;   "Setup emacs English and Chinese font on x window-system."

;;   (if (font-exist-p english)
;;       (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

;;   (if (font-exist-p chinese)
;;       (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;         (set-fontset-font (frame-parameter nil 'font) charset
;;                           (font-spec :family chinese :size (cdr size-pair))))))

;; ;; Setup font size based on emacs-font-size-pair
;; (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

;; (defun emacs-step-font-size (step)
;;   "Increase/Decrease emacs's font size."
;;   (let ((scale-steps emacs-font-size-pair-list))
;;     (if (< step 0) (setq scale-steps (reverse scale-steps)))
;;     (setq emacs-font-size-pair
;;           (or (cadr (member emacs-font-size-pair scale-steps))
;;               emacs-font-size-pair))
;;     (when emacs-font-size-pair
;;       (message "emacs font size set to %.1f" (car emacs-font-size-pair))
;;       (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

;; (defun increase-emacs-font-size ()
;;   "Decrease emacs's font-size acording emacs-font-size-pair-list."
;;   (interactive) (emacs-step-font-size 1))

;; (defun decrease-emacs-font-size ()
;;   "Increase emacs's font-size acording emacs-font-size-pair-list."
;;   (interactive) (emacs-step-font-size -1))

;; (global-set-key (kbd "C-=") 'increase-emacs-font-size)
;; (global-set-key (kbd "C--") 'decrease-emacs-font-size)



(provide 'init-themes)
