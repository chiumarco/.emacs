;;; treemacs-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "treemacs-projectile" "treemacs-projectile.el"
;;;;;;  (23280 34983 0 0))
;;; Generated autoloads from treemacs-projectile.el

(autoload 'treemacs-projectile "treemacs-projectile" "\
Add one of `projectile-known-projects' to the treemacs workspace.

\(fn)" t nil)

(autoload 'treemacs "treemacs-projectile" "\
Initialize or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; treemacs-projectile-autoloads.el ends here
