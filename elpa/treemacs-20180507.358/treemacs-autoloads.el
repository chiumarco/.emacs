;;; treemacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "treemacs" "treemacs.el" (23280 34985 0 0))
;;; Generated autoloads from treemacs.el

(autoload 'treemacs "treemacs" "\
Initialize or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add.

\(fn)" t nil)

(autoload 'treemacs-bookmark "treemacs" "\
Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-find-file "treemacs" "\
Find and focus the current file in the treemacs window.
Will show/create a treemacs buffers if it is not visible/does not exist.
Only useful when `treemacs-follow-mode' is not active.

\(fn)" t nil)

(autoload 'treemacs-find-tag "treemacs" "\
Find and move point to the tag at point in the treemacs view.
Most likley to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root. If no treemacs buffer exists it will be created with the current file's
containing directory as root. Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file.

\(fn)" t nil)

(autoload 'treemacs-select-window "treemacs" "\
Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialize a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "treemacs-mode" "treemacs-mode.el" (23280 34985
;;;;;;  0 0))
;;; Generated autoloads from treemacs-mode.el

(autoload 'treemacs-mode "treemacs-mode" "\
A major mode for displaying the file system in a tree layout.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "treemacs-mouse-interface" "treemacs-mouse-interface.el"
;;;;;;  (23280 34985 0 0))
;;; Generated autoloads from treemacs-mouse-interface.el

(autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "\
Return source buffer or list of buffer and position for the current node.
This information can be used for future display. Stay in the selected window and
ignore any prefix argument.

\(fn &optional _)" t nil)

;;;***

;;;### (autoloads nil nil ("treemacs-async.el" "treemacs-branch-creation.el"
;;;;;;  "treemacs-compatibility.el" "treemacs-customization.el" "treemacs-faces.el"
;;;;;;  "treemacs-filewatch-mode.el" "treemacs-follow-mode.el" "treemacs-impl.el"
;;;;;;  "treemacs-interface.el" "treemacs-macros.el" "treemacs-persistence.el"
;;;;;;  "treemacs-pkg.el" "treemacs-structure.el" "treemacs-tag-follow-mode.el"
;;;;;;  "treemacs-tags.el" "treemacs-visuals.el" "treemacs-workspaces.el")
;;;;;;  (23280 34985 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; treemacs-autoloads.el ends here
