

(defcustom after-save-file-sync-regexps nil
  "A list of cons cells consisting of two strings. The `car' of
each cons cell is the regular expression matching the file(s)
that should be copied, and the `cdr' is the target directory."
  :group 'files
  :type '(repeat (cons string string)))

(defcustom after-save-file-sync-ask-if-overwrite nil
  "Ask the user before overwriting the destination file.
When set to a non-`nil' value, the user will be asked. When
`nil', the file will be copied without asking"
  :group 'files
  :type 'boolean)

(defun after-save-file-sync ()
  "Sync the current file if it matches one of the regexps.

This function will match each regexp in
`after-save-file-sync-regexps' against the current file name. If
there is a match, the current file will be copied to the
configured target directory.

If the file already exist target directory, the option
`after-save-file-sync-ask-if-overwrite' will control if the file
should be written automatically or if the user should be
presented with a question.

In theory, the same file can be copied to multiple target
directories, by configuring multiple regexps that match the same
file."

  (dolist (file-regexp after-save-file-sync-regexps)
    (when (string-match (car file-regexp) (buffer-file-name))
      (let ((directory (file-name-as-directory (cdr file-regexp))))
        (copy-file (buffer-file-name) directory (if after-save-file-sync-ask-if-overwrite 1 t))
        (message "Copied file to %s" directory)))))

(add-hook 'after-save-hook 'after-save-file-sync)
