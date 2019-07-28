;;; ace-popup-menu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ace-popup-menu" "ace-popup-menu.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ace-popup-menu.el

(defvar ace-popup-menu-mode nil "\
Non-nil if Ace-Popup-Menu mode is enabled.
See the `ace-popup-menu-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-popup-menu-mode'.")

(custom-autoload 'ace-popup-menu-mode "ace-popup-menu" nil)

(autoload 'ace-popup-menu-mode "ace-popup-menu" "\
Toggle the `ace-popup-menu-mode' minor mode.

With a prefix argument ARG, enable `ace-popup-menu-mode' if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode is global. When it's active any call to
`x-popup-menu' will result in call of `ace-popup-menu'
instead. This function in turn implements a more efficient
interface to select an option from a list. Emacs Lisp code can
also use `ace-popup-menu' directly, in this case it will work OK
even if the mode is disabled.

\(fn &optional ARG)" t nil)

(autoload 'ace-popup-menu "ace-popup-menu" "\
Pop up a menu in a temporary window and return user's selection.

Argument POSITION is taken for compatibility and ignored unless
it's NIL, in which case this function has no effect.  For meaning
of MENU argument see documentation for `x-popup-menu'.

Every selectable item in the menu is labeled with a letter (or
two).  User can press a key corresponding to desired menu item
and he/she is done.

\(fn POSITION MENU)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-popup-menu" '("ace-popup-menu-show-pane-header")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-popup-menu-autoloads.el ends here
