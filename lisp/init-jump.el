(defvar centaur-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local centaur-jump-handlers '()
  "List of jump handlers local to this buffer.

Jump handlers in this list has the highest priority. A jump
handler jump-handler can be registered by making this call
from a mode hook:

\(add-to-list 'centaur-jump-handlers 'jump-handler\)

Handler in this list is called first so only dynamic handlers like
`lsp' should use this one. Conventional jump handlers should use
`centaur-jump-handlers-MODE' instead.")

(defmacro define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.

This defines a variable `centaur-jump-handlers-MODE' to which
handlers can be added. MODE must be a major mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (handlers-list (intern (format "centaur-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`centaur-default-jump-handlers'.")
                  mode))
       (with-eval-after-load 'bind-map
         (centaur/set-leader-keys-for-major-mode ',mode
                                                 "gg" 'jump-to-definition
                                                 "gG" 'jump-to-definition-other-window)))))

(defun get-jump-handlers ()
  "Combine all jump handlers into a list.

They are in order: `centaur-jump-handlers',
`centaur-jump-handlers-MAJOR-MODE',
`centaur-default-jump-handlers'."
  (let ((handlers-major-mode-list (intern (format "centaur-jump-handlers-%S"
                                                  major-mode))))
    (append centaur-jump-handlers
            (if (boundp handlers-major-mode-list)
                (symbol-value handlers-major-mode-list)
              '())
            centaur-default-jump-handlers)))

(defun jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler (get-jump-handlers))
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (plist-get (cdr-safe -handler) :async)))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `centaur/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (jump-to-definition)))

;; ;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; ;; in an `eval-after-load' macro invocation because doing that prevents
;; ;; `describe-function' from correctly finding the source.
;; ;;
;; ;; See discussion on https://github.com/syl20bnr/centaur/pull/6771
;; (with-eval-after-load 'evil
;;   (evil-set-command-property 'jump-to-definition :jump t))

(provide 'init-jump)
