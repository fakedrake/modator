;;; modator.el
;; Provide some functionatily for the buffer - mode mapping like
;; killing buffers baed on modes.

(defvar mt-ignore-modes '(minibuffer-inactive-mode lisp-interaction-mode))

(defun mt-open-modes (&optional dont-ignore)
  "All open modes. If dont-ignore is non-nil ignore nothing."

  (let ((ignore (if dont-ignore nil mt-ignore-modes)))
    (delete-if
     (lambda (m) (memq (intern m) ignore))
     (delete-dups
      (mapcar (lambda (b) (symbol-name (mt-buffer-mode b)))
	      (buffer-list))))))

(defun mt-ibuffer ()
  "Read a mode then the buffer then switch to that buffer all
  using ido."
  (interactive (ido-completing-read "Buffer of mode: "
				    (mt-open-modes))))

(defmacro mt-buffer-mode (buf)
  "Get the major mode of the buffer."
  `(with-current-buffer ,buf major-mode))

(defun mt-mode-buffers (mode &optional dont-ignore)
  "Get all buffers of mode MODE. If dont-ignore is nil ignore the
`mt-ignore-modes' are ignored otherwise everything is ignored"
  (let ((md (if (stringp mode) (intern mode) mode)))
    (delete-if-not (lambda (b) (eq (mt-buffer-mode b) md))
		   (buffer-list))))

(defun mt-kill-all-mode (mode)
  "Kill all buffers of mode MODE."
  (interactive (list
		(ido-completing-read "Kill buffers of mode: "
				     (mt-open-modes))))
  (dolist (b (mt-mode-buffers mode))
    (kill-buffer b)))

(provide 'modator)
