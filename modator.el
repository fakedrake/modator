;;; modator.el
;; Provide some functionatily for the buffer - mode mapping like
;; killing buffers baed on modes.

(defun mt-open-modes ()
  "All open modes."
  (delete-dups
   (mapcar (lambda (x) (symbol-name (with-current-buffer x major-mode)))
	   (buffer-list))))

(defmacro mt-buffer-mode (buf)
  "Get the major mode of the buffer."
  `(with-current-buffer ,buf major-mode))

(defun mt-mode-buffers (mode)
  "Get all buffers of mode MODE."
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
