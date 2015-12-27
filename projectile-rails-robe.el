(require 'robe)
(require 'projectile-rails)

(setq robe-port 24969)

(defvar projectile-rails-robe-port-hash-table (make-hash-table :test 'equal))

(defun projectile-rails-robe-get-port ()
  (let* ((projectile-rails-root (or (projectile-rails-root) (and inf-ruby-buffer (with-current-buffer (get-buffer inf-ruby-buffer) (projectile-rails-root)))))
         (port (gethash projectile-rails-root projectile-rails-robe-port-hash-table)))
    (or port
        (puthash projectile-rails-root
                 (1+ (if (hash-table-keys projectile-rails-robe-port-hash-table)
                         (apply 'max (hash-table-values projectile-rails-robe-port-hash-table))
                       robe-port)) projectile-rails-robe-port-hash-table))))

(defun robe-port ()
  (if (or (projectile-rails-root) (and inf-ruby-buffer (with-current-buffer (get-buffer inf-ruby-buffer) (projectile-rails-root))))
      (projectile-rails-robe-get-port)
    robe-port))

(defun run-pry()
  (interactive)
  (if (and (boundp 'inf-ruby-buffer)
           (equal inf-ruby-buffer (buffer-name)))
      (delete-window)
    (if (or (not (boundp 'inf-ruby-buffer))
            (not (comint-check-proc inf-ruby-buffer)))
        (rvm-use-default))
    (call-interactively 'inf-ruby)))

(defun run-pry-auto ()
  (if (projectile-rails-root)
      (projectile-rails-console)
    (run-pry)))

(defun get-ruby-buffer()
  (if (projectile-rails-root)
      (format "**%srailsconsole**" (projectile-project-name))
    (if (and inf-ruby-buffer (with-current-buffer (get-buffer inf-ruby-buffer) (projectile-rails-root)))
        inf-ruby-buffer)))

(defun robe-start (&optional force)
  "Start Robe server if it isn't already running.
When called with a prefix argument, kills the current Ruby
process, if any, and starts a new console for the current
project."
  (interactive "P")
  (let* ((ruby-buffer (get-ruby-buffer))
         (process (get-buffer-process ruby-buffer)))
    (when (or force (not process))
      (setq robe-running nil)
      (when process
        (delete-process process))
      (when (buffer-live-p ruby-buffer)
        (kill-buffer ruby-buffer))
      (if (or force
              (yes-or-no-p "No Ruby console running. Launch automatically?"))
          (let ((conf (current-window-configuration)))
            (run-pry-auto)
            (set-window-configuration conf))
        (error "Aborted"))))
  (when (not robe-running)
    (let* ((proc (inf-ruby-proc))
           started failed
           (comint-filter (process-filter proc))
           (tmp-filter (lambda (p s)
                         (cond
                          ((string-match-p "robe on" s)
                           (setq started t))
                          ((string-match-p "Error" s)
                           (setq failed t)))
                         (funcall comint-filter p s)))
           (script (format (mapconcat #'identity
                                      '("unless defined? Robe"
                                        "  $:.unshift '%s'"
                                        "  require 'robe'"
                                        "end"
                                        "Robe.start(%d)\n")
                                      ";")
                           robe-ruby-path (robe-port))))
      (unwind-protect
          (progn
            (set-process-filter proc tmp-filter)
            (comint-send-string proc script)
            (while (not started)
              (unless (process-live-p proc) (setq failed t))
              (when failed
                (ruby-switch-to-inf t)
                (error "Robe launch failed"))
              (accept-process-output proc))
            (set-process-sentinel proc #'robe-process-sentinel))
        (set-process-filter proc comint-filter)))
    (when (robe-request "ping") ;; Should be always t when no error, though.
      (setq robe-running t))))

(defun robe-request (endpoint &rest args)
  (declare (special url-http-end-of-headers))
  (let* ((url (format "http://127.0.0.1:%s/%s/%s" (robe-port) endpoint
                      (mapconcat (lambda (arg)
                                   (cond ((eq arg t) "yes")
                                         ((plusp (length arg))
                                          (url-hexify-string arg))
                                         (t "-")))
                                 args "/")))
         (response-buffer (robe-retrieve url)))
    (if response-buffer
        (prog1
            (with-current-buffer response-buffer
              (goto-char url-http-end-of-headers)
              (let ((json-array-type 'list))
                (json-read)))
          (kill-buffer response-buffer))
      (error "Server doesn't respond"))))

;;;###autoload
(define-minor-mode projectile-rails-robe-mode "separate robe server for every single rails project.")

(provide 'projectile-rails-robe)
