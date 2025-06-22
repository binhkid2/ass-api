;;; Setup script for Ichiran
(require :asdf)

;; Load Quicklisp
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Load ichiran
(handler-case
    (progn
      (format t "Loading ichiran...~%")
      (ql:quickload :ichiran)
      (format t "Ichiran loaded successfully.~%"))
  (error (e)
    (format t "Error loading ichiran: ~a~%" e)))

;; Load settings
(handler-case
    (progn
      (format t "Loading settings...~%")
      (load "/app/settings.lisp")
      (format t "Settings loaded.~%"))
  (error (e)
    (format t "Error loading settings: ~a~%" e)))

;; Initialize database connection and data
(handler-case
    (progn
      (format t "Initializing database connection...~%")
      (ichiran:setup-ichiran)
      (format t "Database initialized.~%"))
  (error (e)
    (format t "Error initializing database: ~a~%" e)))

;; Keep the process running
(format t "Ichiran setup complete. Keeping process alive...~%")
