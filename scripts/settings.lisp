;;; Settings for Ichiran

(in-package :ichiran)

;; Database connection settings
(defparameter *connection*
  (list :host (or (uiop:getenv "DB_HOST") "localhost")
        :port (parse-integer (or (uiop:getenv "DB_PORT") "5432"))
        :database-name (or (uiop:getenv "DB_NAME") "ichiran")
        :username (or (uiop:getenv "DB_USER") "ichiran")
        :password (or (uiop:getenv "DB_PASSWORD") "ichiran_password")))

;; Dictionary data paths
(defparameter *jmdict-path* "/app/data/JMdict_e")
(defparameter *kanjidic-path* "/app/data/kanjidic2.xml")

;; Web server settings (if needed)
(defparameter *web-server-port* 4242)

;; Logging settings
(defparameter *debug* t)

(format t "Ichiran settings loaded:~%")
(format t "  Database host: ~a~%" (getf *connection* :host))
(format t "  Database port: ~a~%" (getf *connection* :port))
(format t "  Database name: ~a~%" (getf *connection* :database-name))
(format t "  JMdict path: ~a~%" *jmdict-path*)
(format t "  Kanjidic path: ~a~%" *kanjidic-path*)
