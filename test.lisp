
;; cd /tmp; openssl req -new -x509 -nodes -out server.crt -keyout server.key

(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot"
		       "cl-js-generator" "cl-cpp-generator"))

(defpackage :web (:use :cl :hunchentoot))
(in-package :web)

;; This url can be accessed by all acceptors
(define-easy-handler (no-ssl :uri "/normal") ()
  (setf (content-type*) "text/plain")
  "NORMAL PAGE")

;; This url can be accessed only by an acceptor named SSL
(define-easy-handler (ssl :uri "/secure" :acceptor-names '(ssl)) ()
  (setf (content-type*) "text/plain")
  "SECURED PAGE")

(defvar *no-ssl-acceptor*
  (make-instance 'easy-acceptor :port 8080))

(defvar *ssl-acceptor*
  (make-instance 'easy-ssl-acceptor
                 :name 'ssl
                 :port 7777
		 :ssl-privatekey-file  #P"/tmp/server.key"
                 :ssl-certificate-file #P"/tmp/server.crt"
		 ;:ssl-privatekey-file #P"/etc/letsencrypt/live/cheapnest.org/privkey.pem"	 :ssl-certificate-file #P"/etc/letsencrypt/live/cheapnest.org/fullchain.pem"
		      ))


(start *ssl-acceptor*)
(start *no-ssl-acceptor*)
