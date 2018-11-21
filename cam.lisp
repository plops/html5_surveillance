;; https://common-lisp.net/project/parenscript/tutorial.html

(mapc #'ql:quickload '(:cl-fad :cl-who :hunchentoot :parenscript))

(defpackage #:cl-cam
  (:use #:cl #:hunchentoot #:cl-who #:parenscript #:cl-fad))
(in-package #:cl-cam)

(setq cl-who:*attribute-quote-char*ribute-quote-char* #\")

(start (make-instance 'easy-acceptor :port 8080))

(define-easy-handler (code :uri "/code.js") ()
  (setf (content-type*) "text/javascript")
  (ps (defun greeting-callback ()
	(alert "hello"))))

(define-easy-handler (e :uri "/e") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "cam")
	    (:script :type "text/javascript"
		     :src "/code.js" ))
     (:body (:h2 "camera")
	    (:a :href "#" :onclick (ps (greeting-callback))
		"hello")))))
