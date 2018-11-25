;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot" "cl-js-generator"))

#+nil
(progn(defpackage #:cl-cam
   (:use #:cl	    ;#:hunchentoot #:cl-who #:cl-js-generator #:cl-fad
	 ))
      (in-package #:cl-cam)
      (setf (readtable-case *readtable*) :invert))

(in-package #:cl-js-generator)

(setq cl-who:*attribute-quote-char* #\")
(setf cl-who::*html-mode* :html5)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))

(cl-js-generator::test)

(let ((script-str
       (;cl-js-generator::emit-js :code ;
	cl-js-generator::beautify-source
	`(let ((player (document.getElementById (string "player")))
	       (log (document.getElementById (string "log"))))
	   (def logger (message)
	     (if (== (string "object") (typeof message))
		 (incf log.innerHTML
		       (+ (? (and JSON
				  JSON.stringify)
			     (JSON.stringify message)
			     message)
			  (string "<br />")))
		 (incf log.innerHTML
		       (+ message
			  (string "<br />")))))
	   (def handle_success (stream)
	     (logger (string "success!"))
	     (setf player.srcObject stream))
	   (logger (string "getUserMedia ..."))
	   
	   
	   (dot
	    (navigator.mediaDevices.enumerateDevices)
	    (then  (lambda (devices)
		     
		     (setf devices
			     (devices.filter
			      (lambda (d)
				(return (===
					 (string "videoinput")
					 d.kind)))))
		     (logger devices)
		     (return devices)))
	    (then
	     (lambda (devices)
	      (dot
	       (navigator.mediaDevices.getUserMedia
		:audio true
		:video (dict (deviceId (dot (aref devices 0)
					    deviceId))))
	       (then handle_success)))))))))
  (format t "~&~a~%" script-str)
 (hunchentoot:define-easy-handler (e :uri "/index.html") ()
   (cl-who:with-html-output-to-string (s)
     (:html
      (:head (:title "cam"))
      (:body (:h2 "camera")
	     (:div :id "log")
	     (:video :id "player" :controls t)
	     (:script :type "text/javascript"
		      (princ script-str s)
		      ))))))

