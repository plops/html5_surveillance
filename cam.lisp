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
	`(let ((player (document.getElementById (string "player"))))
	   (def handle_success (stream)
	     (setf player.srcObject stream))
	   (dot
	    (navigator.mediaDevices.getUserMedia :audio false
						 :video true)
	    (then handle_success))
	   
	   (dot
	    (navigator.mediaDevices.enumerateDevices)
	    (then  (lambda (devices)
		     (return
		       (setf devices
			     (devices.filter
			      (lambda (d)
				(return (===
					 (string "videoinput")
					 d.kind)))))))))))))
  (format t "~&~a~%" script-str)
 (hunchentoot:define-easy-handler (e :uri "/index.html") ()
   (cl-who:with-html-output-to-string (s)
     (:html
      (:head (:title "cam")
	     )
      (:body (:h2 "camera")
	     (:video :id "player" :controls t)
	     (:script :type "text/javascript"
		      (princ script-str s)
		      ))))))

