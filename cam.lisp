;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot" "cl-js-generator"))

(defpackage #:cl-cam
  (:use #:cl ;#:hunchentoot #:cl-who #:cl-js-generator #:cl-fad
	))
(in-package #:cl-cam)



(setq cl-who:*attribute-quote-char* #\")
(setf cl-who::*html-mode* :html5)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))

(cl-js-generator::test)

(cl-js-generator::beautify-source
 `(dot
  (navigator.mediaDevices.enumerateDevices)
  (then (lambda (devices)
	  (return
	    (setf devices
		  (devices.filter
		   (lambda (d)
		     (const-decl bla 3)
		     (return (===
			      (+ 1 (string "videoinput"))
			      (- 3 d.kind)))))))))))

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
	    (then (lambda (devices)
		    (return
		     (setf devices
			   (devices.filter
			    (lambda (d)
			      (const-decl bla 3)
			      (return (===
				       (+ 1 (string "videoinput"))
				      (- 3 d.kind))))))))))))))
  (format t "~&~a~%" script-str)
 (define-easy-handler (e :uri "/index.html") ()
   (with-html-output-to-string (s)
     (:html
      (:head (:title "cam")
	     )
      (:body (:h2 "camera")
	     (:video :id "player" :controls t)
	     (:script :type "text/javascript"
		      (princ script-str s)
		      ))))))

