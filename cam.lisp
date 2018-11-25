;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot" "cl-js-generator"))

(defpackage #:cl-cam
  (:use #:cl #:hunchentoot #:cl-who #:cl-js-generator #:cl-fad))
(in-package #:cl-cam)



(setq cl-who:*attribute-quote-char* #\")

(start (make-instance 'easy-acceptor :port 8080))


(cl-js-generator::beautify-source
 `(do0
   (def has_get_user_media ()
     (and navigator.mediaDevices
	  navigator.mediaDevices.getUserMedia))
   (def greeting_callback ()
     (if (has_get_user_media)
	 (alert (string "cam"))
	 (alert (string "nocam"))))))

(define-easy-handler (code :uri "/code.js") ()
  (setf (content-type*) "text/javascript")
  (cl-js-generator::emit-js :code
			    `(do0
			     (def has-get-user-media ()
			       (and navigator.media-devices
				    navigator.media-devices.get-user-media))
			     (def greeting-callback ()
			       (if (has-get-user-media)
				   (alert "cam")
				   (alert "nocam")))
			     #+nil
			     (def got-devices (device-infos)
			       (loop for info in device-info do
				    (let ((option (document.create-element "option")))
				      (setf option.value info.deviceId)
				      (case info.kind
					("videoinput"
					 (setf option.text (or info.label
							       (+ "camera"
								  (+ 1 video-selesct))))
					 (video-select.append-child option))
					(t (console.log "found another kind of device")))))))
			    #+nil(do0
			     (defun get-stream ()
			       (when window.stream
				 (@ (window.stream.get-tracks)
				    (for-each (lambda (track) (track.stop)))))
			       (let ((constraints
				      (create video
					      (create device-id
						      (create exact
							      video-select.value)))))
				 (chain
				  ((@ navigator media-devices get-user-media) constraints)
				  (then got-stream)
				  (catch handle-error))))
			     (defun got-stream (stream)
			       (setf window.stream stream
				     video-element.src-object stream))
			     (defun handle-error (error)
			       (console.error "error: " error))
			     (let ((video (document.query-selector "video"))
				   (video-select (document.query-selector "select#videoSource"))
				   (constraints (create video t)))
			       (@ (navigator.media-devices.get-user-media constraints)
				  (then (lambda (stream)
					  (setf video.src-object stream))))
			       (@ (navigator.media-devices.enumerate-devices)
				  (then got-devices)
				  (then get-stream)
				  (catch handle-error))
			       (setf video-select.onchange get-stream)))))

(define-easy-handler (e :uri "/e") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "cam")
	    (:script :type "text/javascript"
		     :src "/code.js" ))
     (:body (:h2 "camera")
	    (:a :href "#" :onclick (ps (greeting-callback))
		"hello")
	    (:video)))))
