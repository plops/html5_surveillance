;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '(:cl-fad :cl-who :hunchentoot :parenscript))

(defpackage #:cl-cam
  (:use #:cl #:hunchentoot #:cl-who #:parenscript #:cl-fad))
(in-package #:cl-cam)

(setq cl-who:*attribute-quote-char*ribute-quote-char* #\")

(start (make-instance 'easy-acceptor :port 8080))

(define-easy-handler (code :uri "/code.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (defun has-get-user-media ()
      (and navigator.media-devices
	   navigator.media-devices.get-user-media))
    (defun greeting-callback ()
      (if (has-get-user-media)
	  (alert "cam")
	  (alert "nocam")))
    (defun got-devices (device-infos)
      (loop for info in device-info do
	   (let ((option (document.create-element "option")))
	     (setf option.value info.deviceId)
	     (ecase info.kind
	       ("videoinput"
		(setf option.text (or info.label
				      (+ "camera"
					 (+ 1 video-selesct))))
		(video-select.append-child option))
	       (t (console.log "found another kind of device"))))))
    (defun get-stream ()
      (when window.stream
	(chain (window.stream.get-tracks)
	       (for-each (lambda (track) (track.stop))))))
    (defun got-stream (stream)
      (setf window.stream stream
	    video-element.src-object stream))
    (defun handle-error (error)
      (console.error "error: " error))
    (let ((video (document.query-selector "video"))
	  (video-select (document.query-selector "select#videoSource"))
	  (constraints (create video t)))
      (chain (navigator.media-devices.get-user-media constraints)
	     (then (lambda (stream)
		     (setf video.src-object stream))))
      (chain (navigator.media-devices.enumerate-devices)
	     (then got-devices)
	     (then get-stream)
	     (catch handle-error))
      (setf video-select.onchange get-stream))))

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
