;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot"
		       "cl-js-generator" "cl-cpp-generator"))

(in-package #:cl-cpp-generator)

(defparameter *vertex-shader*
 (cl-cpp-generator::beautify-source
  `(with-compilation-unit
       (raw "#version 300")
       (raw "attribute vec4 a_position;")
     (function (main () "void")
	       (setf gl_Position a_position)))))

(defparameter *fragment-shader*
  (cl-cpp-generator::beautify-source
   `(with-compilation-unit
	(raw "#version 300")
      (raw "precision mediump float;")
      (raw "out vec4 fragColor")
     (function (main () "void")
	       (setf fragColor (funcall vec4 1 0 ".5" 1))))))

(cl-cpp-generator::beautify-source
  `(with-compilation-unit
       (raw "precision mediump float;")
     (function (main () "void")
	       (setf gl_FragColor (funcall vec4 1 0 ".5" 1)))))

(in-package #:cl-js-generator)



cl-cpp-generator::*frag-shader*

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
		:video (dict (deviceId (dot (aref devices 0)
					    deviceId))))
	       (then handle_success)))))
	   (def create_shader (gl type source)
	     (let ((shader (gl.createShader type)))
	       (gl.shaderSource shader source)
	       (gl.compileShader shader)
	       (if (gl.getShaderParameter shader gl.COMPILE_STATUS)
		   (return shader)
		   (do0
		    (logger (gl.getShaderInfoLog shader))
		    (gl.deleteShader shader)))))
	   (def create_program (gl vertex_shader fragment_shader)
	     (let ((program (gl.createProgram))
		   )
	       (gl.attachShader program vertex_shader)
	       (gl.attachShader program fragment_shader)
	       (gl.linkProgram program)
	       (if (gl.getProgramParameter program gl.LINK_STATUS)
		   (return program)
		   (do0
		    (logger (gl.getProgramInfoLog program)
			    )
		    (gl.deleteProgram program)))))
	   (let ((canvas (document.getElementById (string "c")))
		 (gl (canvas.getContext (string "webgl")))
		 (vertex_shader (create_shader gl gl.VERTEX_SHADER
					      (dot (document.getElementById
						    (string
						     "2d-vertex-shader")
						    )
						   text)))
		 (fragment_shader (create_shader gl gl.VERTEX_SHADER
					      (dot (document.getElementById
						    (string
						     "2d-fragment-shader")
						    )
						   text)))
		 (program (create_program gl vertex_shader fragment_shader))))))))
  (format t "~&~a~%" script-str)
 (hunchentoot:define-easy-handler (e :uri "/index.html") ()
   (cl-who:with-html-output-to-string (s)
     (:html
      (:head (:title "cam"))
      (:body (:h2 "camera")
	     (:div :id "log")
	     (:video :id "player" :controls t)
	     (:canvas :id "c")
	     (:script :id (string "2d-vertex-shader")  :type "notjs"
			     (princ  cl-cpp-generator::*vertex-shader*
			     s))
	     (:script :id (string "2d-fragment-shader") :type "notjs"
			     (princ
			     cl-cpp-generator::*fragment-shader* s))
	     (:script :type "text/javascript"
		      (princ script-str s)
		      ))))))

