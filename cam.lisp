;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot"
		       "cl-js-generator" "cl-cpp-generator"))

(in-package #:cl-cpp-generator)

(defparameter *vertex-shader*
  (cl-cpp-generator::beautify-source
   `(with-compilation-unit
	;(raw "#version 100")
      (raw "attribute vec4 a_position;")
      (function (main () "void")
		(setf gl_Position a_position)))))

(defparameter *fragment-shader*
  (cl-cpp-generator::beautify-source
   `(with-compilation-unit
	;(raw "#version 100")
      (raw "precision mediump float;")
      (function (main () "void")
		(let ((c :type "mediump vec4" :init (funcall vec4 1 0 ".5" 1)))
		  (setf gl_FragColor c))))))


(in-package #:cl-js-generator)



(setq cl-who:*attribute-quote-char* #\")
(setf cl-who::*html-mode* :html5)


(defvar *ssl-acceptor*
  (make-instance 'hunchentoot:easy-ssl-acceptor
                 :name 'ssl
                 :port 7777
		 :ssl-privatekey-file  #P"/tmp/server.key"
                 :ssl-certificate-file #P"/tmp/server.crt"
		 ;:ssl-privatekey-file #P"/etc/letsencrypt/live/cheapnest.org/privkey.pem"	 :ssl-certificate-file #P"/etc/letsencrypt/live/cheapnest.org/fullchain.pem"
		      ))

(hunchentoot:start *ssl-acceptor*)

(let ((script-str
       (			     ;cl-js-generator::emit-js :code ;
	cl-js-generator::beautify-source
	`(let-g ((player (document.getElementById (string "player")))
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
	     (				; statement
	      let ((shader (gl.createShader type)))
					;(var-decl shader (gl.createShader type))
	      (logger (string "create_shader .."))
	      (logger type)
	      (gl.shaderSource shader source)
	      (gl.compileShader shader)
	      (let ((success (gl.getShaderParameter shader
						    gl.COMPILE_STATUS)))
		(logger success)
		(if success
		    (do0
		     (logger source)
		     (logger shader)
		     (return shader))
		    (do0
		     (logger (string "error create_shader:"))
		     (logger (gl.getShaderInfoLog shader))
		     (gl.deleteShader shader))))))
	   (def create_program (gl vertex_shader fragment_shader)
	     (let ((program (gl.createProgram)))
	       (logger (string "create_program .."))
	       
	       
	       (logger vertex_shader)
	       (gl.attachShader program vertex_shader)
	       (gl.attachShader program fragment_shader)
	       (gl.linkProgram program)
	       (if (gl.getProgramParameter program gl.LINK_STATUS)
		   (return program)
		   (do0
		    (logger (gl.getProgramInfoLog program))
		    (gl.deleteProgram program)))))
	   (let ((canvas (document.getElementById (string "c")))
		 (gl (canvas.getContext (string "webgl")))
		 (vertex_shader (create_shader gl gl.VERTEX_SHADER
					       (dot (document.getElementById
						     (string
						      "2d-vertex-shader")
						     )
						    text)))
		 (fragment_shader (create_shader gl gl.FRAGMENT_SHADER
						 (dot (document.getElementById
						       (string
							"2d-fragment-shader")
						       )
						      text)))
		 (program (create_program gl vertex_shader
					  fragment_shader))
		 (positon_attribute_location (gl.getAttributeLocation
					      program (string
						       "a_position")))
		 (position_buffer (gl.createBuffer)))
	     (gl.bindBuffer gl.ARRAY_BUFFER positionBuffer)
	     (let ((positions (list 0 0 0 ".5" ".7" 0)))
	       (gl.bufferData gl.ARRAY_BUFFER
			      (new (Float32Array positions))
			      gl.STATIC_DRAWxs)))))))
					;(format t "~&~a~%" script-str)

  (hunchentoot:define-easy-handler (securesat :uri "/secure" :acceptor-names '(ssl)) ()
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




