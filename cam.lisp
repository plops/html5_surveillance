;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" "hunchentoot"
		       "cl-js-generator" "cl-cpp-generator"))




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
;; cd /tmp; openssl req -new -x509 -nodes -out server.crt -keyout server.key
(hunchentoot:start *ssl-acceptor*)




(defun fill-buffer (name &key (type "Float32") data1d (access "gl.STATIC_DRAW")) 
  `(let-g ((,name (gl.createBuffer)))
	  (let ((vec ,data1d))
	    (gl.bindBuffer gl.ARRAY_BUFFER ,name)
	    (gl.bufferData gl.ARRAY_BUFFER
			   (,(format nil "new ~aArray" type)
			    vec) ,access))))

#+nil
(fill-buffer "buffer" :data1d '(list 1 2 3))

(defun bind-attribute (name &key
			      (type "Float32")
			      (gl-type "gl.Float")
			      (offset 0)
			      (size 1)
			      (stride 0) ;; in elements
			      (buffer "buffer")
			      (program "program")
			      )
  (let ((loc (format nil "~a_attribute_location" name)))
    `(do0
      (let-g ((,loc (gl.getAttribLocation ,program (string
						    ,name)))))
      
      (do0
       (gl.enableVertexAttribArray ,loc)
       (gl.bindBuffer gl.ARRAY_BUFFER ,buffer)
       (let ((size ,size)
	     (type ,gl-type)
	     (normalize false)
	     (element_size ,(format
			  nil "~aArray.BYTES_PER_ELEMENT" type))
	     (stride (* ,stride

			element_size))  
	     (offset (* ,offset
			element_size)))
	 (gl.vertexAttribPointer
	  ,buffer size type normalize stride offset))))))

#+nil
(bind-attribute "uv" :size 4 :stride 4)

(progn
  #.(in-package #:cl-cpp-generator)

  (defparameter *vertex-shader*
    (cl-cpp-generator::beautify-source
     `(with-compilation-unit
	  (raw "#version 100")
	(decl ((uv :type "attribute vec2")
	       (pos :type "attribute vec2")
	       (texCoords :type "varying vec2")))
	(function (main () "void")
		  (setf gl_Position (funcall vec4 pos 0 1)
			texCoords uv)))))

  (defparameter *fragment-shader*
    (cl-cpp-generator::beautify-source
     `(with-compilation-unit
	  (raw "#version 100")
	(raw "precision mediump float;")
	(decl (;(pixelCoords :type "varying vec2")
					;(textureSize :type "uniform vec2")
	       (texCoords :type "varying vec2")
	       (textureSampler :type "uniform sampler2D")))
	(function (main () "void")
		  (let (#+nil (c :type "mediump vec4" :init (funcall vec4 1 0
								     ".5" 1))
			      #+nil (textureCoords :type vec2 :init #+nil (/ pixelCoords
									     textureSize)
						   
						   )
			      (textureColor :type vec4 :init (funcall
							      texture2D textureSampler texCoords)))
		    (setf gl_FragColor textureColor))))))
  (format t "~a" *fragment-shader*)

  

  #.(in-package #:cl-js-generator)

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
		    (			; statement
		     let-g ((shader (gl.createShader type)))
					;(var-decl shader (gl.createShader type))
		     (logger (string "create_shader .."))
		     (logger type)
		     (gl.shaderSource shader source)
		     (gl.compileShader shader)
		     (let-g ((success (gl.getShaderParameter shader
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
		    (let-g ((program (gl.createProgram)))
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
		  (def get_context ()
		    (logger (string "get_context .."))
		    (let-g ((canvas (document.getElementById (string "c")))
			    (gl (canvas.getContext (string "webgl"))))
			   (if (not gl)
			       (logger (string "error: no gl available"))
			       (return gl))))
		  (def get_video ()
		    (logger (string "get_video .."))
		    (return (document.getElementById (string
						      "video"))))
		  (def startup ()
		    (logger (string "startup .."))
		    (let-g ((gl (get_context))
			    (video (get_video)))
			   (let-g ((vertex_shader (create_shader gl gl.VERTEX_SHADER
								 (dot (document.getElementById
								       (string
									"2d-vertex-shader"))
								      text)))
				   (fragment_shader (create_shader gl gl.FRAGMENT_SHADER
								   (dot (document.getElementById
									 (string
									  "2d-fragment-shader"))
									text)))
				   (program (create_program gl vertex_shader
							    fragment_shader))
				   )
				  (logger (string "fill-buffer"))
				  ,(fill-buffer
				   "buffer" :data1d
				   '(list 0 0 -1 -1
				     0 1 -1  1
				     1 1  1  1
				     1 0  1 -1))
				  

				  (do0
					;; https://webglfundamentals.org/webgl/lessons/webgl-anti-patterns.html
					(gl.viewport 0 0
						     gl.drawingBufferWidth
						     gl.drawingBufferHeight)
					;(setf aspect (/ gl.canvas.clientWidth gl.canvas.clientHeight) )
					(gl.clearColor .9 .8 .7 1)
					(gl.clear gl.COLOR_BUFFER_BIT)
					(gl.useProgram program)
					#+nil (gl.texImage2D gl.TEXTURE_2D 0
						       gl.RGBA gl.UNSIGNED_BYTE
						       video))
				  (logger (string "bind-attributes"))
				  ,(bind-attribute "uv" :size 4 :stride 4)
				  ,(bind-attribute "pos" :size 4
						  :stride 4 :offset 2)
				  (let ((primitive_type gl.TRIANGLE_FAN)
					(offset 0)
					(count 4))
				    (gl.drawArrays primitive_type offset count))
				  ))
		    )
		  (window.addEventListener (string "load")
					     startup false)
		  ))))
					;(format t "/home/martin/&~a~%" script-str)

    (hunchentoot:define-easy-handler (securesat :uri "/secure" :acceptor-names '(ssl)) ()
      (cl-who:with-html-output-to-string (s)
	(:html
	 (:head (:title "cam"))
	 (:body (:h2 "camera")
		(:div :id "log")
		(:video :id "player" :controls t :width 320 :height 240)
		(:canvas :id "c" :width 320 :height 240)
		(:script :id (string "2d-vertex-shader")  :type "notjs"
			 (princ  cl-cpp-generator::*vertex-shader*
				 s))
		(:script :id (string "2d-fragment-shader") :type "notjs"
			 (princ
			  cl-cpp-generator::*fragment-shader* s))
		(:script :type "text/javascript"
			 (princ script-str s)
			 )))))))




#+nil
((let-g ((position_attribute_location (gl.getAttribLocation
							    program (string
								     "a_position")))
			      (position_buffer (gl.createBuffer)))
			     (gl.bindBuffer gl.ARRAY_BUFFER position_buffer)
			     (let-g ((positions (list -1 -1
						      -1 1
						      1 1
						      1 -1)))
				    (gl.bufferData gl.ARRAY_BUFFER
						   ("new Float32Array" positions)
						   gl.STATIC_DRAW)))
		      (let-g ((uv_attribute_location (gl.getAttribLocation
						      program (string
							       "uv")))
			      (uv_buffer (gl.createBuffer)))
			     (gl.bindBuffer gl.ARRAY_BUFFER uv_buffer)
			     (let-g ((uvs (list 0 0
						0 1
						1 1
						1 0)))
				    (gl.bufferData gl.ARRAY_BUFFER
						   ("new Float32Array" uvs)
						   gl.STATIC_DRAW))))

#+nil
(do0
		       (gl.enableVertexAttribArray
			position_attribute_location)
		      
		       (gl.bindBuffer gl.ARRAY_BUFFER position_buffer)
		       (let ((size 2)
			     (type gl.FLOAT)
			     (normalize false)
			     (stride 0) ;; (* 2 Float32Array.BYTES_PER_ELEMENT)
			     (offset 0))
			 (gl.vertexAttribPointer
			  position_attribute_location
			  size type normalize stride offset)))
