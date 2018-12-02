;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" 
		       "clack" "websocket-driver"
		       "cl-js-generator" "cl-cpp-generator"))

(in-package #:cl-js-generator)


(setq cl-who:*attribute-quote-char* #\")
(setf cl-who::*html-mode* :html5)

;; for development in firefox:
;; about:config network.websocket.allowInsecureFromHTTPS true

(defparameter *wss-port* 7778)
(defparameter *ssl-port* 7777)

(defun handler (env)
  '(200 nil ("hello world")))

(defparameter *clack-server* (clack:clackup (lambda (env)
					      (funcall 'handler env))
					    :port *ssl-port*
					     :use-default-middlewares nil))



(defparameter *echo-server*
  (lambda (env)
    (let ((ws (websocket-driver:make-server env )))
      (websocket-driver:on :message ws
			   (lambda (message) (websocket-driver:send ws message)))
      (lambda (responder)
	(websocket-driver:start-connection ws)))))


(clack:clackup *echo-server* :port *wss-port*)

;; (defvar *wss-acceptor*
;;   (make-instance ; 'hunchensocket:websocket-ssl-acceptor
;; 		 'hunchensocket:websocket-acceptor
;; 		 :name 'wss
;;                  :port *wss-port*
;; 		 ;:ssl-privatekey-file  #P"/tmp/server.key"
;;                  ;:ssl-certificate-file #P"/tmp/server.crt"
;; 		 ))
;; (hunchentoot:start *wss-acceptor*)
;; (defvar *ssl-acceptor*
;;   (make-instance 'hunchentoot:easy-ssl-acceptor
;;                  :name 'ssl
;;                  :port *ssl-port*
;; 		 :ssl-privatekey-file  #P"/tmp/server.key"
;;                  :ssl-certificate-file #P"/tmp/server.crt"
;; 		 ;:ssl-privatekey-file #P"/etc/letsencrypt/live/cheapnest.org/privkey.pem"	 :ssl-certificate-file #P"/etc/letsencrypt/live/cheapnest.org/fullchain.pem"
;; 		      ))
;; ;; cd /tmp; openssl req -new -x509 -nodes -out server.crt -keyout server.key
;; (hunchentoot:start *ssl-acceptor*)






(defun fill-buffer (name &key (type "Float32") data1d (usage-hint "gl.STATIC_DRAW")) 
  `(let-g ((,name (gl.createBuffer)))
	  (let ((vec ,data1d))
	    (gl.bindBuffer gl.ARRAY_BUFFER ,name)
	    (gl.bufferData gl.ARRAY_BUFFER
			   (,(format nil "new ~aArray" type)
			    vec) ,usage-hint))))

; (fill-buffer "buffer" :data1d '(list 1 2 3))

(defun bind-attribute (name &key
			      (type "Float32")
			      (gl-type "gl.FLOAT")
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
      
      (statement
       (if (== -1 ,loc)
	   (statement
	    (logger (+ (string ,loc) (string "=") ,loc) )
	    (gl_error_message gl (gl.getError))))
       (gl.enableVertexAttribArray ,loc)
       (gl.bindBuffer gl.ARRAY_BUFFER ,buffer)
       (let ((size ,size) ;; components per iteration
	     (type ,gl-type)
	     (normalize false)
	     (element_size ,(format
			  nil "~aArray.BYTES_PER_ELEMENT" type))
	     (stride (* ,stride

			element_size)) ;; 0 = move forward size * sizeof(type) each iteration to get the next position
	     (offset (* ,offset
			element_size)))
	 (gl.vertexAttribPointer
	  ,loc size type normalize stride offset))))))

; (bind-attribute "uv" :size 4 :stride 4)
(progn
  #.(in-package #:cl-cpp-generator)

  (defparameter *vertex-shader*
    (cl-cpp-generator::beautify-source
     `(with-compilation-unit
	  (raw "#version 100")
	(decl ((attrib_texCoord :type "attribute vec2")
	       (attrib_position :type "attribute vec2")
	       (texCoords :type "varying vec2")
	       ))
	(function (main () "void")
		  (setf gl_Position (funcall vec4 attrib_position 0 1)
			texCoords attrib_texCoord
			)))))

  (defparameter *fragment-shader*
    (cl-cpp-generator::beautify-source
     `(with-compilation-unit
	  (raw "#version 100")
	(raw "precision mediump float;")
	(decl (
	       (texCoords :type "varying vec2")
	       (textureSampler :type "uniform sampler2D")
	       ))
	(function (main () "void")
		  (let (#+nil (c :type "mediump vec4" :init (funcall vec4 1 0
								     ".5" 1))
			      
		              (textureColor :type vec4 :init (funcall
							      texture2D textureSampler texCoords)))
		    (setf gl_FragColor textureColor
			  ))))))  

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
						      "player"))))
		  (def gl_error_message (gl e)
		    (if (!= e gl.NO_ERROR)
			(statement
			 (logger (string "gl error: "))
			 ,@(loop for (err msg) in 
				'( ;(gl.NO_ERROR 	"No error has been recorded. The value of this constant is 0.")
				  (gl.INVALID_ENUM 	"An unacceptable value has been specified for an enumerated argument. The command is ignored and the error flag is set.")
				  (gl.INVALID_VALUE 	"A numeric argument is out of range. The command is ignored and the error flag is set.")
				  (gl.INVALID_OPERATION 	"The specified command is not allowed for the current state. The command is ignored and the error flag is set.")
				  (gl.INVALID_FRAMEBUFFER_OPERATION 	"The currently bound framebuffer is not framebuffer complete when trying to render to or to read from it.")
				  (gl.OUT_OF_MEMORY 	"Not enough memory is left to execute the command.")
				  (gl.CONTEXT_LOST_WEBGL 	"If
				 the WebGL context is lost, this error
				 is returned on the first call to
				 getError. Afterwards and  until the
				 context has been restored, itreturns
				 gl.NO_ERROR."))
				
			      collect 
				`(if (== ,err e)
				     (logger (string ,(substitute
						       #\Space
						       #\Newline
						       msg))))))))
		  (let-g ((video_arrived_p false))
			 ;; https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Tutorial/Animating_textures_in_WebGL
			 (def setup_video ()
			   (let-g ((video (get_video))
				   (playing false)
				   (timeupdate false))
				  (def check_ready ()
				    (if (and playing
					     timeupdate)
					(setf video_arrived_p true)))
				  
				  (video.addEventListener (string "playing")
							  (lambda ()
							    (setf playing
								  true)
							    (check_ready))
							  true)
				  (video.addEventListener (string "timeupdate")
							  (lambda ()
							    (setf timeupdate
								  true)
							    (check_ready))
							  true)
				  (return video))))
		  (def create_texture (gl)
		    (let-g ((texture (gl.createTexture)))
			   (gl.bindTexture gl.TEXTURE_2D texture)
			   (let ((level 0)
				 (internal_format gl.RGBA)
				 (width 1)
				 (height 1)
				 (border 0)
				 (src_format gl.RGBA)
				 (src_type gl.UNSIGNED_BYTE)
				 (pixel ("new Uint8Array" (list 0 0
								255 255))))
			     (gl.texImage2D gl.TEXTURE_2D level
					    internal_format width
					    height border src_format
					    src_type pixel))
			   (gl.texParameteri gl.TEXTURE_2D
					     gl.TEXTURE_WRAP_S
					     gl.CLAMP_TO_EDGE)
			   (gl.texParameteri gl.TEXTURE_2D
					     gl.TEXTURE_WRAP_T
					     gl.CLAMP_TO_EDGE)
			   (gl.texParameteri gl.TEXTURE_2D
					     gl.TEXTURE_MIN_FILTER
					     gl.NEAREST)
			   (gl.texParameteri gl.TEXTURE_2D
					     gl.TEXTURE_MAG_FILTER
					     gl.NEAREST)
			   (return texture)))
		  (def update_texture (gl texture video)
		    (let ((level 0)
			  (internal_format gl.RGBA)
			  (src_format gl.RGBA)
			  (src_type gl.UNSIGNED_BYTE))
		      (gl.bindTexture gl.TEXTURE_2D texture)
		      (gl.texImage2D gl.TEXTURE_2D level
				     internal_format
				     src_format
				     src_type
				     video)))

		  (def wait (delay_ms)
		    (return ("new Promise" (lambda (x)
					     (setTimeout x
							 delay_ms)))))

		  (def recording_start (stream length_ms)
		    (let-g ((recorder ("new MediaRecorder" stream))
			    (data (list)))
			   (setf recorder.ondataavailable (lambda (event)
							    (data.push
							     event.data)))
			   (recorder.start)
			   (let-g ((stopped ("new Promise"
					     (lambda (resolve reject)
					       (setf recorder.onstop
						     resolve
						     recorder.onerror
						     (lambda (event)
						       (reject
							event.name))))))
				   (recorded (dot (wait length_ms)
						  (then (lambda ()
							  (return (and (==
									(string
									 "recording") recorder.state)
								       (recorder.stop))))))))
				  (return (dot (Promise.all (list stopped
								  recorded))
					       (then (lambda () (return data))))))
			   ))


		  (def create_websocket ()
		    (if window.WebSocket
			(statement (logger (string "WebSocket is supported")))
			(statement (logger (string "Error: WebSocket is not supported"))
				   (return false)))
		    (let-g ((url (+ (string "ws://")
				    ;; alternative:
				    ;; window.location.host
				    (dot (document.getElementById (string "wss-server-connection"))
					 innerText)
				  (string "/echo")))
			    (w ("new WebSocket" url)))
			   (setf w.onopen
				 (lambda ()
				   (logger
				    (string "websocket open")))
				 w.onmessage
				 (lambda (e)
				   (logger (+ (string "websocket received message: ")
					      e.data)))
				 w.onclose
				 (lambda (e)
				   (logger (+ (string "websocket closed: ")
					      e.data)))
				 w.onerror
				 (lambda (e)
				   (logger (+ (string "websocket error: ")
					      e.data))))
			   (return w))
		    
		    )
		  
		  (def startup ()
		    (logger (string "startup .."))
		    
		    (let-g ((ws (create_websocket))
			    (gl (get_context))
			    (video (setup_video)))
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
				    '(list 0 0  -1 -1
				      0 1 -1  1
				      1 1  1  1
				      1 0  1 -1
				      ))
				  

				  (statement
				   ;; https://webglfundamentals.org/webgl/lessons/webgl-anti-patterns.html
				   (gl.viewport 0 0
						gl.drawingBufferWidth
						gl.drawingBufferHeight)
				   (gl_error_message gl (gl.getError))
					;(setf aspect (/ gl.canvas.clientWidth gl.canvas.clientHeight) )
				   (gl.clearColor .9 .8 .7 1)
				   (gl_error_message gl (gl.getError))
				   (gl.clear gl.COLOR_BUFFER_BIT)
				   (gl_error_message gl (gl.getError))
				   (gl.useProgram program)
				   (gl_error_message gl (gl.getError))
				   )
				  
				  (logger (string "bind-attributes"))
				  

				  ,(bind-attribute "attrib_position" :size 2
						   :stride 4 :offset 2)



				  ,(bind-attribute "attrib_texCoord" :size 2
						   :stride
						   4 :offset 0)
				  (let-g ((is_recording_p false))
					 (let-g ((then 0)
						 (tex (create_texture gl)))
						(def render (now)
						  (let-g ((now_seconds (*
								  now .001))
						    (delta_time (- now_seconds
								   then))
						    ))
						  (setf then now)

						  ;; alternative to
						  ;; video_arrived_p
						  ;; is listener on
						  ;; video's canplay

						  ;; use
						  ;; canvas.setAttribute
						  ;; to match width,
						  ;; height of video
						  (if
						   video_arrived_p 
						   (statement
						    
						    (update_texture gl tex video)
						    (gl.bindTexture gl.TEXTURE_2D tex)
						    (let ((primitive_type gl.TRIANGLE_FAN)
							  (offset 0)
							  (count 4)) ;; number of vertices
						      (gl.drawArrays primitive_type
								     offset count)
						      (gl_error_message gl
									(gl.getError)))

						    ))
						  
						  (requestAnimationFrame
						   render)
						  ))
					 (render)
					 
					 (if (not is_recording_p)
					     (statement
					      (setf is_recording_p true)
					      (logger (string "start recording"))
					      (dot (recording_start
						    (dot (document.getElementById
							  (string
							   "c"))
							 (captureStream
							  ;; 0=capture when requestFrame is called
							  ;;0
							  ))
						    5000)
						   (then
						    (lambda
							(recorded_chunks)
						      (let-g
						       ((recorded_blob
							 ("new Blob"
							  recorded_chunks
							  (dict (type
								 (string
								  "video/webm")))))
							)
						       (setf src_url
							     (URL.createObjectURL
							      recorded_blob)
							     download_button
							     (document.getElementById
							      (string
							       "download-button"))
							     download_button.href
							     src_url
							     download_button.download
							     (string "capture.webm")
							     is_recording_p
							     false
							     ))))))))))
		    )
		  (window.addEventListener (string "load")
					   startup false)
		  ))))
    (defun handler (env)
      ;;hunchentoot:define-easy-handler (securesat :uri "/secure" :acceptor-names '(ssl)) ()
      (destructuring-bind (&key server-name &allow-other-keys) env
       `(200 nil
	     (,(cl-who:with-html-output-to-string (s)
		 (cl-who:htm
		  (:html
		   (:head (:title "cam"))
		   (:body (:h2 "camera")
			  (:p (princ (format nil "~a" env) s))
			  (:div :id "wss-server-connection"
				(princ (format nil "~a:~a"
					     (or server-name "localhost")
					     *wss-port*) s))
			  (:a :href (princ (format nil "https://~a:~a/"
					     (or server-name "localhost")
					     *wss-port*) s)
				    "accept secure websocket cert here")
			  
			  (:div :id "log")
			  (:video :id "player" :controls t :width 320 :height
				  240 :autoplay t)
			  (:canvas :id "c" :width 320 :height 240)
			  (:a  :id "download-button" :class "button" "Download")
			  (:script :id (string "2d-vertex-shader")  :type "notjs"
				   (princ  cl-cpp-generator::*vertex-shader*
					   s))
			  (:script :id (string "2d-fragment-shader") :type "notjs"
				   (princ
				    cl-cpp-generator::*fragment-shader* s))
			  (:script :type "text/javascript"
				   (princ script-str s)
				   )))))))))))


