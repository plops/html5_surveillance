;; https://common-lisp.net/project/parenscript/tutorial.html
;; https://www.html5rocks.com/en/tutorials/getusermedia/intro/
(mapc #'ql:quickload '("cl-fad" "cl-who" 
		       "clack" "websocket-driver"
		       "cl-js-generator" "cl-cpp-generator"
		       "event-emitter" "local-time"))

(in-package #:cl-js-generator)


(setq cl-who:*attribute-quote-char* #\")
(setf cl-who::*html-mode* :html5)

(defparameter *wss-port* 7778)
(defparameter *ssl-port* 7777)
(defparameter *server-ip* 
  (let ((ipstr (with-output-to-string (s)
		 (sb-ext:run-program "/usr/bin/hostname" '("-i") :output s)
		 )))
    (string-trim 
     '(#\Space #\Newline #\Backspace #\Tab 
       #\Linefeed #\Page #\Return #\Rubout)
     ipstr)
    ))


(defun handler (env)
  '(200 nil ("hello world")))

;; openssl req -new -x509 -nodes -out /tmp/server.crt -keyout /tmp/server.key



(unless (and (probe-file "/tmp/server.key")
	     (probe-file "/tmp/server.crt"))
  ;; generate keys if they don't exist
  (let* ((p (sb-ext:run-program "/usr/bin/openssl" '("req" "-new" "-x509"
						   "-nodes"  "-out"
						   "server.crt" "-keyout"
						   "server.key")
			      :directory "/tmp/" :output :stream :input :stream
			      :wait nil))
       (stream-in (sb-ext:process-input  p))
       (stream-out (sb-ext:process-output p)))
  (flet ((consume ()
	   (loop while (listen stream-out) do
		(format t "~a" (read-char stream-out))))
	 (consume-until-colon ()
	   (loop for char = (read-char stream-out nil 'foo)
	      until (or (eq char 'foo)
			(eq #\: char))
	      do (format t "~a" char)
		))
	 (consume-until-colon-nowait ()
	   (loop while (listen stream-out) do
		(let ((line (read-line stream-out nil 'foo)))
		  (print line)
		  ))))
    (loop for e in
	 '("NL"
	   "Noord-Brabant" "Veldhoven" "ck" "certifacte_unit"
	   "nn" "kielhorn.martin@gmail.com")
       do
	 (consume-until-colon)
	 (write-line (format nil "~a~%" e) stream-in)
	 (format t "~&> ~a~%" e)
	 (finish-output stream-in)))
  (close stream-in)
  (sb-ext:process-wait p)
  (sb-ext:process-close p)))


(progn
  (defvar *clack-server* nil) ;; initialize with nil
  (when *clack-server* ;; stop pre-existing server
    (clack.handler:stop *clack-server*)
    (setf *clack-server* nil))
  (setf *clack-server* ;; start new server
	(clack:clackup
	 (lambda (env)
	   (funcall 'handler env))
	 :port *ssl-port*
	 :ssl t :ssl-key-file  #P"/tmp/server.key" :ssl-cert-file #P"/tmp/server.crt"
	 :use-default-middlewares nil)))



;; :ssl-key-file #P"/etc/letsencrypt/live/cheapnest.org/privkey.pem"  :ssl-cert-file #P"/etc/letsencrypt/live/cheapnest.org/fullchain.pem"





(let ((ws-connections ()))
  (defun get-ws-connections ()
    ws-connections)
 (defun ws-handler (env)
   (handler-case 
       (destructuring-bind (&key request-uri remote-addr remote-port
				 content-type content-length headers &allow-other-keys)
	   env
	 (format t "ws-handler: ~a~%" env)
	 (let ((ws (websocket-driver:make-server env))
	       (now (local-time:now)))
	   
	   (push `(:remote-addr ,remote-addr
				:remote-port ,remote-port
				:socket ,ws
				:connection-setup-time ,now
				:user-agent ,(gethash "user-agent" headers)
				:last-seen ,now)
		 ws-connections)
	   (event-emitter:on :message ws
			     (lambda (message)
			       (format t "ws-handler received: ~a~%" message)
			       (mapcar #'(lambda (x) (websocket-driver:send (getf x :socket)
									    message))
				       ws-connections)))
	   (event-emitter:on :close ws
			     (lambda (&key code reason)
			       (format t "ws-handler socket closed: code=~a reason=~a~%" code reason)
			       (setf ws-connections
				     (remove-if #'(lambda (x)
						    (and (string= remote-addr
								  (getf x :remote-addr))
							 (eq remote-port (getf x :remote-port))
		     ))
	    ws-connections))
			       ))
	   
	   (event-emitter:on :error ws
			     (lambda (&rest rest)
			       (format t "ws-handler error: ~a~%" rest)
			       (setf ws-connections
				     (remove-if #'(lambda (x)
						    (and (string= remote-addr
								  (getf x :remote-addr))
							 (eq remote-port (getf x :remote-port))
		     ))
	    ws-connections))))
	   (lambda (responder)
	     (format t "ws-handler: start connection ~a~%" responder) 
	     (websocket-driver:start-connection ws))))
     (condition ()
       (format t "This connection wants websocket protocol!~%")
       `(404 nil ("This connection wants websocket protocol!"))))))

(progn
  (defvar *ws-server* nil) ;; initialize with nil
  (when *ws-server* ;; stop pre-existing server
    (clack.handler:stop *ws-server*)
    (setf *ws-server* nil))
  (setf *ws-server* ;; start new server
	(clack:clackup
	 (lambda (env)
	   (funcall 'ws-handler env))
	 :port *wss-port*
	 :ssl t :ssl-key-file  #P"/tmp/server.key" :ssl-cert-file #P"/tmp/server.crt"
	 :use-default-middlewares nil)))

(defun fill-buffer (name &key (type "Float32") data1d (usage-hint "gl.STATIC_DRAW")) 
  `(let ((,name (gl.createBuffer)))
	  (let-l ((vec ,data1d))
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
      (let ((,loc (gl.getAttribLocation ,program (string
						    ,name)))))
      
      (statement
       (if (== -1 ,loc)
	   (statement
	    (logger (+ (string ,loc) (string "=") ,loc) )
	    (gl_error_message gl (gl.getError))))
       (gl.enableVertexAttribArray ,loc)
       (gl.bindBuffer gl.ARRAY_BUFFER ,buffer)
       (let-l ((size ,size) ;; components per iteration
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
	  `(let ((player (document.getElementById (string "player")))
		 (log (document.getElementById (string "log"))))
	     (def logger (message)
	       #-nolog
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
	     (def dbg (title obj)
	       (logger (+ title
			  (? (and JSON
				  JSON.stringify)
			     (JSON.stringify obj)
			     obj))))
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
	     (def get_context ()
	       (logger (string "get_context .."))
	       (let ((canvas (document.getElementById (string "c")))
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
	     (let ((video_arrived_p false))
	       ;; https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Tutorial/Animating_textures_in_WebGL
	       (def setup_video ()
		 (let ((video (get_video))
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
	       (let ((texture (gl.createTexture)))
		 (gl.bindTexture gl.TEXTURE_2D texture)
		 (let-l ((level 0)
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
	       (let-l ((level 0)
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
	       (let ((recorder ("new MediaRecorder" stream))
		     (data (list)))
		 (setf recorder.ondataavailable (lambda (event)
						  (data.push
						   event.data)))
		 (recorder.start)
		 (let ((stopped ("new Promise"
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
	       (if navigator.onLine
		   (statement (logger (string "You are online!")))
		   (statement (logger (string "You are offline!"))
			      (return false)))
	       (if window.WebSocket
		   (statement (logger (string "WebSocket is supported")))
		   (statement (logger (string "Error: WebSocket is not supported"))
			      (return false)))
	       (let ((url (+ (string "wss://")
			     ;; alternative:
			     ;; window.location.host
			     (dot (document.getElementById (string "wss-server-connection"))
				  innerText)
			     (string "/")))
		     (w ("new WebSocket" url)))
		 (setf w.onopen
		       (lambda ()
			 (create_webrtc w)
			 (w.send (+ (string "{startup: '")
				    (dot (document.getElementById (string "ssl-client-connection"))
					 innerText)
				    (string "'}")))
			 (logger
			  (string "websocket open")))
		       w.onmessage
		       (lambda (e)
			 (dbg  (string "websocket received message: e.data=")
			       e.data))
		       w.onclose
		       (lambda (e)
			 (logger (+ (string "websocket closed: ")
				    e.data))
			 (if (!= 1000 e.code)
			     (statement
			      ;; in order to protect browser users this error message is vague according to the w3c websocket standard
			      (if (== 1006 code)
				  (logger (string "websocket: probably authentication error, check your javascript console!")))
			      (logger (+ (string "websocket connection was not closed normally! code=")
					 e.code
					 (string " reason=")
					 e.reason))
			      (if (not navigator.onLine)
				  (logger (string "websocket: You are offline!"))))))
		       w.onerror ;; always followed by close
		       (lambda (e)
			 (logger (+ (string "websocket error: ")
				    e.data))))
		 (return w))
	       
	       )

	     (def create_webrtc ( signaling )
	       (logger (string "create_webrtc .."))
	       (let ((configuration (dict (iceServers (list))))
		     (pc ("new RTCPeerConnection" configuration)))
		 (setf pc.onicecandidate (lambda (event)
					   (if event.candidate
					       (statement ;; send to peer

						
						(dbg (string "onicecandidate: event=")
						     event)
						(signaling.send event.candidate))
					       ;; else all have been sent
					       )))
		 (let ((chan_send (pc.createDataChannel (string
							 "datachannel")
							(dict (reliable
							       false)))))
		   #+nil (setf chan_send.onopen (lambda (s)))
		   (dot (pc.createOffer)
			(then (lambda (offer)
				(dbg (string "createOffer: offer=")
				     offer)
				(pc.setLocalDescription offer)
				(signaling.send
				 (JSON.stringify
				  (dict ((string "messageType")
					 (string  "offer"))
					((string "peerDescription")
					 offer))))))))
		 ))
	     
	     (def startup ()
	       (logger (string "startup .."))
	       
	       (let ((ws (create_websocket))
		     
		     (gl (get_context))
		     (video (setup_video)))
		 
		 (let ((vertex_shader (create_shader gl gl.VERTEX_SHADER
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
		   (let ((is_recording_p false))
		     (let ((then 0)
			   (tex (create_texture gl)))
		       (def render (now)
			 (let ((now_seconds (*
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
			   (let-l ((primitive_type gl.TRIANGLE_FAN)
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
		     (render 0)
		     
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
				  (let
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
      (destructuring-bind (&key server-name remote-addr remote-port &allow-other-keys) env
	`(200 (:content-type "text/html; charset=utf-8")
	      (,(cl-who:with-html-output-to-string (s)
		  (cl-who:htm
		   (:html
		    (:head (:title "cam"))
		    (:body (:h2 "camera")
			   (:div
			    (:video :id "player" :controls t :width 320 :height
				    240 :autoplay t)
			    (:canvas :id "c" :width 320 :height 240)
			    (:a  :id "download-button" :class "button" "Download")

			    #+nil (:remote-addr ,remote-addr
						:remote-port ,remote-port
						:socket ,ws
						:connection-setup-time ,now
						:last-seen ,now)
			    (:table
			     (loop for row in (get-ws-connections) do
				  (cl-who:htm
				   (:tr :cellpadding 4
					(loop for x in '(:remote-addr :remote-port :last-seen :user-agent) do
					     (cl-who:htm
					      (:td (cl-who:fmt "~a" (getf row x)))))))))
			    )
			   
			   (:p (princ (format nil "~a" env) s))
			   (:div :id "wss-server-connection"
				 (princ (format nil "~a:~a"
						*server-ip*
						*wss-port*) s))
			   (:div :id "ssl-client-connection"
				 (princ (format nil "~a:~a"
						(or remote-addr "localhost")
						remote-port) s))
			   
			   (:a :href (format nil "https://~a:~a/"
					     (or server-name *server-ip*)
					     *wss-port*)
			       "accept secure websocket cert here")
			   #-nolog
			   (:div :id "log")
			   
			   (:script :id (string "2d-vertex-shader")  :type "notjs"
				    (princ  cl-cpp-generator::*vertex-shader*
					    s))
			   (:script :id (string "2d-fragment-shader") :type "notjs"
				    (princ
				     cl-cpp-generator::*fragment-shader* s))
			   (:script :type "text/javascript"
				    (princ script-str s)
				    )))))))))))


