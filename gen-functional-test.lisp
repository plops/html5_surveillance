(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-py-generator))

(in-package :cl-py-generator)
;; sudo pacman -S python-selenium geckodriver
;(start-python)

(let ((code
       `(do0
	 (imports (sys
		   (np numpy)
		   (pd pandas)
		   pathlib
		   socket
		   selenium
		   selenium.webdriver.common.keys))
	 (do0
	  (setf driver (selenium.webdriver.Firefox)
		myip (socket.gethostbyname (socket.gethostname))
		url (dot (string "https://{}/")
			   (format myip)))
	  (print (dot (string "open url={}")
		      (format url)))
	  (driver.get url))
	 )))
  ;(run code)
  (write-source "/home/martin/stage/html5_surveillance/functional_test/run00" code))
