(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-py-generator))

(in-package :cl-py-generator)
;; sudo pacman -S python-selenium geckodriver xdotool wmctrl
;(start-python)

(let ((code
       `(do0
	 (imports (sys
		   (np numpy)
		   (pd pandas)
		   pathlib
		   socket
		   selenium
		   selenium.webdriver.common.keys
		   selenium.webdriver.support.expected_conditions
		   selenium.webdriver.support.ui
		   selenium.webdriver.common.by
		   subprocess
		   time))
	 
	 (do0
	  (setf driver (selenium.webdriver.Firefox)
		wait_obj (selenium.webdriver.support.ui.WebDriverWait driver 5))

	  (def wait (selector &key (by (string "css")) (type (string "clickable")))

	    (if (== (string "id") by)
		(setf target (tuple
			   selenium.webdriver.common.by.By.ID
			   selector))
		(if (== (string "css") by)
		 (setf target (tuple
			       selenium.webdriver.common.by.By.CSS_SELECTOR
			       selector))))
	    (if (== (string "clickable") type)
		(setf condition selenium.webdriver.support.expected_conditions.element_to_be_clickable)
		(if (== (string "visible") type)
		    (setf condition selenium.webdriver.support.expected_conditions.visibility_of_element_located)
		    (if (== (string "invisible") type)
		    (setf condition selenium.webdriver.support.expected_conditions.invisibility_of_element_located))))
	    (return (wait_obj.until (condition target))))
	  (setf myip (socket.gethostbyname (socket.gethostname))
		url (dot (string "https://{}:7777/")
			 (format myip)))
	  (print (dot (string "open url={}")
		      (format url)))
	  (driver.get url)
	  (wait (string "a"))
	  (subprocess.call (list (string "wmctrl")
				 (string "-a")
				 (string "cam - Mozilla Firefox")))
	  (subprocess.call (list (string "xdotool")
				 (string "key")
				 (string "alt+a")))))))
  ;(run code)
  (write-source "/home/martin/stage/html5_surveillance/functional_test/run00" code))
