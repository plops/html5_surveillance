import sys
import numpy as np
import pandas as pd
import pathlib
import socket
import selenium
import selenium.webdriver.common.keys

driver=selenium.webdriver.Firefox()
myip=socket.gethostbyname(socket.gethostname())
url="https://{}/".format(myip)

print("open url={}".format(url))
driver.get(url)

