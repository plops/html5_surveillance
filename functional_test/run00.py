import sys
import numpy as np
import pandas as pd
import pathlib
import socket
import selenium
import selenium.webdriver.common.keys
import selenium.webdriver.support.expected_conditions
import selenium.webdriver.support.ui
import selenium.webdriver.common.by
import subprocess
import time

driver=selenium.webdriver.Firefox()
wait_obj=selenium.webdriver.support.ui.WebDriverWait(driver, 5)

def wait(selector, by="css", type="clickable"):
    # wait by={css,id} type={clickable,visible,invisible}
    if ( (("id")==(by)) ):
        target=(selenium.webdriver.common.by.By.ID,selector,)

    else:
        if ( (("css")==(by)) ):
            target=(selenium.webdriver.common.by.By.CSS_SELECTOR,selector,)



    if ( (("clickable")==(type)) ):
        condition=selenium.webdriver.support.expected_conditions.element_to_be_clickable

    else:
        if ( (("visible")==(type)) ):
            condition=selenium.webdriver.support.expected_conditions.visibility_of_element_located

        else:
            if ( (("invisible")==(type)) ):
                condition=selenium.webdriver.support.expected_conditions.invisibility_of_element_located




    return wait_obj.until(condition(target))

myip=socket.gethostbyname(socket.gethostname())
url="https://{}:7777/".format(myip)

print("open url={}".format(url))
driver.get(url)
wait("a")
time.sleep(2)
wait("body").send_keys(((selenium.webdriver.common.keys.Keys.ALT)+("a")))

