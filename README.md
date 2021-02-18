
JT65
====

A SPARK implementation of the JT65 protocol.

Getting Started
---------------

This code base is built and analyzed using GNAT Community 2020. After installing that system,
load the jt65.gpr project in the root folder. Build the `jt65code` program and the `check_pack`
test program. You should also be able to use SPARK to analyze the package bodies in the `src`
folder.

Testing
-------

The test program is written in Python and resides in `check/check.py`. There is a PyCharm
project also defined in the `check` folder. To use it, proceed as follows:

+ Install PyCharm from JetBrains.

+ Install a Python 3.x (note to other CubeSat Laboratory developers: the 3.9.1 installer is in
  our Teams).
  
+ Start PyCharm and load the project by opening the `check` folder. Next you will need to set up
  a Python virtual environment.
  
+ Follow the [instructions in the PyCharm
  documentation](https://www.jetbrains.com/help/pycharm/creating-virtual-environment.html) for
  creating a new virtual environment and selecting the appropriate Python interpreter. PyCharm
  will (likely) suggest a location for the virtual environment of `check/venv`. That location
  should be fine, but be sure you *do not commit the files in the virtual environment!* 
  
+ In PyCharm, right click on the `venv` folder in the project view, then select Git->exclude
  to add the folder to your personal list of excluded files. *Do not fail to complete this
  step!*
