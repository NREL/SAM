LandBOS
=======

A C version of the NREL land-based balance of station model.  A continuously differentiable version is also implemented for use in gradient-based optimization applications.  The repository also includes a Python wrapper, an OpenMDAO wrapper and a C++ wrapper all using the same core implementation.

### Original Excel Spreadsheet:  
BOS_Model_v1.0_Released-1.xlsx

### C Implementation:  

All computations are done here.

LandBOS.c  
LandBOS.h

A "smoothed" version is provided for use in gradient-based optimization applications

LandBOSsmooth.c  (uses same header file)

### C++ wrapper:  

An object-oriented wrapper to simplify i/o.  Uses original implementation (LandBOS.c)

LandBOS.cpp  
LandBOS.hpp  
(also maintest.cpp, makefile, just to test running it)


### Python wrapper:  

A one-to-one mapping, making all C functions available from Python.
By default, the smooth version is used.  But this can be swapped by editing
line 23 in setup.py

_landbos.c  (generated from Cython, can be compiled directly as a Python extension)  
_landbos.pyx, c_landbos.pxd (for development use with Cython)  
setup.py (used to install Python module)

### OpenMDAO wrapper:  

An OpenMDAO assembly to facilitate gradient propogation.

landbos.py


### Unit Tests:  
test/test.py (tests core C functionality.  Unit tests run through the Python wrapper.)  
test/test_openmdao.py (tests OpenMDAO workflow)
