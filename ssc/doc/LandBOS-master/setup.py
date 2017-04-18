# from distutils.core import setup
# from Cython.Build import cythonize

# setup(
#     name = 'LandBOS',
#     # ext_modules = cythonize('src/pybos.pyx')
#     ext_modules = cythonize('yo', ['landbos.pyx'])
# )


from distutils.core import setup
from distutils.extension import Extension

try:
    USE_CYTHON = True
    from Cython.Build import cythonize
except Exception:
    USE_CYTHON = False


ext = '.pyx' if USE_CYTHON else '.c'

extensions = [Extension('_landbos', ['src/_landbos'+ext, 'src/LandBOSsmooth.c'])]

if USE_CYTHON:
    extensions = cythonize(extensions)

setup(
    name='LandBOS',
    description='a translation of the NREL land-based balance of station excel model',
    author='S. Andrew Ning',
    author_email='andrew.ning@nrel.gov',
    package_dir={'': 'src'},
    py_modules=['landbos'],
    license='Apache License, Version 2.0',
    ext_modules=extensions
)
