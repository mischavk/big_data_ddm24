from setuptools import Extension, setup
from Cython.Build import cythonize

extensions = [
    Extension("levy", ["levy.pyx"])
]

setup (
	ext_modules = cythonize(extensions),
)

#HOW TO

##python setup.py build_ext --inplace
