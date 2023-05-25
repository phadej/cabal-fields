#!/usr/bin/env python3

try:
    from setuptools import Extension, setup
except:
    from distutils.core import setup, Extension

setup(
	name = "cabalfields",
	version = "1.0",
	ext_modules = [Extension("cabalfields", ["cabalfieldsmodule.c", "cabalfields.c"])]
	);
