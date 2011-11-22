#!/bin/sh
# this is a simple debian install script for Clozure CL.
# you will need build-essential installed. run it anywhere, i like ~/bin
# if not on linuxx86, fix the svn address and binary name
svn co http://svn.clozure.com/publicsvn/openmcl/release/1.7/linuxx86/ccl
cd ccl
./lx86cl64 -e "(progn (rebuild-ccl :full t) (quit))"
