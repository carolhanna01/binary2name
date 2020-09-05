#!/bin/sh
# 
# A small script to generate the configure script.
# Watch out! Everything has to be just so or 
# autoconf will bite your head off.
#
aclocal -I config
autoconf -I config

