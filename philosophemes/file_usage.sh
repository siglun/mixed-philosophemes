#!/bin/bash

USED_IN=`grep $* *.xml | grep -v includes.xml|grep -v philosophemes.xml`

echo "$USED_IN"
