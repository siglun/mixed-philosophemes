#!/bin/bash

USED_IN=`grep $* *.xml | grep -v includes.xml`

echo "$USED_IN"
