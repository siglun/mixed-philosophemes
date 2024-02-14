#!/bin/bash

USED_IN=`grep $* *.xml`

echo "$USED_IN"
