#!/bin/bash

groff -ms -m pdfpic -m pdfmark  -U -e -s -p  -P-pa4 -Tps excercises.ms > excercises.ps

ps2pdf excercises.ps

