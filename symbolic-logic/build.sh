#!/bin/bash

groff -ms -m pdfpic -m pdfmark -e  -U -s -p  -P-pa4 -Tps excercises.ms > excercises.ps

ps2pdf excercises.ps

