
.SUFFIXES: .pic .ms .pdf .ps .eps
.DEFAULT: .ms.pdf .ps.pdf .eps.pdf

default: logic-with-groff.pdf
URI = https://github.com/siglun/mixed-philosophemes/tree/main/logic-stuff

# 	groff -U -R -sGtep   -ms  -m refer-ms -M ./tmac -m refspec -m refer -m decorations  -m pdfpic \

.ms.pdf:
	groff -U -R -sGtep   -ms  -M ./tmac -m refer-hooks  -m decorations  -m pdfpic \
	-k  -P-pa4 -Tps $<  > $*.ps 
	ps2pdf  $*.ps

.eps.pdf:
	epstopdf  $<

clean:
	rm -f *~ excercises.p* qr.* logic-with-groff.p*

time_series.pdf:
	./model-and-plot.r; grep -vi time shit.text > model_data.text

qr.eps:
	qrencode -l H -v 1 -s 3 -d300 -t EPS -o qr.eps $(URI)

qr.pdf: qr.eps

excercises.pdf: excercises.ms

logic-with-groff.pdf: parameters.ms logic-with-groff.ms fitch-macros.pic  \
	aristotelian-syllogism.ms references.text qr.pdf  Makefile \
	tmac/refer-hooks.tmac

time_series.pdf: model-and-plot.r parameters.r
