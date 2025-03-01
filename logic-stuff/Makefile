
.SUFFIXES: .pic .ms .pdf .ps .eps
.DEFAULT: .ms.pdf .ps.pdf .eps.pdf

default: excercises.pdf
URI = https://github.com/siglun/mixed-philosophemes/tree/main/logic-stuff


.ms.pdf:
	groff -U -R -sGtep  -m pdfpic -m pdfmark -ms \
	-k  -P-pa4 -Tps $<  > $*.ps 
	ps2pdf  $*.ps

.eps.pdf:
	epstopdf  $<

clean:
	rm -f *~ excercises.p* 

time_series.pdf:
	./model-and-plot.r; grep -vi time shit.text > model_data.text

qr.eps:
	qrencode -l H -v 1 -s 3 -d300 -t EPS -o qr.eps $(URI)

qr.pdf: qr.eps

excercises.pdf: excercises.ms 


time_series.pdf: model-and-plot.r parameters.r
