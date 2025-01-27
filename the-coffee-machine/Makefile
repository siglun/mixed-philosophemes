
.SUFFIXES: .pic .ms .pdf .ps .eps
.DEFAULT: .ms.pdf .ps.pdf .eps.pdf

default: paper.pdf
URI = blabla


.ms.pdf:
	groff -U -R -s -t -e -p  -m pdfpic -m pdfmark -ms \
	-k  -s -t -P-pa4 -Tpdf $<  > $*.pdf

.eps.pdf:
	epstopdf  $<

clean:
	rm -f *~ paper.pdf  time_series.pdf

time_series.pdf:
	./model-and-plot.r

qr.eps:
	qrencode -l H -v 1 -s 3 -d300 -t EPS -o qr.eps $(URI)


paper.pdf: paper.ms boxes-and-arrows.pic  paper-parameters.ms \
	qr.pdf time_series.pdf

