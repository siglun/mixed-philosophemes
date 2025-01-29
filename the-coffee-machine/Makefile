
.SUFFIXES: .pic .ms .pdf .ps .eps
.DEFAULT: .ms.pdf .ps.pdf .eps.pdf

default: paper.pdf
URI = https://github.com/siglun/mixed-philosophemes/blob/main/the-coffee-machine/paper.pdf


.ms.pdf:
	groff -U -R -sGtep  -m pdfpic -m pdfmark -ms \
	-k  -s -t -P-pa4 -Tpdf $<  > $*.pdf

.eps.pdf:
	epstopdf  $<

clean:
	rm -f *~ paper.pdf  time_series.pdf shit.text

time_series.pdf:
	./model-and-plot.r; grep -vi time shit.text > model_data.text

qr.eps:
	qrencode -l H -v 1 -s 3 -d300 -t EPS -o qr.eps $(URI)

qr.pdf: qr.eps

paper.pdf: paper.ms boxes-and-arrows.pic  paper-parameters.ms back-matter.ms \
	qr.pdf time_series.pdf Makefile

time_series.pdf: model-and-plot.r parameters.r
