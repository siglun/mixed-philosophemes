
.SUFFIXES: .pic .ms .pdf 
.DEFAULT: .ms.pdf 

default: paper.pdf


.ms.pdf:
	groff -U -R -s -t -e -p  -m pdfpic -m pdfmark -ms \
	-k  -s -t -P-pa4 -Tpdf $<  > $*.pdf	

clean:
	rm -f *~ paper.pdf  time_series.pdf

time_series.pdf:
	./model-and-plot.r

paper.pdf: paper.ms boxes-and-arrows.pic  paper-parameters.ms time_series.pdf

