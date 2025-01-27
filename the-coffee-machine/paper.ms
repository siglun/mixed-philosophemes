.so paper-parameters.ms
.SH
Introduction
.LP
.SH
A model
.LP
.so boxes-and-arrows.pic
.LP
The number of potential customers at time $t$ is $n(t)$
.EQ (1)
diff( n(t) , t ) = - a n(t) + b y(t)  + c x(t) - beta n x(t)
.EN
and  $x(t)$ is the number of customers who actually hate the coffee machine,
.EQ (2)
diff( x(t) , t ) = a (1-p) n(t) - c x(t)  + beta n x(t)
.EN
whereas $y(t)$ is the people who like the coffee machine
.EQ (3)
diff(y(t),t) = a p n(t) - b y(t)
.EN
.PDFPIC -C "time_series.pdf" 12.0c
.so back-matter.ms
