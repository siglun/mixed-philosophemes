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
.G1
frame invis ht 2 wid 3 left solid color "black" \
	bot solid color "black"
	
draw n solid color "black"
# draw n solid color "red"
draw x solid color "black"
# draw x solid color "blue"
draw y solid color "black"
# draw y solid color "gree"

label left "$n$, $x$ and $y$" left 0.2
label bot "Time $t$"

coord x 0, 20 y 0.0, 1.0

copy "model_data.text" thru {
	next n at $2,$3
	next x at $2,$4
	next y at $2,$5
}
"$y$" at 19,0.93
"$n$" at 19,0.13
"$x$" at 19,0.05
.G2
.\" .PDFPIC -C "time_series.pdf" 12.0c
.so back-matter.ms
