.so paper-parameters.ms
.SH
Introduction
.LP
.SH
The Espresso Machine
.LP
The campus canteen has acquired a new Espresso Machine.
The machine has split the opinions of students and faculty into two camps.
The reason is that some think that the machine is unreliable;
those who purport this have typically put their coins into the machine,
which deposited them into its interiors with a clinkety clank.
No coffee were delivered.
They started bring thermoses with coffee, strong and hot, from home.
However, the sometimes machine delivered coffee and good espresso,
as a matter of fact a bright student estimated that the proportion $p$ of the cups sold were good.
.PP
Let us assume that the there are $n(t)$ at potential customers at time $t$.
They buy Espresso at a rate constant per capita rate $a$, i.e.,
customers are removed from the customer class at a rate $an(t)$.
However, only a proportion $p$ of the sales actually lead to delivery.
Therefore $a p n(t)$ of the potential customers are transformed into actual customers, denoted by $y(t)$.
They claim that this is a good espresso, although it was produced through an unreliable process.
The remaining sales occur a rate $a (1 - p) n(t)$ and the affected customers claim that the former are accepting the statement \(lqthe espresso is good\(rq is false.
Let us assume that at time $t$ there $y(t)$ persons who have actually got espresso out of the machine,
and then there are $x(t)$ who have not.
.PP
The satisfied customers are rapidly returning to the group of potential customers; they do so at a per capita rate $c$, i.e., $cy(t)$.
Similarly the unsatisfied ones return at a much lower rate (for instance when they forget their thermoses at home). We assume that that is happening at a $bx(t)$.
.so boxes-and-arrows.pic
.IP
.vs 12
.ps 10
Figure 1.
.LP
There arose a new behaviour among the unsatiesfied customers:
They started to inform potential customers that the espresso machine was really unreliable.
This spread as an epidemic,
so we model it as such,
see for example.
.[\~
#
Anderson May nature 1979
.]\&
Assume that the contact rate beteen potential customers and unsatiesfied ones is proportional to the product between the two,  $beta  x(t) n(t)$
The flows of people between the groups are summarized in Figure 1.
.LP
The number of potential customers at time $t$ is $n(t)$
.EQ (1)
diff( n(t) , t ) = - a n(t) + b y(t)  + c x(t) - beta  n x(t)
.EN
and  $x(t)$ is the number of customers who actually hate the coffee machine,
.EQ (2)
diff( x(t) , t ) = a (1-p) n(t) - c x(t)  + beta  n x(t)
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
