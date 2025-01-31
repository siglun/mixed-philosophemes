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
Let us assume that at time $t$ there $y(t)$ persons who have actually got espresso out of the machine (in the following we refer to them as coffee lovers),
and then there are $x(t)$ who did not get any coffee last time they tried the machine (we refer to them as coffee haters, for short).
.PP
The satisfied customers are rapidly returning to the group of potential customers; they do so at a per capita rate $c$, i.e., $cy(t)$.
Similarly the unsatisfied ones return at a much lower rate (for instance when they forget their thermoses at home). We assume that that is happening at a $bx(t)$.
.KF
.so boxes-and-arrows.pic
.IP
.vs 12
.ps 10
Figure 1. The population is buying cups of coffee at a per capita rate $a$,
hence  customers are buing coffee at the rate $a p n(t)$ and become coffee lovers $x(t)$.
The rate of buys that fails is then $a (1-p) n(t)$ and become coffee haters $y(t)$.
Coffee lovers and haters return to the potential customers at the rates $b x(t)$ and $c y(t)$ respectively.
In addition,
the coffee haters are spreading the rumour at a rate proportional to the product between the numbers of haters and potential customers, i.e., $beta n(t) y(t).$
.KE
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
Assume that the contact rate beteen potential customers and coffee haters is proportional to the product between the two,  $beta  x(t) n(t)$
The flows of people between the groups are summarized in Figure 1.
.PP
Given these considerations, and assumptions,
we can formulate the dynamics of the population using one ordinary differential equation per group.
First, the number of potential customers at time $t$ is $n(t)$, and it changes according to Eq. 1:
.EQ (1)
diff( n(t) , t ) = - a n(t) + b y(t)  + c x(t) - beta  n x(t)
.EN
Then $x(t)$ the number of coffee haters is given in Eq. 2 and
.EQ (2)
diff( x(t) , t ) = a (1-p) n(t) - c x(t)  + beta  n(t) x(t)
.EN
the the number of coffee lovers, $y(t)$ change according to.
.EQ (3)
diff(y(t),t) = a p n(t) - b y(t)
.EN
.PP
.KF
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
.IP
.vs 12
.ps 10
Figure 2.
The development of the population of customers:
$n(t)$ (coffee haters), $x(t)$ (coffee lovers) and $n(t)$ (potential customers),
over a period of 20 days.
The parameters used $a = 1$,
$b = 1$,
$c = 0.1$,
$p = 0.25$ and
$beta = 0.2$.
The $p$ favours the buildup of a sizeable group of coffee haters,
which is further favoured by their spreading of a rumours about the unreliability of the espresso machine.
Finally, the way the model is built, $1/b$ and $1/c$ are the expected time before coffee lovers and haters, respectively, become potential customers after their coffee shoppings.
The two parameters are set such that the coffee lovers are again willing to try the mochine the day after their successful buy,
while the haters bring their thermoses 10 days before they care to try it again.
.KE
.PP
I solved the differential equations using Rscript
.[
statistical computing
.]
with the package deSolve.
.[
deSolve
.]
.so back-matter.ms
