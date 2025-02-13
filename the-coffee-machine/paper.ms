.so paper-parameters.ms
.TL
The Espresso machine and the meaning of reliability
.AU
Sigfrid Lundberg
.LP
\&
.[
#
Zagzebski 2003
.]\&
was one of the architects of what since is referred to as \fIvirtue epistemology\fP.
Her \fIEspresso machine\fP argument was presented as a counter argument against the reliabilism school of thought.
Here I offer an analysis of the dynamics of a hypothetical campus population coffee drinkers.
.PP
The campus canteen has acquired a new Espresso machine.
The machine has split the opinions of students and faculty into two camps.
The reason is that some think that the machines operation is based on an unreliable process.
Those who purport this have typically put their coins into the machine,
which deposited them into its interiors with a clinkety clank without delivering any coffee.
.PP
That party started to bring thermoses with coffee\(emstrong and hot\(emfrom home.
However, the sometimes machine delivered coffee and good espresso,
as a matter of fact a bright student estimated that the proportion $p$ of the cups sold were good.
There was another party of people claiming that good espresso is good espresse in spite of the unreliable process.
.KF
.sp
.so boxes-and-arrows.pic
.IP
.vs 12
.ps 10
Figure 1. The population is buying cups of coffee at a per capita rate $a$,
hence  customers are buing coffee at the rate $a p n(t)$ and become coffee machine lovers $x(t)$.
The rate of buys that fails is then $a (1-p) n(t)$ and become coffee haters $y(t)$.
Coffee machine lovers and haters return to the potential customers at the rates $b x(t)$ and $c y(t)$ respectively.
In addition,
the coffee haters are spreading the rumour at a rate proportional to the product between the numbers of haters and potential customers, i.e., $beta n(t) y(t).$
.sp
.sp
.KE
.PP
Let us assume that the there are $n(t)$ at potential customers at time $t$.
They buy Espresso at a rate constant per capita rate $a$, i.e.,
customers are removed from the customer class at a rate $an(t)$.
However, only a proportion $p$ of the sales actually lead to delivery.
Therefore $a p n(t)$ of the potential customers are transformed into actual customers, denoted by $y(t)$.
The flows of people between the groups are summarized in Figure 1.
They claim that this is a good espresso, although it was produced through an unreliable process.
The remaining sales occur a rate $a (1 - p) n(t)$ and the affected customers claim that the former are accepting the statement \(lqthe espresso is good\(rq is false.
Let us assume that at time $t$ there $y(t)$ persons who have actually got espresso out of the machine (coffee machine lovers),
and then there are $x(t)$ who did not get any coffee last time they tried the machine (coffee machine haters).
.PP
The satisfied customers are rapidly returning to the group of potential customers; they do so at a per capita rate $c$, i.e., $cy(t)$.
Similarly the unsatisfied ones return at a much lower rate (for instance when they forget their thermoses at home). We assume that that is happening at a $bx(t)$.
.PP
There arose a new behaviour among the unsatiesfied customers:
They started to inform potential customers that the espresso machine was really unreliable.
This spread as an epidemic,
so we model it as such,
see for example.
.[\~
#
Anderson May nature 1979
.]\&
Assume that the contact rate beteen potential customers and coffee haters is proportional to the product between the two,  $beta  x(t) n(t)$.
.PP
Given these considerations, and assumptions (Figure 1),
we can formulate the dynamics of the population using one ordinary differential equation per group.
First, the number of potential customers at time $t$ is $n(t)$, and it changes according to Eq. 1:
.LP
.EQ (1)
diff( n(t) , t ) = - a n(t) + b y(t)  + c x(t) - beta  n x(t)
.EN
.LP
Second, $x(t)$, the number of coffee machine haters, follows Eq. 2 and,
.LP
.EQ (2)
diff( x(t) , t ) = a (1-p) n(t) - c x(t)  + beta  n(t) x(t)
.EN
.LP
thirdly, the the number of coffee machine lovers, $y(t)$ change according to.
.LP
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
The development of the population:
$n(t)$  (potential customers),
$x(t)$ (coffee machine lovers) and $y(t)$ (coffee machine haters),
over a period of 20 days.
The initial condition was $n(0)=1$ and $x(0)=y(0)=0$.
The parameters used $a = 1$,
$b = 1$,
$c = 0.1$,
$p = 0.25$ and
$beta = 0.2$.
The $p$ favours the buildup of a sizeable group of coffee machine haters,
a development which is further accelerated by their spreading of the rumours about the unreliability of the espresso machine.
Finally, the way the model is built,
$1/b$ and $1/c$ are the expected times before coffee machine lovers and haters, respectively,
become potential customers.
The two parameters are set such that the coffee lovers are again willing to try the machine the day after their successful buy,
while the haters bring their thermoses 10 days before they care to try it again.
.sp
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
The knowledge of the deficiencies of the espresso machine spread rapidly on the campus.
In less than 10 days close to 80% of faculty and students decided to bring their own coffee, hot and strong, from home.
.so back-matter.ms
