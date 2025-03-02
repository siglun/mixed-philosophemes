.so parameters.ms
.mso tmac/decorations.tmac
.EQ
delim $$
.EN
.so fitch-macros.pic
.TL
How to type-set Fitch natural deductions using
.br
.smallcaps
gnu
./smallcaps
troff, pic and eqn
.AU
Sigrid Lundberg
sigfrid (at) sigfrid-lundberg.se
.SH
Introduction
.LP
.anfang F 3
itch is a notation for natural deduction,
.[
natural deduction stanford
.]
and \f(CRtroff\fP is a software system for type-setting using Unix\f(CR\(tm\fP and related operating systems.
.[
ossanna kernighan
.]
Brian W. Kernighan was one of the creators of Unix and the C programming language.
pic is a system for typesetting graphs, also created by.
.[\& 
#
kernighan pic
.]
.smallcaps
groff
aka
gnu
./smallcaps
troff is the implementation I am using.
.[
Foundation groff
.]
There are other competitors,
but this is the version I use.
.PP
The Fitch notations has got its name after its inventor, Fredric Fitch.
This notation seems to be a de facto standard:
It is used in all the text books I have been able to find electronically,
and seems to be taught at logics courses in mathematics as well as philosophy.
I wrote this note while learning Fitch;
I used the writing was a method for learning.
My intention is to demonstrate how to format natural deduction on this platform.
I cannot teach you how to format scientific text, neither can I give an introduction to natural deduction.
.KF
.ID
.PS
.ps 10
.vs 12
scale=2.54
moveht=0.75
boxht=0.75
movewid=0.9
fitch_lines=0.3

set_steps_and_depths(8,3)

start_proof(START);
add_premis(START,"$A∨B$");
add_premis(START,"$¬  A$");
premis_end(START);

start_proof(SUB1);
add_premis(SUB1,"$A$");
premis_end(SUB1);
add_step(SUB1,"$⊥$","⊥ Intro: 3,2");
add_step(SUB1,"$B$","⊥ Elim: 4");
end_proof(SUB1);

start_proof(SUB2);
add_premis(SUB2,"$B$");
premis_end(SUB2);
add_step(SUB2,"$B$","Reit: 6");
end_proof(SUB2);

add_step(START,"$B$","∨ Elim: 6-7,3-5,1");
end_proof(START)

.PE
.DE
.IP
.vs 12
.ps 10
Figure 1. Proof that $A ∨ B, ¬ A ∴ B$.
.KE
.KF
.ID
.ft CR
.ps 10
.vs 12
set_steps_and_depths(8,3)

start_proof(START);
add_premis(START,"$A∨B$");
add_premis(START,"$¬  A$");
premis_end(START);

start_proof(SUB1);
add_premis(SUB1,"$A$");
premis_end(SUB1);
add_step(SUB1,"$⊥$","⊥ Intro: 3,2");
add_step(SUB1,"$B$","⊥ Elim: 4");
end_proof(SUB1);

start_proof(SUB2);
add_premis(SUB2,"$B$");
premis_end(SUB2);
add_step(SUB2,"$B$","Reit: 6");
end_proof(SUB2);

add_step(START,"$B$","∨ Elim: 6-7,3-5,1");
end_proof(START)
.DE
.IP
.vs 12
.ps 10
Figure 2. The
.smallcaps
pic
./smallcaps
code needed to generate Figure 1.
.KE

.so back-matter.ms
