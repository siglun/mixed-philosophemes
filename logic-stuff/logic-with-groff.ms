.so fitch-macros.pic
.so parameters.ms
.TL
How to type-set logic and natural deductions using
.br
.smallcaps
gnu
./smallcaps
troff, pic and eqn
.AU
Sigrid Lundberg
sigfrid (at) sigfrid-lundberg.se
.LP
.\"anfang F 3
Fitch is a notation for natural deduction.
.[
natural deduction stanford
.]
\f(CRtroff\fP is a software system for type-setting using Unix\f(CR\(tm\fP and related operating systems.
.[
ossanna kernighan
.]
Brian W. Kernighan was one of the creators of Unix and the C programming language.
pic
.[
kernighan pic
.]
and eqn
.[
kernighan mathematics
.]
are systems for typesetting graphs and mathematics, also created by Brian Kernighan and his friends.
This group created a full set of tools for the type-setting of scientific text, graphs and diagrams, mathematics, chemistry, reference management and complex tables.
.smallcaps
groff
aka
gnu
./smallcaps
troff is the implementation I am using.
.[
Foundation groff
.]
There are other competitors, in particular the slightly younger TeX and LaTeX.
There are also alternative implementations of troff,
but this is the version I use.
.PP
The Fitch notations has got its name after its inventor, Fredric Fitch.
This notation seems to be a de facto standard:
It is used in all the text books I have been able to find electronically,
and seems to be taught at logics courses in mathematics as well as philosophy.
I wrote this note while learning Fitch;
My intention is to demonstrate how to write predicate and propositional logic, and deduction on this platform.
I cannot teach you how to format scientific text in general,
neither can I give an introduction to logic and natural deduction.
.SH
Writing equations
.LP
First, we need to be able to write our texts
.[\& (there is a good tutorial by  
#
branden kollar
.]\&) 
and then we can continue with logical formulas and sentences using \f(CReqn\fP.
.[
harding eqn
.]
At a first glance, all of them are are \(lqequations\(rq,
or anyone who are not familiar with mathematics and logic and the differences between the two will regard them as such.
Here is a set of predicate logic sentences, first in \f(CReqn\fP source,
.ID
\&\f(CR.EQ (1)
    pile {
	\\[fa] x SameSize(x)
	above
	\\[fa] x Cube(x) -> Cube(b)
	above	
	(Cube(b) \\[AN] b=c) -> Small(c)
	above
	(Small(b) \\[AN] SameSize(b,c) -> Small(c)
   }
\&.EN\fP
.DE
and then formatted, in Equation (1).
.EQ (1)
pile {
\[fa] x SameSize(x)
above
\[fa] x Cube(x) -> Cube(b)
above
(Cube(b) \[AN] b=c) -> Small(c)
above
(Small(b) \[AN] SameSize(b,c) -> Small(c)
}
.EN
.PP
To write these formulas, you need to use either the unicode characters or their Groff names, see Table 1.
.SH
Using logics inside graphs
.LP
.KF
.sp
.QP
.vs 12
.ps 10
Table 1. Unicode characters for logical signs and operators.
On some operating systems you can type them by pressing \f[CR]ctrl-shift-u\fP and then the four character code (following \f(CRu+\fP). The Groff name is usually better to use than the Unicode character,
but I tend to use the latter.
.TS
allbox center tab(;);
lb lb lb ;
l l l .
Unicode;Character;Groff name
u+00AC;¬;\\[no]	
u+2227;∧;\\[AN]
u+2228;∨;\\[OR]
u+2200;∀;\\[fa]
u+2203;∃;\\[te]
u+2192;→;\\[->]
u+2194;↔;\\[<>]
u+22A5;⊥;\\[pp]
u+22A2;⊢;
u+2261;≡;\\[==]
u+25A1;□;\\[sq]
u+25C7;◇;
u+2234;∴;\\[tf]
U+2208;∈;\\[mo]
U+2209;∉;\\[nm]
U+2286;⊆;\\[ib]
.TE
.KE
.KF
.so aristotelian-syllogism.ms
.QP
.vs 12
.ps 10
The traditional aristotelian syllogisms.
.KE
.KF
.PS
.ps 10
.vs 12
scale=2.54
moveht=0.75
boxht=0.75
movewid=0.9

set_steps_and_depths(8,3)

start_proof(START);
add_premis(START,"$A∨B$");
add_premis(START,"$¬  A$");
premis_end(START);

start_proof(SUB1);
add_premis(SUB1,"$A$");
premis_end(SUB1);
add_step(SUB1,"$\[pp]$","\[pp] Intro: 3,2");
add_step(SUB1,"$B$","\[pp] Elim: 4");
end_proof(SUB1);

start_proof(SUB2);
add_premis(SUB2,"$B$");
premis_end(SUB2);
add_step(SUB2,"$B$","Reit: 6");
end_proof(SUB2);

add_step(START,"$B$","∨ Elim: 6-7,3-5,1");
end_proof(START)
.PE
.QP
.vs 12
.ps 10
Figure 1. Proof that $A ∨ B, ¬ A ∴ B$.
The line numbering is in the left-most margin.
Then there is a vertical line, as long as the proof.
The step 1-2 in the proof is where the premises lives.
The horisontal line after step 2 is usually referred to as the \fIfitch line.\fP
The two groups, 3\(en5 and 3\(en6 are sub-proofs,
with their own premisses,
vertical lines and fitch lines
.sp
.KE
.SH
How to write Fitch in troff
.LP
Any proof is initialized by calling this macro,
which informs all scripts on the number of steps in the proof and its maxiumum depth,
i.e.,
how deep the hierarchy of proofs is.
That is, how many inside proofs, whitin proofs ... do we have.
You better add one or the references at the right will come to close to the logical statements.
.ID
\f(CRset_steps_and_depths(8,3)
.DE
.PP
Any proof (the root proof or any sub-proof) starts
with the start_proof() macro,
which also names that proof.
After we have started the proof,
we add its premises,
and end it with \f(CRpremis_end()\fP.
.ID
\f(CRstart_proof(START);
add_premis(START,"$A∨B$");
add_premis(START,"$¬  A$");
premis_end(START);\fP
.DE
.PP
After ending the premiws section,
we enter the body of our proofs.
In this case we start the sub-proofs
.PP
.ID
\f(CRstart_proof(SUB1);
add_premis(SUB1,"$A$");
premis_end(SUB1);\fP
.DE
.PP
In the body of a proof, we use the \f(CRadd_step()\fP macro,
which has three argument: (i) the name of the current proof,
(ii) the result of the step, and finally (iii)
the references to earlier steps needed for the step.
.ID
\f(CRadd_step(SUB1,"$\[pp]$","\[pp] Intro: 3,2");
add_step(SUB1,"$B$","\[pp] Elim: 4");
end_proof(SUB1);
.DE
.PP
We end a proof (be it a sub_proof or a proof) with the \f(CRend_proof()\fP macro,
which needs the name of the current proof as an argument.
Now we start another subproof.
.ID
\f(CRstart_proof(SUB2);
add_premis(SUB2,"$B$");
premis_end(SUB2);
add_step(SUB2,"$B$","Reit: 6");
end_proof(SUB2);
.DE
.PP
After we have completed the two sub-proofs,
return to the main proof and completes it with a nice $\(OR$ elimination.
.ID
\f(CRadd_step(START,"$B$","∨ Elim: 6-7,3-5,1");
end_proof(START)
.DE
.PP
Note that the macros do not check your references.
Sanity checks and proof reading is your job.
.KF
.EQ
sdefine falsum  %{type "operator" vcenter \[pp]}%
.EN
.ID
.PS
.vs 12
.ps 10
scale=2.54
moveht=0.75
boxht=0.75
movewid=1.0

set_steps_and_depths(12,5);

start_proof(START);
add_premis(START,"$A \[OR] B$");
add_premis(START,"$\[no]B \[OR] C$");
premis_end(START);

start_proof(SUB1);	
add_premis(SUB1,"$A$");
premis_end(SUB1);
add_step(SUB1,"$A \[OR] C$","\[OR] Intro:3");
end_proof(SUB1)

start_proof(SUB2);
add_premis(SUB2,"$B$");
premis_end(SUB2);

start_proof(SUB21);
add_premis(SUB21,"$\[no]B$");
premis_end(SUB21);
add_step(SUB21,"$falsum$","\\[pp] Intro:6,5");
add_step(SUB21,"$A \[OR]  C$","\\[pp] Elim:7");
end_proof(SUB21);

start_proof(SUB22);
add_premis(SUB22,"$C$");
premis_end(SUB22);
add_step(SUB22,"$A \[OR] C$","\[OR] Intro:9");
end_proof(SUB22);

add_step(SUB2,"$A \[OR] C$","\[OR] Elim:6-8,9-10,2");
end_proof(SUB2);

add_step(START,"$A \[OR] C$","\[OR] Elim:1,3-4,5-11");

end_proof(START);
.PE
.DE
.QP
.vs 12
.ps 10
Figure 2. A slightly longer example: Prove that $A\[OR]B, \[no]B\[OR]C ∴ A\[OR]C$.
.KE
.KF
.so QL-proof-1.ms
.QP
.vs 12
.ps 10
A QL proof
.KE
.[
$LIST$
.]
.so back-matter.ms
