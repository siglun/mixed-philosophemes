.mso tmac/decorations.tmac
.so parameters.ms
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
.LP
Fitch is a notation for natural deduction,
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
.IP
Table 1. Unicode characters for logical signs and operators.
On some operating systems you can type them by pressing \f[CR]ctrl-shift-u\fP and then the four character code (following \f(CRu+\fP). The Groff name is usually better to use than the Unicode character,
but I tend to use the latter.
.TS
tab(;);
lb lb lb ;
l l l .
Unicode;Character;Groff name
u+00AC;¬;\\(no	
u+2227;∧;
u+2228;∨;
u+2200;∀;
u+2203;∃;
u+2192;→;
u+2194;↔;
u+22A5;⊥;
u+22A2;⊢;
u+2261;≡;
u+25A1;□;
u+25C7;◇;
u+2234;∴;
U+2208;∈;
U+2209;∉;
.TE
.KE
.KF
.PS
.ps 10
.vs 12
scale=2.54
moveht=0.75
boxht=0.75
movewid=0.9
fitch_margin=0.5

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
.IP
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
.KF
.ID
.ft CR
.ps 10
.vs 12

#
# The proof is initialized by calling this macro, which
# informs the scripts on the number of steps in the proof
# and its maxiumum depth, i.e., how many proofs we have
# inside proofs.
#

set_steps_and_depths(8,3)

#
# Any proof (the root proof or any sub-proof) starts
# with the start_proof() which also names that proof.
#
# after started we add its premises, and end it with
# premis_end()

start_proof(START);
add_premis(START,"$A∨B$");
add_premis(START,"$¬  A$");
premis_end(START);

#
# Here comes the sub-proofs
#

start_proof(SUB1);
add_premis(SUB1,"$A$");
premis_end(SUB1);

#
# The add_step() macro has three argument, the name of the
# current proof, the result of the step, and finally the
# references to the steps needed for reaching the step.
#

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
.KF
.EQ
sdefine falsum  %{type "operator" vcenter ⊥}%
.EN
.ID
.PS
.vs 12
.ps 10
scale=2.54
moveht=0.75
boxht=0.75
movewid=1.0
fitch_margin=0.4

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
add_step(SUB21,"$falsum$","⊥ Intro:6,5");
add_step(SUB21,"$A \[OR]  C$","⊥ Elim:7");
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
.IP
.vs 12
.ps 10
Figure 3. A longer example, "$A\[OR]B, \[no]B\[OR]C ∴ A\[OR]C$".
.KE
.so back-matter.ms
