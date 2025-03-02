.so parameters.ms
.EQ
delim $$
.EN
.so fitch-macros.pic
.PS

scale=2.54
moveht=0.75
boxht=0.75
movewid=0.9
fitch_lines=0.3

set_steps_and_depths(8,3)

start_proof(START);
add_premis(START,"$A∨B$");
add_premis(START,"$¬A$");
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
