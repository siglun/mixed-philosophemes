.EQ
delim $$
.EN
.PS
scale=2.54
moveht=0.75
boxht=0.75
movewid=0.9
fitch_lines=0.3

depth=0;
max_depth=1

stepno=0;
step_total=0;


define set_steps_and_depths {
	step_total = $1;
	max_depth  = $2;
}

define start_proof {
	depth=depth+1;
	$1: box invis at depth*movewid,(step_total-stepno)*moveht;
}

define line_from_to {
	line from $1 to $2
}

define end_proof {
	move to	$1.w.x+fitch_lines,last box.sw.y
	line to $1.w.x+fitch_lines,$1.sw.y-0.2
	depth=depth-1;	
}

define add_premis {
	add_step($1,$2,$3)
}

define premis_end {
	move to	$1.w.x+fitch_lines,last box.sw.y; line right
}

define add_step {
	stepno=stepno+1;
	move to $1.nw;
	for i=1 to stepno do {
		move down;
	}
	box invis sprintf("%g",stepno) at 0,(step_total-stepno)*moveht;
	box invis ljust $2 at depth*movewid,(step_total-stepno)*moveht;
	box invis $3 ljust at max_depth*movewid,(step_total-stepno)*moveht;
}

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
