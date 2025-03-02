
.PS
scale=2.54
moveht=0.75
boxht=0.75
movewid=1.0


proofno=0;
stepno=0;

define start_proof {
	box invis;
}

define line_from_to {
	line from $1 to $2
}

define end_proof {
	move to	START.w.x+0.5,last box.sw.y; move right;	
	line to START.w.x+0.5+movewid,START.sw.y
}

define add_premis {
	add_step($1,$2)
}

define premis_end {
	move to	START.w.x+0.5,last box.sw.y; move right; line right
}

define add_step {
	stepno=stepno+1;
	move to START.nw;
	for i=1 to stepno do {
		move down;
	}
	box invis sprintf("%g",stepno) with .nw at START.nw.x,last box.sw.y;
	box invis ljust $1 with .w at last box.c;
	box invis ljust $2 with .w at last box.e;
}



START: box invis;

start_proof(START);

add_premis("A∨B");
add_premis("¬A");
premis_end;
SUB1: {
add_step("A");
add_step("⊥","⊥ Intro: 3,2");
add_step("B","⊥ Elim: 4");
}
SUB2: {
add_step("B");
add_step("B","Reit: 6");
add_step("B","∨ Elim: 6-7,3-5,1");
}
end_proof(START)

.PE
