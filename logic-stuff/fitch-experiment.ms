
.PS
proofno=0;
stepno=0;
START: box invis;

define start_proof {
	box invis;
}

define end_proof {}

define add_premis {
	add_step($1,$2)
}

define premis_end {
	line right from START.nw.x,last box.sw.y
}

define add_step {
	stepno=stepno+1;
	move to START.nw;
	for i=0 to stepno do {
		move down;
	}
	box invis sprintf("%g",stepno);
	box invis $1 with .w at last box.e;
	box invis $2 with .w at last box.e;
}

start_proof("START");

add_premis("A∨B");
add_premis("¬A");
define premis_end;
add_step("A");
add_step("⊥","⊥ Intro: 3,2");
add_step("B","⊥ Elim: 4");
add_step("B");
add_step("B","Reit: 6");
add_step("B","∨ Elim: 6-7,3-5,1");
.PE
