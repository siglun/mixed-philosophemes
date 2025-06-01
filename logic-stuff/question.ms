.so fitch-macros.pic
.so parameters.ms
.SH
Härledning
.LP
Visa att
.EQ
forall(x) (Tx implies (Lx or Mx))
.EN
Givet
.EQ
not exists(x) (Tx and Sx)
.EN 
och
.EQ
forall(y) (Sy or My or Ly)
.EN
.PS
.ps 10
.vs 12
scale=2.54
moveht=0.75
boxht=0.75
movewid=0.9

set_steps_and_depths(15,5)

start_proof(START);
add_premis(START,"$not exists(x) (Tx and Sx)$");
add_premis(START,"$forall(y) (Sy or My or Ly)$");
premis_end(START);

start_proof(IMPL);
add_premis(IMPL,"$forall(x) Tx$");
premis_end(IMPL);

start_proof(UNIV);
add_premis(UNIV,"$[a]$");
premis_end(UNIV);

add_step(UNIV,"$Sa or (Ma or La)$","$any$ Elim 2");
add_step(UNIV,"$Ta$","$any$ Elim 3");

start_proof(DIS1);
add_premis(DIS1,"$Sa$");
premis_end(DIS1);

add_step(DIS1,"$Ta and Sa$","$and$ Intro 6,7");
add_step(DIS1,"$exists(x)(Tx and Sx)$","$exists()$ Intro 8");
add_step(DIS1,"$falsum$","$falsum$ Intro 9,1");
add_step(DIS1,"$Ma or La$","$falsum$ Elim 10");

end_proof(DIS1);

start_proof(DIS2);
add_premis(DIS2,"$Ma or La$");
premis_end(DIS2);
end_proof(DIS2);

add_step(UNIV,"$Ma or La$","$or$ Elim 5,7-11,12");
end_proof(UNIV);

add_step(IMPL,"$forall(x) (Mx or Lx)$","$any$ Intro 4-13");

end_proof(IMPL);

add_step(START,"$forall(x) Tx implies forall(x)(Mx or Lx)$","$implies$ Intro 3-14");

end_proof(START);
.PE

