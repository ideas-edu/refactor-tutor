public void test(boolean p, boolean q, int x){

  // removeUselessIf_1
  if(p) ;
  if(p) {}

  // removeUselessIf_2
  if(true) x++;
  if(true) {x++;}

  // removeUselessIf_3
  if(false) x++;
  if(false) {x++;}

  // removeUselessIf_1
  if(p) ; else ;
  if(p) {} else {}
  if(p) ; else {}
  if(p) {} else ;

  // removeUselessIf_2
  if(true) x++; else x--;
  if(true) {x++;} else {x--;}

  //removeUselessIf_3
  if(false) x++; else x--;
  if(false) {x++;} else { x--; }

  //removeUselessCheck.1
  if(p) x++; else if (!p) x--;
  if(p) x++; else {if (!p) x--;}
  if(x==1) x++; else if (x!=1) x--;
  if(x>1) x++; else if (x<=1) x--;
    
  //removeUselessCheck.2
  if(p) x++; else {if (!p) x--; else x+=2;}
  if(p) x++; else {if (!p) x--; else x+=2;}
  if(x==1) x++; else {if (x!=1) x--; else x+=2;}

}