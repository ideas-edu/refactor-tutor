public void test(boolean p, boolean q, int x){

   //causes problems
   if (!p)
   {
      x++;
   }
  else 
    x--;
  
  if(p)
        x++;
      
    else x++;

    if(p)
        x++;
    else {}

    if (p) {}
    else x++;

    if (!p)
      {}
    else
      x++;

// does not work yet
/*if(p)
        x++;
      
    else x++;*/
    
}