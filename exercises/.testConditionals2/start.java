public void test(boolean p, boolean q, int x){

  if(p)
  {
    if(q)
      x++;
  }
  else
    x++;

  if(p)
  {
    if(q){
      x++;
    }
  }
  else{
    x++;
  }

  if(p)
  {
    if(q){
      x++;
    }
  }
  else
    x++;
  
  //IfElse p a (IfElse q b a) :~> IfElse (nt p .&&. q) b a 

  if(p)
  {
    x++;
  }
  else
  {
      if(q){
        x--;
        } 
      else x++;
  }

    
}