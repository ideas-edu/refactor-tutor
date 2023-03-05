public void test(boolean p, boolean q, int x){
        
   if (p==true) ;
   if (p!=false);
   if (p && p);
   if (p && (q || p)) ;
   if (p && (q || !p)) ;
   if (p || !p) ;
   if (! (!p) );
   
   x = 4 + x;
   x = 1 + x;
   x = x;
}