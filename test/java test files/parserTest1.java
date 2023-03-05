class Test{
void test()
{
  // comments

  // declarations
  int x;
  int y = 1;
  int a, b;
  int c = 3, d, e = 1;
  boolean b1 = true, b2=false,b3; 
  String s = "Hello"; //"\n\t"

  /*
   * MORE COMMENTS
   */

  // expressions
  x = 6;
  y =-y+1*2-2; //
  y = -y+1*2-2; //
  a =(x%2)-2;
  b3 = x < (a+y);
  x += 2;
  x *= 2;
  x /= x;
  x -= 2 - 2;
  x %= 22;
  x = max(1,2,3);


  // statements
  if (x<7)
  {
      if (x<8)
      {
          if ((x=5)>7) print(2+2);
          for (int i=0; true;i=6)
          {
              for (int j=0; true; j=6) if (true) x = x+1;
          }
      }
  }
  x = 1+(y = 3);
  boolean q = (x=2) > 5;
  a =1- -1; // dit mag in java alleen met spatie ertussen
  a=1+-2;
  b=-1- -1;
  c=5+ +9;
  //boolean bb = !!true; /// :(

  String s = "hello"+"hi";
  while(true)
      while (true)
          print("infinite");

  if (true) ;
  if (true) {}
  if (true) {{}}
  if (true) {;;;;}
  if (true){;;;;x=0;;;;;x=1;;;;;}
  System.out.println((5<=10)&&b2 || !b1 || b1==true);
  		
  int  j;
  for( int k = 1; i<=10;i++)
  {
    int test=0;
    for(j=1;j<=i;j++)
    System.out.print("*");
    System.out.print("+");
  }
  		
  		for (;;)
  		{
  			int test=0;
  			i=99;
  			System.out.println(++i);
              break;
              continue;
  		}
   int x = functie(0, 9+9, "sdf");
   f(true, false);
   y = f(true) + y() + yy("");

  functie(x, "sdf", 1);

}}