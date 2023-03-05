int sum = 0;

foreach(number ->
 { 
   sum = addNumber(sum, number, positivesOnly);
 }
  
  return sum;
}

public int addNumber(int value, int number, boolean positivesOnly)
{
  if(positivesOnly && number >= 0)
    {
            value+= number;       
    }
    else if(!positivesOnly)
    {
        value+= number;
    }

reutrn value;
}