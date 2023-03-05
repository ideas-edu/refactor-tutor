int sum = 0;

foreach(number ->
 {
    if(positivesOnly && number >= 0)
    {
            sum+= number;       
    }
    else if(!positivesOnly)
    {
        sum+= number;
    }
   }
  
  return sum;