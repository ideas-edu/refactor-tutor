int sum = 0;

foreach(number ->
 {
    if(positivesOnly)
    {
        if(number >= 0){
            sum+= number;
         }
    }
    else
    {
        sum+= number;
    }
   }
  
  return sum;