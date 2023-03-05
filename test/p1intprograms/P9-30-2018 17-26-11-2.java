(after changing input type)

int sum = 0;

foreach(number ->
 {
    if(positivesOnly == true)
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
        