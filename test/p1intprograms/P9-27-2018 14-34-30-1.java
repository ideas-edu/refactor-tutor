int sum = 0;
for (int i = 0; i < values.length; i++) {
    if ((positivesOnly == true && values[i] >= 0) || positivesOnly == false) {
              sum += values[i];
       }
 }
return sum;