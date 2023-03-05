int sum = 0;
for (int i : values) {
    if (positivesOnly && i<0) 
       continue;
    
    sum += i;
   
}
return sum;