int sum = 0;
if (positivesOnly == true) {
  for (int i=0; i < values.length; i++) 
     if (values[i] > 0) { sum += values[i]; }
}
else 
 for (int i=0; i < values.length; i++) {
     sum += values[i];
}
return sum;