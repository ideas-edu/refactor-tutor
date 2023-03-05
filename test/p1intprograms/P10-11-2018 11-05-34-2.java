int sum = 0;
for (int i = 0; i < values.length; i++) {
    if (positivesOnly == false) {       
        sum += values[i];
    } else if(values[i] >= 0) {
       sum += values[i];
    }
}
return sum;