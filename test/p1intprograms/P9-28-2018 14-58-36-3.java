int sum = 0;
boolean everything = ! positivesOnly;
for (int i = 0; i < values.length; i++) {
    if ( everything ||  (values[i] >= 0)) {
        sum += values[i];
    }
}
return sum;