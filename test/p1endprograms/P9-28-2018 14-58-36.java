int sum = 0;
boolean everything = ! positivesOnly;
for (int v : values ) {
    if ( everything ||  (v >= 0)) {
        sum += v;
    }
}
return sum;