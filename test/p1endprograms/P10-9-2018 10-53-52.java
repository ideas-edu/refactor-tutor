int sum = 0;
for (int i = 0; i < values.length; i++) {
    if (!positivesOnly || values[i] >=0) {
        sum += values[i];
    }
}
return sum;