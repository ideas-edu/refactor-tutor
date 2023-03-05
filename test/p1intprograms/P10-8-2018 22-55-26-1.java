int sum = 0;
for (int i = 0; i < values.length; i++) {
    if (positivesOnly == true) {
        if (values[i] >= 0) {
            sum += values[i];
        }
    } else {
        sum += values[i];
    }
}
return sum;
