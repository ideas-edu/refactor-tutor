int sum = 0;
for (int value: values) {
    if (positivesOnly) {
        if (value >= 0) {
            sum += value;
        }
    } else {
        sum += value;
    }
}
return sum;