int sum = 0;
for (int value : values) {
    if (!positivesOnly || (positivesOnly && value >= 0)) {
        sum += value;
    }
}
return sum;