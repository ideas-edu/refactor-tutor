int sum = 0;
for (int i = 0; i < values.length; i++) {
    int currentValue = values[i];
    if (positivesOnly && currentValue < 0)
	currentValue = 0;
    //preserve function
    if (positivesOnly == true) {
        if (values[i] >= 0) {
            sum += values[i];
        }
    } else {
        sum += values[i];
    }
}
return sum;

