int sum = 0;
for (int value : values) {
    if (positivesOnly == true) { 
        if (value >= 0) {
            sum += value;
        }
    } else {
        sum += value;
    }
}
return sum;