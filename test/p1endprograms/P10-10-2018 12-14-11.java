int sum = 0;
    if (positivesOnly) {
       sum = values.Sum(number => number > 0 ? number : 0);
    } else {
        sum values.Sum();
    }

return sum;