public class Test { public static int sumValues(int [] values, boolean positivesOnly) {
    int sum = 0;
    for (int i = 0; i < values.length; i++) {
        int currentValue = values[i];
       if (positivesOnly && currentValue < 0)
    	 currentValue = 0;
       sum += currentValue;
       
    }
    return sum;
}
}