int sum = 0;
for (int i = 0; i < values.length; i++) {
    sum += getValueToBeAdded(value[i], positivesOnly);
}
return sum;


private int getValueToBeAdded(int toBeAdded, boolean positivesOnly) {
	if (this.shouldBeAdded(toBeAdded, positivesOnly)) {
              return toBeAdded;
	}
	return 0;
}

private boolean shouldBeAdded(int toBeAdded, boolean positivesOnly) {
	if (positivesOnly && toBeAdded < 0) 
		return false;
	}
	return false;
}
