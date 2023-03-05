int sum = 0;
for (int i = 0; i < values.length; i++) {
    if (this.shouldBeAdded(values[i], positivesOnly)) {
          sum += values[i];
	}
}
return sum;

private boolean shouldBeAdded(int toBeAdded, boolean positivesOnly) {
	if (positivesOnly && toBeAdded < 0) 
		return false;
	return true;
}