public static int oddSum(int[] array) {
     int total = 0;
     boolean stop = false;

     for (int i = 1; i < array.length; i = i + 2) {
         if (stop == false) {
             if (array[i] != -1) {
                 total += array[i];
             } else if (array[i] == -1) {
                 stop = true;
             }
         }
         else {
             total = total;
         }
     }
     return total;
 }