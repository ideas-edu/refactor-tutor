public static int sumValues(int [] values, boolean positivesOnly)
{
 	int sum = 0;                            
 for (int i = 0; i < values.length; i++)
 {                                      
     if (positivesOnly)                 
     {                                  
         if (0 <= values[i])            
         {                              
             sum += values[i];          
         }                              
         else {}                        
     }                                  
     else                               
     {                                  
         sum = values[i] + sum;         
     }                                  
 }                                       
return s;
}