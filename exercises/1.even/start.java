public static int countEven(int [] values)
{
    int count;
    count = 0;
    for (int i = 0;i < values.length;i++)
    {
        if (values[i] % 2 != 1)
        {
            count = count + 1;
        }
        else
        {
            count = count;
        }
    }
    return count;
}
