public static int hasDoubled(double savings, int interest)
{
    double target = 2 * savings;
    int years;
    for (years = 0; ; )
    {
        if(target > savings)
        {
            savings *= interest/100.0 + 1;
            years++;
        }
        else if (target <= savings)
        {
            break;
        }
    }
    return years;
}
