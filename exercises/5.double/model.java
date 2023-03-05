public static int hasDoubled(int savings, int interest)
{
    int target = 2 * savings;
    int jaren = 0;

    while (savings < target)
    {
        savings *= interest/100 + 1;
        jaren++;
    }

    return jaren;
}
