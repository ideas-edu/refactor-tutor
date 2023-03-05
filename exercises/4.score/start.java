public static int calculateScore(int changes, int day)
{
    int score = 10;
    for (int i = 0; i < changes; i++)
    {
        score = score - 1;
    }
    if (day == 6 || day == 7)
    {
        return score;
    }
    else
    {
        score = score - 3;
        return score;
    }
}