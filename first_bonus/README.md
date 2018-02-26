# Answers
## Question 2
 - **What does the helper function (sundays') calculate?**

Since I send `start` and `1` as parameters to *(sundays')* function, it calculates how many Sundays fell on the first of the month, where month starts from `1`, starting from the year `start`. The year ends when year value reaches the year `end`, which is given in the upper level function *sundays1*.
 - **What if you don't define a "rest" and use its expression where it's needed?**

In that case, we would have one less parameter in our helper function. However the `otherwise` part would look harder to understand. Besides, since we are using `rest` in two places, we would need to write its expression twice which is worse than the first situation.

## Question 5
For this question, I created a test function called `day` which calculates the number of days in years. This function takes two parameters. The first parameter is the starting year and the second one is how many year to count. For example if the input is `1900 2` then the function returns the sum of the number of days in years `1900` and `1901`. This function makes use of the `daysInMonth` function.

I also created a test function that tries some starting year values with the second parameter being 400 and checks if that function with those parameters returns a value that is multiple of 7. I run that test and it passed. Therefore I can say that the number of days in 400 years is a multiple of 7.

Since all the weeks in 400 years are whole weeks we can say that all of the days in a week occur at the same rate in those years. The number of all the seven days of a week in 400 years are the same. Therefore, the possibility that a certain day of a month is a Sunday is `1/7`. Also all days are equally possible.
