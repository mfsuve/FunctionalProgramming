# Answers
## Question 2
 - **What does the helper function (sundays') calculate?**

Since I send `start` and `1` as parameters to *(sundays')* function, it calculates how many Sundays fell on the first of the month, where month starts from `1`, starting from the year `start`. The year ends when year value reaches the year `end`, which is given in the upper level function *sundays1*.
 - **What if you don't define a "rest" and use its expression where it's needed?**

In that case, we would have one less parameter in our helper function. However the `otherwise` part would look harder to understand. Besides, since we are using `rest` in two places, we would need to write its expression twice which is worse than the first situation.
