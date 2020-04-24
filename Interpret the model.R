#Interpret the model
##Variables names
##node.caps1,breast.quad1,breast.quad2,breast.quad4 are $X_{1}$,$X_{2}$,$X_{3}$,$X_{4}$, respectively.
##tumor.size is Y
The model above is achieved by using the lm() function in R and the output is called using the summary() function on the model.
Formula Call:
  $Y=4,14+0,69$X_{1}$ +1,36$X_{2}$ +1,56$X_{3}$ +2,20$X_{4}$$

residuals : break down into 5 sumary point:  Min, Max, Median, 1Q,3Q. In the model, we can see that the distribution of the residuals do not appear to be strongly symetrical. 

Cofficients : reprensent the intercept, slope terms, P-value
Another results : R-square = 0.1368, Adjusted R squared = 0.1144, F statistic = 6.092

The following section, we will calculate Y hat:
\
Finally, we will have Residual testing:
  
As you can see on three images above:
+The mean of residuals is approximately zero
+ Variance is always constant
+ Follow a normal distribution
