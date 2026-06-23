# Hypothesis Intervals

Public repository to host code associated with *Getting Straight on Standard Error: Confidence Intervals vs. Hypothesis Intervals*. 

1) zprop.test.R main user-directed function.  Discussed in Section 2.4.  More documentation on this particular function below.
2) two_sample_grid_search.R grid search for example of divergence in two-sample setting.  Discussed in Section 4.
3) HIvsWald_nonrejection.R comparison of hypothesis interval and Wald non-rejection regions (Figure 2).  Discussed in Section 3.2.
4) HIvsWald_disagreement_prob.R disagreement probability under H0 between the hypothesis interval and Wald rules (Figure 3).  Discussed in Section 3.2.



## Documentation for 1 above

### Description
`zprop.test` can be used for testing whether a population proportion is equal to a specified null value.  Notably, it also provides hypothesis intervals that are consistent with the associated z test statistic.  For reasons given in Section 4, this is currently written solely for the single-sample setting. 

### Usage 
zprop.test(x, n, p0 = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, digits = 5)
### Arguments
x: number of success, the numerator of the sample proportion.    
n: number of trials, the denominator of the sample proportion.   
p0: the null hypothesized value (default is p0 = 0.5) of the population proportion.    
alternative: a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".   
conf.level: confidence level of the returned confidence interval. Must be a single number between 0 and 1. Only used when testing the null that a single proportion equals a given value, or that two proportions are equal; ignored otherwise.

### Value
statistic: the value of z test statistic   
p.value: the p-value of the test  
estimate: sample proportion x/n  
conf.int: a hypothesis interval at the specified conf.level.  That is, a confidence interval, using the p0 value in the standard error, such that it is appropriate for pronouncing on the associated hypothesis test.  


