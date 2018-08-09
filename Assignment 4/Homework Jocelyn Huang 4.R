# 1
# a)
x = 15
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)


zscore = (means - 26.7) /(4.4/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.049


# b)
x = 30
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)


zscore = (means - 26.7) /(4.4/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

#[1] 0.0512


# c)
x = 45
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)


zscore = (means - 26.7) /(4.4/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)

proportion = sum(logic)/10000
proportion


# [1] 0.0525

# 2 )
# a )
x = 15
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)

zscore = (means - 26.7) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),14)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)

proportion = sum(logic)/10000
proportion

# [1] 0.0487


# b )
x = 30
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)

zscore = (means - 26.7) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),14)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)

proportion = sum(logic)/10000
proportion
# [1] 0.0435

# c )
x = 45
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)

zscore = (means - 26.7) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),14)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)

proportion = sum(logic)/10000
proportion
# [1] 0.0366

# 3 )
# a)
x = 15
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)


zscore = (means - 26.7) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.0683


# b)
x = 30
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)


zscore = (means - 26.7) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.0581


# c)
x = 45
Samp = replicate(10000,rnorm(x,26.7, 4.4))
Samp[1:30]
means = apply(Samp,2,mean)
stdevs = apply(Samp,2,sd)


zscore = (means - 26.7) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.0544


# 4 )

results1 = c(.049, .0512,.0525)
results2 = c(.0487,.0435,.0366)
results3 = c(.0683, .0581, .0544)

results_mat = cbind(results1, results2, results3 )

# results1 results2 results3
# [1,]   0.0490   0.0487   0.0683
# [2,]   0.0512   0.0435   0.0581
# [3,]   0.0525   0.0366   0.0544

# In problem 1, as n increases, the sample standard deviation decreases, thus decreasing the denominator of the zscores, 
# making them larger. This increases the proportion of rejections.  For problem 2, our proportion of samples reject 
# declines, this makes sense, as an increased n brings the t distribution to look more normal, and less flat, thus 
# less would be rejected as n increases.  The same decreasing pattern occurs in problem 3.  As n increases, the 
# standard deviation decreases.  Altogether, this increases the denominator of the z score as n increases, lowering the
# proportion of rejections.

# 5 )

# a )
Xdata3 <- data.frame(X=c(0,20))
dist3 <- ggplot(Xdata3, aes(x=X))
dist3 + stat_function(fun=dchisq, args=list(df=2))


# b )

# b-a)
K = 10000
a = 2

x = 15
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.0449


# b-b)
x = 30
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

#[1] 0.0472


# b-c)
x = 45
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion


# [1] 0.0503

# c )
# c-a )
x = 15
d = 2
samps <- replicate(K, rchisq(x,d))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.011


# c-b )
x = 30
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0049

# c-c )
x = 45
a=2
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0031

# d )
# d-a)
x = 15
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.1133


# d-b )
x = 30
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0814

# d-c )
x = 45
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0684


# e )

results1_5 = c(.0449, .0472, .0503)
results2_5 = c(.011,.0049, .0031)
results3_5 = c(.1133, .0814, .0684)

results5_mat = cbind(results1_5, results2_5, results3_5)
results5_mat

# > results5_mat
# results1_5 results2_5 results3_5
# [1,]     0.0449     0.0110     0.1133
# [2,]     0.0472     0.0049     0.0814
# [3,]     0.0503     0.0031     0.0684


# Trends are similar to the results in the previous problem, with drawing data from the normal distribution instead
# of the Chi square distribution.  In 1, as n increases, the rejections increase because of the decreasing denominator 
# in the z scores.  In problem 2, as n increases, the rejections decrease, because of the sample standard deviation 
# decreasing and t statistic, and problem 3 also decreases due to sample standard deviations decreasing.  

# f )
#  Both sets of data show similar patterns of increasing for the using the population mean and sample std.  And both
#  sets also show decreasing trends then the sample standard deviation and/or sample means are used.  


#####################
# Also, due to wording, I wasn't sure exactly which parts of problem 1-3 to repeat and which parameters to replace, so
# I repeated the problems with using the parameters given in problem 5, but changing X to 3 to match the degrees of freedom
# of 2.  Because if I used X = 15, 30, and 45, then degrees of freedom would change.  

# b-a)
K = 10000
a = 2

x = 15
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.0449


# b-b)
x = 30
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

#[1] 0.0472


# b-c)
x = 45
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion


# [1] 0.0503

# c )
# c-a )
x = 15
d = 2
samps <- replicate(K, rchisq(x,d))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.011


# c-b )
x = 30
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0049

# c-c )
x = 45
a=2
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0031

# d )
# d-a)
x = 15
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.1133


# d-b )
x = 30
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0814

# d-c )
x = 45
samps <- replicate(K, rchisq(x,a))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion
# [1] 0.0684


# f )

results1_5 = c(.0449, .0472, .0503)
results2_5 = c(.011,.0049, .0031)
results3_5 = c(.1133, .0814, .0684)

results5_mat = cbind(results1_5, results2_5, results3_5)
results5_mat


# b )

# b-a)
K = 10000
x = 3
d = 2

samps <- replicate(K, rchisq(x,d))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(2/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.046


# c )
x = 3
d = 2
samps <- replicate(K, rchisq(x,d))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pt(-abs(zscore),d)*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.1176



# d )
x = 3
d = 2
samps <- replicate(K, rchisq(x,d))
samps[1:30]
means = apply(samps,2,mean)
stdevs = apply(samps,2,sd)


zscore = (means - 2) /(stdevs/(x^.5))
pvalues = pnorm(-abs(zscore))*2
logic = pvalues < .05

adata = cbind(means, stdevs, zscore, pvalues, logic)



proportion = sum(logic)/10000
proportion

# [1] 0.2571


# f )
resultse = c(0.046, 0.1176, 0.2571)




# Works Cited:
#   R documentation
#   http://www.cookbook-r.com/Manipulating_data/Adding_and_removing_columns_from_a_data_frame/
#   https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/cbind


