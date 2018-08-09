# Jocelyn Huang
# 1.
# 1) Use the chi-squared test, to determine whether or not there is a relationship
#    between politcal affiliation and NASCAR attendence.  The data is categorical.

# 2) Ho: mu = 0.  Ha: mu != 0

M <- matrix(c(20,40,70,70), nrow=2, ncol=2)
dimnames(M) <- list(Party=c("Liberal","Conservative"), 
                    Attended=c("Yes","No"))
M
test1 <- chisq.test(M, correct=FALSE)
test1

# Attended
# Party          Yes No
# Liberal       20 70
# Conservative  40 70
# > test1 <- chisq.test(M, correct=FALSE)
# > test1
# 
# Pearson's Chi-squared test
# 
# data:  M
# X-squared = 4.7138, df = 1, p-value = 0.02992


# 3) Yes, there is evidence of a relationship between political view and NASCAR race attendance.
# One's political view does to affect whether or not they've attended a NASCAR race.  
# The p value for the test is 0.02992, which is lower than the .05 significance level.

# 2. 
# 1 ) You are given a population mean, and a sample mean and standard deviation.  Therefore,
# we use the one sample t test, as we're comparing our sample with the population and know
# the population mean but not the standard deviation.

# 2) Ho: mu = 46.  Ha: mu < 46

vacc.samps <- replicate(1, round(rnorm(12, mean=42, sd=11.9)))
t.test(vacc.samps, mu=46, alternative="less")


# 
# One Sample t-test
# 
# data:  vacc.samps
# t = -1.2731, df = 11, p-value = 0.1146
# alternative hypothesis: true mean is less than 46
# 95 percent confidence interval:
#   -Inf 48.19022
# sample estimates:
#   mean of x 
# 40.66667 

# 3 ) The p-value of the test is not under a .05 significance level, 
# therefore, we do not have evidence to dispute the claims that a vacuum cleaner
# expends an average of 46 kw hr/year.

# 3. 

# 1) We are using a two sample z-test, as we are testing the difference of proportions
# with categorical comparisons across two independent populations.
 
# 2) Ho: mu = 0.  Ha: mu != 0

liked <- c(.82, .7)
n <- c(350, 350)
movie <- liked*n

prop3 <- prop.test(movie, n, alternative="two.sided", correct=FALSE)
prop3


# 2-sample test for equality of proportions without continuity correction
# 
# data:  movie out of n
# X-squared = 13.816, df = 1, p-value = 0.0002016
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   0.05735119 0.18264881
# sample estimates:
#   prop 1 prop 2 
# 0.82   0.70 

# 3) The listeners definitely have differing opinions on this movie, as the p-value
# was way under .1, at .0002016

# 4.
# 1)  Seeing as we'd like to compare 2 different groups, but don't have population parameters
# for either, we'd use a 2 sample t test.

# 2) Ho: mu = 0.  Ha: mu != 0

setwd("/Users/workstation/Documents")
table4 =read.table("normtemp.txt", header=TRUE)

women = which(table4$gender == 2)
men = which(table4$gender == 1)
t.test(women, men, mu=0, alternative="two.sided")



# Welch Two Sample t-test
# 
# data:  women and men
# t = 19.598, df = 128, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   58.43749 71.56251
# sample estimates:
#   mean of x mean of y 
# 98        33 


# 3) The p value is so small it's almost zero, therefore, the body temperature between
# genders is definitely significantly different. 

# 5. 
# 1) We are using a one sample t test for proportions, as we are taking the "population mean" or heal successes, 
# and are comparing a single sample.

# 2) Ho: mu = 0.8.  Ha: mu > 0.8

prop5 <- prop.test(148, 178, p=0.8, alternative="less", correct=FALSE)
prop5

phat <- 1/100
z <- (phat-0.8)/sqrt(0.8*0.2/100)
z

pv <- 2*pnorm(z)
pv

sqrt(prop5$statistic)
prop5$p.value


# 1-sample proportions test without continuity correction
# 
# data:  148 out of 178, null probability 0.8
# X-squared = 1.1011, df = 1, p-value = 0.853
# alternative hypothesis: true p is less than 0.8
# 95 percent confidence interval:
#   0.0000000 0.8725711
# sample estimates:
#   p 
# 0.8314607 
# 3) The p value is much higher than .01, therefore, there is no sufficient evidence to dispute the
#manufacturer's claim. 

# 6. 
# 1)  We'd like to compare 2 different groups, but don't have population parameters
# for either, we'd use a 2 sample t test.
# 2) Ho: mu = 0.  Ha: mu != 0

men6 = c(305,16,122,68)
women6 = c(25,68,84,103)
t.test(men6, women6, mu=0, alternative="two.sided")
# 
# Welch Two Sample t-test
# 
# data:  men6 and women6
# t = 0.88739, df = 3.4165, p-value = 0.4329
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -135.783  251.283
# sample estimates:
#   mean of x mean of y 
# 127.75     70.00 

# 3) The p value is quite large, and isn't below our .05 significance level, therefore
# there is no supporting evidence that men download more movies than women. 

# 7. 
# 1) We are again comparing 2 different groups without population parameters, although
# we do have the sample data.  We're also directly comparing data in pairs from
# the same students. We therefore we use a paired t test.
# 2) Ho: mu = 0.  Ha: mu > 0
pretest = c(17, 12, 20, 12, 20, 21, 23, 10, 15, 17, 18, 18)
posttest = c(19, 25, 18, 18, 26, 19, 27, 14, 20, 22, 16, 18)

t.test(pretest, posttest, mu=0, alternative="greater", paired=TRUE)

# 
# Paired t-test
# 
# data:  pretest and posttest
# t = -2.563, df = 11, p-value = 0.9868
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -5.527274       Inf
# sample estimates:
#   mean of the differences 
# -3.25 


# 3) There is no evidence of the student's score improvements, at the .01 significance
# level at least. 

# 8. 

## Note:  UsingR wouldn't install for me, so Professor Martinet sent me the data
## separately in a text file. 

ceopay =read.table("HW5data.txt", header=TRUE)

# a1) We are using a sign test, as we are looking at the median of the distribution
# and checking to see whether or not the sample data aligns with the statement that the
# median salary is 220,000.
# a2) Ho: M=22 vs. Ha: M>22

above <- sum(ceopay>22)
lenceopay <- 199
1-pbinom(above-1,lenceopay,0.5)
binom.test(above,lenceopay,alternative="greater")

# a3) There is evidence that the median pay is more than 220,000 - the p-value
# is just under our significance level of .05 and therefore supports evidence 
# against the median of 220,000.

# b1) As we are using the sample parameters and are looking at the mean, we will use
# the one sample t test. 
# b2) Ho: mu = 22.  Ha: mu > 22.

t.test(ceopay, mu=22, alternative="greater")


# One Sample t-test

# data:  ceopay
# t = 2.5434, df = 197, p-value = 0.005873
# alternative hypothesis: true mean is greater than 22
# 95 percent confidence interval:
#   35.13514      Inf
# sample estimates:
#   mean of x 
# 59.50505 

# b3) Because the p value is so small, there is reason to reject the null hypothesis that
# average ceo salary is around 220,000, and accept the hypothesis that the average ceo salary
# is greater than 220,000.  Therefore, we can conclude that there is evidence to believe
# that average ceo salary is greater than 220,000.

# c) THe first test is the most appropriate, because based off the data from our sample,
# the sample median is 27 (270,000), and the sample mean is 59.50505 (595,051).
# Therefore it looks like 22 (220,000) is the population estimate for the median.  
# The test in b, therefore, makes no sense to compare the average pay to the population median.
# It does however, make sense to compare the sample median to the population median like in part a.


# Resources:
# http://rfunction.com/archives/1001
# http://www.wolframalpha.com/input/?i=2.2e-16
# https://www.r-project.org/other-docs.html
# https://stackoverflow.com/questions/35684126/usingr-package-in-r-is-not-loading-properly
# STAT 3080 Class notes: 4-Hypothesis testing in R
