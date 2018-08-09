install.packages("MASS")
library(MASS)
geyser

install.packages("ggplot2")
library(ggplot2)

install.packages("car")
library(car)


# 1 a
geyser = geyser[,c(2,1)]
test = c(0, geyser$duration)
test
test2 = c(geyser$waiting,0)
test2
test2 = head(test2, -1)
test2 = tail(test2, -1)
test = tail(test, -1)
test = head(test, -1)
df.geyser = data.frame(eruption=test, waiting = test2)
df.geyser[1:10,]
# > df.geyser[1:10,]
# eruption waiting
# 1  4.016667      71
# 2  2.150000      57
# 3  4.000000      80
# 4  4.000000      75
# 5  4.000000      77
# 6  2.000000      60
# 7  4.383333      86
# 8  4.283333      77
# 9  2.033333      56
# 10 4.833333      81


# 1 b
df.geyser.plot <- ggplot(df.geyser, aes(x=eruption, y=waiting)) + geom_point()
df.geyser.plot



# 1 c
df.geyser.fit = lm(waiting ~ eruption, data=df.geyser)
df.geyser.fit
df.geyser.fit$coefficients
df.geyser[1:10,]
# df.geyser.fit$coefficients
# (Intercept)    eruption 
# 34.94516    10.77510  

# 1 d
anova(df.geyser.fit)

# Analysis of Variance Table
# 
# Response: waiting
# Df Sum Sq Mean Sq F value    Pr(>F)    
# eruption    1  45341   45341  1109.6 < 2.2e-16 ***
#   Residuals 296  12096      41                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# I used the f value from an f test, which tests whether or not the model is overall useful.  At a significance level
# of alpha = .05 gives us a f value of 3.89.  The f value of 1109.6 is much larger than 3.89, and it's p value is extremely small.
# Therefore, the model is useful.  


# 1 e 
summary(df.geyser.fit)


  lm(formula = waiting ~ eruption, data = df.geyser)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.6940  -4.4954  -0.0966   3.9544  29.9544 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  34.9452     1.1807   29.60   <2e-16 ***
#   eruption     10.7751     0.3235   33.31   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.392 on 296 degrees of freedom
# Multiple R-squared:  0.7894,	Adjusted R-squared:  0.7887 
# F-statistic:  1110 on 1 and 296 DF,  p-value: < 2.2e-16

# The proportion of variation is 0.7887.

# 1 f


## Residuals vs. predicted plot
ggplot(df.geyser.fit, aes(x=.fitted, y=.resid)) + geom_point() + 
  geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(df.geyser.fit, aes(x=eruption, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")


## Q-Q plot
X = data.frame(resid = residuals(df.geyser.fit))
X

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
  geom_abline(intercept=int, slope=slope) 

# The four assumptions are:
#     1) The errors have a mean of zero.                                #
#     2) The errors have a constant variance.                           #
#     3) The errors are independent.                                    #
#     4) The errors are normally distributed.


# 4. (normal distribution) holds true, because the Q - Q plot shows a straight line.
# 1, 2, and 3 also hold true, as the points in the rest of the plots are all centered
# around zero (upholds mean zero assumption), display no fanning (maintains constant variance), and
# don't show any patterns (are independent).

# 1 g )

# 1 h)
prediction2.5 <- data.frame(eruption=2.5)
predict.lm(df.geyser.fit, newdata=prediction2.5, interval="prediction")
# fit      lwr      upr
# 1 61.88291 49.26632 74.49949

# For an eruption length of 2.5 minutes, you'd probably have to wait 61.88291 years.

prediction3.5 <- data.frame(eruption=3.5)
predict.lm(df.geyser.fit, newdata=prediction3.5, interval="prediction")
# fit      lwr      upr
# 1 72.65801 60.05639 85.25962

# For an eruption length of 3.5 minutes, you'd probably have to wait 72.65801 years.



#### Problem 2 ####

getwd()
setwd('/Users/SheepMaster/Fall2017/STAT3080/Homework')
burnout = read.table("burnout.txt", header=TRUE)

# the model used includes both categorial data (and dummy variables) and continuous data.
# because the response variable is categorial, we but use glm with binomial
burnout.fit = glm(burnout ~ loc + cope + teaching + research + pastoral, data=burnout,family="binomial")
burnout.fit

burnout$burnout <- relevel(burnout$burnout, ref="NotBurntOut")

# Call:
#   lm(formula = burnout ~ loc + cope + teaching + research + pastoral, 
#      data = burnout)
# 
# Coefficients:
#   (Intercept)          loc         cope     teaching     research     pastoral  
# 2.100865    -0.013222    -0.017583     0.011304    -0.001563    -0.004131  


summary(burnout.fit)

nm <- glm(burnout ~ 1, data=burnout, family='binomial')
fm <- glm(burnout ~ ., data=burnout, family='binomial')

step(fm, scope=list(lower=nm, upper=fm), direction="backward")
# Start:  AIC=333.2
# burnout ~ loc + cope + teaching + research + pastoral
# 
# Df Deviance    AIC
# <none>          321.20 333.20
# - research  1   324.71 334.71
# - pastoral  1   333.71 343.71
# - teaching  1   360.42 370.42
# - loc       1   389.96 399.96
# - cope      1   449.32 459.32
# 
# Call:  glm(formula = burnout ~ loc + cope + teaching + research + pastoral, 
#            family = "binomial", data = burnout)
# 
# Coefficients:
#   (Intercept)          loc         cope     teaching     research     pastoral  
# -4.43993      0.11079      0.14234     -0.11216      0.01931      0.04517  
# 
# Degrees of Freedom: 466 Total (i.e. Null);  461 Residual
# Null Deviance:	    530.1 
# Residual Deviance: 321.2 	AIC: 333.2

# Again, the model used includes both categorial data (and dummy variables) and continuous data.
# because the response variable is categorial, we but use glm with binomial, running a logistic binary regression.
# Running our backwards seelction, we find that all variables are significant to predicting burnout. 

# 2 B )
# To determine the significance of the individual variables, we use the wald test, which works similarly to individual t-tests. 
# 'research' is not significant, as it's p-value is above the threshold (significance level at .05)
summary(burnout.fit)
# 
# Call:
#   glm(formula = burnout ~ loc + cope + teaching + research + pastoral, 
#       family = "binomial", data = burnout)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.63636  -0.02966   0.28690   0.48290   2.41592  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.43993    1.08565   4.090 4.32e-05 ***
#   loc         -0.11079    0.01494  -7.414 1.23e-13 ***
#   cope        -0.14234    0.01639  -8.684  < 2e-16 ***
#   teaching     0.11216    0.01977   5.673 1.40e-08 ***
#   research    -0.01931    0.01036  -1.863 0.062421 .  
# pastoral    -0.04517    0.01310  -3.449 0.000563 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 530.11  on 466  degrees of freedom
# Residual deviance: 321.20  on 461  degrees of freedom
# AIC: 333.2
# 
# Number of Fisher Scoring iterations: 6


# 2 C )
exp(burnout.fit$coefficients)
# (Intercept)         loc        cope    teaching    research    pastoral 
# 84.7687261   0.8951302   0.8673297   1.1186946   0.9808738   0.9558316 

# In non statistical terms, when stress from teaching is higher, burnout is more likely.  When the other four variables'
# values are higher, burnout is also likely, but less so than with teaching.  Teaching is most likely to increase chances of
# being burned out.

# 2 D )

vif(burnout.fit)
# loc     cope teaching research pastoral 
# 1.831916 2.850794 3.653803 1.172410 1.417788 


# The VIFs for loc, research, and pastoral are relatively low, so those probably wouldn't be correlated, but
# cope and teaching may be moderately correlated.  However, none of the VIF's reach over 5, so I wouldn't say
# any of the variables show strong signs of multicollinearity.  The VIFs indicate, in percentage how much a variable would be
# if there was no expected multicollinearity, where each tenth of a point indicate 10%.  


# 2 E )

## Identifying outliers in y
burnout.resid <- rstandard(burnout.fit)
burnout.resid[burnout.resid>1.5]
# 71       77      108      178      221      280      285      331 
# 1.536093 2.183233 1.673135 2.451606 1.857940 1.652128 2.330134 1.591038 

## Identifying outliers in x and influential points
summary(influence.measures(burnout.fit))


## Identifying influential points
Rburnout <- rstudent(burnout.fit)
Rburnout[Rburnout>1.5]
# 71       77      108      178      221      280      285      331 
# 1.534589 2.200959 1.674881 2.521459 1.877100 1.653211 2.361354 1.586342


# rstandard and rstudent give the standardized values for all points, therefore I want to find more extreme values at the extreme
# ranges from the rest of the values, therefore, I subset the points that are larger than 1, which are 71, 77, 108, 178,
# 221,280, 285, and 331.  WIth the summary influence.measures function however, these are our influential points:

# dfb.1_ dfb.loc dfb.cope dfb.tchn dfb.rsrc dfb.pstr dffit   cov.r   cook.d hat    
# 7    0.24   0.10    0.18    -0.30    -0.05     0.01     0.38_*  1.06_*  0.01   0.07_*
#   41  -0.09  -0.09   -0.03     0.08    -0.04     0.11     0.25    1.04_*  0.00   0.04_*
#   77  -0.08   0.14    0.27    -0.24     0.09     0.21     0.34    0.94_*  0.03   0.02  
# 108  0.11   0.35    0.02    -0.04    -0.13    -0.11     0.53_*  1.03    0.03   0.06_*
#   140  0.07  -0.13    0.06     0.10    -0.10    -0.14     0.31    1.04_*  0.01   0.05_*
#   166  0.08  -0.08   -0.03     0.14    -0.10    -0.17     0.24    1.06_*  0.00   0.06_*
#   172 -0.12   0.18   -0.05     0.01     0.10     0.04     0.30    1.04    0.01   0.05_*
#   176 -0.05  -0.03   -0.03     0.16    -0.20     0.07     0.43_*  1.03    0.02   0.06_*
#   178 -0.06  -0.08    0.26     0.07     0.02    -0.15     0.51_*  0.93_*  0.09   0.03  
# 189  0.29  -0.02    0.17    -0.13    -0.30    -0.05     0.44_*  1.03    0.02   0.06_*
#   221 -0.09   0.59   -0.13     0.09    -0.03    -0.11     0.92_*  1.11_*  0.11   0.15_*
#   222  0.06  -0.15    0.07     0.12    -0.07    -0.18     0.35_*  1.05_*  0.01   0.06_*
#   233 -0.03   0.02   -0.02    -0.06    -0.04     0.19     0.22    1.05_*  0.00   0.04_*
#   240 -0.04   0.16   -0.09     0.02     0.00     0.03     0.28    1.06_*  0.01   0.06_*
#   270  0.00   0.17   -0.01     0.08     0.05    -0.21     0.40_*  1.05_*  0.01   0.07_*
#   280 -0.15  -0.07    0.11     0.08     0.00     0.09     0.38_*  1.00    0.02   0.04  
# 285  0.08   0.26    0.19    -0.12    -0.05    -0.09     0.38_*  0.93_*  0.04   0.02  
# 290 -0.07  -0.06    0.17     0.04     0.15    -0.13     0.35_*  1.01    0.01   0.04_*
#   304 -0.08  -0.14    0.09     0.20     0.02    -0.18     0.45_*  1.04    0.02   0.06_*
#   310  0.07   0.11    0.01     0.00     0.01    -0.15     0.26    1.04    0.00   0.04_*
#   324 -0.09   0.03    0.09    -0.06     0.23    -0.02     0.26    1.04_*  0.01   0.04_*
#   331 -0.23   0.43   -0.05     0.00     0.18     0.06     0.61_*  1.07_*  0.04   0.09_*
#   341  0.15  -0.01    0.10    -0.09    -0.24     0.07     0.34    1.02    0.01   0.04_*
#   348  0.03   0.32   -0.06    -0.04    -0.11     0.03     0.47_*  1.05_*  0.02   0.07_*
#   352 -0.21   0.09    0.10    -0.02     0.24     0.01    -0.28    0.94_*  0.02   0.01  
# 353  0.06   0.07    0.16    -0.13     0.01    -0.06    -0.23    0.93_*  0.01   0.01  
# 354 -0.18   0.11    0.09    -0.06     0.13     0.12    -0.21    0.92_*  0.01   0.01  
# 355  0.10   0.16    0.16    -0.22    -0.10     0.09    -0.27    0.89_*  0.04   0.01  
# 356 -0.04   0.02    0.00     0.05    -0.03    -0.01    -0.16    0.96_*  0.00   0.01  
# 357 -0.07   0.18    0.09    -0.18     0.07     0.16    -0.25    0.94_*  0.01   0.01  
# 359 -0.23   0.04    0.03     0.05     0.17     0.06    -0.25    0.95_*  0.01   0.01  
# 360  0.29  -0.03    0.12    -0.10    -0.20    -0.19    -0.39_*  0.99    0.02   0.03  
# 363  0.03   0.06    0.06    -0.10    -0.12     0.13    -0.23    0.95_*  0.01   0.01  
# 364  0.00   0.06    0.07    -0.14    -0.15     0.25    -0.33    0.95_*  0.02   0.02  
# 368  0.03   0.09    0.10    -0.08     0.01    -0.05    -0.19    0.94_*  0.01   0.01  
# 370 -0.07   0.04    0.00     0.01    -0.01     0.05    -0.15    0.96_*  0.00   0.00  
# 371 -0.17  -0.18   -0.02     0.12     0.19    -0.03    -0.32    1.05_*  0.01   0.06_*
#   383  0.09  -0.05    0.00    -0.09    -0.02     0.02    -0.20    1.05_*  0.00   0.04_*
#   387 -0.14   0.13    0.11    -0.13     0.09     0.19    -0.23    0.92_*  0.02   0.01  
# 388 -0.10   0.12    0.12    -0.12     0.10     0.10    -0.19    0.93_*  0.01   0.01  
# 389  0.12   0.02    0.13    -0.07     0.05    -0.24    -0.35_*  0.98    0.02   0.03  
# 391 -0.06   0.08    0.07    -0.09    -0.01     0.13    -0.18    0.94_*  0.01   0.01  
# 396 -0.11   0.13    0.12    -0.12     0.14     0.08    -0.22    0.94_*  0.01   0.01  
# 431 -0.23  -0.01   -0.06     0.02     0.21     0.15    -0.31    1.03    0.01   0.04_*
#   446  0.22  -0.07    0.06    -0.12    -0.05    -0.13    -0.32    1.06_*  0.01   0.06_*

# RESOURCES:
# https://www.rdocumentation.org/packages/car/versions/2.1-6/topics/vif
# https://stats.stackexchange.com/questions/232548/r-how-are-the-significance-codes-determined-when-summarizing-a-logistic-regres
# https://stats.stackexchange.com/questions/63503/does-rstandard-standardize-in-z