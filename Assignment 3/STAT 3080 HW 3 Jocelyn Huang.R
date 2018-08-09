# Problem 1
install.packages("ggplot2")
library(ggplot2)

setwd("/Users/SheepMaster/Fall2017/STAT3080/Homework")
getwd()
sleep = read.table("sleep.txt", header=TRUE)
sleep

# 1 A
sleepPlot <- ggplot(sleep, aes(x=Days, y=Reaction))
sleepPlot + aes(x=Days, y=Reaction, linetype=factor(Subject)) + geom_line()

# 1 B
sleepPlot + geom_line(aes(colour=factor(Subject)))

# 1 C
sleepPlot + geom_line(aes(colour=factor(Subject))) + geom_point()

# 1 D
sleepPlot + geom_line(aes(colour=factor(Subject))) + geom_point(aes(shape=factor(Subject)))

# 1 E
sleepPlot + geom_line(aes(colour=factor(Subject))) + geom_point() + ggtitle("Reaction times after days of sleep deprivation")

# 1 F
sleepPlot + geom_line(aes(colour=factor(Subject))) + geom_point() + ggtitle("Reaction times after days of sleep deprivation") + scale_x_continuous(breaks=seq(0,9,1))

# PROBLEM 2 

tv = read.table("tv.txt", header=TRUE)
tv

# 2 A
tvplot = ggplot(tv, aes(x=x, y=y))
tvplot + geom_point(shape =1)

# 2 B
tvplot + geom_point(shape=1) +ggtitle("Evening time at home") + xlab("Sitting (minutes)") + ylab("Watch TV(minutes)")


# 2 C
tvplot + stat_bin2d() +ggtitle("Evening time at home")

# 2 D
tvplot + aes(colour=factor(z)) + geom_point(shape=1) +ggtitle("Evening time at home") + scale_colour_manual("z", values=c("1"="blue","2"="orange")) + xlab("Sitting (minutes)") + ylab("Watch TV(minutes)")


# 2 E
tv_e = geom_point(shape=1,aes(colour=factor(z)), alpha=0.3)  +ggtitle("Evening time at home") + scale_colour_manual("z", values=c("1"="blue","2"="orange")) + xlab("Sitting (minutes)") + ylab("Watch TV(minutes)")
tv_e
# 2 F
tv_f = tvplot + geom_point(shape=1,aes(colour=factor(z)), alpha=0.3) +ggtitle("Evening time at home")+ scale_colour_manual("Exercise", values=c("1"="blue","2"="orange"), labels=c("Yes","No")) + xlab("Sitting (minutes)") + ylab("Watch TV(minutes)")
tv_f
# 2 G
tv_g = tvplot + aes(colour=factor(z)) + geom_point(shape=1, alpha=0.3) +ggtitle("Evening time at home")+ scale_colour_manual("Exercise", values=c("1"="blue","2"="orange"), labels=c("Yes","No")) + xlab("Sitting (minutes)") + ylab("Watch TV(minutes)") +geom_smooth(method=lm, se=F) 
tv_g

# 2 H
tv_g+geom_smooth(method=lm, fill="orange", alpha=0.3) 

# 2 I
tvplot + geom_point(shape=1,aes(colour=factor(z), alpha=0.05) ) +ggtitle("Evening time at home") + scale_colour_manual("Exercise", values=c("1"="blue","2"="orange")) + geom_smooth(method=lm) + xlab("Sitting (minutes)") + ylab("Watch TV(minutes)") +geom_smooth(method=lm, se=F) 



# PROBLEM 3 

comp = read.table("compexp.txt", header=TRUE)
comp

# 3 A
compplot = ggplot(comp, aes(x=clipping, y=yield)) + stat_summary(fun.y=median, geom="bar", fill="white", colour="black")

# 3 B
compplot + stat_summary(fun.data=mean_se, geom="errorbar", width=0.2)

# 3 C
compplot + stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) + ggtitle("Yield of mature plants") + xlab("Treatment") + ylab("Median yield")


# PROBLEM 4

erupt = read.table("erupt.txt", header=TRUE)
erupt

# 4 A

dist = ggplot(erupt, aes(x=x)) 
dist +  geom_histogram(binwidth = .25, fill="white", colour="black")  + stat_function(fun=dnorm) 



## WORKS CITED
# http://www.cookbook-r.com/Manipulating_data/Recoding_data/
# http://www.cookbook-r.com/Manipulating_data/Recoding_data/
# https://www.rdocumentation.org/
# https://stackoverflow.com/questions/23346632/linetype-and-guide-options-for-ggplot2-when-using-geom-smooth-with-continuous-va
# http://environmentalcomputing.net/plotting-with-ggplot-adding-titles-and-axis-names/
# http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
# https://stackoverflow.com/questions/15726907/ggplot-scatterplot-points-with-no-fill
# http://sape.inf.usi.ch/quick-reference/ggplot2/shape
# http://ggplot2.tidyverse.org/reference/