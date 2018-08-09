getwd()
setwd('/Users/SheepMaster/Fall2017/STAT3080')
concerts = read.csv("2016-2017 Programming Data - Sheet1.csv")
concerts[1:10,]
concerts=concerts[!(is.na(concerts$Conductor.Gender) | concerts$Conductor.Gender==""), ]



female =  subset(concerts, Conductor.Gender=='Female')
female[1:3,]

male = subset(concerts, Conductor.Gender=='Male')

# find the # of concerts given males vs. females
FSn = sum(female$X..of.Shows)
MSn = sum(male$X..of.Shows)
TSn = sum(concerts$X..of.Shows)
FSn/TSn  # prop. of Female conductors performed shows
MSn/TSn

# > FSn
# [1] 802
# > MSn
# [1] 7920

# > FSn/TSn
# [1] 0.09195139
# > MSn/TSn
# [1] 0.9080486


# find the # of female vs. male conductors
Fn = nrow(female)
Mn = nrow(male)
Tn=nrow(concerts)
Mn/Fn # About 10 times more males than females... WHY?
# > Fn
# [1] 355
# > Mn
# [1] 3674
# > Mn/Fn
# [1] 10.3493

(Fn/Tn)*100
# > (Fn/Tn)*100
# [1] 8.666992
# Percentage of female conductors compared to male conductors

# "Big 5 Orchestra" programs lead by female conductors
big5 = subset(concerts, Orchestra=='New York Philharmonic'|Orchestra=='Boston Symphony Orchestra'|Orchestra=='Chicago Symphony Orchestra'|Orchestra=='Cleveland Orchestra'|Orchestra=='Philadelphia Orchestra')
big5=big5[!(is.na(big5$Conductor.Gender) | big5$Conductor.Gender==""), ]

nbig5F = nrow(subset(big5, Conductor.Gender=='Female'))
nbig5M = nrow(subset(big5, Conductor.Gender=='Male'))
nbig5 = nrow(subset(big5))

# > nrow(subset(big5, Conductor.Gender=='Male'))
# [1] 502
# > nrow(subset(big5, Conductor.Gender=='Female'))
# [1] 13
(nbig5F/nbig5)*100
# > (nbig5F/nbig5)*100
# [1] 2.509653
# Percentage of female conductors leading programs with "Big 5 Orchestras"

# Major Orchestras
MOrch = subset(concerts, Orchestra=='New York Philharmonic'|Orchestra=='Boston Symphony Orchestra'|Orchestra=='Chicago Symphony Orchestra'|Orchestra=='Cleveland Orchestra'
               |Orchestra=='Philadelphia Orchestra'|Orchestra=='San Francisco Symphony'
               |Orchestra=='Atlanta Symhony Orchestra'|Orchestra=='Houston Symphony'
               |Orchestra=='Baltimore Symphony Orchestra'|Orchestra=='National Symphony Orchestra'
               |Orchestra=='Philadelphia Orchestra'|Orchestra=='Philadelphia Orchestra'
               |Orchestra=='Minnesota Orchestra'|Orchestra=='St. Louis Symphony Orchestra')
MOrch=MOrch[!(is.na(MOrch$Conductor.Gender) | MOrch$Conductor.Gender==""), ]

MOrchF = subset(MOrch, Conductor.Gender=='Female')
MOrchM = subset(MOrch, Conductor.Gender=='Male')
# > nrow(subset(MOrch, Conductor.Gender=='Female'))
# [1] 63
# > nrow(subset(MOrch, Conductor.Gender=='Male'))
# [1] 827


nMOrchF = nrow(subset(MOrch, Conductor.Gender=='Female'))
nMOrchM = nrow(subset(MOrch, Conductor.Gender=='Male'))
nMOrch = nrow(subset(MOrch))
(nMOrchF/nMOrch)*100
# [1] 6.969027
# Percentage of female conductors leading a program with a "Major Orchestra"

(nMOrchM/nMOrch)*100
# [1] 92.921353
# Percentage of male conductors leading a program with a "Major Orchestra"



# bar graph of # of concerts given by female vs. male conductors
bar1 = ggplot(concerts, aes(x=Conductor.Gender, y=X..of.Shows))
bar1 + geom_bar(stat="identity") + ggtitle('# of Concert Programs given M/F Conductors')

# bar graph of # of Major Orchestra concerts given by female vs. male conductors
bar2 = ggplot(MOrch, aes(x=Conductor.Gender, y=X..of.Shows))
bar2 + geom_bar(stat="identity") + ggtitle('# of Major Orchestra Concert Programs given M/F Conductors')

# bar graph of displaying the gender of conductors for Big 5 concerts
bar3 <- ggplot(big5, aes(x=Orchestra, y=X..of.Shows, fill=Conductor.Gender)) 
bar3 + stat_summary(fun.y=mean, geom="bar", position="dodge") + ggtitle("Gender in Big 5 Concerts") +  theme(text = element_text(size=12),
                                                                                                             axis.text.x = element_text(angle=45, vjust=1, hjust = .8))

# bar graph of displaying the gender of conductors for Major Orchestra concerts

bar4 <- ggplot(MOrch, aes(x=Orchestra, y=X..of.Shows, fill=Conductor.Gender)) 
bar4 + stat_summary(fun.y=mean, geom="bar", position='dodge') +  theme(text = element_text(size=12),
                                                                       axis.text.x = element_text(angle=45, vjust=1, hjust = .8)) + ggtitle('Gender by Major Orchestra')
unique(big5$Conductor.Gender)

NYPhil = subset(concerts, Orchestra=='New York Philharmonic')
Boston = subset(concerts, Orchestra=='Boston Symphony Orchestra')
Houston = subset(concerts, Orchestra=='Houston Symphony')
NSO = Boston = subset(concerts, Orchestra=='National Symphony Orchestra')



length(unique(female$Conductor))
length(unique(male$Conductor))
# > length(unique(female$Conductor))
# [1] 29
# > length(unique(male$Conductor))
# [1] 303

length(unique(MOrchF$Conductor))
length(unique(MOrchM$Conductor))
# > length(unique(MOrchF$Conductor))
# [1] 10
# > length(unique(MOrchM$Conductor))
# [1] 102

length(unique(NYPhil$Conductor))
length(unique(Boston$Conductor))
length(unique(Houston$Conductor))
length(unique(NSO$Conductor))
# > length(unique(NYPhil$Conductor))
# [1] 18
# > length(unique(Boston$Conductor))
# [1] 17
# > length(unique(Houston$Conductor))
# [1] 11
# > length(unique(NSO$Conductor))
# [1] 17

# Composers chosen by Conductors
# bar graph of # of Major Orchestra concerts given by female vs. male conductors
install.packages('plyr')
library(plyr)
composer_fpicks = count(concerts, 'Composer', sort=true)
composer_fpicks.head()
sort(composer_fpicks)


install.packages('dplyr')
library(dplyr)
Composers_pickedf = female %>%
  group_by(Conductor.Gender, Composer) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
Composers_pickedf[order(test$freq,decreasing=TRUE),]

Composers_pickedm = male %>%
  group_by(Conductor.Gender, Composer) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
Composers_pickedm[order(test2$freq,decreasing=TRUE),]


#####
## Create a two-way table for 400 individuals' gender and education
test=matrix(data = female$Composer, male$Composer)
test
#  Two-way tables can either show counts or proportions and can         #
#  express either the joint or conditional distributions.               #

## Joint distribution in proportions
Lp <- prop.table(L)
sum(Lp)

# Significance in number of female vs. male conductors
phat <- c(0.02509653, 0.07078652)
n <- c(515, 890)
x <- phat*n

z2.prop <- prop.test(x, n, alternative="two.sided", correct=FALSE)
z2.prop
# p-value = 0.0002623
# The proportion of female conductors leading big 5 orchestras vs. major orchestras is significant

# Quantity of concerts given, in relation to conductor gender
concerts.fit <- lm(X..of.Shows ~ Conductor.Gender, data=concerts)
concerts.fit

concerts.plot <- ggplot(concerts, aes(x=Conductor.Gender, y=X..of.Shows)) + geom_point()
concerts.plot

dtime.fit <- lm(dtime ~ cases, data=deliv)
dtime.fit