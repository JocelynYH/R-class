# Jocelyn Huang 
# HW 1 
# Stat 3080

## ******* PROBLEM 1 ******

## a)
a = rep("a", each=4)
a
#[1] "a" "a" "a" "a"

## b)
b = seq(2,100,2)
b
# [1]   2   4   6   8  10  12  14  16  18  20  22  24  26  28  30  32  34  36  38  40  42  44
# [23]  46  48  50  52  54  56  58  60  62  64  66  68  70  72  74  76  78  80  82  84  86  88
# [45]  90  92  94  96  98 100

## c)
c = rep(0:3, c(4,3,2,1))
c

# [1] 0 0 0 0 1 1 1 2 2 3

## d)
d = rep(1:3, each=3)
d
# [1] 1 1 1 2 2 2 3 3 3

## e)
e <- c(1:5,4:1)
e
# [1] 1 2 3 4 5 4 3 2 1

## f)
f = seq(1,10)
f
# [1]  1  2  3  4  5  6  7  8  9 10

## g)
g = rep(1, each=10)
g = g/1:10
g
# [1] 1.0000000 0.5000000 0.3333333 0.2500000 0.2000000 0.1666667 0.1428571 0.1250000
# [9] 0.1111111 0.1000000

## h)
h = 1:6
h = h^3
h
# [1]   1   8  27  64 125 216

## i)

i = seq(1964,2003)
i
# [1] 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980
# [18] 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
# [35] 1998 1999 2000 2001 2002 2003

## j)
j = seq(0,1000,25)
j
#  [1]    0   25   50   75  100  125  150  175  200  225  250  275  300  325  350  375  400
# [18]  425  450  475  500  525  550  575  600  625  650  675  700  725  750  775  800  825
# [35]  850  875  900  925  950  975 1000

## ******* PROBLEM 2 ******

## a)
poker_vect = c(140, -50, 20, -120, 240)
roulette_vect = c(-30,-50,100,-225,20)
poker_vect
# [1]  140  -50   20 -120  240
roulette_vect
# [1]  -30  -50  100 -225   20

## b)
daily_earnings = poker_vect+roulette_vect
daily_earnings
# [1]  110 -100  120 -345  260

## c)
total_earnings = sum(daily_earnings)
total_earnings
# [1] 45

## d)
(sum(poker_vect)/total_earnings)*100
# 511.11%

## e)
mean(roulette_vect)
# [1] -37

## f)
mean(poker_vect)
# [1] 46
# Dave's earnings are by far higher. 

## g)
names(roulette_vect) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
roulette_vect
# Monday   Tuesday Wednesday  Thursday    Friday 
#    -30       -50       100      -225        20 
roulette_vect>0
# Monday   Tuesday Wednesday  Thursday    Friday 
#   FALSE     FALSE      TRUE     FALSE      TRUE  

#Nancy won on Wednesday and Friday.

## h)
roulette_vect_win = roulette_vect>0
roulette_vect_win
# Monday   Tuesday Wednesday  Thursday    Friday 
#  FALSE     FALSE      TRUE     FALSE      TRUE
roulette_vect[c(3,5)]
# Wednesday    Friday 
#       100        20

## ******* PROBLEM 3 ******
## a)
morning = c(42,29,34,37,28,60,36,27,35,55,30,29,42,33,31,24,36,40,46,43)
evening = c(38,46,59,39,38,55,61,42,54,65,42,39,36,55,48,41,57,43,37,44)
commute_times = rbind(morning, evening)
commute_times
#         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
# morning   42   29   34   37   28   60   36   27   35    55    30    29    42    33    31
# evening   38   46   59   39   38   55   61   42   54    65    42    39    36    55    48
#         [,16] [,17] [,18] [,19] [,20]
# morning    24    36    40    46    43
# evening    41    57    43    37    44

## b)
logic_test = commute_times[1,]>30
logic_test
sum(logic_test, na.rm=TRUE)
# [1] 14

## c)
logic_test_2 = commute_times[2,]>commute_times[1,]
logic_test_2
sum(logic_test_2, na.rm=TRUE)
# [1] 16

## d)
Morning_matrix = matrix(morning,nrow=4,ncol=5,byrow=TRUE)
Morning_matrix
# [,1] [,2] [,3] [,4] [,5]
# [1,]   42   29   34   37   28
# [2,]   60   36   27   35   55
# [3,]   30   29   42   33   31
# [4,]   24   36   40   46   43

Evening_matrix = matrix(evening,nrow=4,ncol=5,byrow=TRUE)
Evening_matrix
# [,1] [,2] [,3] [,4] [,5]
# [1,]   38   46   59   39   38
# [2,]   55   61   42   54   65
# [3,]   42   39   36   55   48
# [4,]   41   57   43   37   44

## e)
apply(Morning_matrix, 1, sum)
# [1] 170 213 165 189
# Week 2 contained the longest average morning commute times.

## f)
apply(Evening_matrix, 1, sum)
# [1] 220 277 220 222
# Week 1 and 3 contained the shortest average evening commute times, both tied for the same average evening commute time.

## g)
commute_times[2,15] = 38
commute_times
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# morning   42   29   34   37   28   60   36   27   35    55
# evening   38   46   59   39   38   55   61   42   54    65
# [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19]
# morning    30    29    42    33    31    24    36    40    46
# evening    42    39    36    55    38    41    57    43    37
# [,20]
# morning    43
# evening    44

Evening_matrix[3,5] = 38
Evening_matrix
# [,1] [,2] [,3] [,4] [,5]
# [1,]   38   46   59   39   38
# [2,]   55   61   42   54   65
# [3,]   42   39   36   55   38
# [4,]   41   57   43   37   44

## h)
# The recording error doesn't change answers to b or c, but 
# obviously it changing the matrixes directly would change a and d.
# However, the answer to f changes (but not e, as that only contains morning values), as changing a value of one of the evening commute times
# changes the average evening commute times.  Therefore, the average commute times in the evening
# changes in week 3, from a total of 220 minutes to 210 minutes, or an average of 44 minutes a day
# or 42 minutes a day.  
apply(Evening_matrix, 1, sum)
# [1] 220 277 210 222

## i)

new_Morning_array = morning[-6:-10]
new_Morning_array
new_Morning_matrix = matrix(new_Morning_array, nrow=3, ncol=5,byrow=TRUE)
new_Morning_matrix
row.names(new_Morning_matrix) <- c("Week1","Week3","Week4")
new_Morning_matrix
#       [,1] [,2] [,3] [,4] [,5]
# Week1   42   29   34   37   28
# Week3   30   29   42   33   31
# Week4   24   36   40   46   43

new_Evening_array = evening[-6:-10]
new_Evening_array
new_Evening_matrix = matrix(new_Evening_array, nrow=3, ncol=5,byrow=TRUE)
new_Evening_matrix
row.names(new_Evening_matrix) <- c("Week1","Week3","Week4")
new_Evening_matrix
#       [,1] [,2] [,3] [,4] [,5]
# Week1   38   46   59   39   38
# Week3   42   39   36   55   48
# Week4   41   57   43   37   44

# References:
#   STAT 3080 HW #1
# https://stackoverflow.com/questions/2190756/how-to-count-true-values-in-a-logical-vector
# https://stackoverflow.com/questions/4131338/is-it-possible-to-have-a-multi-line-comments-in-r
# http://astrostatistics.psu.edu/su07/R/html/base/html/seq.html
# https://stuff.mit.edu/afs/sipb/project/r-project/lib/R/library/base/html/seq.html
# Introduction to R.R by R file by Gretchen Martinet
