#1 
# 1a)
setwd("/Users/SheepMaster/Fall2017/STAT3080/Homework")
nym2002 = read.table("nym2002.txt", header=TRUE)
nym2002

# 1b)
length(nym2002$time)
# [1] 68

# 1c)
range(nym2002$time)
# [1] 163.9333 467.1833

# 1d)
hours = nym2002$time %/% 60
minutes = nym2002$time %% 60
append_mytime = data.frame(nym2002, hours, minutes)
append_mytime
# place gender age home     time hours   minutes
# 1   3592   Male  52  GBR 217.4833     3 37.483333
# 2  13853 Female  40   NY 272.5500     4 32.550000
# 3  12256   Male  31  FRA 265.2833     4 25.283333
# 4  10457 Female  33   MI 256.1500     4 16.150000
# 5   9686   Male  33   NY 252.2500     4 12.250000
# 6   1784   Male  40   NJ 201.9667     3 21.966667
# 7  16020 Female  30   CA 283.5667     4 43.566667
# 8  10805   Male  27  GBR 255.8833     4 15.883333
# 9  12201   Male  42  NED 264.2500     4 24.250000
# 10 19266   Male  48  B.C 306.6833     5  6.683333
# 11 15229 Female  63  GBR 281.2667     4 41.266667
# 12 15260   Male  49  ITA 280.2500     4 40.250000
# 13 10670   Male  33   NY 258.3333     4 18.333333
# 14 10658   Male  43   NY 257.1333     4 17.133333
# 15   200   Male  40  GER 163.9333     2 43.933333
# 16 10178   Male  33   NY 253.6167     4 13.616667
# 17  2467   Male  40  FRA 206.2667     3 26.266667
# 18 10729   Male  36   NY 257.9333     4 17.933333
# 19 12093   Male  49  SWE 262.1333     4 22.133333
# 20 12428 Female  34   NY 264.2167     4 24.216667
# 21  2730   Male  26  DEN 209.7333     3 29.733333
# 22 12179   Male  50  NED 265.2500     4 25.250000
# 23 20460   Male  33   NJ 315.2333     5 15.233333
# 24 23413   Male  71   FL 414.7000     6 54.700000
# 25 19921 Female  39  GER 309.2667     5  9.266667
# 26 11878 Female  28  GBR 261.2667     4 21.266667
# 27 17683   Male  52  GBR 295.3167     4 55.316667
# 28 17933   Male  28  NED 294.8333     4 54.833333
# 29 10151   Male  52   CA 254.2500     4 14.250000
# 30  8915   Male  41  BRA 248.9500     4  8.950000
# 31   213   Male  35   VA 164.5000     2 44.500000
# 32 21647 Female  39   NY 334.0167     5 34.016667
# 33  2721   Male  48  FRA 210.9000     3 30.900000
# 34 14623   Male  33  NED 271.0667     4 31.066667
# 35  4715   Male  35   MA 226.4667     3 46.466667
# 36 10277   Male  46   PR 256.7167     4 16.716667
# 37 12630   Male  30   NY 264.8333     4 24.833333
# 38  3751   Male  42   KY 219.1333     3 39.133333
# 39  6867   Male  31  ITA 237.7000     3 57.700000
# 40   737   Male  40   IL 184.3000     3  4.300000
# 41 10972   Male  41   NY 253.7500     4 13.750000
# 42 12968   Male  33   NY 267.4667     4 27.466667
# 43 17775   Male  37   NY 289.6167     4 49.616667
# 44 10333 Female  25  RSA 256.9667     4 16.966667
# 45 17068   Male  39  GBR 288.0333     4 48.033333
# 46 14690   Male  34   NY 277.4000     4 37.400000
# 47 20253 Female  42   NY 316.3333     5 16.333333
# 48 13301   Male  44  ITA 271.3667     4 31.366667
# 49  8827   Male  45   MA 248.0833     4  8.083333
# 50   836   Male  30   NY 186.4500     3  6.450000
# 51  7132   Male  33   CA 237.2000     3 57.200000
# 52 20479 Female  39  CAN 318.6333     5 18.633333
# 53 23499   Male  67   GA 431.6167     7 11.616667
# 54  4969   Male  35  GBR 227.0167     3 47.016667
# 55 22469 Female  42   NY 353.1000     5 53.100000
# 56 16264 Female  30   MA 284.0167     4 44.016667
# 57 23338 Female  31   NY 407.4333     6 47.433333
# 58  9692   Male  44  FRA 252.6333     4 12.633333
# 59 16010   Male  44  MEX 283.7500     4 43.750000
# 60  3502 Female  38  GBR 218.3833     3 38.383333
# 61  2596   Male  27  MEX 207.0167     3 27.016667
# 62 23603 Female  58   NY 467.1833     7 47.183333
# 63  5909   Male  36   NY 231.4167     3 51.416667
# 64 12677   Male  36  NED 265.1333     4 25.133333
# 65   507   Male  33   CT 177.3667     2 57.366667
# 66 17290   Male  40   NY 292.1333     4 52.133333
# 67  6077 Female  28   NY 234.2000     3 54.200000
# 68 12275 Female  32   NY 264.5000     4 24.500000

# slowest time: 02 hours 44 minutes
# fastest time: 07 hours 47 minutes

# 1e)
nym2002[order(nym2002$time),]
#    place gender age home     time
# 15   200   Male  40  GER 163.9333
# ...
# 55 22469 Female  42   NY 353.1000
# 57 23338 Female  31   NY 407.4333
# 24 23413   Male  71   FL 414.7000
# 53 23499   Male  67   GA 431.6167
# 62 23603 Female  58   NY 467.1833


# There were no men who finished after the slowest woman.  

# 1f)
nym2002[order(nym2002$time),]
#    place gender age home     time
# 15   200   Male  40  GER 163.9333
# 31   213   Male  35   VA 164.5000
# 65   507   Male  33   CT 177.3667

# The fastest runner lives in Germany. 

# 1g)

nym2002[order(nym2002$time),]
# place gender age home     time
# 24 23413   Male  71   FL 414.7000
# 53 23499   Male  67   GA 431.6167
# 62 23603 Female  58   NY 467.1833

# The slowest male runner is age 67.  

# 1h)
# place gender age home     time
# 15   200   Male  40  GER 163.9333
# 31   213   Male  35   VA 164.5000
# 65   507   Male  33   CT 177.3667

# The fastest finisher's place is 15.

# 1i)
nonUS <- subset(nym2002, length(home)>2, select=c("home")
nonUS
length(nonUS$home > 2)

length(nonlist_nonUS)

# Problem 2

# 2a)
type = c("terrestrial","gas")
type = rep(type, each=4)
type

diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
diameter

rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rotation

rings = c("ring", "none")
rings = rep(rings, each = 4)
rings


planets = data.frame(type, diameter, rotation, rings)
planets


# type diameter rotation rings
# 1 terrestrial    0.382    58.64  ring
# 2 terrestrial    0.949  -243.02  ring
# 3 terrestrial    1.000     1.00  ring
# 4 terrestrial    0.532     1.03  ring
# 5         gas   11.209     0.41  none
# 6         gas    9.449     0.43  none
# 7         gas    4.007    -0.72  none
# 8         gas    3.883     0.67  none

# 2b)
closest3 = planets[1:3,]
closest3
# type diameter rotation rings
# 1 terrestrial    0.382    58.64  ring
# 2 terrestrial    0.949  -243.02  ring
# 3 terrestrial    1.000     1.00  ring

# 2c)
row.names(planets) = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
planets$diameter > 1
# [1] FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
# The planets with a diameter greater than Earth's (1), are the last four planets: Jupiter, Saturn, Uranus, and Neptune. 


# 2d)
planets$rotation > 1
# [1]  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
# The planets with a rotation greater than Earth's (1), are Mercury and Mars.


# Problem 3

# 3a)
mylist = list(Name="Gretchen Martinet", Department="Statistics", Courses=c(92559,3080,3220,4993), 
              Enr2559=6, Enr3080=c(40,42), Enr3220=60, Enr4993=1, Day2559="Thursday", Day3080=c("Monday","Wednesday","Friday"), Day3220=c("Monday","Wednesday"),
              Day4993="Thursday")
mylist
# $Name
# [1] "Gretchen Martinet"
# 
# $Department
# [1] "Statistics"
# 
# $Courses
# [1] 92559  3080  3220  4993
# 
# $Enr2559
# [1] 6
# 
# $Enr3080
# [1] 40 42
# 
# $Enr3220
# [1] 60
# 
# $Enr4993
# [1] 1
# 
# $Day2559
# [1] "Thursday"
# 
# $Day3080
# [1] "Monday"    "Wednesday" "Friday"   
# 
# $Day3220
# [1] "Monday"    "Wednesday"
# 
# $Day4993
# [1] "Thursday"


# 3b)
mylist$Enr3080[2]
# [1] 42

# 3c)
length(mylist$Day3220)
# [1] 2

# 3d)
notalist = unlist(mylist)
notalist
length(which(notalist == "Monday"))
length(which(notalist == "Tuesday"))
length(which(notalist == "Wednesday"))
length(which(notalist == "Thursday"))
length(which(notalist == "Friday"))

# > length(which(notalist == "Monday"))
# [1] 2
# > length(which(notalist == "Tuesday"))
# [1] 0
# > length(which(notalist == "Wednesday"))
# [1] 2
# > length(which(notalist == "Thursday"))
# [1] 2
# > length(which(notalist == "Friday"))
# [1] 1

# Therefore, the days with the most class meetings are Monday, Wednesday, and Thursday. 


# Works Cited
# https://stackoverflow.com/questions/1813550/count-of-entries-in-data-frame-in-r
# http://www.r-tutor.com/r-introduction/data-frame/data-import
# https://www.rdocumentation.org/
# https://www.tutorialspoint.com/r/r_lists.htm
# https://stat.ethz.ch/pipermail/r-help/2006-September/112969.html
# https://developmentality.wordpress.com/2010/02/12/r-sorting-a-data-frame-by-the-contents-of-a-column/

