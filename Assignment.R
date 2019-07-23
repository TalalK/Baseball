# Talal Khodr Homework 2 (Predictive Analytics)
install.packages("pastecs")
install.packages("lattice")

# Part 1

library(readr)

bobble = read_csv("/Users/tkhodr/Desktop/Predictive Analytics/Week 2-Files/HW2/bobbleheads.csv")

dim(bobble)

head(bobble)

bobble$ordered_day_of_week
#define an ordered day-of-week variable 
bobble$ordered_day_of_week <- with(data=bobble,
ifelse ((day_of_week == "Monday"),1,
ifelse ((day_of_week == "Tuesday"),2,
ifelse ((day_of_week == "Wednesday"),3,
ifelse ((day_of_week == "Thursday"),4,
ifelse ((day_of_week == "Friday"),5,
ifelse ((day_of_week == "Saturday"),6,7)))))))

bobble$ordered_day_of_week <- factor(bobble$ordered_day_of_week, levels=1:7,
labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

# The difference is how its ordered, Friday just appears first.
# However in the second one it is given significance levels and it understands the 
# Relationship the string has to the factor level provided.

table(bobble$day_of_week)
table(bobble$ordered_day_of_week)

sapply(bobble,class)

"""               year           home_team 
          "numeric"         "character" 
              month                 day 
        "character"           "numeric" 
             attend         day_of_week 
          "numeric"         "character" 
           opponent                temp 
        "character"           "numeric" 
              skies           day_night 
        "character"         "character" 
                cap               shirt 
        "character"         "character" 
          fireworks          bobblehead 
        "character"         "character" 
ordered_day_of_week 
           "factor" 
"""


team_stats <- as.data.frame(table(bobble$home_team))
summary(team_stats)
# 30 teams
# Median :81.0

#Part 2

load("/Users/tkhodr/Desktop/Predictive Analytics/Week 2-Files/HW2/dodgers_bad.Rda")

dim(dodgers_bad)

unique_dodger <- unique(dodgers_bad)

dim(unique_dodger)


library(pastecs)
options(scipen = 1000)
options(digits=2)
stat.desc(unique_dodger$attend)        
"""
 nbr.val     nbr.null       nbr.na          min          max 
       81.00         0.00         0.00     24312.00    100000.00 
       range          sum       median         mean      SE.mean 
    75688.00   3622209.00     40619.00     44718.63      1807.69 
CI.mean.0.95          var      std.dev     coef.var 
     3597.42 264687899.81     16269.23         0.36 
"""
stat.desc(unique_dodger$temp)

"""
  nbr.val     nbr.null       nbr.na          min          max 
       81.00         0.00         0.00       -30.00        95.00 
       range          sum       median         mean      SE.mean 
      125.00      4883.00        71.00        60.28         3.88 
CI.mean.0.95          var      std.dev     coef.var 
        7.72      1218.31        34.90         0.58 
"""
## Temp stands out s it has a 1218 variation
## Max is 100K for attend, more than capacity.

table(unique_dodger$year)
barplot(table(unique_dodger$year))


as.data.frame(table(unique_dodger$opponent)) 
#San Diego Padres   18 Stands out as they were played 18 times.

ids <- subset(unique_dodger, 
              date =='2012-05-07' | 
                date =='2012-05-08' |
                date =='2012-05-09' |
                date =='2012-08-20' |
                date =='2012-08-21' |
                date =='2012-08-22' |
                date =='2012-10-01' |
                date =='2012-10-02' |
                date =='2012-10-03' 
              ,
              select=c(gameid, date, opponent)
)
ids
'''
1628     13 2012-05-07 San Diego Padres
1629     14 2012-05-08 San Diego Padres
1630     15 2012-05-09 San Diego Padres
1674     59 2012-08-20 San Diego Padres
1675     60 2012-08-21 San Diego Padres
1676     61 2012-08-22 San Diego Padres
1694     79 2012-10-01 San Diego Padres
1695     80 2012-10-02 San Diego Padres
1696     81 2012-10-03 San Diego Padres
'''

unique_dodger[complete.cases(unique_dodger),]

'''
     year           home_team month day attend day_of_week
1658 2012 Los Angeles Dodgers   JUL   4  53570   Wednesday
            opponent temp skies day_night cap shirt fireworks
1658 Cincinnati Reds   70 Clear     Night  NO    NO       YES
     bobblehead ordered_day_of_week gameid       date
1658         NO                 Wed     43 2012-07-04
'''
# Just surpirised so many attended for a non booble head game.

'''
1                  1: Original    97
2          2: Year Adj to   81    97
3         3: Attend Adj to  81    97
4     4: XX Opponent Adj to 81    97
5 5: Temp Adj to Average of 81    97

'''

# Part 3

load("/Users/tkhodr/Desktop/Predictive Analytics/Week 2-Files/HW2/dodgers_good.Rda")
#exploratory data analysis with standard graphics: attendance by day of week
with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
                       xlab = "Day of Week", ylab = "Attendance (thousands)", 
                       col = "violet", las = 1))
#Highest attendance is usually on a Tuesday.


#look at the distribution of attend
hist(dodgers$attend, col="blue")
#Shows that most attendance is more than half the capacity.


#capacity
pct_full<-(dodgers$attend/(56000))*100
barplot(pct_full, main="Percent of Stadium Filled",
        xlab="Game ID", ylab = "Percent Full", col="yellow")

# The stadium is never under 40% capacity.

# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
ifelse ((month == "APR"),4,
ifelse ((month == "MAY"),5,
ifelse ((month == "JUN"),6,
ifelse ((month == "JUL"),7,
ifelse ((month == "AUG"),8,
ifelse ((month == "SEP"),9,10)))))))

dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

# exploratory data analysis with standard R graphics: attendance by month 
with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month", 
ylab = "Attendance (thousands)", col = "light blue", las = 1))

# June is the month with the highest attendance at Dodger stadium
# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) # used for plotting 

# let us prepare a graphical summary of the dodgers data
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")
xyplot(attend/1000 ~ temp | skies + day_night, 
data = dodgers, groups = fireworks, pch = group.symbols, 
aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
layout = c(2, 2), type = c("p","g"),
strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
xlab = "Temperature (Degrees Fahrenheit)", 
ylab = "Attendance (thousands)",
key = list(space = "top", 
text = list(rev(group.labels),col = rev(group.colors)),
points = list(pch = rev(group.symbols), col = rev(group.colors),
fill = rev(group.fill)))) 

# The obvious is that fireworks arent done duing the day.
# Fireworks are always either on clear or cloudy nights.

#attendance by opponent and day/night game
group.labels <- c("Day","Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night, 
xlab = "Attendance (thousands)",
panel = function(x, y, groups, subscripts, ...) 
{panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
cex = group.symbols.size, pch = group.symbols, col = "darkblue")
},

key = list(space = "top",
text = list(group.labels,col = "black"),
points = list(pch = group.symbols, cex = group.symbols.size, 
col = "darkblue")))

# Most attendance is at night except for the pirates and white sox.
# Most of the games take place at night as well.
# THe highest attendance was at the Giants game.

#Extra Credit

plot(dodgers$bobblehead, col="blue")

# To check which of the matches had bobbleheads and which did not.

