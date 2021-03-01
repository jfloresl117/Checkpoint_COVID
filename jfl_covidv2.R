# this script is meant to be a presentation for basic uses of R for BEDU
# data analysis certification

rm(list=ls()) # to remove previous data
library(Hmisc)

#setwd
COVID <- read.csv("COVID19_line_list_data.csv")
summary(COVID) 
describe(COVID) # #to call data frame, 27 col, 1085 lines, helps see informative details from the data


# if we look in death, there are 14 indistinct values, some are 1 or 0, empty values or dates
# therefore we need to clean our data

COVID$death_dummy <- as.integer(COVID$death != 0) #if the death col is not 0 then the patient died, if it is = 0, they didnt

unique(COVID$death_dummy) #checking all values are 1 or 0

# death rate
sum(COVID$death_dummy) #since death = 1
nrow(COVID) #total rows
sum(COVID$death_dummy) / nrow(COVID) # death rate = 6.08 percent

# we have learned from COVID that older people die and younger people survive

dead = subset(COVID, death_dummy == 1)

alive = subset(COVID, death_dummy == 0)

mean(dead$age)
mean(alive$age) #NA since age is missing some values

# scatterplot to find out NA
library(ggplot2)

(COVID_sp <- ggplot(COVID, aes(x = age, y = gender)) + geom_point())

mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE) # remove age = NA


# use t test to determine if this statically significant 
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)

# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value < 0, so we reject the null hypothesis and conclude that this is statistically significant

# as was done with age, lets see how it works for gender, as COVID has shown no gender significance

men = subset(COVID, gender == "male")
women = subset(COVID, gender == "female")

mean(men$death_dummy)
mean(women$death_dummy) # remove gender = NA

mean(men$death_dummy, na.rm = TRUE) # 8.5% of men die
mean(women$death_dummy, na.rm = TRUE) # 3.7% of women die

# use t test to determine if this statically significant
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.95)

# men are most likely to die from 1.7% to 7.8%
# p-value = 0.002 < 0.05, so this is statistically significant within the sample
