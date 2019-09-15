# construciton of a data frame
manager <- c(1,2,3,4,5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK") 
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2) 
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1,q2,q3,q4,q5, stringsAsFactors = FALSE)

# using transform function to add dimension to the datagfram
leadership <- transform(leadership,
                    sumx = q1 + q2,
                    meanx = (q1+q1)/2) 


# turning integes into classes by applying 2 methods
leadership$age[leadership$age  == 99]  <- NA  # it is for ruling out unavailable data
leadership$agecat[leadership$age > 75]  <- "elder"
leadership$agecat[leadership$age >= 55 & 
                    leadership <= 75 ]  <- "middle age"
leadership$agecat[leadership$age < 55]  <- "young"

leadership <- within(leadership,{
  agecat <-NA
  agecat[age >75] <- "elder"
  agecat[age >=55 & age<= 75] <- "middle age"
  agecat[age < 55]   <-"young"
})


leadership <- within(leadership,{
  gendercat <- "name"   # assign whatever names to gendercat to "create" gendercat first.
  gendercat[gender == "M"] <- "male"
  gendercat[gender == "F"] <- "Female"
})



# rename variables
names(leadership)[6:10] <- c("item1","item2","item3","item4","item5")
leadership

names(leadership)[6:10] <- c("q1","q2","q3","q4","q5")
leadership

y <- c(1,2,3,NA)
is.na(y)

# ascertaining NA data
is.na(leadership[,6:10])

is.infinite(5/0)

# how to delete incomplete data
leadership_backup <- na.omit(leadership)


# how to formalise data value

strdate <- c("2019-3-17","2019-3-18")
dates <- as.Date(strdate, "%Y-%m-%d")
dates2 <- as.Date(strdate, "%m/%d/%Y")


# format function is used for print strings of date in terms of Date format
myformat <- "%m/%d/%y"
leadership$date <- as.Date(leadership$date, myformat)
leadership

today <- Sys.Date()
format(today, format = "%B %d %Y")
format(today, format = "%Y %B %d")


format(leadership$date, format = "%y %B %d")

today <- Sys.Date()
#remember to indicate the format as "%m/%d/%y"
tomorrow <- as.Date("03/19/19", format = "%m/%d/%y")
difftime(today, tomorrow, units = "weeks")

# how to turn the date format into character format:
strDates <- as.character(Sys.Date())
as.character(c(3,2,3))
is.numeric(date)
# when you are confused about functions, use help("function_name")

# how to order the data
attach(leadership) #attach is a function that specifies the search route
newdata <- leadership[order(age),]
detach(leadership)
