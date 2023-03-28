# Must call library to do skewness and car to do vif
library(e1071)
library(car)

# set working directory
setwd("F:/Master's Program - Business Analytics/GBA 5140 - Stats with R/Project")
setwd("C:/Users/16262/Desktop/5140 R")

# 1. Loading the data
turo.df <- readRDS("turo.data.5140")
turo.df <- readRDS("C:/Users/margriet.situmeang/Desktop/CPP_MasterProgram/GBA 5140/GBA 5140_Assignment/turo.data.5140")

# Exploring the column names
colnames(turo.df)

# 2. Subset the data and filter the state to AZ
turo.az.df <- subset(turo.df, car.state == "az")

# Check to be sure we only have AZ
unique(turo.az.df$car.state)

# Explore and understand the structure of the dataset
str(turo.az.df)

# 3. Compute summary statistics and generate charts for ALL variables in 
# the dataset, after excluding missing values. For each continuous 
# variable, compute min, first quartile, medium, third quartile, 
# max, mean, standard deviation, and skewness as summary statistics,
# and draw histogram. For each categorical variable, compute frequency 
# and relative frequency distributions, and draw bar chart.

# car.city column
# Creating frequency chart and renaming first column
car.city.freq <- data.frame(table(factor(turo.az.df$car.city)))
names(car.city.freq)[1] <- "car.city"
car.city.freq

# Adding relative frequency
car.city.freq$Rel.Freq <- car.city.freq$Freq / sum(car.city.freq$Freq)

# Creating bar chart
barplot(car.city.freq$Freq, names.arg = car.city.freq$car.city, xlab = "CarCity", 
        ylab = "Frequency", main = "Bar Chart of Car City")

###
# car.deliver.airport.num column
car.deliver.airport.num.freq <- data.frame(table(factor(turo.az.df$car.deliver.airport.num)))
names(car.deliver.airport.num.freq)[1] <- "car.deliver.airport.num"
car.deliver.airport.num.freq

# Adding relative frequency
car.deliver.airport.num.freq$Rel.Freq <- car.deliver.airport.num.freq$Freq / sum(car.deliver.airport.num.freq$Freq)

# Creating bar chart
barplot(car.deliver.airport.num.freq$Freq, names.arg = car.deliver.airport.num.freq$car.deliver.airport.num, xlab = "Car Deliver Airport", 
        ylab = "Frequency", main = "Bar Chart of Car Deliver Airport")

###
# car.deliver.hotel.num column
car.deliver.hotel.num.freq <- data.frame(table(factor(turo.az.df$car.deliver.hotel.num)))
names(car.deliver.hotel.num.freq)[1] <- "car.deliver.hotel.num"
car.deliver.hotel.num.freq

# Adding relative frequency
car.deliver.hotel.num.freq$Rel.Freq <- car.deliver.hotel.num.freq$Freq / sum(car.deliver.hotel.num.freq$Freq)

# Creating bar chart
barplot(car.deliver.hotel.num.freq$Freq, names.arg = car.deliver.hotel.num.freq$car.deliver.hotel.num, xlab = "CarDeliverHotel", 
        ylab = "Frequency", main = "Bar Chart of Car Deliver Hotel")

###
##car.deliver.to.you.num
car.deliver.to.you.freq <- data.frame(table(factor(turo.az.df$car.deliver.to.you.num)))
names(car.deliver.to.you.freq)[1] <- "Car Deliver to you"
car.deliver.to.you.freq

# Adding relative frequency
car.deliver.to.you.freq$Rel.Freq <- car.deliver.to.you.freq$Freq / sum(car.deliver.to.you.freq$Freq)
car.deliver.to.you.freq$Rel.Freq

# Creating bar chart
barplot(car.deliver.to.you.freq$Freq, names.arg = car.deliver.to.you.freq$`Car Deliver to you`, xlab = "Car Deliver to you", 
        ylab = "Frequency", main = "Bar Chart of Car Deliver to you (0: Yes, 1:No)")

###
# car.deliver.train.station.num column
car.deliver.train.station.num.freq <- data.frame(table(factor(turo.az.df$car.deliver.train.station.num)))
names(car.deliver.train.station.num.freq)[1] <- "Car Deliver Train Station"
car.deliver.train.station.num.freq

# Adding relative frequency
car.deliver.train.station.num.freq$Rel.Freq <- car.deliver.train.station.num.freq$Freq / sum(car.deliver.train.station.num.freq$Freq)

# Creating bar chart
barplot(car.deliver.train.station.num.freq$Freq, names.arg = car.deliver.train.station.num.freq$CarDeliverTrainStation, xlab = "Car Deliver Train Station", 
        ylab = "Frequency", main = "Bar Chart of Car Deliver Train Station")

###
# car.displayed.turo.review.num column
car.displayed.turo.review.num.freq <- data.frame(table(factor(turo.az.df$car.displayed.turo.review.num)))
names(car.displayed.turo.review.num.freq)[1] <- "car.displayed.turo.review.num"
car.displayed.turo.review.num.freq

# Adding relative frequency
car.displayed.turo.review.num.freq$Rel.Freq <- car.displayed.turo.review.num.freq$Freq / sum(car.displayed.turo.review.num.freq$Freq)

# Creating bar chart
barplot(car.displayed.turo.review.num.freq$Freq, 
        names.arg = car.displayed.turo.review.num.freq$car.displayed.turo.review.num, 
        xlab = "Number of Car Displayed Turo Reviews", 
        ylab = "Frequency", main = "Bar Chart of Number of Car Displayed Turo Reviews")

###
# car.displayed.turo.review.num.past.12m column
car.displayed.turo.review.num.past.12m.freq <- data.frame(table(factor(turo.az.df$car.displayed.turo.review.num.past.12m)))
names(car.displayed.turo.review.num.past.12m.freq)[1] <- "car.displayed.turo.review.num.past.12m"
car.displayed.turo.review.num.past.12m.freq

# Adding relative frequency
car.displayed.turo.review.num.past.12m.freq$Rel.Freq <- car.displayed.turo.review.num.past.12m.freq$Freq / sum(car.displayed.turo.review.num.past.12m.freq$Freq)

# Creating bar chart
barplot(car.displayed.turo.review.num.past.12m.freq$Freq, 
        names.arg = car.displayed.turo.review.num.past.12m.freq$car.displayed.turo.review.num.past.12m, 
        xlab = "Number of Car Displayed Turo Reviews in past 12 Months", 
        ylab = "Frequency", main = "Bar Chart of Number of Car Displayed Turo Reviews in past 12 Months")

###
# car.displayed.turo.review.num.past.18m column
car.displayed.turo.review.num.past.18m.freq <- data.frame(table(factor(turo.az.df$car.displayed.turo.review.num.past.18m)))
names(car.displayed.turo.review.num.past.18m.freq)[1] <- "car.displayed.turo.review.num.past.18m"
car.displayed.turo.review.num.past.18m.freq

# Adding relative frequency
car.displayed.turo.review.num.past.18m.freq$Rel.Freq <- car.displayed.turo.review.num.past.18m.freq$Freq / sum(car.displayed.turo.review.num.past.18m.freq$Freq)

# Creating bar chart
barplot(car.displayed.turo.review.num.past.18m.freq$Freq, 
        names.arg = car.displayed.turo.review.num.past.18m.freq$car.displayed.turo.review.num.past.18m, 
        xlab = "Number of Car Displayed Turo Reviews in past 18 Months", 
        ylab = "Frequency", main = "Bar Chart of Number of Car Displayed Turo Reviews in past 18 Months")

###
# car.displayed.turo.review.num.past.6m column
car.displayed.turo.review.num.past.6m.freq <- data.frame(table(factor(turo.az.df$car.displayed.turo.review.num.past.6m)))
names(car.displayed.turo.review.num.past.6m.freq)[1] <- "car.displayed.turo.review.num.past.6m"
car.displayed.turo.review.num.past.6m.freq

# Adding relative frequency
car.displayed.turo.review.num.past.6m.freq$Rel.Freq <- car.displayed.turo.review.num.past.6m.freq$Freq / sum(car.displayed.turo.review.num.past.6m.freq$Freq)

# Creating bar chart
barplot(car.displayed.turo.review.num.past.6m.freq$Freq, 
        names.arg = car.displayed.turo.review.num.past.6m.freq$car.displayed.turo.review.num.past.6m, 
        xlab = "Number of Car Displayed Turo Reviews in past 6 Months", 
        ylab = "Frequency", main = "Bar Chart of Number of Car Displayed Turo Reviews in past 6 Months")

###
# car.displayed.user.review.num column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and skewness as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.displayed.user.review.num, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.displayed.user.review.num, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.displayed.user.review.num, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.displayed.user.review.num, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.displayed.user.review.num, na.rm = TRUE, type = 2)))
print(paste("First quartile:", quantile(turo.az.df$car.displayed.user.review.num, na.rm = TRUE, 0.25, type = 6)))
print(paste("Median:", quantile(turo.az.df$car.displayed.user.review.num, na.rm = TRUE, 0.5, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.displayed.user.review.num, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.displayed.user.review.num, breaks = "sturges", xlab = "Number of Guest Reviews",
     main = "Histogram of Number of Guest Reviews in Total")

###
# car.displayed.user.review.num.past.12m column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and skewness as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE, type = 2)))
print(paste("First quartile:", quantile(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE, 0.25, type = 6)))
print(paste("Median:", quantile(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE, 0.5, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.displayed.user.review.num.past.12m, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.displayed.user.review.num.past.12m, breaks = "sturges", xlab = "Number of Guest Reviews in Past 12m",
     main = "Histogram of Number of Guest Reviews in Past 12m")

###
# car.displayed.user.review.num.past.18m column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and skewness as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE, type = 2)))
print(paste("First quartile:", quantile(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE, 0.25, type = 6)))
print(paste("Median:", quantile(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE, 0.5, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.displayed.user.review.num.past.18m, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.displayed.user.review.num.past.18m, breaks = "sturges", xlab = "Number of Guest Reviews",
     main = "Histogram of Number of Guest Reviews in Past 18m")

###
# car.displayed.user.review.num.past.6m column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and skewness as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE, type = 2)))
print(paste("First quartile:", quantile(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE, 0.25, type = 6)))
print(paste("Median:", quantile(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE, 0.5, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.displayed.user.review.num.past.6m, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.displayed.user.review.num.past.6m, breaks = "sturges", xlab = "Number of Guest Reviews",
     main = "Histogram of Number of Guest Reviews in Past 6m")

#car.doors column
# Creating frequency chart and renaming first column
car.doors.freq <- data.frame(table(factor(turo.az.df$car.doors)))
names(car.doors.freq)[1] <- "car.doors"


# Adding relative frequency
car.doors.freq$Rel.Freq <- car.doors.freq$Freq / sum(car.doors.freq$Freq)

# Creating bar chart#need
barplot(car.doors.freq$Freq, names.arg = car.doors.freq$car.doors,
        xlab = "Car door number", ylab = "Frequency", main = "Bar Chart of Door Number Frequency")

# car.extra.beach.gear column
# Creating a frequency for the extra.beach.gear column
car.extra.beach.gear.freq <- data.frame(table(factor(turo.az.df$car.extra.beach.gear)))
names(car.extra.beach.gear.freq)[1] <- "extra.beach.gear"

# Adding relative frequency
car.extra.beach.gear.freq$Rel.Freq <- car.extra.beach.gear.freq$Freq / sum(car.extra.beach.gear.freq$Freq)

# Creating bar chart
barplot(car.extra.beach.gear.freq$Freq, names.arg = car.extra.beach.gear.freq$extra.beach.gear,
        xlab = "Car Extra Beach Gear", ylab = "Frequency", main = "Bar Chart of Car Extra Beach Gear Frequency (T/F)")


# car.extra.child.safety.seat. column
# Creating a frequency for the car.transmission column
car.extra.child.safety.seat.freq <- data.frame(table(factor(turo.az.df$car.extra.child.safety.seat)))
names(car.extra.child.safety.seat.freq)[1] <- "car.extra.child.safety.seat"

# Adding relative frequency
car.extra.child.safety.seat.freq$Rel.Freq <- car.extra.child.safety.seat.freq$Freq / sum(car.extra.child.safety.seat.freq$Freq)

# Creating bar chart
barplot(car.extra.child.safety.seat.freq$Freq, names.arg = car.extra.child.safety.seat.freq$car.extra.child.safety.seat,
        xlab = "Car Extra Child Safety Seat (T/F)", ylab = "Frequency", main = "Bar Chart of Car Extra Child Safety Seat Frequency")

# car.extra.cooler column
# Creating a frequency for the car.transmission column
car.extra.cooler.freq <- data.frame(table(factor(turo.az.df$car.extra.cooler)))
names(car.extra.cooler.freq)[1] <- "car.extra.cooler"

# Adding relative frequency
car.extra.cooler.freq$Rel.Freq <- car.extra.cooler.freq$Freq / sum(car.extra.cooler.freq$Freq)

# Creating bar chart
barplot(car.extra.cooler.freq$Freq, names.arg = car.extra.cooler.freq$car.extra.cooler,
        xlab = "Car Extra Cooler", ylab = "Frequency", main = "Bar Chart of Car Extra Cooler (T/F)")

# car.extra.mile.fee column 
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and kurtosis as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.extra.mile.fee, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.extra.mile.fee, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.extra.mile.fee, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.extra.mile.fee, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.extra.mile.fee, na.rm = TRUE, type = 2)))
print(paste("Median:", quantile(turo.az.df$car.extra.mile.fee, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(turo.az.df$car.extra.mile.fee, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.extra.mile.fee, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.extra.mile.fee, breaks = "sturges", xlab = "Car Extra Mile fee",
     main = "Histogram of Car Extra Mile fee")


#car.extra.num column 
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and kurtosis as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.extra.num, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.extra.num, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.extra.num, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.extra.num, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.extra.num, na.rm = TRUE, type = 2)))
print(paste("Median:", quantile(turo.az.df$car.extra.num, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(turo.az.df$car.extra.num, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.extra.num, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.extra.num, breaks = "sturges", xlab = "Car Extra Num",
     main = "Histogram of Car Extra Num")


# car.extra.one.way.trip column
# Creating a frequency for the car.extra.one.way.trip column
car.extra.one.way.trip.freq <- data.frame(table(factor(turo.az.df$car.extra.one.way.trip)))
names(car.extra.one.way.trip.freq)[1] <- "car.extra.one.way.trip"

# Adding relative frequency
car.extra.one.way.trip.freq$Rel.Freq <- car.extra.one.way.trip.freq$Freq / sum(car.extra.one.way.trip.freq$Freq)

# Creating bar chart
barplot(car.extra.one.way.trip.freq$Freq, names.arg = car.extra.one.way.trip.freq$car.extra.one.way.trip,
        xlab = "Car Extra One Way Trip", ylab = "Frequency", main = "Bar Chart of Car Extra One Way Trip (T/F)")

#car.extra.pet.fee
# Creating a frequency for the car.extra.one.way.trip column
car.extra.pet.fee.freq <- data.frame(table(factor(turo.az.df$car.extra.pet.fee)))
names(car.extra.pet.fee.freq)[1] <- "car.extra.pet.fee"

# Adding relative frequency
car.extra.pet.fee.freq$Rel.Freq <- car.extra.pet.fee.freq$Freq / sum(car.extra.pet.fee.freq$Freq)

# Creating bar chart
barplot(car.extra.pet.fee.freq$Freq, names.arg = car.extra.pet.fee.freq$car.extra.pet.fee,
        xlab = "Car Extra Pet Fee", ylab = "Frequency", main = "Bar Chart of Car Extra Pet Fee (T/F)")

# car.extra.phone.mount column
# Creating a frequency for the host.all.star column
car.extra.phone.mount.freq <- data.frame(table(factor(turo.az.df$car.extra.phone.mount)))
names(car.extra.phone.mount.freq)[1] <- "car.extra.phone.mount"

# Adding relative frequency
car.extra.phone.mount.freq$Rel.Freq <- car.extra.phone.mount.freq$Freq / sum(car.extra.phone.mount.freq$Freq)

# Creating bar chart
barplot(car.extra.phone.mount.freq$Freq, names.arg = car.extra.phone.mount.freq$car.extra.phone.mount,
        xlab = "Extra Service Available", ylab = "Frequency", main = "Extra Service Available - Phone Mount (T/F)")

# car.extra.portable.gps  column
# Creating a frequency for the host.all.star column
car.extra.portable.gps.freq <- data.frame(table(factor(turo.az.df$car.extra.portable.gps)))
names(car.extra.portable.gps.freq)[1] <- "car.extra.portable.gps"

# Adding relative frequency
car.extra.portable.gps.freq$Rel.Freq <- car.extra.portable.gps.freq$Freq / sum(car.extra.portable.gps.freq$Freq)

# Creating bar chart
barplot(car.extra.portable.gps.freq$Freq, names.arg = car.extra.portable.gps.freq$car.extra.portable.gps,
        xlab = "Car Extra Portable gps", ylab = "Frequency", main = "Extra Service Portable GPS (T/F)")


# car.extra.post.trip.cleaning column
# Creating a frequency for the host.location.available column
car.extra.post.trip.cleaning.freq <- data.frame(table(factor(turo.az.df$car.extra.post.trip.cleaning)))
names(car.extra.post.trip.cleaning.freq)[1] <- "car.extra.post.trip.cleaning"

# Adding relative frequency
car.extra.post.trip.cleaning.freq$Rel.Freq <- car.extra.post.trip.cleaning.freq$Freq / sum(car.extra.post.trip.cleaning.freq$Freq)

# Creating bar chart
barplot(car.extra.post.trip.cleaning.freq$Freq, names.arg = car.extra.post.trip.cleaning.freq$car.extra.post.trip.cleaning,
        xlab = "Car Extra Post Trip Cleaning", ylab = "Frequency", main = "Extra Service Availability -Post Trip Cleaning (T/F)")

# car.extra.prepaid.ev.recharge column
# Creating a frequency for the car.extra.num column
car.extra.prepaid.ev.recharge.freq <- data.frame(table(factor(turo.az.df$car.extra.prepaid.ev.recharge)))
names(car.extra.prepaid.ev.recharge.freq)[1] <- "car.extra.prepaid.ev.recharge"

# Adding relative frequency
car.extra.prepaid.ev.recharge.freq$Rel.Freq <- car.extra.prepaid.ev.recharge.freq$Freq / sum(car.extra.prepaid.ev.recharge.freq$Freq)

# Creating bar chart
barplot(car.extra.prepaid.ev.recharge.freq$Freq, names.arg = car.extra.prepaid.ev.recharge.freq$car.extra.prepaid.ev.recharge,
        xlab = "Car Extra Prepaid EV Recharge", ylab = "Frequency", main = "Extra Service availability-Prepaid EV Recharge (T/F)")


# car.extra.prepaid.refuel column
# Creating a frequency for the car.extra.num column
car.extra.prepaid.refuel.freq <- data.frame(table(factor(turo.az.df$car.extra.prepaid.refuel)))
names(car.extra.prepaid.refuel.freq)[1] <- "car.extra.prepaid.refuel"

# Adding relative frequency
car.extra.prepaid.refuel.freq$Rel.Freq <- car.extra.prepaid.refuel.freq$Freq / sum(car.extra.prepaid.refuel.freq$Freq)

# Creating bar chart
barplot(car.extra.prepaid.refuel.freq$Freq, names.arg = car.extra.prepaid.refuel.freq$car.extra.prepaid.refuel,
        xlab = "Car Extra Prepaid Refuel", ylab = "Frequency", main = "Extra Service availability-Prepaid Refuel (T/F)")

# car.extra.stroller ====
# |---Frequency distribution ====
car.extra.stroller.freq <- data.frame(table(turo.az.df$car.extra.stroller))
names(car.extra.stroller.freq)[1] <- "Car.Extra.Stroller"
car.extra.stroller.freq

# |---Relative frequency distribution ====
car.extra.stroller.freq$Rel.Freq <- car.extra.stroller.freq$Freq / sum(car.extra.stroller.freq$Freq)
car.extra.stroller.freq

# |---Bar chart ====
barplot(car.extra.stroller.freq$Freq, names.arg = car.extra.stroller.freq$Car.Extra.Stroller,
        xlab = "Extra Service Availability - Stroller", ylab = "Frequency", main = "Bar Chart of Extra Service Availability - Stroller (T/F)")



# car.extra.unlimited.mileage ====
# |---Frequency distribution ====
car.extra.unlimited.mileage.freq <- data.frame(table(turo.az.df$car.extra.unlimited.mileage))
names(car.extra.unlimited.mileage.freq)[1] <- "Car.Extra.Unlimited.Mileage"
car.extra.unlimited.mileage.freq

# |---Relative frequency distribution ====
car.extra.unlimited.mileage.freq$Rel.Freq <- car.extra.unlimited.mileage.freq$Freq / sum(car.extra.unlimited.mileage.freq$Freq)
car.extra.unlimited.mileage.freq

# |---Bar chart ====
barplot(car.extra.unlimited.mileage.freq$Freq, names.arg = car.extra.unlimited.mileage.freq$Car.Extra.Unlimited.Mileage,
        xlab = "Extra Service Availability - Unlimited Mileage", ylab = "Frequency", main = "Bar Chart of Extra Service Availability - Unlimited Mileage (T/F)")



# car.faq.num ====
car.faq.num.freq <- data.frame(table(turo.az.df$car.faq.num))
names(car.faq.num.freq)[1] <- "car.faq.num"
car.faq.num.freq

# |---Relative frequency distribution ====
car.faq.num.freq$Rel.Freq <- car.faq.num.freq$Freq / sum(car.faq.num.freq$Freq)
car.faq.num.freq

# |---Bar chart ====
barplot(car.faq.num.freq$Freq, names.arg = car.faq.num.freq$car.faq.num,
        xlab = "Number of FAQ", ylab = "Frequency", main = "Bar Chart of Number of FAQ")

# car.instant.book ====
# |---Frequency distribution ====
car.instant.book.freq <- data.frame(table(turo.az.df$car.instant.book))
names(car.instant.book.freq)[1] <- "Car.Instant.Book"
car.instant.book.freq

# |---Relative frequency distribution ====
car.instant.book.freq$Rel.Freq <- car.instant.book.freq$Freq / sum(car.instant.book.freq$Freq)
car.instant.book.freq

# |---Bar chart ====
barplot(car.instant.book.freq$Freq, names.arg = car.instant.book.freq$Car.Instant.Book,
        xlab = "Instant Booking Availability", ylab = "Frequency", main = "Instant Booking Availability (T/F)")


# car.insurance ====
# |---Frequency distribution ====
car.insurance.freq <- data.frame(table(turo.az.df$car.insurance))
names(car.insurance.freq)[1] <- "Car.Insurance"

#|--- Creating another frequency table without frequency of 0 ====
car.insurance.freq1 <- car.insurance.freq[!(car.insurance.freq$Freq == "0"),]
car.insurance.freq1

# |---Relative frequency distribution with all values ====
car.insurance.freq$Rel.Freq <- car.insurance.freq$Freq / sum(car.insurance.freq$Freq)

# |---Relative frequency distribution without values that has no freq ====
car.insurance.freq1$Rel.Freq <- car.insurance.freq1$Freq / sum(car.insurance.freq1$Freq)


# |---Bar chart with all values ====
barplot(car.insurance.freq$Freq, names.arg = car.insurance.freq$Car.Insurance, 
        xlab = "Car Insurance Provider", ylab = "Frequency", main = "Bar Chart of Car Insurance Provider (T/F)")

# |---Bar chart without values that has no freq ====
barplot(car.insurance.freq1$Freq, names.arg = car.insurance.freq1$Car.Insurance, 
        xlab = "Car Insurance Provider", ylab = "Frequency", main = "Bar Chart of Car Insurance Provider (T/F)")





# car.make ====
# |---Frequency distribution ====
car.make.freq <- data.frame(table(turo.az.df$car.make))
names(car.make.freq)[1] <- "Car.Make"

#|--- Creating another frequency table without frequency of 0 ====
car.make.freq1 <- car.make.freq[!(car.make.freq$Freq == "0"),]

# |---Relative frequency distribution with all values ====
car.make.freq$Rel.Freq <- car.make.freq$Freq / sum(car.make.freq$Freq)


# |---Relative frequency distribution without values that has no freq ====
car.make.freq1$Rel.Freq <- car.make.freq1$Freq / sum(car.make.freq1$Freq)

# |---Bar chart with all values ====
barplot(car.make.freq$Freq, names.arg = car.make.freq$Car.Make,
        xlab = "Car Make", ylab = "Frequency", main = "Bar Chart of Car Make")


# |---Bar chart without values that has no freq ====
barplot(car.make.freq1$Freq, names.arg = car.make.freq1$Car.Make,
        xlab = "Car Make", ylab = "Frequency", main = "Bar Chart of Car Make")



# car.miles.included ====

# |--- number of "inf" in data set ====
length(which("Inf" == turo.az.df$car.miles.included ))

# |---length of data set without "inf"  ====
length(which("Inf" != turo.az.df$car.miles.included ))

# |---length of data set ====
sum(length(turo.az.df$car.miles.included))

sum(is.finite(turo.az.df$car.miles.included))

# |---Frequency distribution ====
car.miles.included.freq <- data.frame(table(turo.az.df$car.miles.included))
car.miles.included.freq

names(car.miles.included.freq)[1] <- "Car.Miles.Included"
car.miles.included.freq

# |---Relative frequency distribution ====
car.miles.included.freq$Rel.Freq <- car.miles.included.freq$Freq / sum(car.miles.included.freq$Freq)
car.miles.included.freq


# |---Bar chart ====
barplot(car.miles.included.freq$Freq, names.arg = car.miles.included.freq$Car.Miles.Included,
        xlab = "Miles Included", ylab = "Frequency", main = "Bar Chart of Miles Included")


# |---List of finite values (remove "inf") ====
car.miles.included.rminf <- subset(turo.az.df$car.miles.included, turo.az.df$car.miles.included != "Inf")
car.miles.included.rminf

print(paste("Mean (average):", mean(car.miles.included.rminf, na.rm = TRUE)))
print(paste("Minimum:", min(car.miles.included.rminf, na.rm = TRUE)))
print(paste("Maximum", max(car.miles.included.rminf, na.rm = TRUE)))
print(paste("Standard deviation:", sd(car.miles.included.rminf, na.rm = TRUE)))
print(paste("Skewness:", skewness(car.miles.included.rminf, type = 2)))
print(paste("Median:", quantile(car.miles.included.rminf, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(car.miles.included.rminf, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(car.miles.included.rminf, na.rm = TRUE, 0.75, type = 6)))


# |---Creating a histogram of finite values ====
hist(car.miles.included.rminf, breaks = "sturges", xlab = "Miles Included", main = "Histogram of Miles Included (Inf Removed)")


# car.model ====
# |---Frequency distribution ====
car.model.freq <- data.frame(table(turo.az.df$car.model))
names(car.model.freq)[1] <- "Car.Model"

#|--- Creating another frequency table without frequency of 0 ====
car.model.freq1 <- car.model.freq[!(car.model.freq$Freq == "0"),]

# |---Relative frequency distribution with all values ====
car.model.freq$Rel.Freq <- car.model.freq$Freq / sum(car.model.freq$Freq)


# |---Relative frequency distribution without values that has no freq ====
car.model.freq1$Rel.Freq <- car.model.freq1$Freq / sum(car.model.freq1$Freq)

# |---Bar chart with all values ====
barplot(car.model.freq$Freq, names.arg = car.model.freq$Car.Model,
        xlab = "Car Model", ylab = "Frequency", main = "Bar Chart of Car Model")


# |---Bar chart without values that has no freq ====
barplot(car.model.freq1$Freq, names.arg = car.model.freq1$Car.Model,
        xlab = "Car Model", ylab = "Frequency", main = "Bar Chart of Car Model")



# car.photo.num ====
print(paste("Mean (average):", mean(turo.az.df$car.photo.num, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.photo.num, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.photo.num, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.photo.num, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.photo.num, type = 2)))
print(paste("Median:", quantile(turo.az.df$car.photo.num, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(turo.az.df$car.photo.num, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.photo.num, na.rm = TRUE, 0.75, type = 6)))

# |---Creating a histogram ====
hist(turo.az.df$car.photo.num, breaks = "sturges", xlab = "Number of Car Photos", main = "Histogram of Number of Car Photos")




# car.photo.verified ====
# |---Frequency distribution ====
car.photo.verified.freq <- data.frame(table(turo.az.df$car.photo.verified))
names(car.photo.verified.freq)[1] <- "Car.Photo.Verified"


# |---Relative frequency distribution ====
car.photo.verified.freq$Rel.Freq <- car.photo.verified.freq$Freq / sum(car.photo.verified.freq$Freq)


# |---Bar chart ====
barplot(car.photo.verified.freq$Freq, names.arg = car.photo.verified.freq$Car.Photo.Verified,
        xlab = "Car Photo Verified or Not", ylab = "Frequency", main = "Bar Chart of Car Photo Verified or Not (T/F)")



# car.power ====
# |---Frequency distribution ====
car.power.freq <- data.frame(table(turo.az.df$car.power))
names(car.power.freq)[1] <- "Car.Power"


# |---Relative frequency distribution ====
car.power.freq$Rel.Freq <- car.power.freq$Freq / sum(car.power.freq$Freq)

# |---Bar chart ====
barplot(car.power.freq$Freq, names.arg = car.power.freq$Car.Power,
        xlab = "Car Power Type", ylab = "Frequency", main = "Bar Chart of Car Power Type")



# car.rental.type ====
# |---Frequency distribution ====
car.rental.type.freq <- data.frame(table(turo.az.df$car.rental.type))
names(car.rental.type.freq)[1] <- "Car.Rental.Type"


# |---Relative frequency distribution ====
car.rental.type.freq$Rel.Freq <- car.rental.type.freq$Freq / sum(car.rental.type.freq$Freq)


# |---Bar chart ====
barplot(car.rental.type.freq$Freq, names.arg = car.rental.type.freq$Car.Rental.Type,
        xlab = "Car Rental Type", ylab = "Frequency", main = "Bar Chart of Car Rental Type")


# car.self.pickup.avg.price ====
print(paste("Mean (average):", mean(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.self.pickup.avg.price, type = 2, na.rm = TRUE)))
print(paste("Median:", quantile(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.self.pickup.avg.price, na.rm = TRUE, 0.75, type = 6)))

# |---Creating a histogram ====
hist(turo.az.df$car.self.pickup.avg.price, breaks = "sturges", xlab = "Price For Self Pick Up", main = "Histogram of Price For Self Pick Ups")

# car.self.pickup.num
# Creating frequency chart and renaming first column
car.self.pickup.num.freq <- data.frame(table(factor(turo.az.df$car.self.pickup.num)))
names(car.self.pickup.num.freq)[1] <- "car.self.pickup.num"

# Adding relative frequency
car.self.pickup.num.freq$Rel.Freq <- car.self.pickup.num.freq$Freq / sum(car.self.pickup.num.freq$Freq)

# Creating bar chart
barplot(car.self.pickup.num.freq$Freq, names.arg = car.self.pickup.num.freq$car.self.pickup.num,
        xlab = "Car Self Pick Up Num", ylab = "Frequency", main = "Bar Chart of Self Pick Up Num (0: No, 1: Yes) Frequency")

# car.state column
# Creating a frequency for the car.state column
car.state.freq <- data.frame(table(factor(turo.az.df$car.state)))
names(car.state.freq)[1] <- "car.state"

# Adding relative frequency
car.state.freq$Rel.Freq <- car.state.freq$Freq / sum(car.state.freq$Freq)

# Creating bar chart
barplot(car.state.freq$Freq, names.arg = car.state.freq$car.state,
        xlab = "Car State", ylab = "Frequency", main = "Bar Chart of Car State Frequency")

# car.transmission column
# Creating a frequency for the car.transmission column
car.transmission.freq <- data.frame(table(factor(turo.az.df$car.transmission)))
names(car.transmission.freq)[1] <- "car.transmission"

# Adding relative frequency
car.transmission.freq$Rel.Freq <- car.transmission.freq$Freq / sum(car.transmission.freq$Freq)

# Creating bar chart
barplot(car.transmission.freq$Freq, names.arg = car.transmission.freq$car.transmission,
        xlab = "Car Transmission", ylab = "Frequency", main = "Bar Chart of Car Transmission Frequency")

# car.trip.price column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and kurtosis as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$car.trip.price, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$car.trip.price, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$car.trip.price, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$car.trip.price, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$car.trip.price, na.rm = TRUE, type = 2)))
print(paste("Median:", quantile(turo.az.df$car.trip.price, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(turo.az.df$car.trip.price, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$car.trip.price, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$car.trip.price, breaks = "sturges", xlab = "Car Trip Price",
     main = "Histogram of Car Trip Price")

# car.turo.go column
# Creating a frequency for the car.transmission column
car.turo.go.freq <- data.frame(table(factor(turo.az.df$car.turo.go)))
names(car.turo.go.freq)[1] <- "car.turo.go"

# Adding relative frequency
car.turo.go.freq$Rel.Freq <- car.turo.go.freq$Freq / sum(car.turo.go.freq$Freq)

# Creating bar chart
barplot(car.turo.go.freq$Freq, names.arg = car.turo.go.freq$car.turo.go,
        xlab = "Car Turo Go", ylab = "Frequency", main = "Bar Chart of Car Turo Go")

# car.year column
# Creating a frequency for the car.year column
car.year.freq <- data.frame(table(factor(turo.az.df$car.year)))
names(car.year.freq)[1] <- "car.year"

# Adding relative frequency
car.year.freq$Rel.Freq <- car.year.freq$Freq / sum(car.year.freq$Freq)

# Creating bar chart
barplot(car.year.freq$Freq, names.arg = car.year.freq$car.year,
        xlab = "Car Year", ylab = "Frequency", main = "Bar Chart of Car Year")

# host.all.star column
# Creating a frequency for the host.all.star column
host.all.star.freq <- data.frame(table(factor(turo.az.df$host.all.star)))
names(host.all.star.freq)[1] <- "host.all.star"

# Adding relative frequency
host.all.star.freq$Rel.Freq <- host.all.star.freq$Freq / sum(host.all.star.freq$Freq)

# Creating bar chart
barplot(host.all.star.freq$Freq, names.arg = host.all.star.freq$host.all.star,
        xlab = "Host All Star", ylab = "Frequency", main = "Bar Chart of Host All Star (T/F)")

# host.car.num column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and kurtosis as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$host.car.num, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$host.car.num, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$host.car.num, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$host.car.num, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$host.car.num, na.rm = TRUE, type = 2)))
print(paste("Median:", quantile(turo.az.df$host.car.num, na.rm = TRUE, 0.5, type = 6)))
print(paste("First quartile:", quantile(turo.az.df$host.car.num, na.rm = TRUE, 0.25, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$host.car.num, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$host.car.num, breaks = "sturges", xlab = "Host Car Number",
     main = "Histogram of Host Car Number")

# host.location.available column
# Creating a frequency for the host.location.available column
host.location.available.freq <- data.frame(table(factor(turo.az.df$host.location.available)))
names(host.location.available.freq)[1] <- "host.location.available"

# Adding relative frequency
host.location.available.freq$Rel.Freq <- host.location.available.freq$Freq / sum(host.location.available.freq$Freq)

# Creating bar chart
barplot(host.location.available.freq$Freq, names.arg = host.location.available.freq$host.location.available,
        xlab = "Host Location Available", ylab = "Frequency", main = "Bar Chart of Host Location Available (T/F)")

# host.tenure.in.weeks column
# Compute summary statistics and generate charts for ALL variables in the dataset, after excluding missing 
# values. For each continuous variable, compute min, first quartile, medium, third quartile, max, mean,
# standard deviation, and kurtosis as summary statistics, and draw histogram.
print(paste("Mean (average):", mean(turo.az.df$host.tenure.in.weeks, na.rm = TRUE)))
print(paste("Minimum:", min(turo.az.df$host.tenure.in.weeks, na.rm = TRUE)))
print(paste("Maximum", max(turo.az.df$host.tenure.in.weeks, na.rm = TRUE)))
print(paste("Standard deviation:", sd(turo.az.df$host.tenure.in.weeks, na.rm = TRUE)))
print(paste("Skewness:", skewness(turo.az.df$host.tenure.in.weeks, na.rm = TRUE, type = 2)))
print(paste("First quartile:", quantile(turo.az.df$host.tenure.in.weeks, na.rm = TRUE, 0.25, type = 6)))
print(paste("Median:", quantile(turo.az.df$host.tenure.in.weeks, na.rm = TRUE, 0.5, type = 6)))
print(paste("Third quartile:", quantile(turo.az.df$host.tenure.in.weeks, na.rm = TRUE, 0.75, type = 6)))

# Creating a histogram
hist(turo.az.df$host.tenure.in.weeks, breaks = "sturges", xlab = "Host Tenures in Weeks",
     main = "Histogram of Host Tenures in Weeks")

# host.verified.approved.to.drive column
# Creating a frequency for the host.verified.approved.to.drive column
host.verified.approved.to.drive.freq <- data.frame(table(factor(turo.az.df$host.verified.approved.to.drive)))
names(host.verified.approved.to.drive.freq)[1] <- "host.verified.approved.to.drive"

# Adding relative frequency
host.verified.approved.to.drive.freq$Rel.Freq <- host.verified.approved.to.drive.freq$Freq / sum(host.verified.approved.to.drive.freq$Freq)

# Creating bar chart
barplot(host.verified.approved.to.drive.freq$Freq, names.arg = host.verified.approved.to.drive.freq$host.verified.approved.to.drive,
        xlab = "Host Verified Approved to Drive", ylab = "Frequency", main = "Bar Chart of Host Verified Approved to Drive (T/F)")

# host.verified.email column
# Creating a frequency for the host.verified.email column
host.verified.email.freq <- data.frame(table(factor(turo.az.df$host.verified.email)))
names(host.verified.email.freq)[1] <- "host.verified.email"

# Adding relative frequency
host.verified.email.freq$Rel.Freq <- host.verified.email.freq$Freq / sum(host.verified.email.freq$Freq)

# Creating bar chart
barplot(host.verified.email.freq$Freq, names.arg = host.verified.email.freq$host.verified.email,
        xlab = "Host Verified Email", ylab = "Frequency", main = "Bar Chart of Host Verified Email (T/F)")

# host.verified.fb column
# Creating a frequency for the host.verified.fb column
host.verified.fb.freq <- data.frame(table(factor(turo.az.df$host.verified.fb)))
names(host.verified.fb.freq)[1] <- "host.verified.fb"

# Adding relative frequency
host.verified.fb.freq$Rel.Freq <- host.verified.fb.freq$Freq / sum(host.verified.fb.freq$Freq)

# Creating bar chart
barplot(host.verified.fb.freq$Freq, names.arg = host.verified.fb.freq$host.verified.fb,
        xlab = "Host Verified Facebook", ylab = "Frequency", main = "Bar Chart of Host Verified Facebook (T/F)")

# host.verified.phone column
# Creating a frequency for the host.verified.phone column
host.verified.phone.freq <- data.frame(table(factor(turo.az.df$host.verified.phone)))
names(host.verified.phone.freq)[1] <- "host.verified.phone"

# Adding relative frequency
host.verified.phone.freq$Rel.Freq <- host.verified.phone.freq$Freq / sum(host.verified.phone.freq$Freq)

# Creating bar chart
barplot(host.verified.phone.freq$Freq, names.arg = host.verified.phone.freq$host.verified.phone,
        xlab = "Host Verified Phone", ylab = "Frequency", main = "Bar Chart of Host Verified Phone (T/F)")





# 4. Use Inter Quartile Range (IQR) method to identify outliers of all continuous variables, then remove 
# all observations containing outliers. 
# car.deliver.airport.num
iqr <- quantile(turo.az.df$car.deliver.airport.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.deliver.airport.num, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.deliver.airport.num)

# no outliers

# car.deliver.hotel.num
iqr <- quantile(turo.az.df$car.deliver.hotel.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.deliver.hotel.num, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.deliver.hotel.num))

# no outliers

# car.deliver.train.station.num
iqr <- quantile(turo.az.df$car.deliver.train.station.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.deliver.train.station.num, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.deliver.train.station.num))

# no outliers

# car.displayed.turo.review.num
iqr <- quantile(turo.az.df$car.displayed.turo.review.num, 0.75, type = 6, na.rm = TRUE) -
               quantile(turo.az.df$car.displayed.turo.review.num, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.displayed.turo.review.num)

# no outliers

# car.displayed.turo.review.num.past.12m
iqr <- quantile(turo.az.df$car.displayed.turo.review.num.past.12m, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.turo.review.num.past.12m, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.displayed.turo.review.num.past.12m)

# no outliers

# car.displayed.turo.review.num.past.18m
iqr <- quantile(turo.az.df$car.displayed.turo.review.num.past.18m, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.turo.review.num.past.18m, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.displayed.turo.review.num.past.18m)

# no outliers


# car.displayed.turo.review.num.past.6m
iqr <- quantile(turo.az.df$car.displayed.turo.review.num.past.6m, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.turo.review.num.past.6m, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.displayed.turo.review.num.past.6m)

# no outliers

# car.displayed.user.review.num
iqr <- quantile(turo.az.df$car.displayed.user.review.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.user.review.num, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.displayed.user.review.num))

# no outliers

# car.displayed.user.review.num.past.12m
iqr <- quantile(turo.az.df$car.displayed.user.review.num.past.12m, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.user.review.num.past.12m, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.displayed.user.review.num.past.12m))

# Need to get rid of 66 since it's outlier
turo.az.df <- turo.az.df[turo.az.df$car.displayed.user.review.num.past.12m < 66, ]

# car.displayed.user.review.num.past.18m
iqr <- quantile(turo.az.df$car.displayed.user.review.num.past.18m, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.user.review.num.past.18m, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.displayed.user.review.num.past.18m))

# no outliers


# car.displayed.user.review.num.past.6m
iqr <- quantile(turo.az.df$car.displayed.user.review.num.past.6m, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.displayed.user.review.num.past.6m, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.displayed.user.review.num.past.6m))

# no outliers

#car.doors 
iqr <- quantile(turo.az.df$car.doors, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.doors, 0.25, type = 6, na.rm = TRUE)

# no need to remove outliers

unique(turo.az.df$car.doors)


#car.extra.mile.fee
iqr <- quantile(turo.az.df$car.extra.mile.fee, 0.75, type = 6, na.rm = TRUE) - 
        quantile(turo.az.df$car.extra.mile.fee, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.extra.mile.fee)

q1 <- quantile(turo.az.df$car.extra.mile.fee, na.rm = TRUE, 0.25, type = 6)
q1_outliers <- q1 - 1.5 * iqr

q3 <- quantile(turo.az.df$car.extra.mile.fee, na.rm = TRUE, 0.75, type = 6)
q3_outliers <- q3 + 1.5 * iqr

turo.az.df <- turo.az.df[turo.az.df$car.extra.mile.fee > q1_outliers & 
                                 turo.az.df$car.extra.mile.fee < q3_outliers, ]



#car.extra.num 
iqr <- quantile(turo.az.df$car.extra.num, 0.75, type = 6, na.rm = TRUE) - 
        quantile(turo.az.df$car.extra.num, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.extra.num)

# no need to remove the outliers

# car.faq.num
iqr <- quantile(turo.az.df$car.faq.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.faq.num, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.faq.num)

#no outliers

# car.miles.included

# |---List of finite values (remove "inf") ====
turo.az.df <- subset(turo.az.df, turo.az.df$car.miles.included != "Inf")
unique(turo.az.df$car.miles.included)

iqr <- quantile(turo.az.df$car.miles.included, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.miles.included, 0.25, type = 6, na.rm = TRUE)

q1 <- quantile(turo.az.df$car.miles.included, na.rm = TRUE, 0.25, type = 6)
q1_outliers <- q1 - 1.5 * iqr

q3 <- quantile(turo.az.df$car.miles.included, na.rm = TRUE, 0.75, type = 6)
q3_outliers <- q3 + 1.5 * iqr

turo.az.df <- turo.az.df[turo.az.df$car.miles.included > q1_outliers & 
                                 turo.az.df$car.miles.included < q3_outliers, ]

# car.photo.num
iqr <- quantile(turo.az.df$car.photo.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.photo.num, 0.25, type = 6, na.rm = TRUE)

sort(unique(turo.az.df$car.photo.num))

#no outliers

# car.self.pickup.avg.price
iqr <- quantile(turo.az.df$car.self.pickup.avg.price, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$car.self.pickup.avg.price, 0.25, type = 6, na.rm = TRUE)

unique(turo.az.df$car.self.pickup.avg.price)


#no outliers


# car.trip.price
iqr <- quantile(turo.az.df$car.trip.price, 0.75, type = 6, na.rm = TRUE) -
                    quantile(turo.az.df$car.trip.price, 0.25, type = 6, na.rm = TRUE)

q1 <- quantile(turo.az.df$car.trip.price, na.rm = TRUE, 0.25, type = 6)
q1_outliers <- q1 - 1.5 * iqr

q3 <- quantile(turo.az.df$car.trip.price, na.rm = TRUE, 0.75, type = 6)
q3_outliers <- q3 + 1.5 * iqr

turo.az.df <- turo.az.df[turo.az.df$car.trip.price > q1_outliers & 
                              turo.az.df$car.trip.price < q3_outliers, ]

# host.car.num
iqr <- quantile(turo.az.df$host.car.num, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$host.car.num, 0.25, type = 6, na.rm = TRUE)

q1 <- quantile(turo.az.df$host.car.num, na.rm = TRUE, 0.25, type = 6)
q1_outliers <- q1 - 1.5 * iqr

q3 <- quantile(turo.az.df$host.car.num, na.rm = TRUE, 0.75, type = 6)
q3_outliers <- q3 + 1.5 * iqr

turo.az.df <- turo.az.df[turo.az.df$host.car.num > q1_outliers & 
                              turo.az.df$host.car.num < q3_outliers, ]




# host.tenure.in.weeks
iqr <- quantile(turo.az.df$host.tenure.in.weeks, 0.75, type = 6, na.rm = TRUE) -
        quantile(turo.az.df$host.tenure.in.weeks, 0.25, type = 6, na.rm = TRUE)

q1 <- quantile(turo.az.df$host.tenure.in.weeks, na.rm = TRUE, 0.25, type = 6)
q1_outliers <- q1 - 1.5 * iqr

q3 <- quantile(turo.az.df$host.tenure.in.weeks, na.rm = TRUE, 0.75, type = 6)
q3_outliers <- q3 + 1.5 * iqr

turo.az.df <- turo.az.df[turo.az.df$host.tenure.in.weeks > q1_outliers & 
                                 turo.az.df$host.tenure.in.weeks < q3_outliers, ]


# 5. Remove all observations with missing values. (Delete all rows that have missing values)
turo.az.df <- na.omit(turo.az.df)

# Wrote it into a CSV file so we don't have to repeatedly rerun everything
# write.csv(turo.az.df, file = "turo_clean.csv") 

# 6. Build a multiple linear regression model using car.trip.price as dependent 
# variable. Select at least five independent variables. Treat each categorical 
# variable as a single variable although it may be broken into multiple dummy 
# variables. Try different models and choose the best one you can find.

# Reading off the CSV file that we just created
# turo.az.df <- read.csv("turo_clean.csv")

turo.az.lm <- lm(car.trip.price ~ host.all.star + car.displayed.user.review.num +  car.extra.mile.fee +
                         host.tenure.in.weeks + car.extra.num + car.extra.unlimited.mileage + car.model + car.year, data = turo.az.df)

turo.az.lm.summary <- summary(turo.az.lm)
print(turo.az.lm.summary)


vif(turo.az.lm)
turo.az.dv.est <- turo.az.lm$fitted.values
turo.az.res.std <- rstudent(turo.az.lm)
plot(turo.az.res.std ~ turo.az.dv.est, pch = 19, xlab = "Fitted Value of DV",
     ylab = "Standardized Residual", main = "Residual Analysis",
     ylim = c(min(-3, min(turo.az.res.std)), max(3, max(turo.az.res.std))))
abline(h = -1.5, lty = 3)
abline(h = 1.5, lty = 3)