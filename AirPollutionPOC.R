library(lubridate)
pm0<- read.table("RD_501_88101_1999-0.txt", sep = "|", header = FALSE, comment.char = "#", na.strings="")
pm1<- read.table("RD_501_88101_2012-0.txt", sep = "|", header = FALSE, comment.char = "#", na.strings="")
dim(pm0)
dim(pm1)

col1<- readLines("RD_501_88101_1999-0.txt", n=1)
print(col1)
col2<-strsplit(col1, "|", fixed=TRUE)
names(pm0)<- make.names(col2[[1]])

col1<- readLines("RD_501_88101_2012-0.txt",n=1)
col1<- strsplit(col1,"|", fixed = TRUE)
print(col1)
names(pm1)<- make.names(col1[[1]])

pm25_99<- pm0$Sample.Value
pm25_12<- pm1$Sample.Value

summary(pm25_99)
summary(pm25_12)

total_NAs_99<- is.na(pm25_99)
sum(total_NAs_99)
mean(total_NAs_99)

total_NAs_12<- is.na(pm25_12)
sum(total_NAs_12)
mean(total_NAs_12)

negetives_99<- pm25_99<0
sum(negetives_99, na.rm=TRUE)

negetives_12<- pm25_12<0
sum(negetives_12, na.rm = TRUE)

mean(negetives_12,na.rm = TRUE)
str(pm25_99)
str(pm25_12)

boxplot(pm25_99, pm25_12)
boxplot(pm25_99, main="Box plot of 99 data")
boxplot(pm25_12, main = "Box plot of 12 data")
boxplot(log10(pm25_99), log10(pm25_12))
# Since there are -ve pm25 values in 2012 data, lets analysze more

dates<- pm1$Date
dates<- as.Date(as.character(dates),"%Y%m%d")
head(dates)
hist(dates[negetives_12], "month")

head(pm0)

# Lets compare the data from one particular monitor's data between 1999 data and 2012 data instead of looking into whole country data at once
site0<- unique(subset(pm0, State.Code==06, c(County.Code, Site.ID)))
#print(names(site0))
site0<- paste(site0$County.Code, site0$Site.ID, sep=".")


site1<- unique(subset(pm1, State.Code==06, c(County.Code, Site.ID)))
#print(dim(site1))
#class(site1)
site1<- paste(site1$County.Code, site1$Site.ID, sep=".")


both<- intersect(site0, site1)
#print(both)
# Subset data frames for only desired state and site

# Add monitor id (i.e County.Site) column in pm0 and pm1 data frames as follows
pm0$County.Site<- with(pm0, paste(pm0$County.Code, pm0$Site.ID, sep = "."))
pm1$County.Site<- with(pm1, paste(pm1$County.Code, pm1$Site.ID, sep = "."))
#head(pm0)
#head(pm1)

pm0Sub <- subset(pm0, State.Code==06 & County.Site %in% both)
# Lets split the data frame and see which monitor ID has max number of obs.

split(pm0Sub, pm0Sub$County.Site)
# Split has simply splitted data frame, since its a very large data frame.
# Hence, lets apply sapply
sapply(split(pm0Sub, pm0Sub$County.Site), nrow)

pm1Sub<- subset(pm1, State.Code== 06 & County.Site %in% both)
sapply(split(pm1Sub, pm1Sub$County.Site), nrow)
# Since Max is for 19.8, we will procede with this

pm0Sub<- subset(pm0, State.Code==06 & County.Code==19 & Site.ID==8)
pm1Sub<- subset(pm1, State.Code==06 & County.Code==19 & Site.ID==8)
head(pm0Sub)
dim(pm0Sub)
dim(pm1Sub)

# We will plot pm25 value with date

date0<-as.Date( as.character(pm0Sub$Date), "%Y%m%d")
tail(date0)

pm_value0<- pm0Sub$Sample.Value

head(pm_value0)

plot(date0, pm_value0, col = month(date0), pch = 19, main = "Plot of Dust in CA State in relation to Time - 1999")
abline(h=median(pm_value0, na.rm=T), col="black",lwd=2)
legend("top", legend = unique(format(date0,"%b")), col = unique(month(date0)) ,pch = 19)

date1<- as.Date(as.character(pm1Sub$Date),"%Y%m%d")
head(date1)
tail(date1)
pm_value1<- pm1Sub$Sample.Value

plot(date1, pm_value1, col = month(date1), pch = 19, main = "Plot of Dest in CA state in relation to Time - 2012")
abline(h = median(pm_value1, na.rm = T), col = "red", lwd = 2)
legend("top", legend = unique(format(date1,"%b")), col = unique(month(date1)), pch = 19)

par(mfrow = c(1,2), mar = c(4,4,2,1))

plot(date0, pm_value0, col = month(date0), pch = 19, main = "Plot of Dust in CA State in relation to Time - 1999")
abline(h=median(pm_value0, na.rm=T), col="black",lwd=2)
legend("top", legend = unique(format(date0,"%b")), col = unique(month(date0)) ,pch = 19)

plot(date1, pm_value1, col = month(date1), pch = 19, main = "Plot of Dest in CA state in relation to Time - 2012")
abline(h = median(pm_value1, na.rm = T), col = "red", lwd = 2)
legend("top", legend = unique(format(date1,"%b")), col = unique(month(date1)), pch = 19)

# Figure can be misleading because of different ranges, so, adjusts y - axis limits
# to do that find the range of data set
range(pm_value0,na.rm=T)
range(pm_value1, na.rm = T)

rng<- range(pm_value0, pm_value1,na.rm= T)
# now remake plot to fix the range
par(mfrow = c(1,2))
plot(date0, pm_value0, col = month(date0), pch = 19, main = "Plot of Dust in CA State in relation to Time - 1999", ylim = rng)
abline(h=median(pm_value0, na.rm=T), col="black",lwd=2)
legend("top", legend = unique(format(date0,"%b")), col = unique(month(date0)) ,pch = 19)

plot(date1, pm_value1, col = month(date1), pch = 19, main = "Plot of Dest in CA state in relation to Time - 2012", ylim = rng)
abline(h = median(pm_value1, na.rm = T), col = "red", lwd = 2)
legend("top", legend = unique(format(date1,"%b")), col = unique(month(date1)), pch = 19)

### After this lets plot the average change of pollution by all state  and join the points

#for this function its ideal to apply tapply function.
mean_0<- with(pm0, tapply(pm25_99, State.Code, mean, na.rm = T))
mean_1<- with(pm1, tapply(pm25_12, State.Code, mean, na.rm = T))
str(mean_1)

# create a data frame to plot data plots
d0<- data.frame(states = names(mean_0), mean= mean_0)
d1<- data.frame(states = names(mean_1), mean= mean_1)

# now merge these data frame by name
mrg<- merge(d0, d1, by.x = "state", by.y="state")



