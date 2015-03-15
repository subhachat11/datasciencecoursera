getwd()
setwd("E:/R-workdir")
mvt = read.csv("E:/R-datasets/mvtWeek1.csv", sep=",",dec=".")
is.data.frame(mvt) # load AnalyticsEdge MV theft dataset mvtWeek1.csv
ncol(mvt)
nrow(mvt)
max(1:21)
min(-23:-2)
x <- c(1:4,0:5,11)
which.max(x)
which.min(x)
mvt[c(1,1)]
is.vector(mvt) # vector elements are of same type
is.matrix(mvt) # matrix has same number of row and columns
rownames(mvt) # returns all rows
colnames (mvt) # returns all columns
is.list(mvt) # collection of objects of differnt types
mvt[1:5,"ID"] # first 5 observations of variable/column ID
mvt$Beat[1:10] # first 5 observations of variable/column Beat
max(mvt$ID)
min(mvt$Beat)
length(mvt[which(mvt$Arrest==TRUE),]) # result 11
summary(mvt[which(mvt$Arrest==TRUE),]) # mean,median,max,percentiles and value count
# nrow(mvt[which(mvt$Arrest==TRUE),]
length(mvt) # number of columns
str(mvt) # compact summary of the object
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)
summary(mvt)
# date conversion functions
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(w=mvt$Month)
minTheft=min(table(w=mvt$Month))
table(mvt$Weekday)
max(table(mvt$Weekday))
summary(mvt[which(mvt$Arrest==TRUE),])
# table(mvt, which(mvt$Arrest==TRUE)) # Error in sort.list(y)
mvtArrest = mvt[which(mvt$Arrest==TRUE),]
summary(mvtArrest$Month)
# sorted table of Month where Arrest was made
sort(table(mvtArrest$Month)) 
hist(mvt$Date, breaks=100)
boxplot(mvt$Date, subset=mvt$Arrest==TRUE)
# Find Top Locations where Theft happened
sort(table(mvt$LocationDescription, exclude = "OTHER"), decreasing=TRUE)
WHO = read.csv("E:/R-datasets/WHO.csv")
summary(WHO)
nrow(WHO)
ncol(WHO)
mean(WHO$Over60)
# lists all Over60 values of the population
table(WHO$Over60) 
# lists mean of regions Over60
tapply(WHO$Over60, WHO$Region, mean) 
# lists smallest percentage of Over60 population of a Country across regions
which.min(WHO$Over60)
WHO$Country[183]
CountryOver60 = tapply(WHO$Over60, WHO$Country, min)
sort(CountryOver60)
# lists highest literacy rate of a Country across regions
which.max(WHO$LiteracyRate)
WHO$Country[44]
CountryLiteracyRate = tapply(WHO$LiteracyRate, WHO$Country, max )
sort(CountryLiteracyRate)
# lowest average child mortality rate across all countries in that region
MinChildMort = tapply(WHO$ChildMortality, WHO$Region, mean) 
# For what proportion of motor vehicle thefts in 2001 was an arrest made?
summary(mvt)
MVT2001 = subset(mvt$Arrest, mvt$Year >= 2001 & mvt$Year < 2002)
rm(MVTArrest2001)
MVTArrest2001 = subset(mvt$Arrest, mvt$Arrest == TRUE & mvt$Year >= 2001 & mvt$Year < 2002)
ArrestProp2001 = length(MVTArrest2001)/length(MVT2001)
ArrestProp2001
table(mvt$Arrest,mvt$Year)
# Create a subset of your data, only taking observations for which the theft happened 
# in "Gas Station", "Street", "Parking Lot/Garage (Non-Residential)", "Alley", "Driveway (Residential)"
MVTLocationsExOther = sort(table(mvt$LocationDescription, exclude = "OTHER"), decreasing=TRUE)
nrow(MVTLocationsExOther)
print(MVTLocationsExOther)
LenMvtLoc = length(MVTLocationsExOther)
print(LenMvtLoc)

Loc1 = subset(mvt, mvt$LocationDescription == "STREET")
Loc2 = subset(mvt, mvt$LocationDescription == "GAS STATION")
Loc3 = subset(mvt, mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
Loc4 = subset(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")
Loc5 = subset(mvt, mvt$LocationDescription == "ALLEY")
Top5 = c(nrow(Loc1), nrow(Loc2), nrow(Loc3), nrow(Loc4), nrow(Loc5) )
record = 0
for(index in 1:length(Top5) ){  
  # print("enter loop")
  cat("row number", index, " : ", Top5[index], "\n")
  record = record + Top5[index]
  cat("total record : ", record, " \n")
  # if(index == 6) break
}
# Top5Set = c(Loc1, Loc2, Loc3, Loc4, Loc5 )
# Top5Set$LocationDescription = factor(Top5Set$LocationDescription)

table(Top5Set$LocationDescription)
summary(Loc1)
which.max(Top5Set$Arrest == TRUE)
# tapply(nrow(Top5Set$Arrest), Top5Set$LocationDescription, min)
Top5Set = rbind(Loc1,Loc2,Loc3,Loc4,Loc5)
str(Top5Set)
summary(Top5Set)
# Top 5 Location Arrest Rate
table(Top5Set$LocationDescription, Top5Set$Arrest)

Top5Arrest = subset(Top5Set, Top5Set$Arrest == TRUE )
summary(Top5Arrest)
table(Top5Arrest$LocationDescription,Top5Arrest$Arrest)
# On which day of the week do the most motor vehicle thefts at gas stations happen?
Loc2 = subset(mvt, mvt$LocationDescription == "GAS STATION")
summary(Loc2)
DateConvertLoc2 = as.Date(strptime(Loc2$Date, "%m/%d/%y %H:%M"))
Loc2$Date = DateConvertLoc2
Loc2$Weekday = weekdays(DateConvertLoc2)
table(Loc2$Weekday)

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
Loc4 = subset(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")
DateConvertLoc4 = as.Date(strptime(Loc4$Date, "%m/%d/%y %H:%M"))
Loc4$Date = DateConvertLoc4
Loc4$Weekday = weekdays(DateConvertLoc4)
table(Loc4$Weekday)

