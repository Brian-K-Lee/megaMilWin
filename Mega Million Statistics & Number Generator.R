####################################################
#######Mega Million Lottery Numbers Generator#######
####################################################

####This Code Will Choose 6 Winning Numbers From The Previous Drawings With Different Drawing Methods Below####
## 1. Most Frequent Winning Numbers For Five Different Numbers From 1 to 70 (The White Balls) And One Number From 1 to 25 (The Gold Mega Ball)
## 2. Random 5 White Balls From 25% Most Frequent Winning Numbers and 1 Mega Ball From 25% Most Frequent Winning Numbers
## 3. Random 3 White Balls From 25% Most Frequent Winning Numbers and remaining 2 White Balls From 25% Least Frequent Winning Numbers and 1 Mega Ball From 25% Most and 25% Least Frequent Winning Numbers
## 4. Completely Random Numbers For 5 White Balls and 1 Mega Ball


###Load libraries
#install.packages("data.table")
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)

###Import Previous Drawings Copied Directly from ""
mega<-fread("C:/Users/KT/Documents/Projects/Mega Million/WinnigNumbers_103117_042922.csv")

###Data Has A Date followed by 6 Winning Numbers And Megaplier In One Column. 
###The Winning Numbers For Each Date Need To Be In One Column Instead. Below Is The Process.

###Rename Existing Column Name
names(mega)[1] <- "Data"

###Remove Megapliers In the Data
mega <- mega[!grepl("X",mega$Data),]

###Create Empty Data Frame For Clean Data
mega_clean <- data.frame("Date" = NA,
                           "Ball1" = NA,
                           "Ball2" = NA,
                           "Ball3" = NA,
                           "Ball4" = NA,
                           "Ball5" = NA,
                           "MegaBall" = NA)
totalrow <- nrow(mega)/7

###Convert Data Format From Long to Wide
for(j in 1:totalrow){
    mega_clean[j,1] <- mega[1+(j-1)*7]
    mega_clean[j,2] <- mega[2+(j-1)*7]
    mega_clean[j,3] <- mega[3+(j-1)*7]
    mega_clean[j,4] <- mega[4+(j-1)*7]
    mega_clean[j,5] <- mega[5+(j-1)*7]
    mega_clean[j,6] <- mega[6+(j-1)*7]
    mega_clean[j,7] <- mega[7+(j-1)*7]
}


###Combine All Numbers From 5 White Balls
allWB <- rbind(mega_clean$Ball1, mega_clean$Ball2, mega_clean$Ball3,mega_clean$Ball4,mega_clean$Ball5)

###Exclude Mega Ball Winning Numbers Above 25 To Apply New Rule (Current Mega Ball Winning Numbers: 1 - 25)
allWB <- as.numeric(allWB)
#allWB <- allWB[allWB <= 70]

###Create A Frequency Table By Winning Numbers For 5 White Balls
FreqTableWB <- data.frame(table(allWB))
names(FreqTableWB)[1] <- "WinningNumber"

#####Statistics 1 - 5 Most Common Winning Numbers (For White Balls) #####
MostFreq5 <- FreqTableWB[order(-FreqTableWB$Freq)[1:5],]
MostFreq5$WinningNumber <- as.character(MostFreq5$WinningNumber)
# barplot(sort(MostFreq5$Freq),MostFreq5$WinningNumber, horiz = T, space = 1, width = 5,xpd=T)

ggplot(MostFreq5, aes(reorder(WinningNumber,Freq),Freq)) + geom_bar(stat = "identity", width = 0.2, fill = "darkgreen") + coord_flip() + geom_text(aes(y = Freq, label = Freq),position = position_stack(vjust = 1.05)) +labs(x="Winning Number")

mega_clean1<-mega_clean
mega_clean1$Date<-as.Date(mega_clean1$Date,"%m/%d/%Y")
mega_clean1$Date<-as.Date(mega_clean1$Date,"%y-%m-%d")


MostFreq5$WinningNumber<-as.integer(MostFreq5$WinningNumber)
winningnum<-MostFreq5$WinningNumber[1]
winningnum<-MostFreq5$WinningNumber
winningnum<-as.integer(winningnum)
winningnum<-winningnum[c(-2)]
winningnum<-MostFreq5[,1]


daysago<-function(winningnum){
  daysago_return<-Sys.Date()-max(max(mega_clean1$Date[mega_clean1$Ball1==winningnum],na.rm=TRUE),
max(mega_clean1$Date[mega_clean1$Ball2==winningnum],na.rm=TRUE),
max(mega_clean1$Date[mega_clean1$Ball3==winningnum],na.rm=TRUE),
max(mega_clean1$Date[mega_clean1$Ball4==winningnum],na.rm=TRUE),
max(mega_clean1$Date[mega_clean1$Ball5==winningnum]),na.rm=TRUE)
return(daysago_return)}
daysago(4)
daysago_df<-data.frame(WinningNumber=NA,DaysAgo=NA,Date=NA)
for(i in 1:5){
  daysago_df$WinningNumber[i] <- MostFreq5$WinningNumber[i]
daysago_df$DaysAgo[i]<-as.integer(daysago(MostFreq5$WinningNumber[i]))
daysago_df$Date[i]<-format(Sys.Date()-daysago(MostFreq5$WinningNumber[i]),"%b %d, %Y")}

daysago_df1<-data.frame()
for(i in 1:5){
  daysago_df$WinningNumber[i] <- winningnum[i]
  daysago_df$DaysAgo[i]<-as.integer(daysago(winningnum[i]))
  daysago_df$Date[i]<-format(Sys.Date()-daysago(winningnum[i]),"%b %d, %Y")
  daysago_df1<-rbind(daysago_df,daysago_df1)}


as.integer(daysago(MostFreq5$WinningNumber[1]))
Sys.Date()-daysago(MostFreq5$WinningNumber[1])


# 
#   daysago<-function(winningnum){
#     Sys.Date()-max(max(mega_clean1$Date[mega_clean1$Ball1==MostFreq5$WinningNumber[winningnum]]),
#                    max(mega_clean1$Date[mega_clean1$Ball2==MostFreq5$WinningNumber[1]]),
#                    max(mega_clean1$Date[mega_clean1$Ball3==MostFreq5$WinningNumber[1]]),
#                    max(mega_clean1$Date[mega_clean1$Ball4==MostFreq5$WinningNumber[1]]),
#                    max(mega_clean1$Date[mega_clean1$Ball5==MostFreq5$WinningNumber[1]]))



str(mega_clean1)

MostFreq5$Days<-Merge

#####Method 1. Most Frequent Winning Numbers#####
###Get The Most Frequent Winning Numbers for 5 White Balls And Order By Ascending Order
MostFreqWB <- FreqTableWB$WinningNumber[order(FreqTableWB$Freq, decreasing = 1)][1:5]
MostFreqWB <- MostFreqWB[order(MostFreqWB)]
MostFreqWB <- data.frame(MostFreqWB, stringsAsFactors = F)
names(MostFreqWB) <- "MostFrequent"

###Exclude Mega Ball Winning Numbers Above 25 To Apply New Rule (Current Mega Ball Winning Numbers: 1 - 25)
allMB <- rbind(mega$MegaBall)
allMB <- as.numeric(allMB)
allMB <- allMB[allMB <= 25]

###Create A Frequency Table By Winning Numbers For Mega Ball
FreqTableMB <- data.frame(table(allMB))
names(FreqTableMB)[1] <- "WinningNumber"

###Get The Most Frequent Winning Number for Mega Ball
MostFreqMB <- FreqTableMB$WinningNumber[which.max(FreqTableMB$Freq)]
MostFreqMB <- data.frame(MostFreqMB, stringsAsFactors = F)
names(MostFreqMB) <- "MostFrequent"

###Combine Most Frequent Winning 5 White Balls And 1 Mega Ball
MostFreqWin <- rbind(MostFreqWB, MostFreqMB)
row.names(MostFreqWin) <- c("Ball1","Ball2","Ball3","Ball4","Ball5","MegaBall")


#####Method 2. Random Numbers From 25% Most Frequent Winning Numbers#####
###Calculate 25% of 70 (All White Ball Winning Numbers), 25% of 25 (All Mega Ball Winning Numbers) And Round To Integer
per25WB <- round(70 * 0.25)
per25MB <- round(25 * 0.25)

###Generate 25% Most Winning Numbers For White Balls, Choose 5 Random Numbers And Order By Ascending Order
Most25WB <- FreqTableWB$WinningNumber[order(FreqTableWB$Freq, decreasing = 1)][1:per25WB]
Most25WB <- data.frame(Most25WB, stringsAsFactors = F)
Most25WB <- sample_n(Most25WB,5)
names(Most25WB) <- "per25"
Most25WB$per25 <- as.integer(Most25WB$per25)
Most25WB <- data.frame(Most25WB[order(Most25WB$per25),])
names(Most25WB) <- "MostFrequent25Percent"

###Generate 25% Most Winning Numbers For Mega Balls And Choose 1 Random Number
Most25MB <- FreqTableMB$WinningNumber[order(FreqTableMB$Freq, decreasing = 1)][1:per25MB]
Most25MB <- data.frame(Most25MB, stringsAsFactors = F)
Most25MB <- sample_n(Most25MB,1)
names(Most25MB) <- "MostFrequent25Percent"

###Combine The Above Random Numbers For 5 White Balls And 1 Mega Ball From 25% Most Frequent Winning Numbers
Most25FreqWin <- rbind(Most25WB, Most25MB)
row.names(Most25FreqWin) <- c("Ball1","Ball2","Ball3","Ball4","Ball5","MegaBall")


#####Method 3. Random Numbers From 25% Most And 25% Least Frequent Winning Numbers#####
###Calculate 25% of 70 (All White Ball Winning Numbers), 25% of 25 (All Mega Ball Winning Numbers) And Round To Integer
per25WB <- round(70 * 0.25)
per25MB <- round(25 * 0.25)

###Generate 25% Most And Least Frequent Winning Numbers For White Balls, Remove Duplicates, Choose 5 Random Numbers And Order By Ascending Order
#Generate 25% Most and Last Frequent Winning Numbers
Most25WB <- FreqTableWB$WinningNumber[order(FreqTableWB$Freq, decreasing = 1)][1:per25WB]
Most25WB <- data.frame(Most25WB, stringsAsFactors = F)
names(Most25WB) <- "per25"
Least25WB <- FreqTableWB$WinningNumber[order(FreqTableWB$Freq, decreasing = 1)][(70-per25WB):70]
Least25WB <- data.frame(Least25WB, stringsAsFactors = F)
names(Least25WB) <- "per25"
#Remove Duplicates
MostLeast25WB <- rbind(Most25WB, Least25WB)
MostLeast25WB <- MostLeast25WB[!duplicated(MostLeast25WB),]
MostLeast25WB <- data.frame(MostLeast25WB, stringsAsFactors = F)
#Choose 5 Random Numbers And Order By Ascending Order
MostLeast25WB <- sample_n(MostLeast25WB,5)
names(MostLeast25WB) <- "per25"
MostLeast25WB$per25 <- as.integer(MostLeast25WB$per25)
MostLeast25WB <- data.frame(MostLeast25WB[order(MostLeast25WB$per25),])
names(MostLeast25WB) <- "MostLeastFrequent25Percent"

###Generate 25% Most And Least Winning Numbers For Mega Balls, Remove Duplicates And Choose 1 Random Number
#Generate 25% Most and Last Frequent Winning Numbers
Most25MB <- FreqTableMB$WinningNumber[order(FreqTableMB$Freq, decreasing = 1)][1:per25MB]
Most25MB <- data.frame(Most25MB, stringsAsFactors = F)
names(Most25MB) <- "per25"
Least25MB <- FreqTableMB$WinningNumber[order(FreqTableMB$Freq, decreasing = 1)][(25-per25MB):25]
Least25MB <- data.frame(Least25MB, stringsAsFactors = F)
names(Least25MB) <- "per25"
#Remove Duplicates
MostLeast25MB <- rbind(Most25MB, Least25MB)
MostLeast25MB <- MostLeast25MB[!duplicated(MostLeast25MB),]
MostLeast25MB <- data.frame(MostLeast25MB, stringsAsFactors = F)
#Choose 1 Random Number
MostLeast25MB <- sample_n(MostLeast25MB,1)
names(MostLeast25MB) <- "MostLeastFrequent25Percent"

###Combine The Above Random Numbers For 5 White Balls And 1 Mega Ball From 25% Most Frequent and Least Winning Numbers
MostLeast25FreqWin <- rbind(MostLeast25WB, MostLeast25MB)
row.names(MostLeast25FreqWin) <- c("Ball1","Ball2","Ball3","Ball4","Ball5","MegaBall")


#####Method 4. Completely Random Numbers For 5 White Balls and 1 Mega Ball#####
###Pick 5 Random White Balls
RandomWB <- sample_n(FreqTableWB,5)
RandomWB <- RandomWB[,1]
RandomWB <- data.frame(RandomWB)
names(RandomWB) <- "Random"
RandomWB$Random <- as.integer(RandomWB$Random)
RandomWB <- data.frame(RandomWB[order(RandomWB$Random),])
names(RandomWB) <- "Random"

###Pick 1 Random Mega Ball
RandomMB <- sample_n(FreqTableMB,1)
RandomMB <- RandomMB[,1]
RandomMB <- data.frame(RandomMB, stringsAsFactors = F)
names(RandomMB) <- "Random"

###Combine The Above Random Numbers For 5 White Balls And 1 Mega Ball
RandomWin <- rbind(RandomWB, RandomMB)
row.names(RandomWin) <- c("Ball1","Ball2","Ball3","Ball4","Ball5","MegaBall")


####Combine Winning Numbers From Method 1 to 4 And Export The Output
MegaWin <- cbind(MostFreqWin, Most25FreqWin, MostLeast25FreqWin, RandomWin) 
fwrite(MegaWin, "C:/Users/KT/Documents/Projects/Mega Million/WinningNumbersPlease.csv", row.names = T)




# Select the top 10 most frequent numbers as the winning numbers
winning_numbers_1 <- sort(table(numbers), decreasing = TRUE)[1:10]



# Create a new frequency table using only the 25% most frequent numbers
frequent_numbers <- sort(table(numbers), decreasing = TRUE)[1:round(length(numbers)*0.25)]

# Randomly select 5 numbers from the frequent numbers table as the white ball numbers
white_ball_numbers <- sample(frequent_numbers, 5)

# Randomly select 1 number from the frequent numbers table as the Mega Ball number
mega_ball_number <- sample(frequent_numbers, 1)




# Create frequency table of all numbers
freq_table <- table(lottery_data$numbers)

# Sort frequency table in descending order
freq_table <- sort(freq_table, decreasing = TRUE)

# Select 25% most frequent numbers
most_freq_nums <- names(freq_table)[1:floor(length(freq_table) * 0.25)]

# Select 25% least frequent numbers
least_freq_nums <- names(freq_table)[(length(freq_table) - floor(length(freq_table) * 0.25) + 1):length(freq_table)]

# Randomly select 3 numbers from most frequent table
winning_nums1 <- sample(most_freq_nums, 3)

# Randomly select 2 numbers from least frequent table
winning_nums2 <- sample(least_freq_nums, 2)

# Randomly select 1 number from combination of both tables
winning_nums3 <- sample(c(most_freq_nums, least_freq_nums), 1)

# Combine all winning numbers into one vector
winning_nums <- c(winning_nums1, winning_nums2, winning_nums3)

# Print the winning numbers
print(winning_nums)
