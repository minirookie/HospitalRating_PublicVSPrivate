#################################################################################
#                                                                               #
#                       Work with hospital rating data.                         #
#                                                                               #   
#################################################################################


#obtain and set work directory and image saving directory

setwd("/Users/syu/Documents/Documents_syu/PMBA_courses/MIS7190ProgrammingForBusiness/GroupDProject/HospitalGeneral")
image.directory <- c("/Users/syu/Documents/Documents_syu/PMBA_courses/MIS7190ProgrammingForBusiness/GroupDProject/ImagesFromRscript/")
#read original general hosptial rating file
orihosp.info <- read.csv("HospInfo.csv", header = T, sep = ",")

#check the data types and levels before create data frame
dim(orihosp.info)
names(orihosp.info)
head(orihosp.info$Hospital.Ownership, 2)

str(orihosp.info$Hospital.overall.rating)
unique(orihosp.info$Hospital.overall.rating)

str(orihosp.info$Hospital.Ownership)
unique(orihosp.info$Hospital.Ownership)

str(orihosp.info$State)
unique(orihosp.info$State)
table(orihosp.info$State)

str(orihosp.info$ZIP.Code)
str(orihosp.info$Provider.ID)
orihosp.info[is.na(orihosp.info$Hospital.Ownership)]

#combine ID, zipcode, ownership and rating, simplify variable names

keyhosp.infoV1 <- data.frame(orihosp.info$Provider.ID, orihosp.info$ZIP.Code, orihosp.info$Hospital.Ownership, 
                             orihosp.info$State, orihosp.info$Hospital.overall.rating)

names(keyhosp.infoV1) <- c("Provider.ID", "Zip.Code", "ownership", "State", "Overall.Rating")
head(keyhosp.infoV1)
table(keyhosp.infoV1$ownership)

#Change "Not Available" overating to NA value
levels(keyhosp.infoV1$Overall.Rating)
keyhosp.infoV1$Overall.Rating = as.character(keyhosp.infoV1$Overall.Rating)
keyhosp.infoV1$Overall.Rating = as.numeric(keyhosp.infoV1$Overall.Rating)
keyhosp.infoV1$Overall.Rating = as.character(keyhosp.infoV1$Overall.Rating)
list(keyhosp.infoV1$Overall.Rating)

#plot different hospital by owners and calculate average rating for each
keyhosp.owners <- keyhosp.infoV1
levels(keyhosp.owners$ownership) <- c("Federal", "District", "Local", "State", "Physician", 
                                         "Proprietary", "Tribal", "Church", "Other", "Private")
table(sort(keyhosp.owners$ownership))

png(paste0(image.directory, "bar_HospCount_Owners.png", sep = ""))

bar.xvalue01 <- barplot(sort(table(keyhosp.owners$ownership)),ylab = "Hosptial Count", 
                        col = rainbow(10), cex.main = 1.25, ylim = c(0, 2500), adj = 0.5, 
        xlim = c(0, 2.7), width = 0.2, cex.lab = 1, cex.main = 0.95, axisnames = F)

bar.heightvalue01 <- data.frame(sort(table(keyhosp.owners$ownership)))$Freq
legend.txt <- data.frame(sort(table(keyhosp.owners$ownership)))$Var1

legend(0.2, 2500, legend.txt, pch = 22, col = rainbow(10), pt.bg = rainbow(10))
title(xlab = "Different Owners", line = 0.3, adj =0.5)
title(main = " Hospital count of different owners", line = 1, adj = 0.3)
text(bar.xvalue01, bar.heightvalue01, labels = bar.heightvalue01, pos = 3, cex = 1)

dev.off()

overating.inumbers <- as.character(keyhosp.owners$Overall.Rating)
overating.inumbers <- as.numeric(keyhosp.owners$Overall.Rating)
rating.to.owners <-data.frame(keyhosp.owners$ownership, overating.inumbers)

names(rating.to.owners) <- c("owners",  "Overall.Rating")
head(rating.to.owners)
str(rating.to.owners$owners)
Avg.rating.byowner <- aggregate(rating.to.owners$Overall.Rating, by = list(rating.to.owners$owners), 
                                mean, na.rm = T)
Avg.rating.byowner$x <- round(Avg.rating.byowner$x, 2)
colnames(Avg.rating.byowner) <- c("Owners", "Average overall rating") 
print(Avg.rating.byowner)


# reassign the levels for ownership and overall rating
keyhosp.info.2ownership <- keyhosp.infoV1
levels(keyhosp.info.2ownership$Overall.Rating) <- c("1", "2", "3", "4", "5")

levels(keyhosp.info.2ownership$ownership) <- list("Public" = c("Government - Federal", 
                                                      "Government - Hospital District or Authority",
                                                      "Government - Local", "Government - State", "Tribal"), 
                                         "Private" = c("Physician", "Proprietary", "Voluntary non-profit - Church",
                                                       "Voluntary non-profit - Other", "Voluntary non-profit - Private"))
png(paste(image.directory, "bar_HospCount_2Ownership.png", sep = ""))
bar.xvalue02 <- barplot(sort(table(keyhosp.info.2ownership$ownership)), ylab = "Hospital count", 
        col = c("cyan", "red"), ylim = c(0,4100), xlim = c(0, 7))
bar.heightvalue02 <- data.frame(sort(table(keyhosp.info.2ownership$ownership)))$Freq
title(main = "Hospital count of two ownerships", cex.main = 1.2, adj = 0, line = 1.1)
text(bar.xvalue02, bar.heightvalue02, pos = 3, labels = bar.heightvalue02)
dev.off()

##To comparing rating distribution, divide the data frame by ownerships
#Plot public hospital overall rating distribution
public.infoV1 <- subset(keyhosp.info.2ownership, keyhosp.info.2ownership$ownership == "Public")

png(paste(image.directory, "bar_PublicHosp_OveratingDistri.png", sep = ""))
bar.xvalue03 <- barplot(table(public.infoV1$Overall.Rating), ylab = "Hospital count", col = rainbow(5),
        xlim = c(0, 8), ylim = c(0, 380), width = 0.75)
title(main = "Overall rating distribution of public hospitals", 
      cex.main = 1, line = 0.25, adj = 0)
title(xlab = "Overall rating", cex.lab = 0.8, line = 2, adj = 0.25)
bar.heightvalue03 <- data.frame(table(public.infoV1$Overall.Rating))[, "Freq"]
text(bar.xvalue03, bar.heightvalue03, labels = bar.heightvalue03, pos = 3, cex = 0.8)
dev.off()

public.rating.lbls <- c("1","2","3","4","5")
public.rating.pct <- round(prop.table(table(public.infoV1$Overall.Rating))*100, 2)
public.rating.pct <- paste(public.rating.pct, "%")
public.rating.lbls <- paste(public.rating.lbls," ","(", public.rating.pct, ")", sep = "")

png(paste(image.directory, "pie_PublicHosp_OveratingDistri.png", sep = ""))
pie(table(public.infoV1$Overall.Rating), col = rainbow(5), labels = public.rating.lbls,
        main = "Overall rating distribution of public hospitals", cex.main = 1, line = 0)
dev.off()

public.overall.rating <- as.numeric(public.infoV1$Overall.Rating)
mean(public.overall.rating, na.rm = T)
sd(public.overall.rating, na.rm = T)

#Plot public hospital overall rating distribution
private.infoV1 <- subset(keyhosp.info.2ownership, keyhosp.info.2ownership$ownership == "Private")

png(paste(image.directory, "bar_PrivateHosp_OveratingDistri.png", sep = ""))
bar.xvalue04 <- barplot(table(private.infoV1$Overall.Rating), ylab = "Hospital count", col = rainbow(5),
                        xlim = c(0, 8), ylim = c(0, 1600), width = 0.75)
title(main = "Overall rating distribution of private hospitals", 
      cex.main = 1, line = 0.25, adj = 0)
title(xlab = "Overall rating", cex.lab = 0.8, line = 2, adj = 0.25)
bar.heightvalue04 <- data.frame(table(private.infoV1$Overall.Rating))[, "Freq"]
text(bar.xvalue04, bar.heightvalue04, labels = bar.heightvalue04, pos = 3, cex = 0.8)
dev.off()

private.rating.lbls <- c("1","2","3","4","5")
private.rating.pct <- round(prop.table(table(private.infoV1$Overall.Rating))*100, 2)
private.rating.pct <- paste(private.rating.pct, "%")
private.rating.lbls <- paste(private.rating.lbls," ", "(",private.rating.pct,")", sep = "")

png(paste(image.directory, "pie_PrivateHosp_OveratingDistri.png", sep = ""))
pie(table(private.infoV1$Overall.Rating), col = rainbow(5), labels = private.rating.lbls,
        main = "Overall rating distribution of private hospitals",cex.main =1, line = 0)
dev.off()

private.overall.rating <- as.numeric(private.infoV1$Overall.Rating)
mean(private.overall.rating, na.rm = T)
sd(private.overall.rating, na.rm = T)

public.infoV1[is.na(public.infoV1$Overall.Rating),]
private.infoV1[is.na(private.infoV1$Overall.Rating),]

#Group barchart to show overall rating distribution of both public and private hospitals
keyhosp.infoV1.rating.owners <- data.frame(keyhosp.info.2ownership$Overall.Rating, keyhosp.info.2ownership$ownership)
names(keyhosp.infoV1.rating.owners) <- c("Overall rating", "ownership")
table(keyhosp.infoV1.rating.owners)

png(paste(image.directory, "grpBar_OveratingDistri.png", sep = ""))
bar.xvalue05 <- barplot(t(table(keyhosp.infoV1.rating.owners)), col = c("cyan", "red"), 
        ylab = "Hospital Count", beside = T, ylim = c(0,1600), xlim = c(0, 7), 
        width = 0.4, legend.text = c("Public", "Private"), cex.axis = 1)
bar.heightvalue05 <- t(table(keyhosp.infoV1.rating.owners))
title(main = "Overall rating distribution of two ownerships", 
      cex.main = 1, line = 0.5, adj = 0.3)
title(xlab = "Ownership", line = 2, adj = 0.4)
text(bar.xvalue05, bar.heightvalue05, labels = bar.heightvalue05, pos = 3, cex.lab = 0.8)
dev.off()

#################################################################################
#                                                                               #
#         work with Timely and effective care- hospital files                   #
#                                                                               #   
#################################################################################

#set work directory
setwd("/Users/syu/Documents/Documents_syu/PMBA_courses/MIS7190ProgrammingForBusiness/GroupDProject/Hospital_Revised_Flatfiles")

#read Time and Effective Care - Hospital
time.effective.hospital <- read.csv("TimelyandEffectiveCare-Hospital.csv", header = T, sep = ",")

#check data files, data types and levels before merging data
dim(time.effective.hospital)
names(time.effective.hospital)
str(time.effective.hospital$Measure.ID)
levels(time.effective.hospital$Measure.ID)
unique(time.effective.hospital$Measure.ID)
str(time.effective.hospital$Measure.Name)
levels(time.effective.hospital$Measure.Name)
unique(time.effective.hospital$Measure.Name)
str(time.effective.hospital$Score)
levels(time.effective.hospital$Score)

#look at one measurement "Left before being seen" ("OP_22")
left.noseen.sub <- subset(time.effective.hospital, Measure.ID == "OP_22",
                      select = c(Provider.ID, ZIP.Code, Measure.ID, Measure.Name,
                                 Score, City, State))
left.noseen <- data.frame(left.noseen.sub$Provider.ID, left.noseen.sub$ZIP.Code, 
                          left.noseen.sub$Score, left.noseen.sub$City, left.noseen.sub$State)
names(left.noseen) <- c("Provider.ID", "Zip.Code", "Score", "City", "State")
dim(left.noseen)
head(left.noseen)
str(left.noseen)

levels(left.noseen$Score)
table(sort(left.noseen$Score)) ### the levels are not affect by subsetting


## merge with owners, ownership data
str(keyhosp.info.2ownership)  # ownership and rating data frame
str(left.noseen)

head(keyhosp.info.2ownership)
head(left.noseen)
keyhosp.infoV1.left.noseen <- merge(keyhosp.info.2ownership, left.noseen,  
                                    by.x = c("Provider.ID","Zip.Code"), 
                                    by.y = c("Provider.ID","Zip.Code"))
dim(keyhosp.infoV1.left.noseen)
head(keyhosp.infoV1.left.noseen)
str(keyhosp.infoV1.left.noseen)


#subset public and private for plotting
public.leftnoseen <- subset(keyhosp.infoV1.left.noseen, ownership == "Public", 
                            select = c("Provider.ID", "Zip.Code", "ownership", "Overall.Rating",
                                       "Score"))

##get rid of unassociated levels for plot and sorting
#1 public score data type change, followed by quickly check out the new table. 
public.leftnoseen$Score <- as.character(public.leftnoseen$Score)
public.leftnoseen$Score <- as.integer(public.leftnoseen$Score)
public.leftnoseen <- public.leftnoseen[order(public.leftnoseen$Score, decreasing = F, 
                                             na.last = T), ]

unique(public.leftnoseen$Score)
head(public.leftnoseen)
table(public.leftnoseen$Score, useNA = NULL)


#2 private score data type change, followed by quickly check out the new table.
private.leftnoseen <- subset(keyhosp.infoV1.left.noseen, ownership == "Private", 
                            select = c("Provider.ID", "Zip.Code", "ownership", "Overall.Rating",
                                       "Score"))

private.leftnoseen$Score <- as.character(private.leftnoseen$Score)
private.leftnoseen$Score <- as.integer(private.leftnoseen$Score)

private.leftnoseen <- private.leftnoseen[order(private.leftnoseen$Score, decreasing = F,
                                               na.last = T), ]

unique(private.leftnoseen$Score)
head(private.leftnoseen)
table(private.leftnoseen$Score, useNA = NULL)

# plot out the distribution charts for both group. 
##To make the score sorting as integer sorting rather than text, the score was coerced to the integer.
png(paste(image.directory, "bar_PublicHospDistri_leftnosen.png", sep = ""))
bar.xvalue06 <- barplot(table(public.leftnoseen$Score, useNA = NULL), ylab = "Hospital count",
                          col = rainbow(15), ylim = c(0, 220), xlim = c(0, 3), 
                        width = 0.17, cex.names = 0.8)

bar.heightvalue06 <- table(public.leftnoseen$Score, useNA = NULL)
title(main = "Distribution of patients left before seen in public hospital", adj = 0.5,
      cex.main = 1, line = 0)
title(xlab = "The number of patients left before seen", line = 2, adj = 0.5)
text(bar.xvalue06, bar.heightvalue06, labels = bar.heightvalue06, pos = 3)
dev.off()


##create labels for pie chart and make all the labels easy to read
#create a column for labels, and subset missing value out first for For loop
public.leftnoseen.rmiscore <- public.leftnoseen[!is.na(public.leftnoseen$Score), ]
public.leftnoseen.rmiscore$label.4score <- rep("not available", nrow(public.leftnoseen.rmiscore))
unique(public.leftnoseen.rmiscore$Score)
length(public.leftnoseen.rmiscore$Score) 
length(public.leftnoseen.rmiscore$label.4score)

for (i in 1:nrow(public.leftnoseen.rmiscore)) {
  if(public.leftnoseen.rmiscore$Score[i] > 7) {
    public.leftnoseen.rmiscore$label.4score[i] <- "8-15"
    }else
      {public.leftnoseen.rmiscore$label.4score[i] = as.character(public.leftnoseen.rmiscore$Score[i])}}
head(public.leftnoseen.rmiscore)
unique(public.leftnoseen.rmiscore$label.4score)

#Merge the $label.4score back to the public.leftnoseen dataframe
public.leftnoseen <- merge(public.leftnoseen, public.leftnoseen.rmiscore, 
      by.x = c("Provider.ID", "Zip.Code", "ownership", "Overall.Rating", "Score"),
      by.y = c("Provider.ID", "Zip.Code", "ownership", "Overall.Rating", "Score"),
      all.x = T)

public.lfnsn.scorelbs <- unique(public.leftnoseen.rmiscore$label.4score)
public.lfnsn.scorepct <- round(prop.table(table(public.leftnoseen$Score, useNA = NULL))*100, 2)
public.lfnsn.scorepct <- paste(public.lfnsn.scorepct, "%")
public.lfnsn.scorelbs <- paste(public.lfnsn.scorelbs, " ", "(",
                               public.lfnsn.scorepct, ")", sep = "")

png(paste(image.directory, "pie_PublicHospDistri_leftnosen.png", sep = ""))
pie(table(public.leftnoseen$label.4score, useNA = NULL), edges = 100, radius = 0.85, 
    col = rainbow(15), labels = public.lfnsn.scorelbs, line = 0.3, cex.main = 1,
    main = "The distribution of patients left before seen in public hospitals")
dev.off()


png(paste(image.directory, "bar_PrivateHosp_leftnosen.png", sep = ""))
bar.xvalue07 <- barplot(table(private.leftnoseen$Score, useNA = NULL), ylab = "Hospital count",
        col = rainbow(15), ylim = c(0, 1200), xlim = c(0, 3), 
        width = 0.17, cex.names = 0.8)

bar.heightvalue07 <- table(private.leftnoseen$Score, useNA = NULL)
title(main = "Distribution of patients left before seen in private hospital", adj = 0.5,
      cex.main = 1, line = 0.5)
title(xlab = "The number of patients left before seen", line = 2, adj = 0.5)
text(bar.xvalue07, bar.heightvalue07, labels = bar.heightvalue07, pos = 3)
dev.off()

##Same as above for public.left.noseen data, create labels for pie chart and make all the labels easy to read
#create a column for labels, and subset missing value out first for For loop
private.leftnoseen.rmiscore <- private.leftnoseen[!is.na(private.leftnoseen$Score), ]
private.leftnoseen.rmiscore$label.4score <- rep("not available", nrow(private.leftnoseen.rmiscore))
unique(private.leftnoseen.rmiscore$Score)
length(private.leftnoseen.rmiscore$Score) 
length(private.leftnoseen.rmiscore$label.4score)

for (j in 1:nrow(private.leftnoseen.rmiscore)) {
  if(private.leftnoseen.rmiscore$Score[j] > 7) {
    private.leftnoseen.rmiscore$label.4score[j] <- "8-15"
  }else
  {private.leftnoseen.rmiscore$label.4score[j] = as.character(private.leftnoseen.rmiscore$Score[j])}}

head(private.leftnoseen.rmiscore)
unique(private.leftnoseen.rmiscore$label.4score)

#Merge the $label.4score back to the private.leftnoseen dataframe
private.leftnoseen <- merge(private.leftnoseen, private.leftnoseen.rmiscore, 
                           by.x = c("Provider.ID", "Zip.Code", "ownership", "Overall.Rating", "Score"),
                           by.y = c("Provider.ID", "Zip.Code", "ownership", "Overall.Rating", "Score"),
                           all.x = T)

private.lfnsn.scorelbs <- unique(private.leftnoseen.rmiscore$label.4score)
private.lfnsn.scorepct <- round(prop.table(table(private.leftnoseen$Score, useNA = NULL))*100, 2)
private.lfnsn.scorepct <- paste(private.lfnsn.scorepct, "%")
private.lfnsn.scorelbs <- paste(private.lfnsn.scorelbs, " ", "(",
                                private.lfnsn.scorepct, ")", sep = "")

png(paste(image.directory, "Pie_PrivateHosp_leftnoseen.png", sep = ""))
pie(table(private.leftnoseen$label.4score, useNA = NULL),edges = 200, cex.main = 1,
    labels = private.lfnsn.scorelbs, radius = 0.85, col = rainbow(15), line = 0,
    main = "The distribution of patients left before seen in private hospitals")
dev.off()

png(paste(image.directory, "Plot_PatientLeft_@eachRating.png", sep = ""))
plot(public.leftnoseen$Overall.Rating, public.leftnoseen$Score, xlab = "Overall rating", 
     ylab = "The number of patients left before seen", ylim = c(0, 30), pch = 22,
     main = "The number of patients left at each rating", col = "cyan",  bg = "cyan")
points(private.leftnoseen$Overall.Rating, private.leftnoseen$Score, col = "red", 
       pch = 20)
legend(4, 25, c("public", "private"), col = c("cyan", "red"), pch = c(22,20),
       pt.bg = c("cyan", "red"))
dev.off()

###Group barplot
#to make a matrix for group barplot, score and ownership only, score is coerced to integer
score.ownership <- data.frame(keyhosp.infoV1.left.noseen$Score, keyhosp.infoV1.left.noseen$ownership)
names(score.ownership) <- c("Score", "ownership")
score.ownership$Score <- as.integer(as.character(score.ownership$Score))

head(score.ownership) 
table(score.ownership)
t(table(score.ownership))

png(paste(image.directory, "grbar_DistributionComparison_leftnoseen.png", sep = ""))
bar.xvalue08 <- barplot(t(table(score.ownership, useNA = NULL)), beside = T, 
      cex.names = 0.7, adj = 0.5,ylab = "Hospital count", col=c("cyan","red"), 
      width = 0.08, ylim = c(0, 1150), xlim = c(0, 4), legend.text = c("Public", "Private")) 
bar.heightvalue08 <- t(table(score.ownership, useNA = NULL))
title(xlab = "The number of patients left before seen", line = 2)
title(main = "Number distribution of patients left before seen", line = 1, cex.main = 1)
text(bar.xvalue08, bar.heightvalue08, labels = bar.heightvalue08, pos = 3, cex = 0.6)

dev.off()

## change factors to integers for aggregation and quickly check the output
left.noseen$Score <- as.character(left.noseen$Score)
left.noseen$Score <- as.integer(left.noseen$Score)
str(left.noseen)
is.na(left.noseen$Score) #"not avaiable" becomes NA
dim(left.noseen)

#aggregate data by average and summary of two different ownership.
avg.leftnoseen.byownership <- aggregate(left.noseen$Score, by = list(keyhosp.infoV1.left.noseen$ownership),
                                        FUN = mean, na.rm = T)
sum.leftnoseen.byownership <- aggregate(left.noseen$Score, by = list(keyhosp.infoV1.left.noseen$ownership),
                                        FUN = sum, na.rm = T)
AvgnSum.leftnoseen.byownership <- merge(avg.leftnoseen.byownership, sum.leftnoseen.byownership, 
                                        by = c("Group.1"))
AvgnSum.leftnoseen.byownership[, -1] <- round(AvgnSum.leftnoseen.byownership[, -1], 2)
colnames(AvgnSum.leftnoseen.byownership) <- c("ownership", "Average score", "Total patients left")

median(left.noseen$Score, na.rm = T)
print(AvgnSum.leftnoseen.byownership)

##plot the left before seen score
png(paste(image.directory, "Comparison_AvgScore_leftnoseen.png", sep = ""))
bar.xvalue09 <- barplot(avg.leftnoseen.byownership$x,  ylab = "Avg score of patient left before seen", 
        names.arg = c("Public", "Private"), col = c("cyan", "red"), xlim = c(0, 0.1),
        ylim = c(0,2.1), width = 0.015, cex.names = 0.85) 
bar.heightvalue09 <- round(avg.leftnoseen.byownership$x, 2)
title(xlab = "Two ownerships", line = 2, adj = 0.15)
title(main = "The average score comparison of left before seen", line = 1, 
      adj = 0, cex.main = 1)
text(bar.xvalue09, bar.heightvalue09, labels = bar.heightvalue09, pos = 3)
dev.off()


#################################################################################
#                                                                               #
#                       work with Readmission and Death file                    #
#                                                                               #   
#################################################################################

setwd("/Users/syu/Documents/Documents_syu/PMBA_courses/MIS7190ProgrammingForBusiness/GroupDProject/Hospital_Revised_Flatfiles")
ori.RnD <- read.csv("Readmissions and Deaths - Hospital.csv", header = T, sep = ",")
head(ori.RnD, 2)
colnames(ori.RnD)

##subset the to get the information needed for analysis
#keep some columns for further subsetting and simplify the character information
ori.RnD.concise <- subset(ori.RnD, select = c(Provider.ID, ZIP.Code, Measure.Name, 
                                              Measure.ID, Compared.to.National, Score,
                                              Lower.Estimate, Higher.Estimate) )
str(ori.RnD.concise)
ori.RnD.concise$Measure.Name <- as.character(ori.RnD.concise$Measure.Name)
ori.RnD.concise$Measure.ID <- as.character(ori.RnD.concise$Measure.ID)
ori.RnD.concise$Score <- as.double(as.character(ori.RnD.concise$Score))

levels(ori.RnD.concise$Compared.to.National) <- c("Better","Same", "NA", "Inconclusive",  "Worse")
table(ori.RnD.concise$Compared.to.National)
ori.RnD.concise$Compared.to.National <- as.character(ori.RnD.concise$Compared.to.National)
ori.RnD.concise$Compared.to.National <- gsub("NA", NA, ori.RnD.concise$Compared.to.National)

ori.RnD.furtherconcise <- subset(ori.RnD.concise, select = c(Provider.ID, ZIP.Code, Measure.Name, 
                                                            Measure.ID, Compared.to.National, Score))


#further subsetting dataset by measure ID
unique(ori.RnD.furtherconcise$Measure.ID)

RnD.mort30.all <- ori.RnD.furtherconcise[grep("MORT_30_", ori.RnD.furtherconcise$Measure.ID),]
RnD.mort30.all$Measure.ID <- gsub("MORT_30_", "M30.", RnD.mort30.all$Measure.ID)
unique(RnD.mort30.all$Measure.ID)

Remd30.all <- ori.RnD.furtherconcise[grep("READM_30", ori.RnD.furtherconcise$Measure.ID),]
Remd30.all$Measure.ID <- gsub("READM_30_", "ReAd30.", Remd30.all$Measure.ID)
unique(Remd30.all$Measure.ID)


##To merge with data frame have ownership info (keyhosp.info.2ownership & keyhosp.owners).
Twownership.RnD.M30all <- merge(keyhosp.info.2ownership, RnD.mort30.all, all = F,
      by.x = c("Provider.ID", "Zip.Code"), by.y = c("Provider.ID", "ZIP.Code"))
Twownership.RnD.M30all$ownership <- as.character(Twownership.RnD.M30all$ownership)
Public.RnD.M30all <- Twownership.RnD.M30all[grep("Public", Twownership.RnD.M30all$ownership),]
Private.RnD.M30all <- Twownership.RnD.M30all[grep("Private", Twownership.RnD.M30all$ownership),]

Twownership.Remd.30all <- merge(keyhosp.info.2ownership, Remd30.all, all = F,
     by.x = c("Provider.ID", "Zip.Code"), by.y = c("Provider.ID", "ZIP.Code"))
Twownership.Remd.30all$ownership <- as.character(Twownership.Remd.30all$ownership)
Public.Remd.30all <- Twownership.Remd.30all[grep("Public", Twownership.Remd.30all$ownership),]
Private.Remd.30all <- Twownership.Remd.30all[grep("Private", Twownership.Remd.30all$ownership),]


######### Analysis on Mort.30.COPD measurement
Twownership.RnD.MortCOPD <- Twownership.RnD.M30all[grep("M30.COPD", Twownership.RnD.M30all$Measure.ID),]

Public.RnD.MortCOPD <- Public.RnD.M30all[grep("M30.COPD", Public.RnD.M30all$Measure.ID),]
Public.RnD.MortCOPD <- Public.RnD.MortCOPD[order(Public.RnD.MortCOPD$Compared.to.National,
                       Public.RnD.MortCOPD$Overall.Rating, decreasing = F),]

Private.RnD.MortCOPD <- Private.RnD.M30all[grep("M30.COPD", Private.RnD.M30all$Measure.ID),]
Private.RnD.MortCOPD <- Private.RnD.MortCOPD[order(Private.RnD.MortCOPD$Compared.to.National,
                                                   Private.RnD.MortCOPD$Overall.Rating, decreasing = F),]

##Bar chart to compare the mortality COPD comparison to national
png(paste(image.directory, "bar_Mort30PublicHosp_comparN.png", sep = ""))
bar.xvalue10 <- barplot(table(Public.RnD.MortCOPD$Compared.to.National), col = rainbow(4), ylim = c(0, 800),
        xlim = c(0,0.7), width = 0.12, ylab = "Hospital count")
bar.heightvalue10 <- table(Public.RnD.MortCOPD$Compared.to.National)
title(main = "COPD 30-day mortality comparison of public hospitals", cex.main = 1, adj = 0, line = 0.7)
title(xlab = "Compared to national", line = 2, adj = 0.4)
text(bar.xvalue10, bar.heightvalue10, labels = bar.heightvalue10, pos = 3)

dev.off()

png(paste(image.directory, "bar_COPDMort30PrivateHosp_comparN.png", sep = ""))
bar.xvalue11 <- barplot(table(Private.RnD.MortCOPD$Compared.to.National), col = rainbow(4), ylim = c(0, 3000),
                        xlim = c(0,0.7), width = 0.12, ylab = "Hospital count")
bar.heightvalue11 <- table(Private.RnD.MortCOPD$Compared.to.National)
title(main = "COPD 30-day mortality comparison of private hospitals", cex.main = 1, adj = 0, line = 0.7)
title(xlab = "Compared to national", line = 2, adj = 0.4)
text(bar.xvalue11, bar.heightvalue11, labels = bar.heightvalue11, pos = 3)
dev.off()

png(paste(image.directory, "grpbar_COPDMort30allHosp_comparN.png", sep = ""))
MortCOPD.comparcnt.2ownership <- cbind(table(Public.RnD.MortCOPD$Compared.to.National), 
                                       table(Private.RnD.MortCOPD$Compared.to.National))

bar.xvalue12 <- barplot(t(MortCOPD.comparcnt.2ownership), beside = T, col = c("cyan", "red"), ylim = c(0, 3000),
        xlim = c(0,1.5), width = 0.12, ylab = "Hospital count", legend.text = c("Public", "Private"))

bar.heightvalue12 <- t(MortCOPD.comparcnt.2ownership)
title(main = "COPD 30-day mortality comparison of all hospitals", cex.main = 1, adj = 0, line = 0.7)
title(xlab = "Compared to national", line = 2, adj = 0.5)
text(bar.xvalue12, bar.heightvalue12, labels = bar.heightvalue12, pos = 3)
dev.off()


#piechart to find the ratios of COPD mortality comparsion
png(paste(image.directory, "pie_COPDMort30PublicHosp_comparN.png", sep = ""))
public.mortCOPD.lbls <- unique(Public.RnD.MortCOPD$Compared.to.National, na.rm =T)
public.mortCOPD.lbls <- public.mortCOPD.lbls[!is.na(public.mortCOPD.lbls)]
public.mortCOPD.pct <- round(prop.table(table(Public.RnD.MortCOPD$Compared.to.National))*100, 2)
public.mortCOPD.pct <- data.frame(public.mortCOPD.pct)$Freq 
public.mortCOPD.lbls <- paste(public.mortCOPD.lbls, " ","(", public.mortCOPD.pct, "%", ")", sep ="")

pie(table(Public.RnD.MortCOPD$Compared.to.National), col = rainbow(4), radius = 0.8,
    main = "The public hosptial distribution of COPD mortality comparison", 
    labels = public.mortCOPD.lbls, cex.main = 0.9, line =0)
dev.off()

png(paste(image.directory, "pie_COPDMort30PrivateHosp_comparN.png", sep = ""))
private.mortCOPD.lbls <- unique(Private.RnD.MortCOPD$Compared.to.National, na.rm =T)
private.mortCOPD.lbls <- private.mortCOPD.lbls[!is.na(private.mortCOPD.lbls)]
private.mortCOPD.pct <- round(prop.table(table(Private.RnD.MortCOPD$Compared.to.National))*100, 2)
private.mortCOPD.pct <- data.frame(private.mortCOPD.pct)$Freq 
private.mortCOPD.lbls <- paste(private.mortCOPD.lbls, " ","(", private.mortCOPD.pct, "%", ")", sep ="")

pie(table(Private.RnD.MortCOPD$Compared.to.National),col = rainbow(4),radius = 0.73,
    main = "The private hosptial distribution of COPD mortality comparison", 
    labels = private.mortCOPD.lbls, cex.main = 0.9, line =0)
dev.off()

##calculate COPD mortality score
Avg.score.M30COPD <- aggregate(Twownership.RnD.MortCOPD$Score, by = list(Twownership.RnD.MortCOPD$ownership),
                               FUN = mean, na.rm =T)
Avg.score.M30COPD$x <- round(Avg.score.M30COPD$x, 2)

colnames(Avg.score.M30COPD) <- c("Ownership", "Average score of COPD mortality")
print(Avg.score.M30COPD)


######### Analysis on ReAdmission.COPD measurement
Twownership.Remd.ReCOPD <- Twownership.Remd.30all[grep("ReAd30.COPD", Twownership.Remd.30all$Measure.ID),]

Public.Remd.MortCOPD <- Public.Remd.30all[grep("ReAd30.COPD", Public.Remd.30all$Measure.ID),]
Public.Remd.MortCOPD <- Public.Remd.MortCOPD[order(Public.Remd.MortCOPD$Compared.to.National,
                                                 Public.Remd.MortCOPD$Overall.Rating, decreasing = F),]

Private.Remd.MortCOPD <- Private.Remd.30all[grep("ReAd30.COPD", Private.Remd.30all$Measure.ID),]
Private.Remd.MortCOPD <- Private.Remd.MortCOPD[order(Private.Remd.MortCOPD$Compared.to.National,
                                                   Private.Remd.MortCOPD$Overall.Rating, decreasing = F),]


##Bar chart to compare the ReAdmission COPD comparison to national

png(paste(image.directory, "bar_COPDReAd30PublicHosp_comparN.png", sep = ""))
bar.xvalue13 <- barplot(table(Public.Remd.MortCOPD$Compared.to.National), col = rainbow(4), ylim = c(0, 750),
                        xlim = c(0,0.7), width = 0.12, ylab = "Hospital count")
bar.heightvalue13 <- table(Public.Remd.MortCOPD$Compared.to.National)
title(main = "COPD 30-day readmission comparison of public hospitals", cex.main = 1, adj = 0, line = 0.7)
title(xlab = "Compared to national", line = 2, adj = 0.4)
text(bar.xvalue13, bar.heightvalue13, labels = bar.heightvalue13, pos = 3)

dev.off()

png(paste(image.directory, "bar_COPDReAd30PrivateHosp_comparN.png", sep = ""))
bar.xvalue14 <- barplot(table(Private.Remd.MortCOPD$Compared.to.National), col = rainbow(4), ylim = c(0, 3000),
                        xlim = c(0,0.7), width = 0.12, ylab = "Hospital count")
bar.heightvalue14 <- table(Private.Remd.MortCOPD$Compared.to.National)
title(main = "COPD 30-day readmission comparison of private hospitals", cex.main = 1, adj = 0, line = 0.7)
title(xlab = "Compared to national", line = 2, adj = 0.4)
text(bar.xvalue14, bar.heightvalue14, labels = bar.heightvalue14, pos = 3)
dev.off()

png(paste(image.directory, "grpbar_COPDReAd30allHosp_comparN.png", sep = ""))
ReAdCOPD.comparcnt.2ownership <- cbind(table(Public.Remd.MortCOPD$Compared.to.National), 
                                       table(Private.Remd.MortCOPD$Compared.to.National))

bar.xvalue15 <- barplot(t(ReAdCOPD.comparcnt.2ownership), beside = T, col = c("cyan", "red"), ylim = c(0, 3000),
                        xlim = c(0,1.5), width = 0.12, ylab = "Hospital count", legend.text = c("Public", "Private"))

bar.heightvalue15 <- t(ReAdCOPD.comparcnt.2ownership)
title(main = "COPD 30-day readmission comparison of all hospitals", cex.main = 1, adj = 0, line = 0.7)
title(xlab = "Compared to national", line = 2, adj = 0.5)
text(bar.xvalue15, bar.heightvalue15, labels = bar.heightvalue15, pos = 3)
dev.off()

#piechart to find the ratios of COPD readmission comparsion

png(paste(image.directory, "pie_COPDRe30PublicHosp_comparN.png", sep = ""))
public.ReAdCOPD.lbls <- unique(Public.Remd.MortCOPD$Compared.to.National, na.rm =T)
public.ReAdCOPD.lbls <- public.ReAdCOPD.lbls[!is.na(public.ReAdCOPD.lbls)]
public.ReAdCOPD.pct <- round(prop.table(table(Public.Remd.MortCOPD$Compared.to.National))*100, 2)
public.ReAdCOPD.pct <- data.frame(public.ReAdCOPD.pct)$Freq 
public.ReAdCOPD.lbls <- paste(public.ReAdCOPD.lbls, " ","(", public.ReAdCOPD.pct, "%", ")", sep ="")

pie(table(Public.Remd.MortCOPD$Compared.to.National), col = rainbow(4), radius = 0.8,
    main = "The public hosptial distribution of COPD readmission comparison", 
    labels = public.ReAdCOPD.lbls, cex.main = 0.9, line =0)
dev.off()

png(paste(image.directory, "pie_COPDRe30PrivateHosp_comparN.png", sep = ""))
private.ReAdCOPD.lbls <- unique(Private.Remd.MortCOPD$Compared.to.National, na.rm =T)
private.ReAdCOPD.lbls <- private.ReAdCOPD.lbls[!is.na(private.ReAdCOPD.lbls)]
private.ReAdCOPD.pct <- round(prop.table(table(Private.Remd.MortCOPD$Compared.to.National))*100, 2)
private.ReAdCOPD.pct <- data.frame(private.ReAdCOPD.pct)$Freq 
private.ReAdCOPD.lbls <- paste(private.ReAdCOPD.lbls, " ","(", private.ReAdCOPD.pct, "%", ")", sep ="")

pie(table(Private.Remd.MortCOPD$Compared.to.National),col = rainbow(4),radius = 0.7,
    main = "The private hosptial distribution of COPD readimssion comparison", xpd = T,
    labels = private.ReAdCOPD.lbls, cex.main = 0.9, line =0)
dev.off()

##calculate  COPD Readmission score
Avg.score.ReAd30COPD <- aggregate(Twownership.Remd.ReCOPD$Score, by = list(Twownership.Remd.ReCOPD$ownership),
                               FUN = mean, na.rm =T)
Avg.score.ReAd30COPD$x <- round(Avg.score.ReAd30COPD$x, 2)
colnames(Avg.score.ReAd30COPD) <- c("Ownership", "Average score of COPD readmission")
print(Avg.score.ReAd30COPD)



#####Mapping the COPD mortality and readmission to different owners
##Need owners information
head(keyhosp.owners)

RnD.mort30.all <- ori.RnD.furtherconcise[grep("MORT_30_", ori.RnD.furtherconcise$Measure.ID),]
RnD.mort30.all$Measure.ID <- gsub("MORT_30_", "M30.", RnD.mort30.all$Measure.ID)
unique(RnD.mort30.all$Measure.ID)

Remd30.all <- ori.RnD.furtherconcise[grep("READM_30", ori.RnD.furtherconcise$Measure.ID),]
Remd30.all$Measure.ID <- gsub("READM_30_", "ReAd30.", Remd30.all$Measure.ID)
unique(Remd30.all$Measure.ID)


##To merge with data frame have ownership info (keyhosp.info.2ownership & keyhosp.owners).
