library(data.table) # Due to large Dataset
airfare_rev <- read.csv("D:/EXCELR 2ND PROJECT/Concatenate_B2C_B2E.csv") # Import the original Client data
View(airfare_rev)
dim(airfare_rev) # Rows 278466 x  Colmn 4
class(airfare_rev) # data.table
summary(airfare_rev)
str(airfare_rev) # all 4 variable format is chr
attach(airfare_rev)

library(Hmisc)
describe(airfare_rev)

###################### Datatype ########################

# Change datatype of InvoiceDate  to POSIXct
library(lubridate)
airfare_rev$InvoiceDate <- dmy_hm(airfare_rev$InvoiceDate) 
class(airfare_rev$InvoiceDate) # POSIXct

library(dplyr)
airfare_rev <- airfare_rev %>%
  mutate(Year = year(airfare_rev$InvoiceDate),Month =month(airfare_rev$InvoiceDate),
         Day = wday(airfare_rev$InvoiceDate),Date = day(airfare_rev$InvoiceDate),Hour = hour(airfare_rev$InvoiceDate),
         Minutes = minute(airfare_rev$InvoiceDate))

# change datatype of NetFare
airfare_rev$NetFare <- as.numeric(airfare_rev$NetFare) # change to numeric

airfare_rev<- airfare_rev[,2:10] # removed InvoiceDate as we have splitted & added

str(airfare_rev) # crosscheck the changed datatype as above

################# EDA Plot without imputing missing values ###############
library(dplyr)
library(ggplot2)

# Filter by Itinerytype & plot Avg of NetFare by Product Type (Barplot)

airfare_rev %>%
  filter(ItineraryType=="Domestic")%>%
  group_by(Day)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare,na.rm = T),Minimum=min(NetFare,na.rm = T),Maximum=max(NetFare,na.rm = T))%>%
  ggplot(aes(x = Day, y = mean_Netfare,fill = Day ))+
  geom_bar(stat = "identity") + theme_classic() +
  labs(x = "WeekDay",y = "Avg NetFare",title = paste("Avg NetFare by WeekDay for Itinery 'Domestic'"))

########## Missing Value #########

sum(is.na(airfare_rev)) # 60903
sapply(airfare_rev,function(x) sum(is.na(x))) # 60891,0,0,2,2,2,2,2,2

# Replace blank fields with NA in PT,IT
airfare_rev$ProductType [airfare_rev$ProductType==""] <- NA
airfare_rev$ItineraryType [airfare_rev$ItineraryType==""] <-NA

sum(is.na(airfare_rev)) # 93682
sapply(airfare_rev,function(x) sum(is.na(x))) # 60891,2,32777,2,2,2,2,2,2

####### Missing Value Imputation ######
# Define Mode Function 
getmode <- function(v) {
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v , uniqv)))] }

# Imputation of missing values with mode in ItineryType
getmode(airfare_rev$ItineraryType) # Domestic
airfare_rev$ItineraryType[is.na(airfare_rev$ItineraryType)] <- getmode(airfare_rev$ItineraryType)
sum(is.na(airfare_rev$ItineraryType)) # 0

# Removing NA from ProductType
library(dplyr)
library(tidyr)
airfare_rev<- airfare_rev%>%
  drop_na(ProductType)
sum(is.na(airfare_rev$ProductType)) # 0

##### Imputate missing values of NetFare based on ProductType

# First crosscheck NA values in netfare based on PT 'refund' & 'payment' prior to imputation
airfare_rev %>%
  filter(ProductType=="refund")%>%
  group_by(NetFare)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare),Minimum=min(NetFare),Maximum=max(NetFare))
# Found 5495 records with NA value in NetFare under PT 'refund'

airfare_rev %>%
  filter(ProductType=="payment")%>%
  group_by(NetFare)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare),Minimum=min(NetFare),Maximum=max(NetFare))
# Found 55394 records with NA value in NetFare under PT 'payment'

airfare_rev$NetFare[is.na(airfare_rev$NetFare)] <- 0

sapply(airfare_rev,function(x) sum(is.na(x))) # 0,0,0,0,0,0,0,0,0 

##### Split the data based on Itinerytype for future model building ####
# Create new dataset for domestic 
domestic <- airfare_rev %>%
 filter(ItineraryType=="Domestic" & ProductType=="Air")

# Create new dataset for international
international <- airfare_rev %>%
  filter(ItineraryType=="International" & ProductType=="Air")
##############################Forecasting-DOMESTIC##########################
#Merging the split of date using sprintf fun and created a new DOMESTIC dataset
InvoiceDate1<-with(domestic,dmy(sprintf('%02d%02d%04d',Date,Month,Year)))
InvoiceDate1     
#Creat a new data frame with only NetFare and InvoiceDate
domestic1<-data.frame(domestic$NetFare,InvoiceDate1)

#View(domestic1)
names(domestic1)<-c("NetFare","InvoiceDate1")
#str(domestic1)
#_________________________________________________________________
      #######Grouping based on Dates--Domestic############
#Created a new internatinal2 data frame inorder to group the date
domestic2<-data.frame(domestic1)%>%
  group_by(InvoiceDate1)%>%
  summarise(mean_netfare=mean(NetFare,na.rm = T))
names(domestic2)<-c("InvoiceDate1","M_NetFare")
View(domestic2)
#Plot
plot(domestic2$M_NetFare,main='Plot of Mean NetFare',type = 'o')
#ggplot Netfare v/s Invoicedates
ggplot(domestic2,aes(x=InvoiceDate1,y=M_NetFare,group=1))+
  geom_point()+geom_line(color="#69b3a3")+theme_classic()+
  labs(X="date",y="M_NetFare",title="Netfare v/s Invoicedate")+
  scale_x_date(date_labels = "%y-%b-%d",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))
#domestic2<-domestic2[1:403,]
#getwd()
write.csv(domestic2, file="FinalDomesticDataset.csv")
##################International##################
#____________________________________________________________________
#Merging the split of date using sprintf fun and created a new international dataset
InvoiceDate2<-with(international,dmy(sprintf('%02d%02d%04d',Date,Month,Year)))
InvoiceDate2     
#Creat a new data frame with only NetFare and InvoiceDate
international1<-data.frame(international$NetFare,InvoiceDate2)
#View(international1)
names(international1)<-c("NetFare","InvoiceDate2")
#str(international1)
#######Grouping based on Dates############
#Created a new internatinal2 data frame inorder to group the date
international2<-data.frame(international1)%>%
  group_by(InvoiceDate2)%>%
  summarise(mean_netfare=mean(NetFare,na.rm = T))
names(international2)<-c("InvoiceDate2","M_NetFare2")
View(international2)
#Plot
plot(international2$M_NetFare2,main='Plot of Mean NetFare',type = 'o')
#ggplot Netfare v/s Invoicedates
ggplot(international2,aes(x=InvoiceDate2,y=M_NetFare2,group=1))+
  geom_point()+geom_line(color="#69b3a3")+theme_classic()+
  labs(X="date",y="M_NetFare",title="Netfare v/s Invoicedate")+
  scale_x_date(date_labels = "%y-%b-%d",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))

getwd()
write.csv(international2,file="FinalInternationalDataset.csv")
#___________________________________________________________________

