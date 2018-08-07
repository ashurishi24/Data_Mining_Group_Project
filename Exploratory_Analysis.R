library(dplyr)
library(tidyverse)
setwd("C:/Users/Ashutosh Kumar/Desktop/DataMining_GP")
getwd()

dir()

input <- read.csv("services_annual_dataset.csv",header=TRUE) # Input Data Frame
input_count <- read.csv("countries_grouped.csv",header=TRUE) #Data Frame with Grouped Countries

dim(input)
#2501284      14

input_v1 = merge(x = input, y = input_count, by = "Reporter_Description", all.x = TRUE)

dim(input_v1)
#Count [1] 25,01,284      15

class(input_v1)

#To check if any blank data is there or not
map(input_v1, sum(is.na(5)))
#No NA  found

#Important Columns
glimpse(input_v1)

summary(input_v1$Value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1696       0       0    1283      15 5351191 
#

#To See Frequency of important Columns#
t=table(input_v1$Type_reporter)
t1 = as.data.frame(t)
names(t1)[1] = 'Type_reporter'
t1.sort <-t1[order(-t1$Freq),]
t1.sort

t=table(input_v1$Partner_Description)
t1 = as.data.frame(t)
names(t1)[1] = 'Partner_Description'
t1.sort <-t1[order(-t1$Freq),]
t1.sort
# There are about 2,12,226 entries for world as partner trade(Import and Export) which may imply non disclosure agreement between reporter and partner countries#

t2=table(input_v1$Indicator_Description)
t3 = as.data.frame(t2)
names(t3)[1] = 'Indicator_Description'
t3.sort <-t3[order(-t3$Freq),]
t3.sort

t4=table(input$Flow_Description)
t5 = as.data.frame(t4)
names(t5)[1] = 'Flow_Description'
t5.sort <-t5[order(-t5$Freq),]
t5.sort

t4=table(input_v1$Year )
t5 = as.data.frame(t4)
names(t5)[1] = 'Year'
t5.sort <-t5[order(-t5$Year ),]
t5.sort

#To find mean Value of Trade with respect to Reporter Country and Indicator Services
x <- tapply(input_v1$Value, list(input_v1$Reporter_Description,input_v1$Indicator_Description),mean)
x1 <- tapply(input_v1$Value, list(input_v1$Reporter_Description,input_v1$Partner_Description),mean)

#G7 Countries --Canada, France, Germany, Italy, Japan, the United Kingdom, and the United States
#BRICS Brazil, Russia, India, China and South Africa
#G20 Argentina, Australia, Brazil, Canada, China, France, Germany, India, Indonesia, Italy, Japan, México, Russia, Saudi Arabia, South Africa, Korea, Turkey, the United Kingdom, United States and European Union

write.csv(x1,file="data_p.csv")


# Grouping the variables and creating dummy variables
#Categorising as per most important indicators Services which covers almost 99 % of services offered

input_v1 = input_v1 %>%
  mutate(Ind_Service = as.numeric(Indicator_Description %in% c("Services", "Other business services")),
         Ind_CommService = as.numeric(Indicator_Description %in% c("Commercial services", "Other commercial services", "Goods-related services", "Government goods and services n.i.e.","Operating leasing services")),
         Ind_Comm_Seroth_bus_serv = as.numeric(Indicator_Description %in% c("Technical, trade-related, and other business services", "Legal, accounting, management consulting, and public relations services")),
         Ind_Engg_Manuf_Serv = as.numeric(Indicator_Description %in% c("Manufacturing services on physical inputs owned by others", "Maintenance and repair services n.i.e.", "Architectural, engineering, scientific, and other technical services", "Engineering services")),
         Ind_Fin_Serv = as.numeric(Indicator_Description %in% c("Financial services", "Insurance and pension services","Trade-related services")),
         Ind_Network_IT_Serv = as.numeric(Indicator_Description %in% c("Telecommunications, computer, and information services", "Computer services","Telecommunications services","Information services")),
         Ind_PR_Serv = as.numeric(Indicator_Description %in% c("Professional and management consulting services", " Advertising, market research, and public opinion polling services ")),
         Ind_Res_n_Dev = as.numeric(Indicator_Description %in% c("Charges for the use of intellectual property n.i.e.", "Research and development services")),
         Ind_Transport = as.numeric(Indicator_Description %in% c("Travel", "Transport", "Sea transport","Air transport","Freight  (Sea)","Passenger (Air)","Other modes of transport","Freight (All modes of transport)","Freight (Other)","Other (All modes of transport)","Freight (Air)","Passenger (All modes of transport)")),
         Ind_Construction = as.numeric(Indicator_Description == "Construction"),
         Ind_oth_bus_serv = as.numeric(Indicator_Description %in% c("Other services", "Personal", "Other business services n.i.e.","Other (Personal)","Personal, cultural, and recreational services","Business"))) %>%
  select(-Indicator_Description,-Indicator_Code,-Unit,-Flag,-Note,-Source_Description)

#Creating Dummy as per Export and IMport
input_v1=input_v1 %>%
  mutate(Flow_Import=as.numeric(Flow_Description=="Imports"),
         Flow_Export=as.numeric(Flow_Description=="Export")
  ) %>%
  select(-Flow_Description,-Flow_Code)

#Creating Dummy as Dev,Emerging ...
input_v1=input_v1 %>%
  mutate(Rep_Dev=as.numeric(Type_reporter=="Developed"),
         Rep_Emerg=as.numeric(Type_reporter=="Emerging"),
         Rep_EU = as.numeric(Type_reporter=="European Union"),
         Rep_Oth = as.numeric(Type_reporter== "Others"),
         Rep_Und_Dev = as.numeric(Type_reporter== "Under Developed"),
         Rep_CW_Nat = as.numeric(Type_reporter== "CommonWealthNations"),
         Rep_CW_Nat = as.numeric(Type_reporter== "world"),
         Rep_Trade_Ass = as.numeric(Type_reporter== "Trade Association")
  ) %>%
  select(-Reporter_Description,-ï..Reporter_Code)

#Important Columns
glimpse(input_v1)


