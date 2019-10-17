library(tidyverse)
library(rjson)
library(httr)         #Package used for API interaction in R
library(ggplot2)      #For plotting 
library(visdat)
sample <- GET("https://catalogue.data.govt.nz/api/3/action/datastore_search?resource_id=bdfe0e4c-1554-4701-a8fe-ba1c8e0cc2ce&limit=2556")


sample1 <- content(sample)
#head(sample1)
sam =sample1$result$records
sam


#Initialising
School_Id <- c()
Org_Name  <- c()
URL  <- c()
Urban_Area  <- c()
Org_Type  <- c()
Definition  <- c()
Authority  <- c()
CoEd_Status  <- c()
Regional_Council  <- c()
Territorial_Authority  <- c()
Local_Office_Name  <- c()
Education_Region  <- c()
General_Electorate  <- c()
Maori_Electorate  <- c()
Latitude  <- c()
Longitude  <- c()
Decile  <- c()
Total  <- c()
European  <- c()
Maori  <- c()
Pacific  <- c()
Asian  <- c()
MELAA  <- c()
Other  <- c()
International  <- c()


str(sam)
sam[[1]]$Territorial_Authority
#Iteration of  Schoollist array to gather only the required values
for (i in 1:length(sam))
{
  
  if (is.null(sam[[i]]$School_Id)){School_Id[i] <- ""} else{School_Id[i] <- sam[[i]]$School_Id}
  if (is.null(sam[[i]]$Org_Name)){Org_Name[i] <- ""} else{Org_Name[i] <- sam[[i]]$Org_Name}
  if (is.null(sam[[i]]$URL)){URL[i] <- ""} else {URL[i] <- sam[[i]]$URL}
  if (is.null(sam[[i]]$Urban_Area)) {Urban_Area[i] <- ""} else{Urban_Area[i] <- sam[[i]]$Urban_Area}
  if (is.null(sam[[i]]$Org_Type )){Org_Type[i] <- ""} else{Org_Type[i] <- sam[[i]]$Org_Type}
  if (is.null(sam[[i]]$Definition)){Definition[i] <- ""} else{ Definition[i] <- sam[[i]]$Definition}
  if (is.null(sam[[i]]$Authority)){Authority[i] <- ""} else{Authority[i] <- sam[[i]]$Authority}
  if (is.null(sam[[i]]$CoEd_Status)) {CoEd_Status[i] <- ""} else{CoEd_Status[i] <- sam[[i]]$CoEd_Status}
  #if (is.null(sam[[i]]$Regional_Council)) {Regional_Council[i] <-  "Not"} else{Regional_Council[i] <- sam[[i]]$Regional_Council}
  if(sam[[i]]$Regional_Council=="") {Regional_Council[i] <-  "Data unknown"} else{Regional_Council[i] <- sam[[i]]$Regional_Council}
  if (is.null(sam[[i]]$Territorial_Authority)) {Territorial_Authority[i] <- ""} else {Territorial_Authority[i] <- sam[[i]]$Territorial_Authority}
  if (is.null(sam[[i]]$Local_Office_Name)){Local_Office_Name[i] <- ""}else {Local_Office_Name[i] <- sam[[i]]$Local_Office_Name}
  if (is.null(sam[[i]]$Education_Region)) {Education_Region[i] <- ""} else {Education_Region[i] <- sam[[i]]$Education_Region}
  if (is.null(sam[[i]]$General_Electorate)){General_Electorate[i] <-""}else{General_Electorate[i] <- sam[[i]]$General_Electorate}
  if (is.null(sam[[i]]$Maori_Electorate)){Maori_Electorate[i] <- ""} else{Maori_Electorate[i] <- sam[[i]]$Maori_Electorate}
  if (is.null(sam[[i]]$Latitude)) {Latitude[i] <- ""} else {Latitude[i] <- sam[[i]]$Latitude}
  if (is.null(sam[[i]]$Longitude)){Longitude[i] <-""} else{Longitude[i] <- sam[[i]]$Longitude}
  if (is.null(sam[[i]]$Decile)) {Decile[i] <- ""} else {Decile[i] <- sam[[i]]$Decile}
  if (is.null(sam[[i]]$Total)) {Total[i] <-""} else{Total[i] <- sam[[i]]$Total}
  if (is.null(sam[[i]]$European)) {European[i] <- ""} else{European[i] <- sam[[i]]$European}
  if (is.null(sam[[i]]$Maori)){Maori[i] <-""} else{Maori[i] <- sam[[i]]$Maori}
  if (is.null(sam[[i]]$Pacific)){Pacific[i] <- ""} else{Pacific[i] <- sam[[i]]$Pacific}
  if (is.null(sam[[i]]$Asian)) {Asian[i] <- ""} else{Asian[i] <- sam[[i]]$Asian}
  if (is.null(sam[[i]]$MELAA)) {MELAA[i] <- ""} else{MELAA[i] <- sam[[i]]$MELAA}
  if (is.null(sam[[i]]$Other)){Other[i] <- ""} else{Other[i] <- sam[[i]]$Other}
  if (is.null(sam[[i]]$International)){International[i] <-""} else{International[i] <- sam[[i]]$International}
  
}

#Creating a dataframe with columns required
table1 <- tibble(School_Id = School_Id, Org_Name= Org_Name, URL = URL, Urban_Area = Urban_Area, Org_Type = Org_Type, Definition = Definition, Authority = Authority, CoEd_Status = CoEd_Status ,Regional_Council= Regional_Council ,Territorial_Authority = Territorial_Authority ,Local_Office_Name = Local_Office_Name, Education_Region = Education_Region, General_Electorate = General_Electorate, Maori_Electorate = Maori_Electorate, Latitude = Latitude, Longitude = Longitude,Decile = Decile,Total = Total,European = European,Maori= Maori,Pacific = Pacific, Asian = Asian,MELAA = MELAA, Other = Other,International = International)
table1

vis_miss(table1)
# to make the Latitude and Lonigtude vaues as Numeric 
table1$Latitude <- as.numeric(table1$Latitude)
table1$Longitude <- as.numeric(table1$Longitude)

# to remove the null values in Latitude and longitude
drop_column <- table1 %>%
  drop_na(Latitude, Longitude) %>%
  filter(Regional_Council != "Data unknown")
  

vis_miss(drop_column)

#write_excel_csv(table1, "Map_data.csv")

# to get the top 5 regions with highest school numbers
table3<- drop_column %>%
  group_by(Regional_Council) %>%
  summarise(school_count = n(), Latitude =mean(Latitude), Longitude= mean(Longitude)) %>%
  arrange(desc(school_count)) %>%
  top_n(5, school_count)



#devtools::install_github("dkahle/ggmap")
library(ggplot2)
library(ggmap)



register_google(key = "AIzaSyBZsoAyHJ78wBOTAFFq3no1Am5Gw6jhnLM")
has_google_key()
google_key()
has_google_client()
has_google_signature()

# to plot the the top five regions with highest school number
NZ_map <- get_map("New Zealand", zoom = 5)
Points <- ggmap(NZ_map) + geom_point(aes(x = Longitude, y = Latitude),col = "red", data = table3, alpha = 1)
Points


table4 <- drop_column %>%
  group_by(Regional_Council) %>%
  summarise(International_students_count = sum(International), Maori_students_count = sum(Maori),European_students_count= sum(European),Asian_students_count= sum(Asian),Total_Students = sum(Total))%>%
  arrange(desc(International_students_count, Maori_students_count, Total_Students)) %>%
  mutate(Proportion_International =(International_students_count/Total_Students), Proportion_Maori = (Maori_students_count/Total_Students)) %>%
  mutate(Proportion_European= (European_students_count/Total_Students),Proportion_Asian = (Asian_students_count/Total_Students))

par(mar = c(7, 4, 2, 2) + 0.8) #add room for  labels

Block_name <-c("Proportion_International","Proportion_Asian","Proportion_Maori","Proportion_European")
colors <- c("blue", "green", "yellow", "red")

count <-c(table4$Proportion_International,table4$Proportion_Asian, table4$Proportion_Maori,table4$Proportion_European)
Values <- matrix(count, nrow = 4, ncol = 17, byrow = TRUE)
Values
##las for space for label

barplot(Values ,names.arg=table4$Regional_Council,col= colors,ylim = c(0,1),ylab = "Proportion", main ="Proportions of students in NZ region based on origin",border= c("blue", "green", "yellow", "red"),las = 2, beside =TRUE)
legend("topleft", Block_name, cex = .7, fill = colors)


x <- ggplot(table4, aes(x = Regional_Council, y = International_students_count)) + 
  geom_col(width = 0.5, position = position_dodge())
y <- ggplot(table4, aes(x = Regional_Council, y = Maori_students_count)) + 
  geom_col(width = 0.5, position = position_dodge())
x
y
region <- c("Proportion_International","Proportion_Maori")
region
counts()