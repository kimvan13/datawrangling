library(tidyverse)
library(httr)         #Package used for API interaction in R
library(ggplot2)      #For plotting 
library(visdat)
School_sample_api <- GET("https://catalogue.data.govt.nz/api/3/action/datastore_search?resource_id=bdfe0e4c-1554-4701-a8fe-ba1c8e0cc2ce&limit=2556")


NZ_School_sample1 <- content(School_sample_api)

School_list =sample1$result$records



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


#Iteration of  Schoollist array to gather only the required values
for (i in 1:length(School_list))
{
  
  if (is.null(School_list[[i]]$School_Id)){School_Id[i] <- ""} else{School_Id[i] <- School_list[[i]]$School_Id}
  if (is.null(School_list[[i]]$Org_Name)){Org_Name[i] <- ""} else{Org_Name[i] <- School_list[[i]]$Org_Name}
  if (is.null(School_list[[i]]$URL)){URL[i] <- ""} else {URL[i] <- School_list[[i]]$URL}
  if (is.null(School_list[[i]]$Urban_Area)) {Urban_Area[i] <- ""} else{Urban_Area[i] <- School_list[[i]]$Urban_Area}
  if (is.null(School_list[[i]]$Org_Type )){Org_Type[i] <- ""} else{Org_Type[i] <- School_list[[i]]$Org_Type}
  if (is.null(School_list[[i]]$Definition)){Definition[i] <- ""} else{ Definition[i] <- School_list[[i]]$Definition}
  if (is.null(School_list[[i]]$Authority)){Authority[i] <- ""} else{Authority[i] <- School_list[[i]]$Authority}
  if (is.null(School_list[[i]]$CoEd_Status)) {CoEd_Status[i] <- ""} else{CoEd_Status[i] <- School_list[[i]]$CoEd_Status}
  #if (is.null(sam[[i]]$Regional_Council)) {Regional_Council[i] <-  "Not"} else{Regional_Council[i] <- sam[[i]]$Regional_Council}
  if(School_list[[i]]$Regional_Council=="") {Regional_Council[i] <-  "Data unknown"} else{Regional_Council[i] <- School_list[[i]]$Regional_Council}
  if (is.null(School_list[[i]]$Territorial_Authority)) {Territorial_Authority[i] <- ""} else {Territorial_Authority[i] <- School_list[[i]]$Territorial_Authority}
  if (is.null(School_list[[i]]$Local_Office_Name)){Local_Office_Name[i] <- ""}else {Local_Office_Name[i] <- School_list[[i]]$Local_Office_Name}
  if (is.null(School_list[[i]]$Education_Region)) {Education_Region[i] <- ""} else {Education_Region[i] <- School_list[[i]]$Education_Region}
  if (is.null(School_list[[i]]$General_Electorate)){General_Electorate[i] <-""}else{General_Electorate[i] <- School_list[[i]]$General_Electorate}
  if (is.null(School_list[[i]]$Maori_Electorate)){Maori_Electorate[i] <- ""} else{Maori_Electorate[i] <- School_list[[i]]$Maori_Electorate}
  if (is.null(School_list[[i]]$Latitude)) {Latitude[i] <- ""} else {Latitude[i] <- School_list[[i]]$Latitude}
  if (is.null(School_list[[i]]$Longitude)){Longitude[i] <-""} else{Longitude[i] <- School_list[[i]]$Longitude}
  if (is.null(School_list[[i]]$Decile)) {Decile[i] <- ""} else {Decile[i] <- School_list[[i]]$Decile}
  if (is.null(School_list[[i]]$Total)) {Total[i] <-""} else{Total[i] <- School_list[[i]]$Total}
  if (is.null(School_list[[i]]$European)) {European[i] <- ""} else{European[i] <- School_list[[i]]$European}
  if (is.null(School_list[[i]]$Maori)){Maori[i] <-""} else{Maori[i] <- School_list[[i]]$Maori}
  if (is.null(School_list[[i]]$Pacific)){Pacific[i] <- ""} else{Pacific[i] <- School_list[[i]]$Pacific}
  if (is.null(School_list[[i]]$Asian)) {Asian[i] <- ""} else{Asian[i] <- School_list[[i]]$Asian}
  if (is.null(School_list[[i]]$MELAA)) {MELAA[i] <- ""} else{MELAA[i] <- School_list[[i]]$MELAA}
  if (is.null(School_list[[i]]$Other)){Other[i] <- ""} else{Other[i] <- School_list[[i]]$Other}
  if (is.null(School_list[[i]]$International)){International[i] <-""} else{International[i] <- School_list[[i]]$International}
  
}

#Creating a dataframe with columns required
School_table <- tibble(School_Id = School_Id, Org_Name= Org_Name, Urban_Area = Urban_Area, Org_Type = Org_Type, Definition = Definition, Authority = Authority, CoEd_Status = CoEd_Status ,Regional_Council= Regional_Council ,Territorial_Authority = Territorial_Authority ,Local_Office_Name = Local_Office_Name, Education_Region = Education_Region, General_Electorate = General_Electorate, Maori_Electorate = Maori_Electorate, Latitude = Latitude, Longitude = Longitude,Decile = Decile,Total = Total,European = European,Maori= Maori,Pacific = Pacific, Asian = Asian,MELAA = MELAA, Other = Other,International = International)
School_table

vis_miss(School_table)
# to make the Latitude and Lonigtude vaues as Numeric 
School_table$Latitude <- as.numeric(School_table$Latitude)
School_table$Longitude <- as.numeric(School_table$Longitude)

# to remove the null values in Latitude and longitude
School_list_drop_na_column <- School_table %>%
  drop_na(Latitude, Longitude) %>%
  filter(Regional_Council != "Data unknown")
  

vis_miss(School_list_drop_na_column)

#write_excel_csv(table1, "Map_data.csv")

# to get the top 5 regions with highest school numbers
table3<- School_list_drop_na_column %>%
  group_by(Regional_Council) %>%
  summarise(school_count = n(), Latitude =mean(Latitude), Longitude= mean(Longitude)) %>%
  arrange(desc(school_count)) %>%
  top_n(5, school_count)



#devtools::install_github("dkahle/ggmap")
library(ggplot2)
library(ggmap)

#register_google(key = "",write = TRUE)
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

