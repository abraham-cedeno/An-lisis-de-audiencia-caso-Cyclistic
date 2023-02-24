# Packages installation
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# PREPARE PHASE----------------------------------------------------------------
# Import all csv files as data frames (I did this manually on the console)
BikeTrips_2021_11_04_v31 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2021_11_04_v31.csv")
BikeTrips_2021_11_04_v32 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2021_11_04_v32.csv")
BikeTrips_2021_12_08_v3 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2021_12_08_v3.csv")
BikeTrips_2022_01_06_v3 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_01_06_v3.csv")
BikeTrips_2022_02_02_v3 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_02_02_v3.csv")
BikeTrips_2022_03_02_v3 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_03_02_v3.csv")
BikeTrips_2022_04_06_v3 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_04_06_v3.csv")
BikeTrips_2022_05_03_v3 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_05_03_v3.csv")
BikeTrips_2022_06_03_v31 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_06_03_v31.csv")
BikeTrips_2022_06_03_v32 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_06_03_v32.csv")
BikeTrips_2022_07_15_v31 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_07_15_v31.csv")
BikeTrips_2022_07_15_v32 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_07_15_v32.csv")
BikeTrips_2022_08_05_v31 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_08_05_v31.csv")
BikeTrips_2022_08_05_v32 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_08_05_v32.csv")
BikeTrips_2022_09_08_v31 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_09_08_v31.csv")
BikeTrips_2022_09_08_v32 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_09_08_v32.csv")
BikeTrips_2022_10_11_v31 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_10_11_v31.csv")
BikeTrips_2022_10_11_v32 <- read_csv("Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it/Version 3/BikeTrips_2022_10_11_v32.csv")


# Data frame BikeTrips_2021_12_08_v3 - Changing column name from stated_time to started_time
colnames(BikeTrips_2021_12_08_v3)[4] <- "started_time"
colnames(BikeTrips_2022_02_02_v3)[4] <- "started_time"
colnames(BikeTrips_2022_02_02_v3)[5] <- "end_date"
colnames(BikeTrips_2022_02_02_v3)[6] <- "end_time"

# Merge all dataframes into 1 dataframe that I called BikeTrips_todos
BikeTrips_todos <- rbind(BikeTrips_2021_11_04_v31, BikeTrips_2021_11_04_v32, BikeTrips_2021_12_08_v3, BikeTrips_2022_01_06_v3, BikeTrips_2022_02_02_v3, BikeTrips_2022_03_02_v3, BikeTrips_2022_04_06_v3, BikeTrips_2022_05_03_v3, BikeTrips_2022_06_03_v31, BikeTrips_2022_06_03_v32, BikeTrips_2022_07_15_v31, BikeTrips_2022_07_15_v32, BikeTrips_2022_08_05_v31, BikeTrips_2022_08_05_v32, BikeTrips_2022_09_08_v31, BikeTrips_2022_09_08_v32, BikeTrips_2022_10_11_v31, BikeTrips_2022_10_11_v32)

# Transform two columns (started_date and end_date) from BikeTrips_todos from chr datatype to date
BikeTrips_todos$started_date <- mdy(BikeTrips_todos$started_date)
class(BikeTrips_todos$started_date)
BikeTrips_todos$end_date <- mdy(BikeTrips_todos$end_date)
class(BikeTrips_todos$end_date)

# I've sorted the date from ascending according to started_date and started_time
BikeTrips_todos<- arrange(BikeTrips_todos,started_date,started_time)

# PROCESS PHASE----------------------------------------------------------------
# I've used the function duplicated and saves the duplicates in a dataframe called duplicates (The I'ds where duplicated but the data was different)
duplicates <-BikeTrips_todos[duplicated(BikeTrips_todos$ride_id)|duplicated(BikeTrips_todos$ride_id, fromLast=TRUE),]

# I've check max length of chr datatypes
max(nchar(BikeTrips_todos$rideable_type))
min(nchar(BikeTrips_todos$rideable_type))

# I've check max length of num datatypes
max(BikeTrips_todos$start_lng)
min(BikeTrips_todos$start_lng)

# I've used this function which is similar to countIF to know how many values had more than 8 characters
sum(nchar(BikeTrips_todos$rideable_type) > 8, na.rm=TRUE)

# I've used with this function which is similar to countIF to know how many values were above 8
sum(BikeTrips_todos$start_lat > 8, na.rm=TRUE)


# I've use this function to erase all blank spaces before and after all characters 
BikeTrips_todos <- BikeTrips_todos %>% mutate(across(where(is.character), str_trim))










# ANALYSIS PHASE----------------------------------------------------------------
# S.M.A.R.T question #1
#"What was the percentual distribution of bike preferences among casual and member riders in the last 12 months?”

  
# I've created a function called conteo where the count of rides per type of riders is calculated
conteo<- function(casual_or_member) {BikeTrips_todos %>%
  filter(member_casual == casual_or_member) %>%
  group_by(rideable_type) %>% # Variable to be transformed
  count(rideable_type)  %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))  }

# I created 2 tibbles called conteo A and conteo B filtering the type of rider
conteoA <- conteo("casual")
conteoB <- conteo("member")

# I visualized the tibbles
conteoA
conteoB


# I've created a plot to visualise bike preference for casual riders in a pie chart
a1 <- ggplot(conteoA, aes(x ="", y = perc, fill = rideable_type)) +
  geom_col(color="white") +
  scale_fill_manual(values=c("#cce6ea", "#d9f150", "#3aaca6"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=5) +
  coord_polar(theta = "y") +
  labs(title = "         Bike preference - casual riders", y= "%", x= "%")
a1

# I've created a plot to visualise bike preference for member riders in a pie chart
b1 <- ggplot(conteoB, aes(x ="", y = perc, fill = rideable_type)) +
  geom_col(color="white") +
  scale_fill_manual(values=c("#cce6ea", "#3aaca6"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=5) +
  coord_polar(theta = "y") +
  labs(title = "         Bike preference - member riders", y= "%", x= "%")
b1

# Finally, I plot the two charts one above the other using the ggarrange function
theme_set(theme_grey())
vis_question1 <- ggarrange(a1, b1,
                    ncol = 1, nrow = 2)
vis_question1
















# S.M.A.R.T question #2
# “On Which days of the week have our casual riders used more often the services in the last 12 months?”


# I used the factor function to order the column of day_of_week
BikeTrips_todos_ordenado_por_dia<-BikeTrips_todos
BikeTrips_todos_ordenado_por_dia$day_of_week <- factor(BikeTrips_todos_ordenado_por_dia$day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# I created a function called day which counts the number of trips per day of the week
day <- function(casual_or_member) {BikeTrips_todos_ordenado_por_dia %>%
  filter(member_casual == casual_or_member) %>%
  group_by(day_of_week) %>% # Variable to be transformed
  count(day_of_week)  %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) } 

# I filtered the function by casual and member riders
day_casual <- day("casual")
day_member <- day("member")

day_casual
day_member


# I've created a plot to visualize day preference for casual riders in a pie chart
a2 <- ggplot(day_casual, aes(x ="", y = perc, fill = day_of_week)) +
  geom_col(color="white") +
  scale_fill_manual(values=c("#cce6ea", "#d9f150", "#3aaca6", "#AB613A", "#F7B03B", "#F7746D","#32AB60"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "         Day preference - casual riders", y= "%", x= "%")
a2

# I've created a plot to visualize day preference for member riders in a pie chart
b2 <- ggplot(day_member, aes(x ="", y = perc, fill = day_of_week)) +
  geom_col(color="white") +
  scale_fill_manual(values=c("#cce6ea", "#d9f150", "#3aaca6", "#AB613A", "#F7B03B", "#F7746D","#32AB60"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "         Day preference - member riders", y= "%", x= "%")
b2

# I plotted the two graphs in one image
vis_question2 <- ggarrange(a2, b2,
                           ncol = 1, nrow = 2)
vis_question2


# I created a plot to visualize day preference per type of rider
c2 <- ggplot(BikeTrips_todos_ordenado_por_dia, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  scale_fill_manual(values=c("#3aaca6", "#d9f150"))+
  labs(title = "         Day pereference per type of rider", y= "count of trips", x= "day of the week")
c2











# SMART Question three: 
# “At which times of the day have our members used more often the services in the last 12 months?”


# I calculated the count of trips started second by second for the whole riders, and for casual and member riders.
Tiempo_conteo <- BikeTrips_todos %>%
  group_by(started_time,member_casual) %>%
  count(started_time, sort = TRUE) 
Tiempo_conteo

Tiempo_conteo_casual <- BikeTrips_todos %>%
  filter(member_casual == "casual") %>%
  group_by(started_time) %>%
  count(started_time, sort = TRUE) 
Tiempo_conteo_casual

Tiempo_conteo_member <- BikeTrips_todos %>%
  filter(member_casual == "member") %>%
  group_by(started_time) %>%
  count(started_time, sort = TRUE) 
Tiempo_conteo_member


# I converted time to date time for plotting later on
Tiempo_conteo2 <- Tiempo_conteo%>%
  mutate( started_time= as.POSIXct(started_time), 
         started_time= if_else(started_time < as.POSIXct('1970-01-01 00:00:00', 'UTC'), started_time + 86400, started_time)) 

Tiempo_conteo_casual2 <- Tiempo_conteo_casual%>%
  mutate( started_time= as.POSIXct(started_time), 
          started_time= if_else(started_time < as.POSIXct('1970-01-01 00:00:00', 'UTC'), started_time + 86400, started_time)) 

Tiempo_conteo_member2 <- Tiempo_conteo_member%>%
  mutate( started_time= as.POSIXct(started_time), 
          started_time= if_else(started_time < as.POSIXct('1970-01-01 00:00:00', 'UTC'), started_time + 86400, started_time)) 



# I plotted casual riders against member riders
ggplot(data = Tiempo_conteo2, mapping = aes(x = started_time, y = n, color =member_casual)) + geom_point(size=3, alpha=0.5) + labs( title="# of trips per time of the day - last 12 months", y= "count of trips", x= "time measured by seconds") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_datetime(date_labels = '%H:%M', 
                   limits = c(as.POSIXct('1970-01-01 00:00:00', tz = 'UTC'), 
                              as.POSIXct('1970-01-02 00:00:00', tz = 'UTC')), 
                   breaks = '2 hour')



# I assigned the plot of casual riders into a3
a3 <- ggplot(data = Tiempo_conteo_casual2, mapping = aes(x = started_time, y = n)) +  geom_point(size=3, alpha=0.5, color="#f5746c") + geom_smooth()+
  labs(title="# of trips per time of the day for casual riders- last 12 months", y= "count of trips", x= "time measured by seconds")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="none")+
  scale_x_datetime(date_labels = '%H:%M', 
                   limits = c(as.POSIXct('1970-01-01 00:00:00', tz = 'UTC'), 
                              as.POSIXct('1970-01-02 00:00:00', tz = 'UTC')), 
                   breaks = '2 hour')


# I assigned the plot of member riders into b3
b3 <- ggplot(data = Tiempo_conteo_member2, mapping = aes(x = started_time, y = n)) +  geom_point(size=3, alpha=0.5, color="#19b5bb") + geom_smooth()+
  labs(title="# of trips per time of the day for member riders- last 12 months", y= "count of trips", x= "time measured by seconds") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="none")+
  scale_x_datetime(date_labels = '%H:%M', 
                   limits = c(as.POSIXct('1970-01-01 00:00:00', tz = 'UTC'), 
                              as.POSIXct('1970-01-02 00:00:00', tz = 'UTC')), 
                   breaks = '2 hour')
  


# I created a picture with both visualizations a3 and b3
vis_question3 <- ggarrange(a3, b3,
                           ncol = 1, nrow = 2)
vis_question3


























# SMART Questions four and five: 
# “What was the average trip duration of our members in the last 12 months?"
# “What was the average trip duration of our casual riders in the last 12 months?”



# I converted time to date time for plotting later on
BikeTrips_todos_casual <- BikeTrips_todos%>%
  filter(member_casual == "casual") %>%
  mutate( ride_length= as.POSIXct(ride_length), 
          ride_length= if_else(ride_length < as.POSIXct('1970-01-01 00:00:00', 'UTC'), ride_length + 86400, ride_length)) 

BikeTrips_todos_member <- BikeTrips_todos%>%
  filter(member_casual == "member") %>%
  mutate( ride_length= as.POSIXct(ride_length), 
          ride_length= if_else(ride_length < as.POSIXct('1970-01-01 00:00:00', 'UTC'), ride_length + 86400, ride_length)) 



# I calculated the mean for casual riders and convert it into a numeric value
mean1 <- BikeTrips_todos_casual %>%
   summarize(mean(ride_length))

mean1_numeric <- as.numeric(mean1)
mean1


# I calculated the median for casual riders and convert it into a numeric value
median_casual <- BikeTrips_todos_casual %>%
  summarize(median(ride_length))

median1_numeric <- as.numeric(median1)
median_casual


# I calculated the mean for member riders and convert it into a numeric value
mean2 <- BikeTrips_todos_member %>%
  summarize(mean(ride_length))

mean2_numeric <- as.numeric(mean2)
mean2

# I calculated the median for casual riders and convert it into a numeric value
median_member <- BikeTrips_todos_member %>%
  summarize(median(ride_length))

median2_numeric <- as.numeric(median2)
median_member






# I created two data frames with the means so that I can plot them later on

mean1_vertical <- data.frame(x=c(mean1_numeric,mean1_numeric), y=c(-5,5))

mean2_vertical <- data.frame(x=c(mean2_numeric,mean2_numeric), y=c(-5,5))



# I plotted a histogram with the mean 
ggplot(BikeTrips_todos_casual, aes(x=ride_length)) + geom_histogram(fill="#3aaca6", color="white") +
  geom_vline(data=mean1_vertical, aes(x, y), xintercept = mean1_numeric , color="#AB613A")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_datetime(date_labels = '%H:%M', 
                   limits = c(as.POSIXct('1970-01-01 00:00:00', tz = 'UTC'), 
                              as.POSIXct('1970-01-01 01:00:00', tz = 'UTC')), 
                   breaks = '10 mins') +
  labs(title = "Distribution of trip duration and average trip duration - Casual riders", y= "count of trips", x= "trip duration")




# I plotted a histogram with the mean 
ggplot(BikeTrips_todos_member, aes(x=ride_length)) + geom_histogram(fill="#3aaca6", color="white") +
  geom_vline(data=mean2_vertical, aes(x, y), xintercept = mean2_numeric , color="#AB613A")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_datetime(date_labels = '%H:%M', 
                   limits = c(as.POSIXct('1970-01-01 00:00:00', tz = 'UTC'), 
                              as.POSIXct('1970-01-01 01:00:00', tz = 'UTC')), 
                   breaks = '10 mins') +
  labs(title = "Distribution of trip duration and average trip duration - Member riders", y= "count of trips", x= "trip duration")
















# SMART Questions six: 
#“On which stations did the member riders start their rides more often in the last 12 months?

# I printed the top ten start stations for member riders
start_station_member <- BikeTrips_todos %>%
  filter(member_casual == "member") %>%
  count(start_station_name, sort = TRUE)

start_station_member %>%
print(n=11 )


# I printed the top ten start stations for casual riders
start_station_casual <- BikeTrips_todos %>%
  filter(member_casual == "casual") %>%
  count(start_station_name, sort = TRUE) 

  start_station_casual %>%
  print(n=11 )



  
#SMART Question Seven
  # I printed the top ten end stations for casual riders

end_station_casual <- BikeTrips_todos %>%
  filter(member_casual == "casual") %>%
  count(end_station_name, sort = TRUE) 

end_station_casual %>%
print(n = 11)


# I printed the top ten end stations for member riders
end_station_member <- BikeTrips_todos %>%
  filter(member_casual == "member") %>%
  count(end_station_name, sort = TRUE) 


end_station_member %>%
  print(n = 11)




#SMART Question eight, nine, ten, eleven
#"What was the distribution of rides starting point per longitude for both casual and member riders in the last 12 months?”
#“What was the distribution of rides starting point per latitude for both casual and member riders in the last 12 months?”
#"What was the distribution of rides ending point per longitude for both casual and member riders in the last 12 months?”
#“What was the distribution of rides ending point per latitude for both casual and member riders in the last 12 months?”



#I determined the range of start latitudes of trips
max(BikeTrips_todos$start_lat)
min(BikeTrips_todos$start_lat)

# I plotted a histogram to see the distribution of start latitudes
ggplot(BikeTrips_todos, aes(x=start_lat)) + geom_histogram(fill="#3aaca6", color="white") +
  facet_wrap(~member_casual)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_continuous(limits = c(41.75, 42.1)) +
  labs(title = "Distribution of starting latitude of trips for casual and member riders", y= "count of trips", x= "starting latitude")









#I determined the range of start longitudes of trips

max(BikeTrips_todos$start_lng)
min(BikeTrips_todos$start_lng)

# I plotted a histogram to see the distribution of start longitudes
ggplot(BikeTrips_todos, aes(x=start_lng)) + geom_histogram(fill="#3aaca6", color="white") +
  facet_wrap(~member_casual)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_continuous(limits = c(-87.8, -87.55)) +
  labs(title = "Distribution of starting longitude of trips for casual and member riders", y= "count of trips", x= "starting longitude")





#I determined the range of end latitudes

max(BikeTrips_todos$end_lat)
min(BikeTrips_todos$end_lat)

# I plotted a histogram to see the distribution of end latitudes
ggplot(BikeTrips_todos, aes(x=end_lat)) + geom_histogram(fill="#3aaca6", color="white") +
  facet_wrap(~member_casual)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_continuous(limits = c(41.70, 42.10)) +
  labs(title = "Distribution of ending latitudes of trips for casual and member riders", y= "count of trips", x= "ending latitude")



#I determined the range of end longitudes

max(BikeTrips_todos$end_lng)
min(BikeTrips_todos$end_lng)

# I plotted a histogram to see the distribution of end longitudes
ggplot(BikeTrips_todos, aes(x=end_lng)) + geom_histogram(fill="#3aaca6", color="white") +
  facet_wrap(~member_casual)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  scale_x_continuous(limits = c(-87.80, -87.55)) +
  labs(title = "Distribution of ending longitudes of trips for casual and member riders", y= "count of trips", x= "ending longitude")










# Finally, I exported some data to import it in tableau

write.csv(Tiempo_conteo2, "Desktop/AC/Useful/Carrer/Cursos/2. Data analysis-Google/Capstone Case study 1/How I solved it/Data - I organize it\\BikeTrips_todos_R_Export.csv", row.names=FALSE)