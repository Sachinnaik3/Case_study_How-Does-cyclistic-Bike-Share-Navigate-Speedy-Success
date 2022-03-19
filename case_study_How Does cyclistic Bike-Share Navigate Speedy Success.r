install.packages("tidyverse")

library(tidyverse)

df1=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-04-divvy-tripdata.csv")

df2=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-05-divvy-tripdata.csv")

df3=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-06-divvy-tripdata.csv")

df4=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-07-divvy-tripdata.csv")

df5=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-08-divvy-tripdata.csv")

df6=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-09-divvy-tripdata.csv")

df7=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-10-divvy-tripdata.csv")

df8=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-11-divvy-tripdata.csv")

df9=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-12-divvy-tripdata.csv")

df10=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2022-01-divvy-tripdata.csv")

df11=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2022-02-divvy-tripdata.csv")

df12=read.csv("C:/Users/sachi/OneDrive/Desktop/Case Study_ Bike_Share/Bike_Share_12_month_data/Original_data_files/2021-03-divvy-tripdata.csv")

head(df1)

colnames(df1)

colnames(df11)

install.packages("janitor")

library(janitor)

compare_df_cols(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

union_df=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

head(union_df)

tail(union_df)

install.packages("readr")
install.packages("dplyr")
library(readr)
library(dplyr)

install.packages("skimr")
install.packages("here")
library(skimr)
library(here)

library(lubridate)

skim_without_charts(union_df)

union_df%>%
    distinct(.keep_all = TRUE) %>%
    skim_without_charts()


new_df=union_df

class(new_df$started_at)
class(new_df$ended_at)

new_df$started_at=ymd_hms(new_df$started_at)
new_df$ended_at=ymd_hms(new_df$ended_at)

class(new_df$started_at)
class(new_df$ended_at)

new_df$weekday=weekdays(new_df$started_at)

new_df$month=months(new_df$started_at)

month=months(new_df$started_at)

str(new_df)

# weekday and month has been added to dataframe

new_df$weekend_weekday=ifelse(new_df$weekday==c("Saturday","Sunday"),"weekend","weekday")

str(new_df)

new_df$duration_hr=round(difftime(new_df$ended_at,new_df$started_at,units="hours"),digits = 2)

str(new_df)

head(new_df)

# number of casul and menber riders 
new_df %>%
      group_by(member_casual) %>%
      summarise(rider_count = n())
options(repr.plot.width = 5, repr.plot.height = 2.1)
new_df %>%
      group_by(member_casual) %>%
      summarise(rider_count = n())%>%
      ggplot()+
      geom_col(mapping = aes(x=member_casual,y=rider_count,fill=member_casual))+
      scale_y_continuous(labels = comma)
      

#ride time of casual and member rider

new_df %>%
      group_by(member_casual) %>%
      summarise(ride_duration=sum(duration_hr))

options(repr.plot.width = 5, repr.plot.height = 2.1)
new_df %>%
      group_by(member_casual) %>%
      summarise(ride_duration=sum(duration_hr))%>%
      ggplot()+
      geom_col(mapping = aes(x=member_casual,y=ride_duration,fill=member_casual)) +
      labs(title = "Ride time of casual and member rider" )+
      scale_y_continuous(labels = comma)


### avg ride time of casual and member rider
new_df %>%
      group_by(member_casual) %>%
      summarise(avg_ride_duration=mean(duration_hr))

options(repr.plot.width = 5, repr.plot.height = 2.1)
new_df %>%
      group_by(member_casual) %>%
      summarise(avg_ride_duration=mean(duration_hr))%>%
      ggplot()+
      labs(title = "Avg ride time of casual and member rider" )+
      geom_col(mapping = aes(x=member_casual,y=avg_ride_duration,fill=member_casual))

### ride count of casul and member riders during week 

options(repr.plot.width = 7, repr.plot.height = 2.4)
new_df %>%
      group_by(member_casual,weekday,weekend_weekday,month) %>%
      summarise(ride_count=n(),ride_duration=sum(duration_hr),avg_ride_duration=mean(duration_hr))%>%
      ggplot(mapping = aes(x=reorder(weekday,ride_count),y=ride_count,fill=member_casual))+
      labs(title = "Ride count of casul and member riders during week" )+
      geom_col(position = "dodge") +
      scale_y_continuous(labels = comma)

### ride count of casul and member riders during weekday and weekend 

options(repr.plot.width = 7, repr.plot.height = 2)
new_df %>%
      group_by(member_casual,weekday,weekend_weekday,month) %>%
      summarise(ride_count=n(),ride_duration=sum(duration_hr),avg_ride_duration=mean(duration_hr))%>%
      ggplot(mapping = aes(x=reorder(weekend_weekday,ride_count),y=ride_count,fill=member_casual))+
      labs(title = "Ride count of casul and member riders during weekday and weekend " )+
      geom_col(position = "dodge") +
      scale_y_continuous(labels = comma)




###  Ride count of casul and member riders by month
options(repr.plot.width = 10, repr.plot.height = 3)
new_df %>%
      group_by(member_casual,weekday,weekend_weekday,month) %>%
      summarise(ride_count=n(),ride_duration=sum(duration_hr),avg_ride_duration=mean(duration_hr))%>%
      ggplot(mapping = aes(x=reorder(month,ride_count),y=ride_count,fill=member_casual,color=member_casual))+
      labs(title = "Ride count of casul and member riders by month" )+
      geom_col(position = "dodge") +
      scale_y_continuous(labels = comma)

17445

### Total ride durarion of casul and member riders  by month
options(repr.plot.width = 10, repr.plot.height = 3)
new_df %>%
      group_by(member_casual,weekday,weekend_weekday,month) %>%
      summarise(ride_count=n(),ride_duration=sum(duration_hr),avg_ride_duration=mean(duration_hr))%>%
      ggplot(mapping = aes(x=reorder(month,ride_count),y=avg_ride_duration,fill=member_casual,color=member_casual))+
      labs(title = "Total ride durarion of casul and member riders by month" )+
      geom_col(position = "dodge") +
      scale_y_continuous(labels = comma)

### ridable type by riders type

options(repr.plot.width = 18, repr.plot.height = 5)
new_df %>%
group_by(member_casual,rideable_type)%>%
summarise(ride_count=n())%>%
     ggplot(aes(x="",y=ride_count,fill=rideable_type))+
     geom_bar(stat = "identity",width = 2,position = "fill")+
     coord_polar(theta = "y")+
     facet_wrap(~member_casual)+
     theme(strip.text = element_text(size = 30),legend.title =element_text(size =(28)),legend.text = element_text(size = 20))

