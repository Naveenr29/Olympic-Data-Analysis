##### Olympics Games Analysis #####
##### Naveen N Peddyreddy #########


#### Install required Packages ####

#install.packages("dplyr")
#install.packages("stats")
#install.packages("tidyverse")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")

library(dplyr)
library(stats)
library(tidyverse)
library(rpart)
library(rpart.plot)



#Read the data
athlete_events <- read_csv(file.choose()) #choose the file athlete_events.csv
noc_regions <- read_csv(file.choose())  # choose the file noc_regions.csv

#Updating the Attributes to remove NA values
#Cleaning Data

athlete_events$Weight <- ifelse(is.na(athlete_events$Weight),
                                ave(athlete_events$Weight,FUN = function(x) mean(x,na.rm = TRUE)),
                                athlete_events$Weight)

athlete_events$Height <- ifelse(is.na(athlete_events$Height),
                                ave(athlete_events$Height,FUN = function(x) mean(x,na.rm = TRUE)),
                                athlete_events$Height)

athlete_events$Age <- ifelse(is.na(athlete_events$Age),
                             ave(athlete_events$Age,FUN = function(x) mean(x,na.rm = TRUE)),
                             athlete_events$Age)

class(athlete_events$Medal)
athlete_events$Medal[is.na(athlete_events$Medal)] <- "No Medal" #NA value in medal is Non Winners

noc_regions$notes = NULL
noc_regions1 <- na.omit(noc_regions)  #omitting NA values
#colSums(is.na(noc_regions))

#Dimensions of data
dim(athlete_events)

#Name of the columns
names(athlete_events)

#Checking for null values
is.null(athlete_events)

#Creating a Data Frame
athlete_events = as.data.frame(athlete_events)
str(athlete_events)

#Summary of data
summary(athlete_events)

#Data Exploration-(EDA) with plots

#Getting the count of Athletes, Nations and events

counts <- athlete_events  %>%
  group_by(Year, Season) %>%
  summarize(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )

counts

#Plotting the participation statistics

#Number of Athletes
athletes_plot <- ggplot(counts, aes(x=Year, y=Athletes, group=Season, color=Season)) +
  geom_point(size=2) + ggtitle("Number of Athletes participated over the Years") +
  geom_line() 

athletes_plot

#Number of Nations
nations_plot <- ggplot(counts, aes(x=Year, y=Nations, group=Season, color=Season)) +
  geom_point(size=2) + ggtitle("Number of Countries participated over the Years") +
  geom_line() 

nations_plot

#Number of Events
events_plot <- ggplot(counts, aes(x=Year, y=Events, group=Season, color=Season)) +
  geom_point(size=2) + ggtitle("Number of Events occurred over the Years") +
  geom_line() 

events_plot

#Number of Male and Female participants over the years 

mf_counts <- athlete_events %>% filter(Sport != "Art Competitions") #filtering all competitions except art competition
original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  mf_counts$Year <- gsub(original[i], new[i], mf_counts$Year)
}
mf_counts$Year <- as.integer(mf_counts$Year)

counts_sex <- mf_counts %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)

gender_plot <- ggplot(counts_sex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  labs(title = "Number of Male and Female participation over the Years") +
  theme(plot.title = element_text(hjust = 0.5))

gender_plot

#Winning Trends

#Total number of medals for top 10 nations
medal_counts <- athlete_events %>%
  filter(Medal != "No Medal") %>% #Filtering countries who have not received any medals
  group_by(Team) %>%
  summarise(Total = length(Medal))%>%
  arrange(desc(Total)) %>%  #for most number of medals
  ungroup() %>%
  mutate(country = reorder(Team,Total)) %>%
  top_n(10) #for top 10 countries with most medals

#plotting the top 10 countries with most medals  
total_medals_plot <- ggplot(medal_counts, aes(x = country,y = Total)) +
  geom_bar(stat='identity',colour="white", fill = "lightgreen") +
  geom_text(aes(x = country, y = .1, label = paste0("(",round(Total,2),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(title = "Top 10 Nations - Total Number of Medals") +
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10)) +
  labs(x = 'Country', 
       y = 'Number of Medals'
  ) +
  coord_flip() + 
  theme_bw()

total_medals_plot

#Medal count for women in 1936 olympics

# Counting the number of medals awarded to each location in 1936 Olympics
counts_1936 <- athlete_events %>% filter(Year==1936, Medal != "No Medal", Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

lev_1936 <- counts_1936 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_1936$NOC <- factor(counts_1936$NOC, levels=lev_1936$NOC)

# Plot 1936
ggplot(counts_1936, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Number of medals for women at the 1936 Olympics") +
  theme(plot.title = element_text(hjust = 0.5))

#Medal count for women in 2016 olympics

# Counting the number of medals awarded to each location in 1936 Olympics
counts_2016 <- athlete_events %>% filter(Year==2016, Medal != "No Medal", Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

lev_2016 <- counts_2016 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_2016$NOC <- factor(counts_2016$NOC, levels=lev_2016$NOC)

# Plot 2016
ggplot(counts_2016, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Number of medals for women at the 2016 Olympics") +
  theme(plot.title = element_text(hjust = 0.5))


#Countries with the most art medal
# Subset to Art Competitions and variables of interest
art <- athlete_events %>% 
  filter(Sport == "Art Competitions") %>%
  select(Name, Sex, Age, Team, NOC, Year, City, Event, Medal)

#Number of medals per team
medal_counts_art <- art %>% filter(!is.na(Medal), Medal != "No Medal")%>%
  group_by(Team, Medal) %>%
  summarize(Count=length(Medal)) 

# order Team by total medal count
lev_art <- medal_counts_art %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Team)
medal_counts_art$Team <- factor(medal_counts_art$Team, levels=lev_art$Team)

# plot
ggplot(medal_counts_art, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Historical medal counts from Art Competitions") +
  theme(plot.title = element_text(hjust = 0.5))


#Analyzing the Heighy and Weight factors for winning medals

ggplot(subset(athlete_events,Medal!="No Medal"), aes(x = Medal, y = Height)) + geom_boxplot(fill="cyan") + facet_grid(.~Sex) 
#Those who did not receive any medals had lower height in case of both genders. Also, Those males who won gold medals have higher median height.

ggplot(subset(athlete_events,Medal!="No Medal"), aes(x = Medal, y = Weight)) + geom_boxplot(fill="cyan") + facet_grid(.~Sex) 
#Similarly, Those who did not receive any medals had lower weight in case of both genders


##### Analysis - Decision Tree #####

attach(athlete_events)

#Setting some variables as NULL which has no Correlation with the prediction
athlete_events$ID<-NULL
athlete_events$Name <- NULL
athlete_events$Games <- NULL
athlete_events$Event <- NULL
athlete_events$NOC <- NULL
athlete_events$City <- NULL
athlete_events$Sport <- NULL
athlete_events$Team <- NULL

#converting all the values as numeric for classification
athlete_events$Age <- as.numeric(athlete_events$Age)
athlete_events$Height <- as.numeric(athlete_events$Height)
athlete_events$Year <- as.numeric(athlete_events$Year)


str(athlete_events) #viewing the structure

dt_model<-rpart(Season~.,data = athlete_events)

rpart.plot(dt_model,type = 1) #plotting tree for the model

library(caret)
#Splitting the data into test and train (75-25%)
index_dt_1 <- createDataPartition(athlete_events$Season, p = 0.75, list = FALSE)
train_dt_1 <- athlete_events[index_dt_1,]
test_dt_1 <- athlete_events[-index_dt_1,]

#train model
dt_model_train <- rpart(Season~., data = train_dt_1)
summary(dt_model_train)

rpart.plot(dt_model_train, type = 1) 

#predicting Values using train data

p1 <- predict(dt_model_train, type = "class")

l1 <- table(p1, train_dt_1$Season)  #creating a confusion matrix
l1
sum(diag(l1))/sum(l1) #calculating the accuracy for training dataset.

#Accuracy of training dataset is ~91.19%

#test model
dt_model_test <- rpart(Season~., data = test_dt_1)
summary(dt_model_test)

rpart.plot(dt_model_test, type = 1) 

#predicting values using test data

p2 <- predict(dt_model_test, type = "class")

l2 <- table(p2, test_dt_1$Season) #creating a confusion matrix
l2
sum(diag(l2))/sum(l2) #calculating the accuracy of testing dataset

# Accuracy os testing dataset is ~90.99%

