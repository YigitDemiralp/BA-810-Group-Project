## Imports
library(data.table)
library(ggplot2)
library(ggthemes)
library(glmnet)
library(tidyr)
library(corrplot)
library(RColorBrewer)
library(dplyr)
theme_set(theme_bw())

## Loading the data

df <- read.csv("C:/Users/YEET/Documents/BU/Classes/ba 810/Project/aug_train.csv")
df <- as.data.table(df)


#viewing the head of dataset
head(df)

#structure of the dataset
str(df)

#summary to view data types and missing values 
summary(df)


## Data Cleaning

#assigning missing values of 'last_new_job' to 0 
#updating observations with 'never' to 0
levels(df$last_new_job) <- c(levels(df$last_new_job), 0) 
df$last_new_job[df$last_new_job  == 'never']  <- 0 
df$last_new_job[df$last_new_job == ""]<-0
table(df$last_new_job)


# Drop "Primary School" under the education_level column 
df <-df[!(education_level) %like% 'Primary School']

#Drop missing values for education level
df[df$enrollee_id == ''] = NA
df[df$city == ''] = NA
df[df$enrolled_university == ''] = NA
df[df$city_development_index == ''] = NA
df[df$gender == ''] = NA
df[df$relevent_experience == ''] = NA
df[df$education_level == ''] = NA
df[df$major_discipline == ''] = NA# #Drop missing values for major discipline
df[df$experience == ''] = NA
df[df$company_size == ''] = NA
df[df$company_type == ''] = NA
df[df$last_new_job == ''] = NA
df[df$training_hours == ''] = NA
df[df$target == ''] = NA


#Experience for more than 4 becomes 5, never becomes 0
levels(df$last_new_job) <- c(levels(df$last_new_job), '5') 
df$last_new_job[df$last_new_job == '>4'] <- '5'
unique(df$last_new_job)

levels(df$last_new_job) <- c(levels(df$last_new_job), '0') 
df$last_new_job[df$last_new_job == 'never'] <- '0'
#Change column to numeric
df$last_new_job <- as.numeric(as.character(df$last_new_job))

#Company size for less than 10 becomes 0-9,
levels(df$company_size) <- c(levels(df$company_size), '0-9')
df$company_size[df$company_size == '<10'] <- '0-9'

#Excperience for more than 20 becomes 21                                        
levels(df$experience) <- c(levels(df$experience), '21')
df$experience[df$experience == '>20'] <- '21'
unique(df$experience)

#Excperience for less than 1 becomes 0
levels(df$experience) <- c(levels(df$experience), '0')
df$experience[df$experience == '<1'] <- '0'

#Change column to numeric
df$experience <- as.numeric(as.character(df$experience))

#replace missing values in enrolled university with 'no enrollment'
df[is.na(enrolled_university), enrolled_university := 'no_enrollment']
#confirming that there are 0 observations with null values in enrolled_university
sum(is.na(df$enrolled_university))


#confirming there are no missing values for education_level and major_discipline
df[!(is.na(dd$education_level)) & !(is.na(dd$major_discipline))]

# Company-size:
# 1) Impute missing values to mode ("50-99" has the highest frequency)
a <- table(df$company_size)   
# count the values
a

df$company_size[is.na(df$company_size)] <- '50-99'

#conforming that there are no missing values in company size 
unique(df$company_size)    

# 2) change '10/49' to '10-49'
levels(df$company_size) <- c(levels(df$company_size), '10-49') 
df$company_size[df$company_size == '10/49'] <- '10-49'
unique(df$company_size)  


# 4) Company-type: drop missing values
df <- na.omit(df)
sum(is.na(df))
str(df)



M <-cor(select(df, city_development_index, target, training_hours))
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



#Education level partition
# count how many people in different education level where target equal to 1
phd <- df[df$education_level == 'Phd' & df$target == 1, .N ]
mas <- df[df$education_level == 'Masters' & df$target == 1, .N]
gra <- df[df$education_level == 'Graduate' & df$target == 1, .N]
# the percentage of people in different education level where target equal to 1
all_target <- df[df$target == 1, .N]
phd_p <- phd/all_target
mas_p <- mas/all_target
gra_p <- gra/all_target

ggplot(data = df) +
  geom_bar(mapping = aes(x = education_level))


#Training hour regarding target
ggplot(data = df, mapping = aes(group = target, y = training_hours)) +
  geom_boxplot()
#experience regarding experience
ggplot(data = df) +
  geom_count(mapping = aes(x = target, y = experience))
#discuss city development index
ggplot(data = df) +
  geom_count(mapping = aes(x = target, y = city_development_index))
ggplot(data = df) +
  geom_bar(mapping = aes(x = city_development_index))
ggplot(data = df, mapping = aes(x = city_development_index)) +
  geom_freqpoly(mapping = aes(group = target,color = target), binwidth = .01)


# count how many people in different major where target equal to 1
stem <- df[df$major_discipline == 'STEM' & df$target == 1, .N ]
hum <- df[df$major_discipline == 'Humanities' & df$target == 1, .N]
other <- df[df$major_discipline == 'Other' & df$target == 1, .N]
bus <- df[df$major_discipline == 'Business Degree' & df$target == 1, .N]
art <- df[df$major_discipline == 'Arts' & df$target == 1, .N]
# the percentage of people in different major where target equal to 1
stem_p <- stem/all_target
hum_p <- hum/all_target
other_p <- other/all_target
hum_p <- hum/all_target
art_p <- art/all_target
# plot of major discipline
ggplot(df, aes(df$major_discipline)) +
  geom_bar()


# gender count
ggplot(data = df) +
  stat_count(mapping = aes(x = gender))
# min, max and median training hours
ggplot(data = df) +
  stat_summary(
    mapping = aes(x = company_type, y = training_hours),
    fun.min = min,
    fun.max = max,
    fun = median
  )
#gender count by company type
ggplot(data = df) + geom_bar(mapping = aes(x = company_type, fill = gender))
#gender count by last new job
ggplot(data = df) +
  geom_bar(mapping = aes(x = gender, fill = last_new_job), position = "dodge")
#detecting outliers for training hours by gender
ggplot(data = df, mapping = aes(x = gender, y = training_hours)) +
  geom_boxplot()
# university enrollments by education level
bar <- ggplot(data = df) +
  geom_bar(
    mapping = aes(x = enrolled_university, fill = education_level),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()
# University Enrollments by Training Hours
ggplot(df, aes(x=enrolled_university, y=training_hours)) +
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  labs(title="University Enrollments by Training Hours") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Plot
ggplot(df, aes(x=education_level, y=training_hours)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=education_level, 
                   xend=education_level, 
                   y=0, 
                   yend=training_hours)) + 
  labs(title="Lollipop Chart", 
       subtitle="education_level Vs training_hours", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
