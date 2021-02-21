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
sum(is.na(df))

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





