## Imports
library(data.table)
library(ggplot2)
library(ggthemes)
library(glmnet)
library(tidyr)
theme_set(theme_bw())

## Loading the data

df <- read.csv("C:/Users/YEET/Documents/BU/Classes/ba 810/Project/aug_train.csv")

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

#remove null values in gender
na.omit(df, cols="gender")
#confirming that there are 0 observations with null values in gender
sum(is.na(df$gender))

#replace missing values in enrolled university with 'no enrollment'
df[is.na(enrolled_university), enrolled_university := 'no_enrollment']
#confirming that there are 0 observations with null values in enrolled_university
sum(is.na(df$enrolled_university))

# Drop “Primary School” under the education_level column 
df <-df[!(education_level) %like% “Primary School”]

#Drop missing values for education level
df[df$education_level == ‘’] = NA

#Drop missing values for major discipline 
df[df$major_discipline == ‘’] = NA

#confirming there are no missing values for education_level and major_discipline
df <- df[!(is.na(dd$education_level)) & !(is.na(dd$major_discipline))]

# Company-size:
# 1) Impute missing values to mode (“50-99” has the highest frequency)
a <- table(df$company_size)   
# count the values
a
df$company_size[is.na(df$company_size)] <- '50-99'

#conforming that there are no missing values in company size 
unique(df[c(“company_size”)])    

# 2) change ‘10/49’ to ’10-49
df$company_size[df$company_size == ‘10/49’] <- ‘10-49’
unique(df[c(“company_size”)])

# 3) if the company belongs to an early stage start up then it falls under 10-49
# only 427. Should we really need to change?
b <- df[ which( df$company_type == “Early Stage Startup” & df$company_size != '10-49') , ]
c <- data.frame(b)
c


# 4) Company-type: drop missing values
unique(df[c(“company_type”)])
ct <- na.omit(df, cols=c(“company_type”))
sum(is.na(md))