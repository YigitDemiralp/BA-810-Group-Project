library(data.table)
library(ggplot2)
library(ggthemes)
library(glmnet)
theme_set((theme_bw()))

df <- fread("C:/Users/ruchi/OneDrive/Documents/Ruchika/Boston University/BA810/Team Project/aug_train.csv")

str(df)
summary(df)

#remove null values in gender
na.omit(df, cols="gender")
sum(is.na(df$gender))

#replace missing values in enrolled university with 'no enrollment'
df[is.na(enrolled_university), enrolled_university := 'no_enrollment']
sum(is.na(df$enrolled_university))

df$enrolled_university

