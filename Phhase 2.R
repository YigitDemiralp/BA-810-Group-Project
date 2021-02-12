library(data.table)
library(ggplot2)
library(scales)

df <- read.csv("C:/Users/YEET/Documents/BU/Classes/ba 810/Project/aug_train.csv")
head(df)

levels(df$last_new_job) <- c(levels(df$last_new_job), 0) 
df$last_new_job[df$last_new_job  == 'never']  <- 0 
df$last_new_job[df$last_new_job == ""]<-0

head(df)

unique(df['last_new_job'])


df[which((df$company_type == 'Early Stage Startup') & (df$company_size == '10/49'))]

table(df$last_new_job)



