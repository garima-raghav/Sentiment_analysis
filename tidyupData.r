library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidytext)
women <- read.csv(file= "/home/khalsa/sentiment Analysis/Womens Clothing E-Commerce Reviews.csv")
summary(women)  #23486 observations of 11 variables

#deleting the first columns without name specifying the S.No.
women <- women[-1] # 10 variables

#renaming column names
colnames(women) <- c('ID', 'Age', 'Title', 'Review', 'Rating', 'Recommend', 'Liked', 'Division', 'Dept', 'Class')

#filtering according to the reviews , eliminating ones which have null(NA) value
clothesr <- women %>% filter(!is.na(Review))  #22641 obs of 10 variables

#extracting rows with or without title
notitle <- clothesr %>% filter(is.na(Title)) %>% select(-Title)
wtitle <- clothesr %>% filter(!is.na(Title)) %>% unite(Review, c(Title, Review), sep = ' ')

#reducing column by merging title and review
main <- bind_rows(notitle, wtitle)  #22641 obs. of 9 variables

 write.csv(main, file="/home/khalsa/sentiment Analysis/Tidydata.csv")
