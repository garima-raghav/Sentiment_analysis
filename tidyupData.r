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

#Top-ten Clothing ID

REVIEWS = main %>%
  group_by(`Clothing ID`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(clothid = reorder(`Clothing ID`,Count)) %>%
  head(10)
  
 REVIEWS %>%
  ggplot(aes(x = clothid, y = Count)) +
  geom_bar(stat='identity',colour="white", fill = "orange") +
  geom_text(aes(x = clothid, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'ID of Cloth', 
       y = 'Count', 
       title = 'Clothing ID and Count') +
  coord_flip() +
  theme_bw()
 
 write.csv(main, file="/home/khalsa/sentiment Analysis/Tidydata.csv")
