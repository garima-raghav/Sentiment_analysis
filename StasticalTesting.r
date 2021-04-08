tidyD <- read.csv(file= "/home/khalsa/sentiment Analysis/Tidydata.csv")

#hypothesis :- Rating and Liked(+ve Feedback) of products must be interrelated

table(tidyD$Rating)
table(tidyD$Liked)
table(tidyD$Rating,tidyD$Liked)

#checking statistical independance between rating and liked(+ve feedback) using chi-squared test
#as we have categorical data
summary(table(tidyD$Rating,tidyD$Liked)) #less p-value indicates there is no realtion
#between rating and liking of any product, not compulsurily highly rated product got more likes
#but practically speaking -> highly rated got more likes compared to others

mean(tidyD$Rating) #true mean
sd(tidyD$Rating)   #true SD

#NORMALIZING DATA
scale(tidyD$Rating)
str(tidyD)
x <- rnorm(100, mean = 4, sd=1)
x
t.test(x, mu= 4, conf.level = 0.99) # p-value>0.05 indicating acceptance of null hypothesis
#confidence level for median
wilcox.test(x, conf.int = T) #rejecting null hypothesis


#ACC. to hypothesis let mean=4 with confidence of 80%
t.test(tidyD$Rating, mu= 4, conf.level = 0.80)#smaller p value indicated rejection of hypothesis
#indicating mean is <4 or >4

#confidence level for median
wilcox.test(tidyD$Rating, conf.int = T) #rejecting null hypothesis

#Testing for NORMALITY
shapiro.test(x) #normally distributed


#checking for correlation between Rating and Id of product
#as our data is normally distributed we will use "PEARSON METHOD"

cor(tidyD$Rating, tidyD$ID)
cor.test(tidyD$Rating, tidyD$ID)# significant correlation doesn't exists
#that means a product with same ID must have got different ratings by different customers
#of different age group


#even the correlation between Customer Age and Product Id doesn't exist indicating
#Different choice for one particular age group
cor(tidyD$Age, tidyD$ID)
cor.test(tidyD$Age, tidyD$ID)


