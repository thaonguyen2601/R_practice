# Projects with R basic & advance for data analysis and visualization

These are exercises to practice and enhance my self-learn R skills to work with data & do some interesting thing:
  - Data Preparation
  - Data Mining
  - Data Exploration
  - Data Analysis
  - Data Visualization

# Exercises: 
Working with data of more than 600 movies in different genre, ratings, budget & profits to figure out helpful insights


**Step 1: Install all packages for further use**
```sh
install.packages("ggplot2")
install.packages("dplyr")
install.packages('hrbrthemes')
install.packages('viridis')
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(reshape2)
```
**Step 2: Insert CSV file & look through data**
```sh
setwd('D:\\Download\\R practice')
movie <- read.csv('Section6-Homework-Data-2.csv')
head(movie)
str(movie)
```
![alt text](headmovie.PNG "Logo Title Text 1")
![alt text](str.PNG "Logo Title Text 1")

=> Movie data set has a wide range of genre and studios. If we keep it that way, visualization will get some troubles since there are so many variables. 

**Step 3: Classify data to select top 6 Genre/Studios by sales/numbers of movies, asign value "Others" to the rest of Genre/Studios and create new dataframe**
```sh
choosing <- movie %>%
  group_by(Genre) %>%
  summarise(sum_adj = sum(AdjGrossM))
choosing[order(unlist(choosing$sum_adj), decreasing = TRUE),]
```
![alt text](choosing.PNG "Logo Title Text 1")

=> base on choosing => list out top 6 genres & asign the rest as "Others"
```sh
`%nin%` = Negate(`%in%`)

movie.data <- subset(movie, movie$Genre %in% c('action', 'adventure', 'animation', 'drama', 'comedy', 'sci-fi'))
movie.data2 <- subset(movie, movie$Genre %nin% c('action', 'adventure',     'animation','drama','comedy', 'sci-fi'))
movie.data2$Genre <- c('Others')
movie2 <- rbind(movie.data, movie.data2)

choosing3 <- movie %>%
  group_by(Studio) %>%
  summarise(sum_adj = sum(GrossM))
choosing3[order(unlist(choosing3$sum_adj), decreasing = TRUE),]

studio.data <- subset(movie, movie$Studio %in% c('Buena Vista Studios','WB','Fox','Universal', 'Paramount Pictures','Sony'))
studio.data2 <- subset(movie, movie$Studio %nin% c('Buena Vista Studios', 'WB','Fox','Universal', 'Paramount Pictures','Sony'))
studio.data2$Studio <- c('Others')
studio2 <- rbind(studio.data, studio.data2)
```
![alt text](choosing3.PNG "Logo Title Text 1")

=> base on choosing3 => list out top 6 studios & asign the rest Studios as "Others" 


**Step 4: Start playing with data to see whether there are any interesting facts:**
* In the data set, we have 2 kinds of ratings, 1 from IMDB and 1 from MovieLens (both are reputative rating websites)
```sh
o <- ggplot(data=movie.data, aes(x=IMDB, y=MovieLens, color=Genre))
o + geom_point() + geom_smooth(fill=NA)
```
![alt text](Rplot_LensvsIMDB.PNG "Logo Title Text 1")

=> From the the scatterplot, we can see that 2 websites have a positive correlation. Therefore, in my preference, I will use IMDB score as the standard movie rating in this exercise 

* **_Correlation between Budget and Revenue divided by Genre_**
```sh
o + geom_point(aes(x=BudgetM, y=GrossM)) + geom_smooth(aes(x=BudgetM, y=GrossM), fill=NA) +
  facet_grid(.~Genre) +
  xlab('Budget Millions')  +
  ylab('Gross Millions')
```
![alt text](Rplot_BudvsRev.PNG "Logo Title Text 1")

=> Positive correlation between Budget and Sales, the more you give, the more you get. However, be noted that this is not a constant situation and can not apply to Comedy.


* **_Growth of movie industry by Genre_**
```sh
s <- ggplot(data=movie2, aes(x=Year))

s + geom_histogram(aes(fill=Genre), color='White') +
  ylab('# movies') +
  labs(title='Growth of Movie industry by Genre') +
  theme_ipsum()
```
![alt text](Rplot_growthmovie.PNG "Logo Title Text 1")

=> Movie industry starts exploding since the 1980s, with fast-speed increase in no. of movies, leading by Action, Animation & Comedy

* **_Average Budget & Profit by Genre, remove outlier_**
```sh
movie3 <- subset(movie2, select = c('Genre','BudgetM','ProfitM'))
movie32 <- melt(movie3, id.vars='Genre')
n <- ggplot(data=movie32, aes(x=Genre, y=value, fill=variable))

n + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim=c(0,1000)) +
  ggtitle('Avg Budget & Profit by Genre') +
  ylab('mUSD') +
  theme_ipsum()
```
![alt text](Rplot_AvgBudnPro.PNG "Logo Title Text 1")

=> We can see that Action movies have high Budget but bring the lowest Profit among Genre, which leads to the lowest %Profit/Revenue. Sci-fi has the same situation but quite better. Drama & Comedy on the other hand, have both low Budget and Profit in terms of value, but have highest %Profit/Revenue. 
...picture

* **_Revenue & Profit of each Studio during 10 years_**
```sh
bar_lab <- studio.data %>%
  group_by(Studio) %>%
  summarise(Gross=sum(GrossM), Profit=sum(ProfitM))

bar_lab
bar_lab <- as.data.frame(bar_lab)
bar_lab$ProfitPer <- paste(bar_lab$ProfitPer, '%')

library(reshape2)
bar_lab2 <- melt(bar_lab2, id.vars = 'Studio')

ggplot() +  
  geom_bar(data= subset(bar_lab2, variable %in% c('Gross','Profit')), aes(x=Studio, y=value, fill=variable), stat='identity',width=0.5, position = 'dodge') +
  labs(title = 'Revenue & Profit of each Studio during 1967-2015', color='', fill='') +
  geom_text(data= subset(bar_lab2, variable %in% c('Gross','Profit')),aes(x=Studio, y=value, color=variable, label=(round(value))), vjust = -0.3, 
            position = position_dodge(width = 0.5)) +
  geom_point(data= subset(bar_lab2, variable %in% c('ProfitPer')), aes(x=Studio, y=value*50000/100, fill=variable, color=variable), group=1, size=3) +
  scale_y_continuous(
    name=expression('mUSD'),
    sec.axis = sec_axis(~.*100/50000, name='%Profit') ) +
  geom_text(data= subset(bar_lab2, variable %in% c('ProfitPer')), aes(x=Studio, y=value*50000/100, color=variable, label=round(value,0)), size=4, vjust=-0.7) +
  theme_ipsum()
```

![alt text](Rplot_RevnPro.PNG "Logo Title Text 1")

=> Buena Vista is the highest revenue & profit studio throughout the period. And guess what? It's Walt Disney! Following by WB (Warner Bros) & Fox. 
However, when looking at Profit/Revenue,  Universal seems to be the leader with 80% return.




