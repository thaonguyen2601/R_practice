
getwd()

setwd('D:\\Download\\R practice')

movie <- read.csv('Section6-Homework-Data-2.csv')

head(movie)


colnames(movie) <- c('DayofWeek', 'Director', 'Genre', 'MovieTitle', 'ReleaseDate', 'Studio',
                     'AdjGrossM', 'BudgetM', 'GrossM', 'IMDB', 'MovieLens', 'OverseasM', 
                     'OverseasPer', 'ProfitM', 'ProfitPer', 'Runtime', 'USM', 'GrossUSPer', 'Year')
head(movie)
str(movie)
typeof(movie)
summary(movie)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")

library(dplyr)
install.packages('tidyverse')

install.packages('hrbrthemes')
library(hrbrthemes)
import_roboto_condensed()

install.packages('viridis')
library(viridis)

choosing <- movie %>%
  group_by(Genre) %>%
  summarise(sum_adj = sum(AdjGrossM))

choosing

typeof(choosing)

choosing[order(unlist(choosing$sum_adj), decreasing = TRUE),]

choosing2 <- movie %>%
  group_by(Genre) %>%
  summarise(totalmovie = n())

choosing2

typeof(choosing2)

choosing2[order(unlist(choosing2$totalmovie), decreasing = TRUE),]
  
choosing3 <- movie %>%
  group_by(Studio) %>%
  summarise(sum_adj = sum(GrossM))

choosing3[order(unlist(choosing3$sum_adj), decreasing = TRUE),]


# base on choosing and choosing2 => choose 6 genres to do analysis

movie.data <- subset(movie, movie$Genre %in% c('action', 'adventure', 'animation',
                                               'drama', 'comedy', 'sci-fi'))
movie.data

`%nin%` = Negate(`%in%`)

movie.data2 <- subset(movie, movie$Genre %nin% c('action', 'adventure', 'animation',
                                                'drama', 'comedy', 'sci-fi'))
movie.data2$Genre %nin% c('action', 'adventure', 'animation','drama', 'comedy', 'sci-fi') <- c('Others')

movie.data2$Genre <- c('Others')
movie.data2

movie2 <- rbind(movie.data, movie.data2)
movie2
summary(movie2)
tail(movie2)
str(movie2)

studio.data <- subset(movie, movie$Studio %in% c('Buena Vista Studios','WB','Fox','Universal',
                                                 'Paramount Pictures','Sony'))
studio.data2 <- subset(movie, movie$Studio %nin% c('Buena Vista Studios','WB','Fox','Universal',
                                                 'Paramount Pictures','Sony'))
studio.data2$Studio <- c('Others')
studio.data2

studio2 <- rbind(studio.data, studio.data2)
head(studio2)
tail(studio2)

# base on choosing3 => choose 6 studios to do analysis

o <- ggplot(data=movie2, aes(x=IMDB, y=MovieLens, color=Genre))

o + geom_point() + geom_smooth(fill=NA) + 
  labs(title='Correlation between MovieLens and IMDB scores') +
  theme_ipsum()

# IMDB and MovieLens have positive correlation => choose IMDB as standard metric

o + geom_point(aes(x=BudgetM, y=GrossM)) + geom_smooth(aes(x=BudgetM, y=GrossM), fill=NA) +
  facet_grid(.~Genre) +
  labs(title='Correlation between Budget and Revenue') +
  xlab('Budget Millions')  +
  ylab('Gross Millions') +
  theme_ipsum()

cor(x=movie.data$BudgetM, y=movie.data$GrossM, use='complete.obs', method='pearson')
# Correlation between Budget and Revenue divided by Genre

o + geom_point(aes(x=IMDB, y=GrossM)) + geom_smooth(fill=NA) +
  facet_grid(Genre~.) +
  xlab('IMDB')  +
  ylab('Gross Millions')

cor(x=movie.data$IMDB, y=movie.data$GrossM, use='complete.obs', method='pearson')
# Correlation between IMDB score and Gross
movie2[which.max(movie2$GrossM),]

p <- ggplot(data=movie2, aes(x=Genre, y=BudgetM, color=Genre))

p + geom_jitter() +
  geom_boxplot(outlier.colour = NA, alpha = 0.5)

p <- ggplot(data=movie2, aes(x=Genre, y=IMDB, color=Genre))

p + geom_jitter(aes(size=BudgetM)) +
  geom_boxplot(outlier.colour = NA, alpha = 0.5)


mean(movie.data$BudgetM)
apply(movie.data, 1, mean)

# Avg IMDB score and budget of each genre
s <- ggplot(data=movie2, aes(x=Year))

s + geom_histogram(aes(fill=Genre), color='White') +
  ylab('# movies') +
  labs(title='Growth of Movie industry by Genre') +
  theme_ipsum()
# Growth of movie industry by Genre

n <- ggplot(data=studio.data, aes(x=Year))

n + geom_histogram(aes(fill=Studio), color='White') +
  ylab('# movies') 
# Growth of movie industry by Studio

e <- ggplot(data=studio.data, aes(x=sum(GrossM), fill=Studio))

e + geom_bar(stat='identity', width=1)


r <- ggplot(data=bar_labm, aes(x=Studio, y=value, fill=Type))
bar_labm

ggplot() +  
  geom_bar(data=bar_labm, aes(x=Studio, y=value, fill=Type), stat='identity',width=0.5, position = 'dodge') +
  labs(title = 'Revenue & Profit of each Studio') +
  geom_text(data=bar_labm, aes(x=Studio, y=value, color=Type, label=(round(value))), vjust = 0, 
            position = position_dodge(width = 0.5)) +
  geom_point(data=bar_lab, aes(x=Studio, y=ProfitPer*50000/100, color=ProfitPer, group=1), size=3, color='green', fill='green', shape=21, inherit.aes = FALSE) +
    scale_y_continuous(
    name=expression('mUSD'),
    sec.axis = sec_axis(~.*100/50000, name='%Profit') ) +
  geom_text(data=bar_lab, aes(x=Studio, y=ProfitPer*50000/100, label=round(ProfitPer,0))) 

ggplot() +  
  geom_bar(data= subset(bar_lab2, variable %in% c('Gross','Profit')), aes(x=Studio, y=value, fill=variable), stat='identity',width=0.5, position = 'dodge') +
  labs(title = 'Revenue & Profit of each Studio during 1967-2015', color='', fill='') +
  geom_text(data= subset(bar_lab2, variable %in% c('Gross','Profit')),aes(x=Studio, y=value, color=variable, label=(round(value))), vjust = -0.3, 
            position = position_dodge(width = 0.5)) +
  geom_point(data= subset(bar_lab2, variable %in% c('ProfitPer')), aes(x=Studio, y=value*65000/100, fill=variable, color=variable), group=1, size=3) +
  scale_y_continuous(
    name=expression('mUSD'),
    sec.axis = sec_axis(~.*100/65000, name='%Profit') ) +
  geom_text(data= subset(bar_lab2, variable %in% c('ProfitPer')), aes(x=Studio, y=value*65000/100, color=variable, label=round(value,0)), size=4, vjust=-0.7) +
  theme_ipsum()

##### perfecr chartttt



  
cols <- c('Gross'='Yellow', 'Profit'='Blue', 'ProfitPer'='Green')  
scale_color_manual(NULL, values='black') #2 values needed but only 1 provided?

bar_lab
bar_lab$ProfitPer <- paste(bar_lab$ProfitPer, '%')

bar_lab2 <- bar_lab

bar_lab2
bar_lab2 <- melt(bar_lab2, id.vars = 'Studio')

u <- ggplot(data=bar_lab, aes(x=Studio, y=Profit, fill=Studio))
ggplot(data=bar_lab) + 
  geom_point(aes(x=Studio, y=ProfitPer*45000/100, fill=Studio),  size=3, shape=21) +
  geom_bar(aes(x= Studio, y=Profit, fill=Studio), stat='identity', width = 0.5) + 
  geom_text(aes(x=Studio, y=Profit, label=round(Profit, 0)), size=3) +
  scale_y_continuous(
    name=expression('Profit (mUSD)'),
    sec.axis = sec_axis(~. *100/45000,name='%Profit'),) +
  theme_ipsum()

?scale_y_continuous()
bar_lab$ProfitPer <- round((bar_lab$Profit / bar_lab$Gross * 100), 1)

bar_lab <- studio.data %>%
  group_by(Studio) %>%
  summarise(Gross=sum(GrossM), Profit=sum(ProfitM))

bar_lab
bar_lab <- as.data.frame(bar_lab)

library(reshape2)
bar_labm <- melt(bar_lab, id.vars = 'Studio')
bar_labm

colnames(bar_labm) <- c('Studio', 'Type', 'value')
colnames(bar_lab) <- c('Studio', 'Gross', 'Profit')

bar_labm$ProfitPer <- bar_labm$
# Revenue & Profit of each studio during 10 years

studio <- movie.data %>%
  group_by(Studio, Genre) %>%
  summarise(n())

studio
str(studio)

i <- ggplot(data=movie2, aes(x=Genre, y=ProfitM, color=Genre))

#pending combining these 2 charts

movie2
movie3 <- subset(movie2, select = c('Genre','BudgetM','ProfitM'))
movie3
movie32 <- melt(movie3, id.vars='Genre')
i
n <- ggplot(data=movie32, aes(x=Genre, y=value, fill=variable))

n + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim=c(0,1000)) +
  ggtitle('Avg Budget & Profit by Genre') +
  ylab('mUSD') +
  theme_ipsum()
# n is the correct chart for Avg Budget and Profit by Genre, remove outlier

i + geom_jitter() + geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  coord_cartesian(ylim=c(0,1500))

i + geom_jitter(aes(y=BudgetM)) + geom_boxplot(aes(y=BudgetM),outlier.shape = NA, alpha = 0.5) +
  ylab('Budget Millions')

# avg profit and avg budget by genre , remove outlier already

y <- ggplot(data=studio.data, aes(x=Studio, y=ProfitM, color=Studio))

y + geom_jitter() + geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  coord_cartesian(ylim=c(0,1500))
# avg profit per top studios, remove outlier already

n <- ggplot(data=movie.data, aes(x=Genre, y=ProfitM/GrossM*100, color=Genre))

n + geom_jitter() + geom_boxplot(alpha = 0.5) +
  ylab('%Profit')
# avg %profit per top genre

q <- ggplot(data=studio.data, aes(x=Studio, y=ProfitM/GrossM*100, color=Studio))

q + geom_jitter() + geom_boxplot(alpha = 0.5) +
  ylab('%Profit')

# avg %profit per top studio

