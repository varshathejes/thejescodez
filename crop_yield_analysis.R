Initialise
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(qcc)
library(dplyr)

#Read and Know the Data
crop.data <- read.csv("C:/Users/VARSHA THEJES S R/Downloads/crop.csv.csv", header = TRUE)
summary(crop.data)

#statistical concepts
#Mean
crop.data %>% summarise_if(is.numeric, mean)
#Median
crop.data %>% summarise_if(is.numeric, median)
#Mode
crop.data %>% summarise_if(is.numeric, mode)
#Min
crop.data %>% summarise_if(is.numeric, min)
#Max
crop.data %>% summarise_if(is.numeric, max)
#Sum
crop.data %>% summarise_if(is.numeric, sum)
#sd
crop.data %>% summarise_if(is.numeric, sd)
#var
crop.data %>% summarise_if(is.numeric, var)

#Anova Tests
#One-way ANOVA
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)

#Two-way ANOVA
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)

#Plotting the data
two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#Adding interactions between variables
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)





#Adding a blocking variable
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking)

#Find the best-fit model
model.sett <- list(one.way, two.way, interaction, blocking)
model.namess <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.sett, modnames = model.namess)

#Check for homoscedasticity
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))





#Charts
aggregate(yield ~ fertilizer, data = crop.data, mean)
attach(crop.data)
values = qcc.groups(crop.data$yield,crop.data$block)
qcc(values, type="xbar", std.dev = "UWAVE-SD")
qcc(values, type ="S", xlab="Block", ylab="Yield")
qcc(q, type = "c")
q=crop.data$yield
crop.data = qcc(values, type = "R", nsigmas = 3)


#Plotting results in a graph
mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  ) 
mean.yield.data$group <- c("a","b","b","b","b","c")
mean.yield.data

#Two-way ANOVA
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)



#Plotting the data
two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))


#Adding the means and standard errors to the graph
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))
two.way.plot






#Splitting up the data
two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)
two.way.plot

#Making the graph ready for publication
two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")
two.way.plot





#NON-PARAMETRIC TESTS
#kruskal
kruskal.test(block ~yield , data = crop.data)
#chi-squared
chivalue <- chisq.test(crop.data$yield)
chivalue
#wilcox
wilcox <- wilcox.test(crop.data$yield)
wilcox




#Time Series
library(readxl)
yield = read_excel("C:/Users/VARSHA THEJES S R/Downloads/tsjee.xlsx")    
yieldrate <- ts(yield, frequency=12, start=c(2006,1))
yieldrate
plot.ts(yieldrate)
birthstimeseriescomponents <- decompose(yieldrate)
plot(birthstimeseriescomponents)




















