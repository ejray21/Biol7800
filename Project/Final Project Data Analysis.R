## Import csv with data
data <- POR_Master_Sheet
treatment <- data$Treatment

## First compare standard length, mass, and condition factor across treatments
## These should not be statistically different because we want fish that are
## about the same age and size and have the same body condition

#Standard Length
Standard.Length.Aov <- aov(data$`standard length (mm)`~ data$Treatment)
summary(Standard.Length.Aov)
TukeyHSD(Standard.Length.Aov)

#Body Mass
Body.Mass.Aov <- aov(data$`mass (g)`~data$Treatment)
summary(Body.Mass.Aov)
TukeyHSD(Body.Mass.Aov)

#Condition Factor
Condition.Factor <- 100*(data$`mass (g)`/ (data$`standard length (mm)`^3))
Condition.Factor.Aov <- aov(Condition.Factor~data$Treatment)
summary(Condition.Factor.Aov)
TukeyHSD(Condition.Factor.Aov)

### Now making a table of the body mass, standard length, and condition factor
library(tables)
library(tidyr)
##first subset to remove rows with na's
data.table <- subset(data, !is.na(data$`mass (g)`))
treatment.table <- (as.factor(data.table$Treatment))
Body.Mass <- (data.table$`mass (g)`)
Standard.Length <- (data.table$`standard length (mm)`)
Condition.Factor <- na.omit(Condition.Factor)
##create table
body.stats.table <- tabular((treatment.table + 1) ~ (n=1) + Format(digits = 2)* (Body.Mass + Standard.Length + Condition.Factor)*(mean + sd), data.table)
## Using booktabs to make the table look nice
final.dissection.table <- booktabs(body.stats.table)
final.dissection.table
latex.dissection.table <- toLatex(body.stats.table)

##Comparing behaviors across treatment groups

#distance traveled
Distance.Traveled.Aov <- aov(data$`distance 30 min (mm)`~data$Treatment)
summary(Distance.Traveled.Aov)
TukeyHSD(Distance.Traveled.Aov)
distance.f <- Distance.Traveled.Aov$

#time in association zone
AZ.time.Aov <- aov(data$`acclimation AZ time (s)`~data$Treatment)
Summary(AZ.time.Aov)
TukeyHSD(AZ.time.Aov)

## I use an association index because multiple behaviors could be indicative of the fish's response

#here I am multiplying the time spent in the association zone by the speed

AI <- (data$`visible time in AZ (s)`* data$`toxtrac velocity 30 min (mm/s)`)
AI.Aov <- aov(AI~data$Treatment)
Summary(AI.Aov)
TukeyHSD(AI.Aov)


### Making graphs ###
treatment <- factor(data$Treatment, levels=c("Control", "Chemosensory", "Visual", "C + V"))
library(ggplot2)

## Condition factor plot ##
condition.factor.plot <- ggplot(data, aes(x=treatment, y = Condition.Factor, fill= treatment))+ geom_boxplot() + labs(y = "Condition Factor", x = " ") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
condition.factor.plot

## Distance Traveled Plot ##
distance <- data$`distance 30 min (mm)`
distance.plot <- ggplot(data, aes(x=treatment, y = distance, fill= treatment)) + geom_boxplot() + labs(y = "Total Distance Traveled (mm)", x = " ") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
distance.plot

## Time in association zone ##
AZ.time <- data$`visible time in AZ (s)`
AZ.time.plot <- ggplot(data, aes(x=treatment, y = AZ.time, fill= treatment)) + geom_boxplot() + labs(y = "Time in Association Zone (s)", x = " ") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
AZ.time.plot

##Association Index Plot ##
AI.plot <- ggplot(data, aes(x=treatment, y = AI, fill= treatment)) + geom_boxplot() + labs(y = "Association Index", x = " ") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
AI.plot
##Need to check this data for outliers ##

