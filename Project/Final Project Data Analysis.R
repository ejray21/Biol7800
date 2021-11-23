## Import csv with data
data <- POR_Master_Sheet

## First compare standard length, mass, and condition factor across treatments
## These should not be statistically different

#Standard Length
Standard.Length.Aov <- aov(data$`standard length (mm)`~ data$Treatment)
TukeyHSD(Standard.Length.Aov)

#Body Mass
Body.Mass.Aov <- aov(data$`mass (g)`~data$Treatment)
TukeyHSD(Body.Mass.Aov)

#Condition Factor
Condition.Factor <- 100*(data$`mass (g)`/ (data$`standard length (mm)`^3))
Condition.Factor.Aov <- aov(Condition.Factor~data$Treatment)
TukeyHSD(Condition.Factor.Aov)

##Comparing behaviors across treatment groups

#distance traveled
Distance.Traveled.Aov <- aov(data$`distance 30 min (mm)`~data$Treatment)
TukeyHSD(Distance.Traveled.Aov)

#speed
speed.Aov <- aov(data$`toxtrac velocity 30 min (mm/s)`~data$Treatment)
TukeyHSD(speed.Aov)

#time in association zone
AZ.time.Aov <- aov(data$`acclimation AZ time (s)`~data$Treatment)
TukeyHSD(AZ.time.Aov)

## I use an association index because multiple behaviors could be indicative of the fish's response

#here I am multiplying the time spent in the association zone by the speed

AI <- (data$`visible time in AZ (s)`* data$`toxtrac velocity 30 min (mm/s)`)
AI.Aov <- aov(AI~data$Treatment)
TukeyHSD(AI.Aov)



