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