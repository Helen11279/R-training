# ANOVA on iris data set
# YuJing
# Jan 21, 2024

#Q: are there differences in petal length among the three species?

aov(formula = Petal.Length~Species, data=iris)
petal_length_aov <- aov(formula = Petal.Length~Species, data=iris)


#Nothing prints
#To see the results of the ANOVA, we call the `summary` function.
summary(object =petal_length_aov)

#to save results, call sink.
sink(file = "r-stats/output/Peteal_length_anova.txt")
summary(object=petal_length_aov)
sink()

### Challenge 2  
#Use ANOVA to test for differences in sepal width among the three species. What 
#is the value of the _F_-statistic?
aov(formula = Sepal.Width~Species, data=iris)
sepal_width_aov <-aov(formula = Sepal.Width~Species, data=iris)
sink(file="r-stats/output/sepal_width_anova.txt")
summary(object = sepal_width_aov)
sink()

