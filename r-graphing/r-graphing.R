# Graphing Life Expectancy vs. GDP
# YuJing
# ychen10@swarthmore.edu
#Jan 20, 2024

#Read data
all_gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv", stringsAsFactors = TRUE)

#Subset data
gapminder <- all_gapminder[all_gapminder$year == 2002,]

#Make new vector of Log GDP
gapminder$Log10GDP <-log10(gapminder$gdpPercap)


#parameters
symbol <-18
sym_size <- 1.2
continents <- levels(gapminder$continent)
continent_colors <-c("red", "orange", "forestgreen", "darkblue", "violet")

# What are the possible values for continent?
levels(gapminder$continent)


#create a new vector for colors
gapminder$colors <- NA
for (i in 1:length(continents)) {
  gapminder$colors[gapminder$continent == continents[i]] <-continent_colors[i]
}

# Create main plot
plot(x = gapminder$Log10GDP,
     y = gapminder$lifeExp,
     main = "Life expectancy vs. GDP",
     xlab = "Log(GDP Per Capita)",
     ylab = "Life expectancy (years)",
     col = gapminder$colors,
     pch = symbol,
     cex = sym_size) #A diamond symbol

legend("topleft", 
       legend = levels(gapminder$continent), 
       col = continent_colors,
       pch = symbol)

lifeExp_lm <- lm(gapminder$lifeExp ~ gapminder$Log10GDP)
abline(reg = lifeExp_lm, lty = 2, lwd = 2)



