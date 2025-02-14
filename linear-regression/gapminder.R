# Test relationship between life expectancy and GDP
all_gapminder <-read.csv(file = "data/gapminder-FiveYearData.csv",)
head(all_gapminder)
summary(all_gapminder)
gapminder <- all_gapminder[all_gapminder$year == 2007,]
gapminder$logGDP <-log10(gapminder$gdpPercap)

plot(x=gapminder$logGDP, 
     y=gapminder$lifeExp,
     xlab = "log10(GDP)", 
     ylab = "Life Expectancy")

lifeExp_v_gdp <-lm(formula = lifeExp ~ logGDP, data = gapminder)

# Save results to file
sink(file = "output/lifeExp-gdp-regression.txt")
summary(lifeExp_v_gdp)
sink()
