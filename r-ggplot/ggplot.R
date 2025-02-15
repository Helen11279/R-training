#Title: visualization with ggplot
#author: Yu-Jing Chen
#ychen10@swarthmore.edu

dir.create(path = "data")
dir.create(path = "output")
download.file(url = "http://tinyurl.com/gapminder-five-year-csv", 
              destfile = "data/gapminder.csv")
  
#install ggplot 
#install.packages("ggplot2")
library("ggplot2")

#constructing scatterplot
gapminder <- read.csv(file = "data/gapminder.csv")

LifeExp_plot <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp,
                                                       color = continent)) +
  xlab("GDP per capita") +
  ylab("Life Expectancy") +
  scale_x_log10() +
  geom_point(alpha = 0.5) 

LifeExp_plot
  
ggsave(filename = "output/gdp-LifeExp-plot.png", plot = LifeExp_plot)

#drawing violin plot
violin_plot <- ggplot(data = gapminder, mapping = aes(x = continent, y=gdpPercap)) +
  geom_violin()

violin_plot
ggsave(filename = "output/gdp-violin.png")
