rm(list=ls())

library(ggplot2)      ## ggplot graphics package
library(RColorBrewer) ## for colors
library(lemon)        ## to "freshen up" ggplot2
library(ggrepel)      ## to avoid point overlap
library(simcf)        ## from faculty.washington.edu/cadolph/software
## for extractdata()
library(gapminder)


p1 <- ggplot(data = gapminder, mapping = aes(x=year, y=gdpPercap, color=gdpPercap))
p1 + facet_wrap(~continent, ncol=5) + 
  geom_line(aes(group=country)) + 
  scale_y_log10(labels=scales::dollar)


xbreaks <- c(500, 1000, 2000, 5000, 10000, 20000, 50000)

p2 <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p2 <- p2 + goldenScatterCAtheme +
  geom_point(alpha=0.55) +
  scale_x_log10(breaks=xbreaks,
                labels=xbreaks/1000) +
  scale_color_brewer(palette = "Set1") +
  labs(x="GDP per capita in $k",
       y="Life expectancy in years")

p2 + geom_smooth(method=MASS::rlm, method.args=list(method='MM'),
                 size = 0.5, inherit.aes=F,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

