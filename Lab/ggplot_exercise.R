## ggplot example:  Gapminder scatterplot
## Christopher Adolph    faculty.washington.edu/cadolph
## 29 January 2019
##
## The code below works through many improvements step-by-step
## as a pedagogial exercise.  As a result, many of the early
## iterations are imperfect or intentionally flawed. In 
## particular, plots earlier than 4.1 should definitely not
## be used, as they lack an appropriate style sheet.
##
## See the associated lecture at
##
##    http://faculty.washington.edu/cadolph/vis/Rgraphics.pdf
##
## for more details and be sure to carefully select from the
## many examples below.

rm(list=ls())

library(ggplot2)      ## ggplot graphics package
library(RColorBrewer) ## for colors
library(lemon)        ## to "freshen up" ggplot2
library(ggrepel)      ## to avoid point overlap
library(simcf)        ## from faculty.washington.edu/cadolph/software
                      ## for extractdata()
library(gapminder)    ## for data


## My ggplot theme for a "golden scatterplot"
## Christopher Adolph (with help from Kai Ping Leung)
## faculty.washington.edu/cadolph
goldenScatterCAtheme <- theme(
   ## Removes main plot gray background
   panel.background = element_rect(fill = "white"), 

   ## Golden rectangle plotting area (leave out for square)
   aspect.ratio = ((1 + sqrt(5))/2)^(-1), 

   ## All axes changes
   axis.ticks.length = unit(0.5, "char"),  # longer ticks
     
   ## Horizontal axis changes
   axis.line.x.top = element_line(size = 0.2),    # thinner axis lines
   axis.line.x.bottom = element_line(size = 0.2), # thinner axis lines
   axis.ticks.x = element_line(size = 0.2),       # thinner ticks
   axis.text.x = element_text(color = "black", size = 12),
                 ## match type of axis labels and titles
   axis.title.x = element_text(size = 12,
                    margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
                    ## match type; pad space between title and labels

   ## Vertical axis changes
   axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
   axis.text.y = element_text(color = "black", size = 12,
                   margin = margin(t = 0, r = -4, b = 0, l = 0)),
                   ## match type of axis labels and titles, pad
   axis.title.y = element_text(size = 12,
                   margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
                   ## match type of axis labels and titles, pad

   ## Legend
   legend.key = element_rect(fill = NA, color = NA),
                ## Remove unhelpful gray background

   ## Gridlines (in this case, horizontal from left axis only
   panel.grid.major.x = element_blank(),
   panel.grid.major.y = element_line(color = "gray45", size = 0.2),
     
   ## Faceting (small multiples)
   strip.background = element_blank(),
                ## Remove unhelpful trellis-like shading of titles
   strip.text.x = element_text(size=12),  # Larger facet titles
   strip.text.y = element_blank(),        # No titles for rows of facets
   strip.placement = "outside",           # Place titles outside plot
   panel.spacing.x = unit(1.25, "lines"), # Horizontal space b/w plots
   panel.spacing.y = unit(1, "lines")     # Vertical space b/w plots
)
class(goldenScatterCAtheme)  ## Create this as a class



## Make some nice colors to use by name
brewer <- brewer.pal(9, "Set1")
red <- brewer[1]    
blue <- brewer[2]
green <- brewer[3]
purple <- brewer[4]
orange <- brewer[5]  
brown <- brewer[7]
pink <- brewer[8]
gray <- brewer[9]
black <- "black"
nicegray <- "gray45"
pastels <- brewer.pal(9, "Pastel1")
lightred <- pastels[1]

## Focus on 2002 data
gap2002 <- gapminder[gapminder$year==2002,]

## Load some additional data
hivData <- read.csv("http://faculty.washington.edu/cadolph/vis/hivGapminder.csv")
gap2002 <- merge(gap2002, hivData, all.x=TRUE)



## Overview of plots.  Note there are several iterations for each example.

## Example 1: Default ggplot scatter of gapminder data: life expect vs GDP per capita
## Example 2: Default ggplot with region colors
## Example 3: Default ggplot with log axis appropriately scaled
## Example 4: Improve style of ggplot (background, aspect ratio, frame thickness, spacing, text size)
## Example 5: Add smoother (first loess, then ls, then MM)
## Example 6: Move legend
## Example 7: Bubble plot: Add population of country as size of circle
## Example 8: Small multiples
## Example 9: Annotate for narrative
## Example 10: Zoom in on narrative


## Example 1.1: Default ggplot scatter of gapminder data:
## life expect vs GDP per capita

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p +
     geom_point()

ggsave("ggScatterEx1_1.pdf")


## Example 1.2:  Let's not use a square -- change aspect ratio to golden rectangle

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p +
     geom_point()

width <- 7
ggsave("ggScatterEx1_2.pdf",
       width=width,
       height=width/1.618)


## Example 1.3:  Custom axis titles

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p +
     geom_point() +
     labs(x="GDP per capita in dollars",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx1_3.pdf",
       width=width,
       height=width/1.618)


## Example 2.1: Default ggplot with region colors

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p +
     geom_point() +
     labs(x="GDP per capita in dollars",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx2_1.pdf",
       width=width,
       height=width/1.618)


## Example 2.2: Change the color palette for clearer pairwise distinctions

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p +
     geom_point() +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in dollars",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx2_2.pdf",
       width=width,
       height=width/1.618)


## Example 2.3 & 2.4: Changing the device size (using the existing object)

width <- 9
ggsave("ggScatterEx2_3.pdf",
       width=width,
       height=width/1.618)


width <- 3
ggsave("ggScatterEx2_4.pdf",
       width=width,
       height=width/1.618)



## Example 3.1: Default ggplot with log axis 

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p +
     geom_point() +
     scale_x_log10() +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in dollars",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx3_1.pdf",
       width=width,
       height=width/1.618)


## Example 3.2: Default ggplot with log axis -- replace axis labels with shorter numbers

xbreaks <- c(500, 1000, 2000, 5000, 10000, 20000, 50000)

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p +
     geom_point() +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx3_2.pdf",
       width=width,
       height=width/1.618)

## Example 3.2: transparency for points -- better for overlaps, but gray background a bigger problem

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx3_3.pdf",
       width=width,
       height=width/1.618)


## Example 3.2a: with full gapminder dataset, not just 2002

p <- ggplot(data=gapminder,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx3_3a.pdf",
       width=width,
       height=width/1.618)



## Example 4.1a: Improve style of ggplot with goldenScatterCAtheme
## (background, aspect ratio, frame thickness, spacing, text size)

p <- ggplot(data=gapminder,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx4_1a.pdf",
       width=width,
       height=width/1.618)


## Example 4.1: better theme with just 2002 data

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx4_1.pdf",
       width=width,
       height=width/1.618)


## Example 5.1a: Add smoother:  loess on full data (total mess!)

p <- ggplot(data=gapminder,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_point(alpha=0.55) +
     geom_smooth() +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx5_1a.pdf",
       width=width,
       height=width/1.618)


## Example 5.2: Add smoother:  loess on 2002 data, impose common fit

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_point(alpha=0.55) +
     geom_smooth(size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx5_2.pdf",
       width=width,
       height=width/1.618)


## Example 5.3: Deal with layering.
## Note that ggplot layers things in order we "+" them on to the plot
## we want shaded regions *behind* points, so...

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx5_3.pdf",
       width=width,
       height=width/1.618)


## Example 5.4:  Smoother:  simple linear regression

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=lm, size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx5_4.pdf",
       width=width,
       height=width/1.618)


## Example 5.5: Custom fitting:  here, a robust and resistant fit

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years")

width <- 7
ggsave("ggScatterEx5_5.pdf",
       width=width,
       height=width/1.618)


## Example 6.1: Move legend to bottom of plot

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years") +
     theme(legend.title = element_blank(),
           legend.position = "bottom")

width <- 7
ggsave(filename="ggScatterEx6_1.pdf",
       width=width,
       height=width/1.618)


## Example 6.2:  Remove legend, method 1: "none" for position

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years") +
     theme(legend.position = "none")

width <- 7
ggsave(filename="ggScatterEx6_2.pdf",
       width=width,
       height=width/1.618)


## Example 6.2a:  Remove legend, method 2: guide=FALSE

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1", guide=FALSE) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years") 

width <- 7
ggsave(filename="ggScatterEx6_2a.pdf",
       width=width,
       height=width/1.618)


## Example 6.3: Move legend into plot with ggplot

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years") +
     theme(legend.title = element_blank(),
           legend.position = c(0.925, 0.22),
           legend.background =
               element_rect(fill=alpha("white", 0)))

width <- 7
ggsave(filename="ggScatterEx6_3.pdf",
       width=width,
       height=width/1.618)


## Example 6.3a: Move legend into plot with lemon

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years") +
     theme(legend.title = element_blank())

width <- 7
ggsave(filename="ggScatterEx6_3a.pdf",
       ## Move legend to plot for single plot
       plot = reposition_legend(p, "bottom right"),
       width=width,
       height=width/1.618)

## to reposition more precisely -- see help in lemon::reposition_legend
## reposition_legend(p, x=0.3, y=0, just=c(0, -0.5))


## Example 7.1: Add population of country as size of circle (bubble plot)

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55, aes(size=pop)) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years") +
     theme(legend.title = element_blank(),
           legend.position = c(0.925, 0.37),
           legend.background =
               element_rect(fill=alpha("white", 0)))

width <- 7
ggsave(filename="ggScatterEx7_1.pdf",
       width=width,
       height=width/1.618)


## Example 7.2: Custom legend

popBreaks <- rev(c(3, 10, 30, 100, 300)*1000000)

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + themeCAscatter +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55, aes(size=pop)) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          col="Region",
          size="Population\nin millions")

width <- 7
ggsave(filename="ggScatterEx7_2.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 7.3: Label points using ggrepel

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55, aes(size=pop)) +
     geom_text_repel(aes(label=country), size=2,
                    alpha=0.55, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          col="Region",
          size="Population\nin millions")

width <- 7
ggsave(filename="ggScatterEx7_3.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 7.3: Selectively label points

ctyLabs <- as.character(gap2002$country)
ctyLabs[gap2002$pop<quantile(gap2002$pop, probs=0.75)] <- ""

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, se=FALSE, inherit.aes=FALSE,  
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55, aes(size=pop)) +
     geom_text_repel(aes(label=ctyLabs), size=2,
                    alpha=0.55, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          col="Region",
          size="Population\nin millions")

width <- 7
ggsave(filename="ggScatterEx7_4.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 8.1: Switch to small multiples

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp, color=continent))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.55, aes(size=pop)) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_brewer(palette = "Set1") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          col="Region",
          size="Population\nin millions") +
     facet_wrap(~continent)

width <- 9
ggsave("ggScatterEx8_1.pdf",
       width=width,
       height=width/1.618)


### Example 8.2: Remove color -- now unnec and could be used for another variable

p <- ggplot(data=gap2002,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.40, aes(size=pop)) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population\nin millions") +
     facet_wrap(~continent)

width <- 9
ggsave(filename="ggScatterEx8_2.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 8.3: Efficient layout (remove Oceania; move legend); label some cases

gap2002noO <- gap2002[gap2002$continent!="Oceania",]

ctyLabs0 <- as.character(gap2002noO$country)
ctyLabs0[gap2002noO$pop<quantile(gap2002noO$pop, probs=0.75)] <- ""

p <- ggplot(data=gap2002noO,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.40, aes(size=pop)) +
     geom_text_repel(aes(label=ctyLabs0), size=2.5,
                    alpha=0.40, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population in millions") + 
     facet_wrap(~continent) +
     theme(legend.position="bottom")

width <- 8
ggsave(filename="ggScatterEx8_3.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 9.1: Use color to highlight high HIV countries

hivCutoff <- 2

p <- ggplot(data=gap2002noO,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.40, aes(size=pop,
                        col=hivPrevalence15to49>hivCutoff)) +
     geom_text_repel(aes(label=ctyLabs0,
                   col=hivPrevalence15to49>hivCutoff), size=2.5,
                     alpha=0.40, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     ##scale_color_manual(values=c(black, red), guide="none") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     guides(size = guide_legend(nrow = 1)) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population in millions") + 
     facet_wrap(~continent) +
     theme(legend.position="bottom") +
     annotate(geom="text", x=30000, y=45,
              label=paste0("HIV rate\nover ", hivCutoff, "%"),
              col=red, size=2.5)

width <- 8
ggsave(filename="ggScatterEx9_1.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 9.2:  Assign colors to categories; don't show NAs

hivCutoff <- 2

p <- ggplot(data=gap2002noO,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.40, aes(size=pop,
                        col=hivPrevalence15to49>hivCutoff)) +
     geom_text_repel(aes(label=ctyLabs0,
                   col=hivPrevalence15to49>hivCutoff), size=2.5,
                     alpha=0.40, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_manual(values=c(black, red)) +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     guides(size = guide_legend(nrow = 1)) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population in millions",
          color="HIV prevalence >2%") + 
     facet_wrap(~continent) +
     theme(legend.position="bottom") +
     annotate(geom="text", x=30000, y=45,
              label=paste0("HIV rate\nover ", hivCutoff, "%"),
              col=red, size=2.5)

width <- 8
ggsave(filename="ggScatterEx9_2.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 9.3:  Remove NAs from data to avoid fitting on them

mdata1 <- extractdata(lifeExp~gdpPercap + continent + hivPrevalence15to49,
                      data=gap2002, extra=~country+pop, na.rm=TRUE)

mdata1 <- mdata1[mdata1$continent!="Oceania",]

ctyLabs1 <- as.character(mdata1$country)
ctyLabs1[mdata1$pop<quantile(mdata1$pop, probs=0.75)] <- ""

hivCutoff <- 2

p <- ggplot(data=mdata1,
            mapping=aes(x=gdpPercap, y=lifeExp))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, inherit.aes=FALSE,
                 aes(x=gdpPercap, y=lifeExp), color="black") +
     geom_point(alpha=0.40, aes(size=pop,
                        col=hivPrevalence15to49>hivCutoff)) +
     geom_text_repel(aes(label=ctyLabs1,
                   col=hivPrevalence15to49>hivCutoff), size=2.5,
                     alpha=0.40, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_manual(values=c(black, red)) +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     guides(size = guide_legend(nrow = 1)) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population in millions",
          color="HIV prevalence >2%") + 
     facet_wrap(~continent) +
     theme(legend.position="bottom") +
     annotate(geom="text", x=30000, y=45,
              label=paste0("HIV rate\nover ", hivCutoff, "%"),
              col=red, size=2.5)

width <- 8
ggsave(filename="ggScatterEx9_3.pdf",
       plot = p,
       width=width,
       height=width/1.618)



## Example 10.1: Revert to single panel;
##    two reg lines on one panel works here because they separate well

mdata2 <- extractdata(lifeExp~gdpPercap + hivPrevalence15to49,
                     data=gap2002, extra=~country+pop, na.rm=TRUE)

ctyLabs2 <- as.character(mdata2$country)
ctyLabs2[mdata2$pop<quantile(mdata2$pop, probs=0.75)] <- ""

p <- ggplot(data=mdata2,
            mapping=aes(x=gdpPercap, y=lifeExp,
             color=hivPrevalence15to49>hivCutoff))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5) +
     geom_point(alpha=0.40, aes(size=pop)) +
     geom_text_repel(aes(label=ctyLabs2), size=2,
                    alpha=0.40, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_manual(values=c(black, red), guide="none") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     guides(size = guide_legend(nrow = 1)) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population in millions") +
     theme(legend.position="bottom") +
     annotate(geom="text", x=20000, y=54,
              label=paste0("HIV rate\nover ", hivCutoff,"%"),
              col=red, size=4) +
     annotate(geom="text", x=800, y=75,
              label=paste0("HIV rate\nbelow ", hivCutoff,"%"),
              col=black, size=4)

width <- 7
ggsave(filename="ggScatterEx10_1.pdf",
       plot = p,
       width=width,
       height=width/1.618)


## Example 10.2: Fix color of ribbon -- tricky!

p <- ggplot(data=mdata2,
            mapping=aes(x=gdpPercap, y=lifeExp,
             color=hivPrevalence15to49>hivCutoff))
p <- p + goldenScatterCAtheme +
     geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                 size=0.5, aes(fill=hivPrevalence15to49>hivCutoff)) +
     geom_point(alpha=0.40, aes(size=pop)) +
     geom_text_repel(aes(label=ctyLabs2), size=2,
                    alpha=0.40, segment.size=0.2) +
     scale_x_log10(breaks=xbreaks,
                   labels=xbreaks/1000) +
     scale_color_manual(values=c(black, red), guide="none") +
     scale_fill_manual(values=c(gray, lightred), guide="none") +
     scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
     guides(size = guide_legend(nrow = 1)) +
     labs(x="GDP per capita in $k",
          y="Life expectancy in years",
          size="Population in millions") +
     theme(legend.position="bottom") +
     annotate(geom="text", x=20000, y=54,
              label=paste0("HIV rate\nover ", hivCutoff,"%"),
              col=red, size=4) +
     annotate(geom="text", x=800, y=75,
              label=paste0("HIV rate\nbelow ", hivCutoff,"%"),
              col=black, size=4)

width <- 7
ggsave(filename="ggScatterEx10_2.pdf",
       plot = p,
       width=width,
       height=width/1.618)











