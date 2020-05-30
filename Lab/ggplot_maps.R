library(maps)
library(ggplot2)
library(socviz)
library(dplyr)
library(RColorBrewer)
library(ggthemes)

us_state <- map_data("usa")

p1 <- ggplot(data=us_state, mapping=aes(x=long, y=lat, group=group, fill=region))

p1 + geom_polygon(color='gray90', size=0.1) + guides(fill=F) 


# county data and map
county_full <- left_join(county_map, county_data, by='id')

p2 <- ggplot(data=county_full, mapping=aes(x=long, y=lat, fill=pop_dens, group=group))

p2 + geom_polygon(color='gray90', size=0.1) + coord_equal() +
  scale_fill_brewer(palette = 'Blues', labels=c('0-10', '10-50', '50-100', '100-500', 
                                                '500-1000', '1000-5000', '5000-71672')) +
  theme_map() + 
  theme(legend.title = element_blank(),
        legend.position = 'right') 
