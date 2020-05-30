library(dplyr)
library(ggplot2)
library(MASS)
library(maps)
library(reshape2)
library(RColorBrewer)

dt_location <- '/Users/nantang/Google Drive/CS&SS 569/HW2'
setwd(dt_location)

# prepare dataset
house_dt <- read.csv('kc_house_data.csv')
house_dt <- na.omit(house_dt)
house_dt <- house_dt %>% mutate(sqft_all = sqft_living + sqft_lot + sqft_above + sqft_basement)

house_new <-house_dt %>% 
  dplyr::select(price, bedrooms, bathrooms,sqft_living, sqft_lot, 
                sqft_above, sqft_basement,floors, waterfront, 
                condition, grade, yr_built, yr_renovated, lat, long)

#  visualize paired correlation
house_corr <- cor(house_new)
house_corr_df <- melt(house_corr)
house_corr_df <- house_corr_df %>% 
  mutate(value = replace(value, value==1, NA))

p1 <- ggplot(data=house_corr_df, mapping=aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white', size=0.75) + 
  scale_fill_distiller(palette = "YlGnBu", breaks=seq(-0.3, 0.9, 0.3), 
                       name=NULL, na.value='gray95', direction = 1) +
  geom_text(aes(label=round(value, 3), col=-value)) + 
  scale_color_distiller(palette = "Greys", breaks=seq(-0.3, 0.9, 0.3), 
                       name=NULL, na.value='gray95', direction = 1) +
  ggtitle('Matrix of Correlation Coefficients') +
  theme (
    axis.text.x = element_text(angle=45, vjust=0.6),
    axis.text.y = element_text(angle=0, hjust=0.6), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text( hjust=0.5, vjust=0),
    legend.text = element_text(face='bold'),
    legend.key.width = grid::unit(0.3,'cm'),
    panel.background = element_blank(),
    panel.border=element_blank(),
    panel.grid.major = element_blank()
  )

width = 7
ggsave("hw2-1.pdf",
       width=width,
       height=width/1.2)



# first attempt
house_reg_dt <- house_new %>% 
  dplyr::select(price, sqft_living, waterfront, lat, floors) %>%
  mutate(waterfront = as.factor(ifelse(waterfront == '0', 'Inland', 'Waterside'))) %>%
  mutate(floors = as.factor(floors))

sp_index <- which(house_reg_dt$waterfront=='Waterside')
sp_index <- c(sp_index, sample(which(house_reg_dt$waterfront=='Inland'), 500))
house_reg_sp <- house_reg_dt[sp_index,]

p2 <- ggplot(data=house_reg_sp, aes(x=sqft_living, y=price)) +
  geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
              formula = y~x, size=0.5, color='black', level=0.95, fill='gray75') +
  geom_point(aes(col=lat), alpha=0.75, shape=16) + 
  scale_x_log10(name='Logarithm of Living Room Square Feet') +
  scale_y_log10(breaks=c(3e+05, 1e+06, 3E+06), 
                labels=c(0.3, 1, 3),name='Logarithm of Price in Million') +
  scale_color_distiller(palette = 'YlGnBu', direction = 1, name='Latitude',
                        breaks=c(47.3, 47.5, 47.7))  +
  facet_grid(cols=vars(floors), rows=vars(waterfront)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45),
        legend.key.width = unit(0.3,'cm'))

width = 9
ggsave("hw2-2.pdf",
       width=width,
       height=width/1.6)



# regression (choose relatively independent variables)
house_reg_dt <- house_new %>% 
  dplyr::select(price, sqft_living, waterfront, lat, floors) %>%
  mutate(waterfront = as.factor(ifelse(waterfront == '0', 'Inland', 'Waterside'))) %>%
  mutate(floors = ifelse(floors < 2, 'Floors < 2', 'Floors >= 2')) %>%
  mutate(floors = as.factor(floors))

# sample 500 obs from origin data
sp_index <- which(house_reg_dt$waterfront=='Waterside')
sp_index <- c(sp_index, sample(which(house_reg_dt$waterfront=='Inland'), 500))
house_reg_sp <- house_reg_dt[sp_index,]

title_col <- 'gray10'
grid_col <- 'lightgray'

p3 <- ggplot(data=house_reg_sp, aes(x=sqft_living, y=price)) +
  geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
              formula = y~x, size=0.5, color='black', level=0.95, fill='gray75') +
  geom_point(aes(col=lat), alpha=0.75, shape=16) + 
  scale_x_log10(name='Living Room Square Feet ') +
  scale_y_log10(breaks=c(3e+05, 1e+06, 3E+06), 
                labels=c(0.3, 1, 3),name='House Price in Million') +
  scale_color_distiller(palette = 'YlGnBu', direction = 1, name='Latitude',
                        breaks=c(47.3, 47.5, 47.7))  +
  facet_grid(cols=vars(floors), rows=vars(waterfront)) +
  theme(
    aspect.ratio = ((1 + sqrt(5))/2)^(-1),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle=45, vjust=0.6),
    axis.title.x = element_text(margin=margin(t=10), size=12, color=title_col),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(hjust=0),
    axis.title.y = element_text(margin=margin(r=10), size=12, color=title_col),
    
    legend.key.width = grid::unit(0.4,'cm'),
    legend.key.height =  grid::unit(0.8,'cm'),
    
    strip.background = element_blank(),
    strip.text.x = element_text(size=12, color=title_col), 
    strip.placement = "outside",      
    
    panel.spacing.x = unit(1, "lines"), 
    panel.spacing.y = unit(1, "lines"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color=grid_col, size=0.1),
    panel.grid.major.y = element_line(color=grid_col, size=0.1),
  )

width = 8
ggsave("hw2-3.pdf",
       width=width,
       height=width/1.4)



library(grid)
library(tile)
library(MASS)

house_reg_dt <- house_new %>% 
  dplyr::select(price, sqft_living, waterfront, lat, floors) %>%
  mutate(waterfront = as.factor(ifelse(waterfront == '0', 'Inland', 'Waterside')))  %>%
  mutate(lat_intv = as.factor(round(lat, 1)))

# sample 300 obs from origin data
sp_index <- which(house_reg_dt$waterfront=='Waterside')
sp_index <- c(sp_index, sample(which(house_reg_dt$waterfront=='Inland'), 150))
house_reg_sp <- house_reg_dt[sp_index,]

inland_sp <- house_reg_sp %>% 
  filter(waterfront == 'Inland')
waterside_sp <- house_reg_sp %>%
  filter(waterfront == 'Waterside')

inland_lat_col <- brewer.pal(9, 'Greens')[3:9]
inland_col_sys <- mapvalues(inland_sp$lat_intv, from=levels(inland_sp$lat_intv), to=inland_lat_col)
inland_col_sys <- as.character(inland_col_sys)

water_lat_col <- brewer.pal(9, 'Blues')[3:9]
water_col_sys <- mapvalues(waterside_sp$lat_intv, from=levels(waterside_sp$lat_intv), to=water_lat_col)
water_col_sys <- as.character(water_col_sys)

trace1 <- scatter(
  x = inland_sp$sqft_living,
  y = log10(inland_sp$price),
  fit = list(method="mmest", ci = 0.95, col=inland_lat_col[2]),
  col = inland_col_sys,
  pch = rep(1, ncol=length(inland_sp)),
  plot = 1
)

trace2 <- scatter(
  x = waterside_sp$sqft_living,
  y = log10(waterside_sp$price),
  fit = list(method="mmest", ci = 0.95, col=water_lat_col[2]),
  col = water_col_sys,
  pch = rep(16, ncol=length(waterside_sp)),
  plot = 1
)

legendSymbols1 <- pointsTile(
  y = c(5.3, 5.2),
  x = c(5000, 5000),
  col = c(water_lat_col[4], inland_lat_col[4]),
  cex = 1.5, 
  pch = 16,
  plot = 1
)

legendLabels1 <- textTile(
  labels = c('Waterside', 'Inland'),
  x = c(6000, 6000),
  y = c(5.3, 5.2),
  col = c(water_lat_col[4], inland_lat_col[4]),
  fontsize = 8,
  plot=1
)

legendSymbols2 <- pointsTile(
  y = c(seq(6.9, 6.6, -0.05), seq(6.9, 6.6, -0.05)),
  x = c(rep(600, times=7), rep(650, times=7)),
  col = c(rev(water_lat_col),rev(inland_lat_col)),
  cex = 1.5, 
  pch = c(rep(15, times=7), rep(0, times=7)),
  plot = 1
)

legendlabels2 <- textTile(
  labels = c('47.8', '47.2'),
  y = c(6.9, 6.6),
  x = c(750, 750),
  col = c('gray25', 'gray 55'),
  fontsize = 7,
  plot = 1
)

rug_y <- rugTile(
  y = log10(house_reg_sp$price),
  type='dots',
  thickness=2,
  plot = 1
)

tile(
  trace1,
  trace2,
  legendSymbols1,
  legendLabels1,
  legendSymbols2,
  legendlabels2,
  rug_y,
  
  xaxis = list(log = TRUE, fontsize = 10, rot = 45,
               tick.length= 0.2, major=FALSE, at=c(500, 1000, 2000,5000)),
  xaxistitle = list(labels = c('Square Feet of Living Room'), fontsize = 10),
  yaxis = list(fontsize = 10, labels=c(0.1, 0.3, 1, 3, 10), major=FALSE),
  yaxistitle = list(labels = c('Price in Million'), fontsize = 10),
  rightaxis = list(labels=c('ee')),
  plottitle = list(labels = c('House Price in King County'), fontsize = 10),
  gridlines = list(type='y'),

  limits = c(500, 7000, 5, 7),
  RxC = c(1 ,1),
  height = list(plot=1.6, plottitle=1, xaxistitle=1),
  width = list(plot=2.2, yaxistitle=0.5, rightborder=4),
  
  output=list(file='HW2-4', width=8)
)








