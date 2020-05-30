library('tidyverse') 
library('ggmap')
library('car')
library('MASS')
library('hrbrthemes')
library('RColorBrewer')
library('grid')
library('tile')
library('gridExtra')
library('simcf')


## prepare data
dir <- '/Users/nantang/Desktop/HW3'
setwd(dir)
efw_dt <- read.csv('efw_cc.csv')

NA_index <- numeric(ncol(efw_dt))
for (i in 1:ncol(efw_dt)) {
  NA_index[i] <- length(which(is.na(efw_dt[,i])))
}
efw_dt <- efw_dt[-NA_index, ]

efw_dt$countries <- as.character(efw_dt$countries)

world <- map_data("world")
diff1 <- setdiff(world$region, efw_dt$countries)
diff2 <- setdiff(efw_dt$countries, world$region)

efw_dt <- efw_dt %>%
  mutate(countries = replace(countries, countries=='United States', 'USA')) %>%
  mutate(countries = replace(countries, countries=='United Kingdom', 'UK')) %>%
  mutate(countries = replace(countries, countries=='Korean, South', 'South Korean')) %>%
  mutate(countries = replace(countries, countries=='Congo, Dem. R.', 'Democratic Republic of the Congo')) %>%
  mutate(countries = replace(countries, countries=='Congo, Rep. Of', 'Republic of Congo')) %>%
  rename(region = countries, ef = ECONOMIC.FREEDOM)

#
#
#
## heat map
top_30 <- c("Argentina", "Brazil", "Canada", "China",
            "Egypt", "France", "Greece", "India", "Indonesia",
            "Iran","Israel" , "Italy" ,  "Korea, South", "Malaysia", 
            "Mexico",  "Norway", "Pakistan", "Portugal", "Russia", 
            "Turkey", "Taiwan","Netherlands","Japan",
            "Finland", "Germany", "UK", "USA", "Austria", "Australia", "Switzerland")

efw_dt_30 <- efw_dt %>%
  filter(region %in% top_30) %>%
  filter(year %in% seq(1999, 2016, 1)) %>%
  mutate(ef_round = factor(round(ef), levels=c('9','8','7','6','5','4')))


p_efw_heat <- ggplot(data=efw_dt_30, aes(x=factor(year), y=factor(region)) ) +
  geom_tile(aes(fill=ef_round), color='white', size=0.5) +
  scale_fill_manual(name='Economic\nFreedom\nIndex',
    labels=c('8.5-9.5', '7.5-8.5', '6.5-7.5', '5.5-6.5', '4.5-5.5', '3.5-4.5'),
    breaks=c('9','8','7','6','5','4'),
    #values=c('#edf8fb','#ccece6','#99d8c9','#66c2a4','#2ca25f','#006d2c'),
    #values=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
    values=c('#edf8fb','#bfd3e6','#9ebcda','#8c96c6','#8856a7','#810f7c'),
    na.value='gray50') +
  scale_x_discrete(expand=c(0,0), breaks=seq(2000, 2016, 4)) +
  scale_y_discrete(expand=c(0,0)) +
  labs(title='Econ Freedom Indices of 30 Regions') +
  theme_gray(base_size = 10) +
  theme(
    axis.ticks=element_line(size=0.4),
    axis.title = element_blank(),
    plot.title = element_text(face='bold', size=9),
    legend.text=element_text(face='bold'),
    legend.title = element_text(size=8),
    
    panel.background = element_rect(fill = "grey80"),
    panel.grid = element_blank(),
    panel.border=element_blank()
  ) 
  
ggsave('heatmap_30.pdf', plot=p_efw_heat, width=8, height=5)

#
#
#
## world map
start_yr <- 2000
end_yr <- 2016
select_qt <- 1

start_yr_dt <- efw_dt %>% filter(year==start_yr) %>% dplyr::select(year, region, ef_start=ef)

yr_efw_dt <- efw_dt %>% 
  filter(year==end_yr)

yr_efw_dt <- inner_join(yr_efw_dt, start_yr_dt, by=c('region'))

yr_efw_dt <- yr_efw_dt %>% mutate(ef_change = ef - ef_start)

yr_world_efw_map <- inner_join(world, yr_efw_dt, by='region')

#poly_color <- rep(NA, nrow(yr_world_efw_map))
#poly_color[which(yr_world_efw_map$quartile==select_qt)] = '#5aae61'

p_world_efw <- ggplot(data=yr_world_efw_map, mapping=aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill=ef_change), size=0.15) +
  #scale_fill_distiller(palette = 'BuPu', na.value='grey90', name='Economics \nFreedom') +
  scale_fill_gradient2(midpoint = 0, 
                       mid='#f7f7f7', low='#1a9850', high='#d73027',
                       na.value='gray80') +
  scale_x_continuous(breaks=c(-100, 0, 100)) +
  labs(title='Change in Econ Freedom Indices from 2000 to 2016') +
  theme_bw(base_size=10) +
  theme (
    axis.text.x = element_text(angle=0, vjust=0.6),
    axis.text.y = element_text(angle=0, hjust=0.6), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    plot.title = element_text(hjust = 0, vjust=0.5, face='bold', size=9),
    legend.text.align = 1,
    legend.key.width = grid::unit(0.3,'cm'),
    legend.title = element_blank(),
    panel.border=element_blank()
  )

ggsave('world_change.pdf', plot=p_world_efw, width=10, height=6.25)

#
#
#
## model selection
ols_efw_dt <- efw_dt %>%
  dplyr::select(ef, region, year, gov_consump_1=X1a_government_consumption, gov_etps_1=X1c_gov_enterprises,
         ppr_2=X2c_protection_property_rights, jud_indep_2=X2a_judicial_independence,
         money_grow_3=X3a_money_growth, free_for_curr_3=X3d_freedom_own_foreign_currency,
         tariff_4=X4a_tariffs, black_market_4=X4c_black_market, 
         control_ppl_4=X4d_control_movement_capital_ppl, cred_reg_5=X5a_credit_market_reg,
         labor_reg_5=X5b_labor_market_reg)


ols_efw_dt <- ols_efw_dt[-which(is.na(ols_efw_dt$ef)),]

fit_ols <- lm(data = ols_efw_dt, ef~.-year-region)

#
#
#
##scatter plots
efw_2016_dt <- ols_efw_dt %>%
  dplyr::filter(year==2016)

choose_20 <- c("Argentina", "Brazil", "Canada", "China",
               "Egypt", "France", "Greece", "India","Iran", "Italy" ,  
               "Mexico",  "Norway", "Portugal", "Russia", "Turkey","Japan",
               "Germany", "UK", "USA", "Switzerland", "Hong Kong")

efw_2016_dt <-efw_2016_dt %>% 
  mutate(plot_label = ifelse(region %in% choose_20, region, NA))
efw_2016_dt <- efw_2016_dt[-which(efw_2016_dt$region=='Venezuela'), ]

## section 1
efw_2016_dt1 <- efw_2016_dt

xy_limits <- c(min(efw_2016_dt1$gov_consump_1), max(efw_2016_dt1$gov_consump_1), 
               min(efw_2016_dt1$ef), max(efw_2016_dt1$ef))

p_s1 <- ggplot(efw_2016_dt1, aes(x=gov_consump_1, y=ef, color=gov_etps_1)) +
  geom_point(alpha=0.8) +
  geom_text(aes(label=plot_label), 
            show.legend=F,size=2.5, col='black',
            nudge_x=0, nudge_y=-0.15) +
  geom_smooth(method=lm, col='#636363', size=0.5, fill='gray70') +
  geom_rug(color='black', size=0.2, sides='b') +
  scale_y_continuous(name = 'Economic Freedom', limits = c(xy_limits[3], xy_limits[4])) +
  scale_x_continuous(name = 'Goverment Consumption', limits=c(xy_limits[1], xy_limits[2])) +
  scale_color_distiller(name='Gov\nInvest', palette = 'BuPu', direction=1) +
  theme(
    panel.background = element_rect(fill=NA),
    axis.ticks = element_blank(),
    axis.title = element_text(size=10, face='bold'),
    legend.key.width = grid::unit(0.3,'cm'),
    legend.title = element_text(size=10, face='bold'),
    legend.text = element_text(size=8, face='bold')
  )

## section 2
efw_2016_dt2 <- efw_2016_dt[-which(is.na(efw_2016_dt$jud_indep_2)),]

xy_limits <- c(min(efw_2016_dt2$jud_indep_2), max(efw_2016_dt2$jud_indep_2), 
               min(efw_2016_dt2$ef), max(efw_2016_dt2$ef))


p_s2 <- ggplot(efw_2016_dt2, aes(x=jud_indep_2, y=ef, color=ppr_2)) +
  geom_point(alpha=0.8) +
  geom_text(aes(label=plot_label), 
            show.legend=F,size=2.5, col='black',
            nudge_x=0, nudge_y=-0.15) +
  geom_smooth(method=lm, col='#636363', size=0.5, fill='gray70') +
  geom_rug(sides='b', color='black', size=0.1) +
  scale_y_continuous(name = 'Economic Freedom', 
                     limits = c(xy_limits[3], xy_limits[4])) +
  scale_x_continuous(name = 'Judicial Independence', limits=c(xy_limits[1], xy_limits[2])) +
  scale_color_distiller(name='Property\nRight', palette = 'BuPu', direction=1) +
  theme(
    panel.background = element_rect(fill=NA),
    axis.ticks = element_blank(),
    axis.title = element_text(size=10, face='bold'),
    
    legend.key.width = grid::unit(0.3,'cm'),
    legend.title = element_text(size=10, face='bold'),
    legend.text = element_text(size=8, face='bold')
  )

## section 3
efw_2016_dt3 <- efw_2016_dt
efw_2016_dt3$free_for_curr_3 <- as.integer(efw_2016_dt3$free_for_curr_3)

xy_limits <- c(min(efw_2016_dt3$money_grow_3), max(efw_2016_dt3$money_grow_3), 
               min(efw_2016_dt3$ef), max(efw_2016_dt3$ef))


p_s3 <- ggplot(efw_2016_dt3, aes(x=money_grow_3, y=ef))+
  geom_point(alpha=0.8, aes(color=factor(free_for_curr_3, labels=c('Low', 'Mid', 'High')))) +
  geom_text(aes(label=plot_label), 
            show.legend=F,size=2.5, col='black',
            nudge_x=0, nudge_y=-0.15) +
  geom_smooth(method=lm, col='#636363', size=0.5, fill='gray70') +
  geom_rug(sides='b', color='black', size=0.1) +
  scale_y_continuous(name = 'Economic Freedom', 
                     limits = c(xy_limits[3], xy_limits[4])) +
  scale_x_continuous(name = 'Money Growth / Inflaction', limits=c(xy_limits[1], xy_limits[2])) +
  scale_color_brewer(name='Freedom\nOwn\nCurrency', type='seq', palette = 'BuPu') +
  guides(
    color=guide_legend(override.aes = list(shape = 15, size=5))
  ) +
  theme(
    panel.background = element_rect(fill=NA),
    axis.ticks = element_blank(),
    axis.title = element_text(size=10, face='bold'),
    
    legend.key = element_rect(fill='white'),
    legend.title = element_text(size=10, face='bold'),
    legend.text = element_text(size=8, face='bold')
  )

## section 4
efw_2016_dt4 <- efw_2016_dt
efw_2016_dt4 <- efw_2016_dt4[-which(is.na(efw_2016_dt4$tariff_4)),]
efw_2016_dt4 <- efw_2016_dt4 %>%
  mutate(allow_bm = ifelse(black_market_4==10.0, 'Active', 'Not Active'))
efw_2016_dt4 <- efw_2016_dt4[-which(is.na(efw_2016_dt4$allow_bm)),]

xy_limits <- c(min(efw_2016_dt4$tariff_4), max(efw_2016_dt4$tariff_4), 
               min(efw_2016_dt4$ef), max(efw_2016_dt4$ef))


p_s4 <- ggplot(efw_2016_dt4, aes(x=tariff_4, y=ef, group=factor(allow_bm))) +
  geom_point(alpha=0.8, aes(col=control_ppl_4, shape=allow_bm)) +
  geom_smooth(aes(group='Active'), method=lm, 
              color='#636363') +
  geom_rug(sides='b', color='black', size=0.1) +
  scale_y_continuous(name = 'Economic Freedom', 
                     limits = c(xy_limits[3], xy_limits[4])) +
  scale_x_continuous(name = 'Tariffs', limits=c(xy_limits[1], xy_limits[2])) +
  scale_color_distiller(name='Movement\nCapital\nPeople', palette = 'BuPu', direction=1) +
  scale_shape_manual(name='Black\nMarket', values=c(16, 1)) + 
  geom_text(aes(label=plot_label), 
            show.legend=F,size=2.5, col='black',
            nudge_x=0, nudge_y=-0.15) +
  guides(
    shape=guide_legend(override.aes = list(size=2))
  ) +
  theme(
    panel.background = element_rect(fill=NA),
    axis.ticks = element_blank(),
    axis.title = element_text(size=10, face='bold'),
    
    legend.key = element_rect(fill='white'),  
    legend.key.width = grid::unit(0.3,'cm'),
    legend.title = element_text(size=10, face='bold'),
    legend.text = element_text(size=8, face='bold')
  )

## section 5
efw_2016_dt5 <- efw_2016_dt

xy_limits <- c(min(efw_2016_dt5$labor_reg_5), max(efw_2016_dt5$labor_reg_5), 
               min(efw_2016_dt5$ef), max(efw_2016_dt5$ef))


p_s5 <- ggplot(efw_2016_dt5, aes(x=labor_reg_5, y=ef, color=cred_reg_5)) +
  geom_point(alpha=0.8) +
  geom_text(aes(label=plot_label), 
            show.legend=F,size=2.5, col='black',
            nudge_x=0, nudge_y=-0.15) +
  geom_smooth(method=lm, col='#636363', size=0.5, fill='gray70') +
  geom_rug(sides='b', color='black', size=0.1) +
  scale_y_continuous(name = 'Economic Freedom', 
                     limits = c(xy_limits[3], xy_limits[4])) +
  scale_x_continuous(name = 'Labor Market Regulations', limits=c(xy_limits[1], xy_limits[2])) +
  scale_color_distiller(name='Credit\nMarket\nRegulations', palette = 'BuPu', direction=1) +
  theme(
    panel.background = element_rect(fill=NA),
    axis.ticks = element_blank(),
    axis.title = element_text(size=10, face='bold'),
    
    legend.key.width = grid::unit(0.3,'cm'),
    legend.title = element_text(size=10, face='bold'),
    legend.text = element_text(size=8, face='bold')
  )

grd1 <- grid.arrange(p_s1, p_s3, p_s2, p_s4, p_s5, 
                     layout_matrix = rbind(c(1,2,4),c(3,5,NA)))


ggsave('area_scatter.pdf', plot=grd1, width=25, height=12)

#
#
#
## ropladder 
model2 <- ef~gov_consump_1 + gov_etps_1 + ppr_2 + jud_indep_2 + money_grow_3 + 
  free_for_curr_3 + tariff_4 + black_market_4 + control_ppl_4 + cred_reg_5 + labor_reg_5

fit_ols <- lm(data = ols_efw_dt, model2)
fit_ols_pe <- fit_ols$coefficients
fit_ols_vc <- vcov(fit_ols)

xscen <- cfMake(model2, data=ols_efw_dt, nscen=11)

xscen <- cfName(xscen, 'Gov consumption + 0.5 sd', scen=11)
xscen <- cfChange(xscen, 'gov_consump_1', 
                  x=mean(na.omit(ols_efw_dt$gov_consump_1)) - 
                    0.5 * sd(na.omit(ols_efw_dt$gov_consump_1)),
                  scen=11)

xscen <- cfName(xscen, 'Gov Invest/Sponsor + 0.5 sd', scen=7)
xscen <- cfChange(xscen, 'gov_etps_1', 
                  x=mean(na.omit(ols_efw_dt$gov_etps_1)) + 
                    0.5 * sd(na.omit(ols_efw_dt$gov_etps_1)),
                  scen=7)

xscen <- cfName(xscen, 'Protection Property Rights + 0.5 sd', scen=2)
xscen <- cfChange(xscen, 'ppr_2', 
                  x=mean(na.omit(ols_efw_dt$ppr_2)) +
                    0.5 * sd(na.omit(ols_efw_dt$ppr_2)),
                  scen=2)

xscen <- cfName(xscen, 'Judicial Independence + 0.5 sd', scen=1)
xscen <- cfChange(xscen, 'jud_indep_2', 
                  x=mean(na.omit(ols_efw_dt$jud_indep_2)) + 
                    0.5 * sd(na.omit(ols_efw_dt$jud_indep_2)),
                  scen=1)

xscen <- cfName(xscen, 'Inflation + 0.5 sd', scen=6)
xscen <- cfChange(xscen, 'money_grow_3', 
                  x=mean(na.omit(ols_efw_dt$money_grow_3)) + 
                    0.5 * sd(na.omit(ols_efw_dt$money_grow_3)),
                  scen=6)

xscen <- cfName(xscen, 'Freedom Own Foreign Currency + 0.5 sd', scen=4)
xscen <- cfChange(xscen, 'free_for_curr_3', 
                  x=mean(na.omit(ols_efw_dt$free_for_curr_3)) + 
                    0.5 * sd(na.omit(ols_efw_dt$free_for_curr_3)),
                  scen=4)

xscen <- cfName(xscen, 'Tariff + 0.5 sd', scen=9)
xscen <- cfChange(xscen, 'tariff_4', 
                  x=mean(na.omit(ols_efw_dt$tariff_4)) + 
                    0.5 * sd(na.omit(ols_efw_dt$tariff_4)),
                  scen=9)

xscen <- cfName(xscen, 'Black Market Active + 0.5 sd', scen=3)
xscen <- cfChange(xscen, 'black_market_4', 
                  x=mean(na.omit(ols_efw_dt$black_market_4)) + 
                    0.5 * sd(na.omit(ols_efw_dt$black_market_4)),
                  scen=3)

xscen <- cfName(xscen, 'Capital Control Move + 0.5 sd', scen=10)
xscen <- cfChange(xscen, 'control_ppl_4', 
                  x=mean(na.omit(ols_efw_dt$control_ppl_4)) + 
                    0.5 * sd(na.omit(ols_efw_dt$control_ppl_4)),
                  scen=10)

xscen <- cfName(xscen, 'Labor Market Reg + 0.5 sd', scen=5)
xscen <- cfChange(xscen, 'labor_reg_5', 
                  x=mean(na.omit(ols_efw_dt$labor_reg_5)) + 
                    0.5 * sd(na.omit(ols_efw_dt$labor_reg_5)),
                  scen=5)

xscen <- cfName(xscen, 'Credit Market Reg + 0.5 sd', scen=8)
xscen <- cfChange(xscen, 'cred_reg_5', 
                  x=mean(na.omit(ols_efw_dt$cred_reg_5)) + 
                    0.5 * sd(na.omit(ols_efw_dt$cred_reg_5)),
                  scen=8)

sims <- 10000

simbeta_lm <- mvrnorm(sims, fit_ols_pe, fit_ols_vc)
lm2_qoi <- linearsimfd(xscen, simbeta_lm, ci=0.95)

trace1 <- ropeladder(
  x=lm2_qoi$pe,
  lower=lm2_qoi$lower,
  upper=lm2_qoi$upper,
  labels=row.names(xscen$x),
  plot=1
)

singlevertmark <- linesTile(
  x=c(0,0),
  y=c(0,1),
  lty='solid',
  plot=1)

tile(
  RxC = c(1,1),
  width=list(plot=2, yaxistitle=0.5, rightborder=2),
  singlevertmark,
  trace1,
  xaxis = list(at=c(-0.06, -0.03, 0, 0.03, 0.06, 0.09)),
  xaxistitle=list(labels='E(Economic Freedom Index)'),
  topaxis = list(at=c(-0.06, -0.03, 0, 0.03, 0.06, 0.09), 
                labels=c('0.99x', '0.995x','1x', '1.005x', '1.01x', '1.015x'),
                add=T),
  topaxistitle = list(labels='E(Econ Freedom Index) / average'),
  gridlines = list(type='t'),
  output = list(file='rop_lm', width=8)
)


#
#
#
## select region 
select_region <- 'China'

time_col <- brewer.pal(5, 'Set2')

reg_efw_dt <- efw_dt %>% filter(region==select_region)
reg_efw_dt <- reg_efw_dt[-which(is.na(reg_efw_dt$ef)),]

from_yr <- min(reg_efw_dt$year)
to_yr <- max(reg_efw_dt$year)

## time series 
p_time_efw <- ggplot(data=reg_efw_dt, aes(x=year)) +
  geom_path(aes(y=ef), color=time_col[1]) +
  geom_line(aes(y=X1a_government_consumption), color=time_col[2]) + 
  geom_line(aes(y=X2c_protection_property_rights), color=time_col[3]) + 
  geom_line(aes(y=X4a_tariffs), color=time_col[4]) +
  geom_line(aes(y=X2a_judicial_independence), color=time_col[5]) + 
  geom_point(aes(y=ef), color=time_col[1]) +
  geom_point(aes(y=X1a_government_consumption), color=time_col[2]) + 
  geom_point(aes(y=X2c_protection_property_rights), color=time_col[3]) + 
  geom_point(aes(y=X4a_tariffs), color=time_col[4]) +
  geom_point(aes(y=X2a_judicial_independence), color=time_col[5]) + 
  xlim(min(from_yr), max(to_yr)) +
  theme_ipsum() + theme (
    axis.text.x = element_text(angle=45, vjust=0.6),
    axis.text.y = element_text(angle=0, hjust=0.6), 
    plot.title = element_text( hjust=0.5, vjust=0),
    legend.text = element_text(face='bold'),
    legend.title = element_blank(),
    legend.key.width = grid::unit(0.3,'cm'),
    panel.border=element_blank()
  )






