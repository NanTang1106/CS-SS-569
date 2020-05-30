library('shiny')
library('dplyr') 
library('ggmap')
library('maps')
library('car')
library('MASS')
library('hrbrthemes')
library('RColorBrewer')
library('gapminder')


#dir <- '/Users/nantang/Desktop/HW3'
#setwd(dir)
efw_dt <- read.csv('efw_cc.csv')

# <- numeric(ncol(efw_dt))
#for (i in 1:ncol(efw_dt)) {
#  NA_index[i] <- length(which(is.na(efw_dt[,i])))
#}
#efw_dt <- efw_dt[-NA_index, ]

## merge econ data with map data
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

shinyServer(function(input, output){
  
  ## reactive for map and time series
  rv <- reactiveValues(yr=2015, qtl=1, region='USA')
  
  observeEvent(input$mapping, {
    rv$yr=input$select_year
    rv$qtl=input$quartile
  })
  
  output$efw_map <- renderPlot({
    select_yr <- rv$yr
    select_qt <- rv$qtl
    
    yr_efw_dt <- efw_dt %>% filter(year==select_yr)
    
    yr_world_efw_map <- inner_join(world, yr_efw_dt, by='region')
    
    poly_color <- rep(NA, nrow(yr_world_efw_map))
    poly_color[which(yr_world_efw_map$quartile==select_qt)] = '#5aae61'
    
    p_world_efw <- ggplot(data=yr_world_efw_map, mapping=aes(x=long, y=lat, group=group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill=ef), color=poly_color, size=0.3) +
      scale_fill_distiller(palette = 'BuPu', na.value='grey90', name='Economics \nFreedom') +
      theme_bw() +
      theme (
        axis.text.x = element_text(angle=0, vjust=0.6),
        axis.text.y = element_text(angle=0, hjust=0.6), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text( hjust=0.5, vjust=0),
        legend.text = element_text(face='bold'),
        legend.key.width = grid::unit(0.3,'cm'),
        legend.title = element_text(size=8),
        panel.border=element_blank()
      )
    
    p_world_efw
  })
  
  observeEvent(input$timeseries, {
    rv$region = input$country }
  )

  output$efw_time <- renderPlot({
    
    select_region <- 'USA'
    
    select_region <- rv$region
    
    time_col <- brewer.pal(5, 'Set1')
    
    reg_efw_dt <- efw_dt %>% filter(region==select_region)
    
    if(length(which(is.na(reg_efw_dt$ef))) > 0) {
      reg_efw_dt <- reg_efw_dt[-which(is.na(reg_efw_dt$ef)),]
    }
    
    from_yr <- min(na.omit(reg_efw_dt$year))
    to_yr <- max(na.omit(reg_efw_dt$year))
    
    ## time series 
    p_time_efw <- ggplot(data=reg_efw_dt, aes(x=year)) +
      geom_line(aes(y=ef, color='ef')) +
      geom_line(aes(y=X1a_government_consumption, color='gc')) + 
      geom_line(aes(y=X2c_protection_property_rights, color='ppr')) + 
      geom_line(aes(y=X4a_tariffs, color='tf')) +
      geom_line(aes(y=X2a_judicial_independence, color='ji')) + 
      geom_point(aes(y=ef, color='ef')) +
      geom_point(aes(y=X1a_government_consumption, color='gc')) + 
      geom_point(aes(y=X2c_protection_property_rights, color='ppr')) + 
      geom_point(aes(y=X4a_tariffs, color='tf')) +
      geom_point(aes(y=X2a_judicial_independence, color='ji')) + 
      scale_color_manual(breaks=c('ef', 'gc', 'ppr', 'tf', 'ji'), 
                         values=time_col, 
                         labels=c('Econ Freedom', 'Gov Consumption', 
                                  'Prot Prop. Rights', 'Tariffs', 
                                  'Judicial Indep')) +
      xlim(min(from_yr), max(to_yr)) +
      labs(
        y='Indices',
        x='Year'
      ) + 
      guides(
        color=guide_legend(override.aes = list(shape = 15, size = 5))
      ) +
      theme_ipsum() + theme (
        axis.text.x = element_text(angle=45, vjust=0.6),
        axis.text.y = element_text(angle=0, hjust=0.6), 
        legend.text = element_text(face='bold'),
        legend.title = element_blank(),
        legend.key.width = grid::unit(0.5,'cm'),
        legend.position = "bottom",
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank()
      )
    
    p_time_efw
  })
})




