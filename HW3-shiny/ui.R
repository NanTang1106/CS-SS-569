## please run the following commented code before clicking on 'run app' button 

#install.packages(c('gapminder', 'dplyr', 'ggmap', 'maps', 'car', 'MASS','hrbrthemes', 'RColorBrewer', 'gapminder'))

library('shiny')
library('gapminder')
library('dplyr')
library('ggmap')
library('maps')

#dir <- '/Users/nantang/Desktop/HW3'
#setwd(dir)
efw_dt <- read.csv('efw_cc.csv')

## remove NA data
NA_index <- numeric(ncol(efw_dt))
for (i in 1:ncol(efw_dt)) {
  NA_index[i] <- length(which(is.na(efw_dt[,i])))
}
efw_dt <- efw_dt[-NA_index, ]

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

## min and max value of slider
earlies_yr <- min(efw_dt$year)
latest_yr <- max(efw_dt$year)

## choice for selectinput
country_names <-unique(efw_dt$region)


shinyUI(fluidPage(
  headerPanel('An Overview of World Economic Freedom'),
  
  column(
    width=12, 
    
    p('Economic Freedom is the basic right of every individual, where every person 
      is free to use their income,property & resources and invest according to their will, 
      without much intervention from the Governmental Policies. This is very important 
      for overall prosperity of the society, democracy and the fundamental right to make 
      economic decisions.'),
    
    p('This analysis is based on the data of 2018 index of economic freedom, world’s 
      premier measurement of economic freedom, ranking countries based on five areas: 
      size of government, legal structure and security of property rights, access to 
      sound money, freedom to trade internationally, and regulation of credit, labour 
      and business.'),
    
    tabsetPanel(
      tabPanel(
        title='map', 
        h2(),
        
        plotOutput('efw_map'), 
        hr(),
        fluidRow(
          column(4,
                 sliderInput('select_year', 
                             label='Choose A Year',
                             min=earlies_yr,
                             max=latest_yr,
                             value=2015,
                             step=5),
          ),
          column(3, offset=1,
                 radioButtons('quartile',
                            label='Choose A Quartile',
                              c(
                                'High' = 1,
                                'Medium-High' = 2,
                                'Restricted-Medium' =3,
                                'Restricted'=4
                              )),
          ),
          column(width = 1, offset = 0, style='padding:0px;'),
          column(width = 2,
                 actionButton(inputId='mapping', 
                              label='See Map'),
          )
          
        ),
      ),
      tabPanel(
        title='time series',
        
        h2(),
        
        
        sidebarPanel(
          
          selectInput(inputId = 'country', label = 'Choose A Country', 
                      choices = as.character(country_names),
                      selected='USA'), 
          
          actionButton(inputId='timeseries', 
                       label='See Time Series'),
        ),
        
        mainPanel(
          plotOutput('efw_time')
          )
      )
    )
  ),
  
  ## plot world map 
  #mainPanel(
  #  plotOutput('efw_map')
  #)
  
))

