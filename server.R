library(shiny)
library(tidyverse)


init <- function(){
  inFile <- './data/vgsales.csv'
  sales <- read_csv(inFile, col_types = 'icfiffddddd')
  sales <- drop_na(sales) %>%
    filter(Year < 2016, Publisher != 'Unknown')
  
  big.platforms <- sales %>%
    group_by(Platform) %>%
    summarise(tot.games = n(),
              tot.sales = sum(Global_Sales))  %>%
    filter(tot.games > 20, tot.sales > 25) %>%
    select(Platform)
  
  big.publishers <- sales %>%
    group_by(Publisher) %>%
    summarise(tot.games = n(),
              tot.sales = sum(Global_Sales))  %>%
    filter(tot.games > 20, tot.sales > 25) %>%
    select(Publisher)
  
  sales <- sales %>% filter(Publisher %in% big.publishers$Publisher,
                            Platform %in% big.platforms$Platform)
  
  print(dim(sales))
  return(sales)
}

function(input, output) {
  sales <- init()
  
  vgsales <- reactive({
    # print(summary(sales))
    print(dim(sales))
    sales
  })
  
  output$table.interactive <- renderDataTable({
    vgsales()
  })
  quick.plot <- function(X){
    qplot(data = X, y = YearlySales, x = Year) +
      labs(title = 'Yearly sales by platform') +
      facet_wrap(~Platform)
  }
  output$yearly.sales.by.platform <- renderPlot({
    # obtenir une table ou on a les recettes groupees par platforme et aggregees par annee.
    vgsales() %>% group_by()
  })
  
  output$plot1 <- renderPlot({
    ggplot(vgsales()) + geom_bar(aes(x = Genre))
  })
  
  
  
}
