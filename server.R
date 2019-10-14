library(shiny)
library(tidyverse)

function(input, output) {
  vgsales <- eventReactive(input$apply, {
    print(input$year)
    print(input$sales)
    tmp <- sales %>%
      filter(Year >= input$year[1], Year <= input$year[2],
             Global_Sales >= input$sales[1], Global_Sales <= input$sales[2])
    
    if(input$genre != 'Tous'){
      tmp <- tmp %>% filter(Genre == input$genre)
    }
    
    if(input$platform != 'Toutes'){
      if(input$platform == 'Autres'){
        tmp <- tmp %>% filter( !(Platform %in% big.platforms) )
      } else {
        tmp <- tmp %>% filter(Platform == input$platform)
      }
    }
    
    if(input$publisher != 'Tous'){
      if(input$publisher == 'Autres'){
        tmp <- tmp %>% filter( !(Publisher %in% big.publishers) )
      } else {
        tmp <- tmp %>% filter(Publisher == input$publisher)
      }
    }
    
    
    # print(dim(tmp))
    tmp
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
    plt <- ggplot(vgsales(), aes(.data[[input$plot1_varsel1]],
                                 .data[[input$plot1_varsel2]])) +
      geom_point(aes(color = Genre), size = 3)
    if(input$add.lm){
      plt <- plt + geom_smooth(method = 'lm')
    }
    
    
    top.ten.x <- vgsales() %>% arrange(desc(.data[[input$plot1_varsel1]])) %>% slice(1:10)
    top.ten.y <- vgsales() %>% arrange(desc(.data[[input$plot1_varsel2]])) %>% slice(1:10)
    
    plt <- plt + 
      geom_text(data = top.ten.x, color = 'red', alpha = .8,
                aes(label = .data[[input$point.label]])) +
      geom_text(data = top.ten.y, size = 4, color = 'darkblue', alpha = .8,
                aes(label = .data[[input$point.label]]))
      
    
    plt + xlab(axis.names[input$plot1_varsel1]) +
      ylab(axis.names[input$plot1_varsel2])
  })
  
  output$plot2 <- renderPlot({
    plt <- ggplot(vgsales(), aes(.data[[input$plot2_varsel2]],
                                 .data[[input$plot2_varsel1]])) +
      geom_boxplot(fill = 'wheat', color = 'tomato4') +
      geom_jitter(color = 'green', alpha = .5)
    
    plt + xlab(axis.names[input$plot2_varsel1]) +
      ylab(axis.names[input$plot2_varsel2])
  })
  
  
  
}
