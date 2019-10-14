library(shiny)
library(tidyverse)
library(reshape2)

function(input, output) {
  vgsales <- eventReactive(input$apply, {
    
    tmp <- sales %>%
      filter(Year >= input$year[1], Year <= input$year[2],
             Global_Sales >= input$sales[1], Global_Sales <= input$sales[2])
    
    if(input$genre != 'Tous'){
      tmp <- tmp %>% filter(Genre == input$genre)
    }
    
    if(input$platform != 'Toutes'){
      if(input$platform == 'Autres'){
        tmp <- tmp %>% filter( !(Platform %in% big.platforms$Platform) )
      } else {
        tmp <- tmp %>% filter(Platform == input$platform)
      }
    }
    
    if(input$publisher != 'Tous'){
      if(input$publisher == 'Autres'){
        tmp <- tmp %>% filter( !(Publisher %in% big.publishers$Publisher) )
      } else {
        tmp <- tmp %>% filter(Publisher == input$publisher)
      }
    }
    
    # criteres avancés
    platforms <- sales %>%
      group_by(Platform) %>%
      summarise(tot.sales = sum(Global_Sales, na.rm = TRUE),
                nb.games = n()) %>%
      filter(tot.sales >= input$platformSales[1], tot.sales <= input$platformSales[2],
             nb.games >= input$platformGames[1], nb.games <= input$platformGames[2]) %>%
      select(Platform) %>% distinct
    
    tmp <- filter(tmp, Platform %in% platforms$Platform)
    # print(dim(tmp))
    
    
    publishers <- sales %>%
      group_by(Publisher) %>%
      summarise(total.sales = sum(Global_Sales, na.rm = TRUE)) %>%
      filter(total.sales >= input$publisherSales[1], total.sales <= input$publisherSales[2]) %>%
      select(Publisher) %>% distinct
    
    # print('Nintendo' %in% publishers$Publisher)
    # print(summary(tmp %>% select(Publisher) %>% distinct))

    tmp <- filter(tmp, Publisher %in% publishers$Publisher)
    
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
    
    plt + xlab(axis.names[input$plot2_varsel2]) +
      ylab(axis.names[input$plot2_varsel1])
  })
  
  
  output$plot3 <- renderPlot({
    tab <- vgsales() %>%
      select(.data[[input$plot3_varsel1]], .data[[input$plot3_varsel2]]) %>%
      table()
    
    # print(class(tab))
    # print(tab)
    
    melted_tab <- melt(tab)
    # print(head(melted_tab))
    # print(length(melted_tab))
    
    plt <- ggplot(melted_tab, aes(x = .data[[input$plot3_varsel1]],
                                  y = .data[[input$plot3_varsel2]])) +
      geom_tile(aes(fill = value))
      
      
    
    plt + xlab(axis.names[input$plot3_varsel1]) +
      ylab(axis.names[input$plot3_varsel2])
  })
  
  output$plot4 <- renderPlot({
  
    plt <- ggplot(vgsales(), aes(x = .data[[input$plot4_varsel]]))
    if(input$plot4_varsel %in% quali.vars){
      plt <- plt + geom_bar()
    } else {
      plt <- plt + geom_histogram()
    }
    
    plt + xlab(axis.names[input$plot3_varsel1])
  })
  
  
  
  
  ## Analyse univariee ----
  output$nbGames.v.Year <- renderPlot({
    ggplot(sales) + 
      geom_bar(aes(x = Year))
  })
  
  output$nbGames.v.Year.summary <- renderTable({
    sales %>% group_by(Year) %>%
      summarise(Nb_Jeux = n()) %>%
      summarise(Minimum = min(Nb_Jeux),
                Maximum = max(Nb_Jeux),
                Moyenne = mean(Nb_Jeux),
                Mediane = median(Nb_Jeux))
  })
  
  
  output$nbGames.v.Genre.tab <- renderTable({
    sales %>% group_by(Genre) %>%
      summarise(Nb_Jeux = n()) %>%
      select(Genre, Nb_Jeux) %>%
      arrange(desc(Nb_Jeux))
  })
  output$nbGames.v.Genre.plot <- renderPlot({
    ggplot(sales) + 
      geom_bar(aes(x = Genre))
  })
  
  output$nbGames.v.Publisher <- renderTable({
    sales %>% group_by(Publisher) %>%
      summarise(Nb_Jeux = n()) %>%
      select(Publisher, Nb_Jeux) %>%
      arrange(desc(Nb_Jeux)) %>%
      slice(1:10)
  })
  output$nbPublishers <- renderText({
    sales %>% select(Publisher) %>% distinct() %>% count() %>% as.numeric()
  })
  
  output$platform.summary <- renderPrint({
    sales %>% group_by(Platform) %>%
      summarise(Nb_Jeux = n()) %>%
      select(Platform, Nb_Jeux) %>%
      arrange(desc(Nb_Jeux)) %>%
      slice(1:5)
  })
  
  output$platform.summary <- renderPrint({
    sales %>% group_by(Platform) %>%
      summarise(Nb_Jeux = n()) %>%
      select(Platform, Nb_Jeux) %>%
      arrange(desc(Nb_Jeux)) %>%
      slice(1:5)
  })
  
  output$hist.ventesGlobales <- renderPlot({
    hist1 <- ggplot(sales) +
      geom_histogram(aes(x = Global_Sales))
    
    hist2 <- ggplot(sales %>% filter(Global_Sales <= 1)) +
      geom_histogram(aes(x = Global_Sales))
    
    cowplot::plot_grid(hist1, hist2, nrow = 1)
  })
  output$hist.ventesEU <- renderPlot({
    hist1 <- ggplot(sales) +
      geom_histogram(aes(x = EU_Sales))
    
    hist2 <- ggplot(sales %>% filter(EU_Sales <= 1)) +
      geom_histogram(aes(x = EU_Sales))
    
    cowplot::plot_grid(hist1, hist2, nrow = 1)
  })
  output$hist.ventesNA <- renderPlot({
    hist1 <- ggplot(sales) +
      geom_histogram(aes(x = NA_Sales))
    
    hist2 <- ggplot(sales %>% filter(Global_Sales <= 1)) +
      geom_histogram(aes(x = NA_Sales))
    
    cowplot::plot_grid(hist1, hist2, nrow = 1)
  })
  output$hist.ventesJP <- renderPlot({
    hist1 <- ggplot(sales) +
      geom_histogram(aes(x = JP_Sales))
    
    hist2 <- ggplot(sales %>% filter(Global_Sales <= 1)) +
      geom_histogram(aes(x = JP_Sales))
    
    cowplot::plot_grid(hist1, hist2, nrow = 1)
  })
  output$hist.ventesOthers <- renderPlot({
    hist1 <- ggplot(sales) +
      geom_histogram(aes(x = Other_Sales))
    
    hist2 <- ggplot(sales %>% filter(Global_Sales <= 1)) +
      geom_histogram(aes(x = Other_Sales))
    
    cowplot::plot_grid(hist1, hist2, nrow = 1)
  })
  
  ## Analyse multivariee ----
  output$best.genre.v.year <- renderPlot({
    tmp <- sales %>%
      group_by(Year, Genre) %>%
      summarise(nb = n(),
                tot.sales = sum(Global_Sales)) %>%
      filter(nb == max(nb)) %>%
      arrange(Year)
    ggplot(tmp, aes(x = Year, y = nb)) +
      geom_line(color = 'tomato4') +
      geom_point(aes(color = Genre)) +
      geom_text(aes(label = Genre), color = 'darkblue')
  })
  
  output$sales.v.nbGames <- renderPrint({
    tmp <- sales %>%
      group_by(Year, Genre) %>%
      summarise(nb = n(),
                tot.sales = sum(Global_Sales)) %>%
      filter(nb == max(nb)) %>%
      arrange(Year)
    cor(tmp$nb, tmp$tot.sales, method = 'spearman')
  })
  
  output$corr.plot <- renderPlot({
    corrs <- cor(sales[,c('Global_Sales', 'NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales')], method = 'spearman')
    
    corrs <- round(corrs, 2)
    
    melted_corrs <- melt(corrs)
    
    ggplot(data = melted_corrs, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white") +
      geom_text(aes(label = value)) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed()
  })
  
  output$cor.test.output <- renderPrint({
    cor.test(sales$EU_Sales, sales$JP_Sales, alternative = 'less', method = 'spearman')
  })
  
  lifetime <- sales %>%
    group_by(Platform) %>%
    summarise(begin = min(Year, na.rm = TRUE), end = max(Year, na.rm = TRUE),
              num.games = n(), tot.sales = sum(Global_Sales, na.rm = TRUE)) %>%
    filter(num.games >= 20) %>%
    arrange(end)
  
  output$platform.lifetime <- renderTable({
    lifetime
  })
  
  output$platform.sales <- renderPlot({
    big.platforms <- sales %>%
      select(Platform) %>%
      distinct() %>%
      filter( Platform %in% lifetime$Platform )
    
    platform.sales <- sales %>%
      filter( Platform %in% big.platforms$Platform ) %>%
      group_by(Platform, Year) %>%
      summarise(year.sales = sum(Global_Sales, na.rm = TRUE))
    
    ggplot(platform.sales, aes(x = Year, y = year.sales)) +
      geom_line() +
      geom_point(color = 'blue', alpha = .8) + 
      facet_wrap(~ Platform)
  })
}
