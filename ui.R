library(shiny)


quali.vars <- c('Platform', 'Genre', 'Publisher')
quanti.vars <- c('Rank', 'Year', 'NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales')
variables <- c(quanti.vars, quali.vars)

genres <- sales %>% select(Genre) %>% 
  distinct() %>%
  arrange(Genre)

navbarPage("Ventes de Jeux Videos",
           tabPanel('Exploration',
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput('year', label = 'Date de publication',
                                    min = 1980, max = 2016, value = c(1980,2015)),
                        sliderInput('sales', label = 'Copies Vendues',
                                    min = .1, max = 90, value = c(1,25)),
                        selectInput('platform', label = 'Platform',
                                    choices = c('Toutes', big.platforms, 'Autres')),
                        selectInput('publisher', label = 'Publisher',
                                    choices = c('Tous', big.publishers, 'Autres')),
                        selectInput('genre', label = 'Genre',
                                    choices = c('Tous', genres)),
                        h2('Criteres avances:'),
                        sliderInput('platformSales', label = 'Ventes par plateforme',
                                    min = 0, max = 1500, value = c(3,500)),
                        sliderInput('publisherSales', label = 'Ventes par publisher',
                                    min = 0, max = 1000, value = c(3,300)),
                        sliderInput('platformGames', label = 'Nombre de jeux par plateforme',
                                    min = 1, max = 2200, value = c(200,2000)),
                        actionButton('apply', label = 'Appliquer')
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Graphiques",
                                   fluidRow(
                                     column(10,
                                            tags$h3("Nuage de points"),
                                            fluidRow(
                                              column(3,
                                                     selectInput(paste('plot',1,'_varsel1', sep = ''), label = 'Abscisse',
                                                                 choices = quanti.vars)
                                              ),
                                              column(3,
                                                     selectInput(paste('plot',1,'_varsel2', sep = ''), label = 'Ordonnée',
                                                                 choices = quanti.vars)
                                              ),
                                              column(3,
                                                     selectInput('point.label', label = 'Type de Label',
                                                                 choices = c('Name', quali.vars))
                                              ),
                                              column(6,
                                                     checkboxInput('add.lm', 'Regression Lineaire')
                                              )
                                              
                                            ),
                                            plotOutput(paste('plot', 1, sep = ''))
                                     ),
                                     column(10,
                                            fluidRow(
                                              column(5,
                                                     selectInput(paste('plot',2,'_varsel1', sep = ''), label = 'Choose',
                                                                 choices = quanti.vars)
                                              ),
                                              column(5,
                                                     selectInput(paste('plot',2,'_varsel2', sep = ''), label = 'Choose',
                                                                 choices = quali.vars)
                                              )
                                            ),
                                            plotOutput(paste('plot', 2, sep = ''))
                                     ),
                                     column(10,
                                            fluidRow(
                                              column(5,
                                                     selectInput(paste('plot',3,'_varsel1', sep = ''), label = 'Choose',
                                                                 choices = quali.vars)
                                              ),
                                              column(5,
                                                     selectInput(paste('plot',3,'_varsel2', sep = ''), label = 'Choose',
                                                                 choices = quali.vars)
                                              )
                                            ),
                                            plotOutput(paste('plot', 3, sep = ''))
                                     ),
                                     column(10,
                                            fluidRow(
                                              column(5,
                                                     selectInput(paste('plot',4,'_varsel', sep = ''), label = 'Choose',
                                                                 choices = variables)
                                              )
                                            ),
                                            plotOutput(paste('plot', 4, sep = ''))
                                     )
                                   )
                          ),
                          tabPanel("Donnees", 
                                   dataTableOutput("table.interactive")
                          )
                        )
                      )
                      
                    )
           ),
           # my own analysis with some insights
           tabPanel("Analyse",
                    h3('Analyse univariée'),
                    fluidRow(
                      column(8, plotOutput('yearly.sales.by.platform'))
                    ),
                    h3('Analyse multivariée')
                    
           ),
           
           # panel for the description of the dataset
           tabPanel("Infos",
                    HTML(
                      '<p>Ce jeu de donnees est tire de <a href="https://www.kaggle.com/gregorut/videogamesales">cette page Kaggle.</a> <br/>
             Il decrit le nombre de ventes de jeux videos allant de 1980 a 2016.
             Il contient les variables suivantes:
             <ul>
              <li><strong>Rank:</strong></li>
              <li><strong>Name:</strong></li>
              <li><strong>Platform:</strong></li>
              <li><strong>Year:</strong></li>
              <li><strong>Genre:</strong></li>
              <li><strong>Publisher:</strong></li>
              <li><strong>NA_Sales:</strong></li>
              <li><strong>EU_Sales:</strong></li>
              <li><strong>JP_Sales:</strong></li>
              <li><strong>Other_Sales:</strong></li>
              <li><strong>Global_Sales:</strong></li>
             </ul>
             </p>
             <p>
             Cette application Shiny a ete developpee par Mahdiou Diallo. Le code est disponible <a href="#">ici</a>.
             </p>'
                    )
           )
)