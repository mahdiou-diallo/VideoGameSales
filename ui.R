library(shiny)




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
                        sliderInput('publisherSales', label = 'Ventes par éditeur',
                                    min = 0, max = 2000, value = c(3,300)),
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
                                                                 choices = quanti.vars, selected = quanti.vars[3])
                                              ),
                                              column(3,
                                                     selectInput(paste('plot',1,'_varsel2', sep = ''), label = 'Ordonnée',
                                                                 choices = quanti.vars, selected = quanti.vars[4])
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
                                                     selectInput(paste('plot',2,'_varsel1', sep = ''), label = 'Abscisse',
                                                                 choices = quanti.vars, selected = quanti.vars[5])
                                              ),
                                              column(5,
                                                     selectInput(paste('plot',2,'_varsel2', sep = ''), label = 'Ordonnée',
                                                                 choices = quali.vars, selected = quali.vars[2])
                                              )
                                            ),
                                            plotOutput(paste('plot', 2, sep = ''))
                                     ),
                                     column(10,
                                            fluidRow(
                                              column(5,
                                                     selectInput(paste('plot',3,'_varsel1', sep = ''), label = 'Ligne',
                                                                 choices = quali.vars, selected = quali.vars[2])
                                              ),
                                              column(5,
                                                     selectInput(paste('plot',3,'_varsel2', sep = ''), label = 'Colonne',
                                                                 choices = quali.vars, selected = quali.vars[1])
                                              )
                                            ),
                                            plotOutput(paste('plot', 3, sep = ''))
                                     ),
                                     column(10,
                                            fluidRow(
                                              column(5,
                                                     selectInput(paste('plot',4,'_varsel', sep = ''), label = 'Variable',
                                                                 choices = variables, selected = quali.vars[2])
                                              )
                                            ),
                                            plotOutput(paste('plot', 4, sep = ''))
                                     )
                                   )
                          ),
                          tabPanel("Données", 
                                   dataTableOutput("table.interactive")
                          )
                        )
                      )
                      
                    )
           ),
           # my own analysis with some insights
           tabPanel("Analyse",
                    h3('Analyse univariée'),
                    tags$p("Dans cette section, nous allons analyser chacune des variables individuellement.",
                           tags$br(),
                           "Avant de procéder à l'analyse, les lignes du jeu de données contenant des NA ont été supprimées ainsi que celles dont l'éditeur était 'Unknown'.",
                           tags$br(),
                           "Les jeux publiés à partir de 2016 sont aussi ignorés parce que ce jeux de données a été construit en cours de cette année et est donc incomplet.",
                           tags$br(),
                           "Le jeu de données résultant contient 15849 observations différentes."),
                    tags$ol(
                      tags$li(
                        "La date de publication du jeu:",
                        tags$br(),
                        "Sur le graphe suivant, on observe une forte croissance du nombre de jeux publiés jusqu'en 2009, suivie d'une décroissance tout aussi forte.",
                        plotOutput('nbGames.v.Year'),
                        "Comme on peut le voir dans le tableau qui suit, au moins 9 jeux sont publiés chaque année, avec une moyenne de 440 jeux et le maximum de 1430 qui a été atteint en 2009",
                        tableOutput('nbGames.v.Year.summary')
                        
                      ),
                      tags$li(
                        "Le type de jeu:",
                        tags$br(),
                        "Le nombre de jeux de chaque type est affiché dans le tableau suivant",
                        tableOutput('nbGames.v.Genre.tab'),
                        "On observe que les jeux de sport et d'action sont les plus publiés.",
                        tags$br(),
                        "Puzzle et Strategy sont les types de jeu les moins actifs avec 558 et 654 jeux différents, respectivement.",
                        tags$br(),
                        "Pour les autres types de jeux, la distribution est quasiment uniforme comme on peut le voir sur le diagramme suivant:",
                        plotOutput('nbGames.v.Genre.plot')
                      ),
                      tags$li(
                        "Les éditeurs du jeu:",
                        tags$br(),
                        "Les éditeurs de jeux les plus prolifiques sont affichés dans le tableau suivant",
                        tableOutput('nbGames.v.Publisher'),
                        tags$br(),
                        "Le nombre d'éditeurs différents présents dans ce jeu de données est de:",
                        verbatimTextOutput('nbPublishers')
                      ),
                      tags$li(
                        "La plateforme du jeu:",
                        tags$br(),
                        "Les jeux ont été publiés sur de nombreuses plateformes au fil des années.",
                        verbatimTextOutput('platform.summary'),
                        "La PS2 et la DS on plus de 2000 jeux développés tandis que les plateformes qui suivent ont juste la moitié.",
                        tags$br(),
                        "Cela témoigne de l'intérêt que les éditeurs ont témoigné à ces plateformes et aussi la facilité de développer des jeux pour celles-ci."
                      ),
                      tags$li(
                        "Les ventes globales:",
                        tags$br(),
                        "L'histogramme des ventes globales nous montre que la majorité des jeux ont vendu moins de 250,000 copies.",
                        tags$br(),
                        "Le jeu le plus populaire contribue à lui seul à près d'un pourcent des ventes globales avec 82.74 millions de ventes.",
                        tags$br(),
                        "L'histogramme montre clairement que la distribution des ventes globales n'est pas équilibrée. Cela se voit aussi sur les autres histogrammes.",
                        plotOutput('hist.ventesGlobales')
                      ),
                      tags$li(
                        "Les ventes en Europe:",
                        plotOutput('hist.ventesEU')
                      ),
                      tags$li(
                        "Les ventes au Japon:",
                        plotOutput('hist.ventesJP')
                      ),
                      tags$li(
                        "Les ventes en Amérique du nord:",
                        plotOutput('hist.ventesNA')
                      ),
                      tags$li(
                        "Les ventes dans le reste du monde:",
                        plotOutput('hist.ventesOthers')
                      )
                    ),
                    h3('Analyse multivariée'),
                    tags$p("L'observation simultanée de plusieurs variables nous permet de tirer de meilleures conclusions de nos données."),
                    "Le graphe suivant montre le genre de jeu pour lequel le plus de jeux ont été publiés chaque année",
                    tags$br(),
                    "Les jeux d'action et de plateforme ont dominé les années 1980. Ils ont été remplacés pendant la première moitié des années 1990 par les jeux de combats.",
                    tags$br(),
                    "Les jeux de sport ont connu un essor très rapide à partir de 1995 et ont été au sommet des publications jusqu'à être détrônés par les jeux d'action qui continuent encore à dominer le marché.",
                    plotOutput('best.genre.v.year'),
                    "Le nombre de ventes est fortement corrélé avec le nombre de jeux publiés comme le montre le coefficient de corrélation de Spearman calculé ci-dessous.",
                    verbatimTextOutput('sales.v.nbGames'),
                    "Le graphique suivant montre le coefficient de corrélation des ventes dans les 5 zones géographiques.",
                    plotOutput('corr.plot'),
                    "On peut noter que les ventes au Japon sont très faiblement (et négativement) corrélées avec les autres ventes. Un test de corrélation nous permet de conclure.",
                    verbatimTextOutput('cor.test.output'),
                    "Il existe bien une corrélation négative entre les ventes japonnaises et les ventes européennes.",
                    tags$br(),
                    "En combinant la plateforme, l'année de publication des jeux et le nombre de copies vendues, on peut déterminer la période d'existence de chaque plateforme.",
                    tags$br(),
                    "La table suivante représente ces données pour les plateformes pour lesquelles au moins 20 jeux ont été développés.",
                    tags$br(),
                    tableOutput('platform.lifetime'),
                    "On peut diviser les plateformes en deux générations dont la transition est marquée par la création de la PS2. La plupart des consoles qui ont existé au cours des années 2000 ont connu un nombre élevé de ventes.",
                    tags$br(),
                    "Le graphe suivant montre l'évolution des ventes annuelles de jeux pour chaque console ayant plus de 20 jeux.",
                    plotOutput('platform.sales'),
                    tags$br(),
                    "On observe clairement le début de la seconde génération de consoles, avec l'apogée et le déclin de la majorité.",
                    tags$br(),
                    "La présence d'un jeu isolé en 1985 pour la DS semble être une erreur commise par le créateur du jeu de données. Cela est facilement confirmé en verifiant que la Nintendo DS est sortie en 2004",
                    tags$br(),
                    "Le PC semble donc être la plateforme qui a la plus longue durée de vie. Cela n'est pas forcément vrai car plusieurs générations de PC avec des processeurs différents et des systèmes d'exploitation différents sont tous classés sous le nom de 'PC' tandis que, par exemple, les différents modèles de Play Station sont vendus sous des noms différents.",
                    tags$br(),
                    "Les graphiques intéractifs permettent de tirer encore plus de conclusions du jeu de données. Par exemple, il est possible d'observer que:",
                    tags$ul(
                      tags$li("la Wii détient le record de ventes pour un seul jeu"),
                      tags$li("Sur la Wii, les plus grands succès sont des jeux de sports"),
                      tags$li("Parmi les jeux de sports, le football a plus de succès en Europe tandis que le football américain en a plus en Amérique du Nord.")
                    )
                    
           ),
           
           # panel for the description of the dataset
           tabPanel("Infos",
                    HTML(
                      "<p>Ce jeu de donnees est tire de <a href=\"https://www.kaggle.com/gregorut/videogamesales\">cette page Kaggle.</a> <br/>
             Il decrit le nombre de ventes de jeux videos allant de 1980 a 2016. <br/>
             Il contient les variables suivantes:
             <ul>
              <li><strong>Rank:</strong> Le rang du jeu en fonction du nombre global de ventes.</li>
              <li><strong>Name:</strong> Le nom du jeu</li>
              <li><strong>Platform:</strong> Le nom de la plateforme (console) de publication du jeu.</li>
              <li><strong>Year:</strong>  L\'année de publication du jeu.</li>
              <li><strong>Genre:</strong> Le genre du jeu (exemple: Sports).</li>
              <li><strong>Publisher:</strong> Le nom de l'entreprise qui a développé le jeu.</li>
              <li><strong>NA_Sales:</strong> Le nombre total de copies vendues en Amérique du Nord</li>
              <li><strong>EU_Sales:</strong> Le nombre total de copies vendues en Europe</li>
              <li><strong>JP_Sales:</strong> Le nombre total de copies vendues au Japon</li>
              <li><strong>Other_Sales:</strong> Le nombre total de copies vendues dans le reste du monde</li>
              <li><strong>Global_Sales:</strong> Le nombre total de copies vendues dans le monde</li>
             </ul>
             </p>
             <p>
             Cette application Shiny a été développée par Mahdiou Diallo. Le code est disponible <a href=\"https://github.com/mahdiou/VideoGameSales\">à cette adresse</a>.
             </p>"
                    )
           )
)