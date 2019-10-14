inFile <- './data/vgsales.csv'
sales <- read_csv(inFile, col_types = 'icfiffddddd')
sales <- drop_na(sales) %>%
  filter(Year < 2016, Publisher != 'Unknown')


quali.vars <- c('Platform', 'Genre', 'Publisher')
quanti.vars <- c('Rank', 'Year', 'NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales')
variables <- c(quanti.vars, quali.vars)


big.platforms <- sales %>%
  group_by(Platform) %>%
  summarise(tot.games = n(),
            tot.sales = sum(Global_Sales))  %>%
  filter(tot.games > 20, tot.sales > 25) %>%
  arrange(desc(tot.sales)) %>%
  select(Platform)

big.publishers <- sales %>%
  group_by(Publisher) %>%
  summarise(tot.games = n(),
            tot.sales = sum(Global_Sales))  %>%
  filter(tot.games > 20, tot.sales > 25) %>%
  arrange(desc(tot.sales)) %>%
  select(Publisher)

# sales <- sales %>% filter(Publisher %in% big.publishers$Publisher,
#                           Platform %in% big.platforms$Platform)

axis.names <- list(Name = 'Nom', Global_Sales = 'Ventes Globales', NA_Sales = 'Ventes en Amérique du Nord',
                EU_Sales = 'Ventes en Europe', JP_Sales = 'Ventes au Japon', Other_Sales = 'Autres Ventes',
                Publisher = '', Platform = 'Plateforme', Year = 'Année de Publication', Genre = 'Genre', Rank = 'Rang')