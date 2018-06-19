library(forcats)
library(gapminder)

### part 1
# check levels
levels(gapminder$continent)
# check no. of levels
nlevels(gapminder$continent)
# generate a frequency table
kable(table(gapminder$continent))

### part 2
# drop unused levels
h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_gap <- gapminder %>%
  filter(country %in% h_countries)
h_gap_dropped <- h_gap %>% 
  droplevels()

h_gap$country %>%
  fct_drop() %>%
  levels()