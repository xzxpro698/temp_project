library(forcats)
library(gapminder)
library(ggplot2)

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
nlevels(h_gap$country)

h_gap_dropped <- h_gap %>% 
  droplevels()
nlevels(h_gap_dropped$country)

h_gap$country %>%
  fct_drop() %>%
  levels()

# change order
gapminder$continent %>%
  fct_infreq() %>%
  levels()
?fct_infreq # syntax

gapminder$continent %>%
  fct_infreq() %>% # head(20)
  fct_rev() %>% # head(20)
  levels()

# change order by another variable
# median by default
fct_reorder(gapminder$country, gapminder$lifeExp) %>%
  levels() %>% head()
# min, instead of median
fct_reorder(gapminder$country, gapminder$lifeExp, min) %>%
  levels() %>% head()
# backwards
fact_reorder(gapminder$country, gapminder$lifeExp, .desc = T) %>%
  levels() %>% head()
?fct_reorder # syntax

gap_asia_2007 <- gapminder %>% filter(year == 2007, continent == "Asia")
ggplot(gap_asia_2007, aes(x = lifeExp, y = country)) + geom_point()
ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) + geom_point()

# fct_reorder2
# to draw a line chart of quantitative x against a quantitative y, and the factor provides the color
ggplot(h_gap, aes(x = year, y = lifeExp, color = country)) + geom_line()
ggplot(h_gap, aes(x = year, y = lifeExp, color = fct_reorder2(country, year, lifeExp))) +
  geom_line() + labs(color = 'country')

# fct_relevel
?fct_relevel
h_gap$country %>% fct_relevel("Romania") %>% levels()

# recode the levels
?fct_recode

# merge levels from different data frames
?fct_c

