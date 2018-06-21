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


### chapter 2
library(stringr)

# detect a literal string
str_detect(fruit, pattern = 'fruit')
fruit[str_detect(fruit, pattern = 'fruit')]
# keep only the matching elements
(my_fruit <- str_subset(fruit, pattern = 'fruit'))

# split a string by delimiter
str_split(my_fruit, pattern = ' ') # bummer: output a list
# it has to be a list, no one knows how many pieces there will be

# get a character matrix, commit to the number of pieces
str_split_fixed(my_fruit, pattern = ' ', n = 2)

library(tidyr)
# if the to-be-split variable lives in a data frame, separate will split it into 2 or more variables
tibble(my_fruit) %>%
  separate(my_fruit, into = c('pre', 'post'), sep = ' ')

# string extraction
length(my_fruit) # the number of strings in a vector
str_length(my_fruit) # the number of characters in each string

head(fruit) %>%
  str_sub(1, 3) # start = 1, end = 3, applies to all elements

tibble(fruit) %>%
  head() %>%
  mutate(snip = str_sub(fruit, 1:6, 3:8)) # start = 1:6, end = 3:8

# replacement
x = head(fruit)
str_sub(x, 1, 3) <- 'AAA'

# collapse a vector
head(fruit) %>%
  str_c(collapse = ', ')

# catenate multiple vectors
# if vectors have the same length, in other words, the same number of elements
str_c(fruit[1:4], fruit[5:8], sep = " & ")
# combined with collapsing
str_c(fruit[1:4], fruit[5:8], sep = " & ", collapse = ", ")

# tidyr
fruit_df <- tibble(
  fruit1 = fruit[1:4],
  fruit2 = fruit[5:8]
)

fruit_df %>%
  unite("flavor_combo", fruit1, fruit2, sep = ' & ')

# replacement with a pattern
str_replace(my_fruit, pattern = "fruit", replacement = "THINGY")

