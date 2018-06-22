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
# replacing NA
melons <- str_subset(fruit, pattern = "melon")
melons[2] <- NA
str_replace_na(melons, 'UNKNOWN MELON')
# tidyr::replace_na()
tibble(melons) %>%
  replace_na(replace = list(melons = "Unknown Melon"))

# chapter 3
# regular expression, aka 'regexes'
library(gapminder)
countries <- levels(gapminder$country)

# metacharacter: period .
# it stands for any single character
# \n represents a newline
str_subset(countries, pattern = 'i.a')

# metacharacter: \b and \B
# \b, indicates a word boundary
# \B, indicates NOT a word boundary
str_subset(fruit, pattern = 'melon')
str_subset(fruit, pattern = '\\bmelon') # gives a word boundary, normally it is a whitespace, then followed by 'melon'
str_subset(fruit, pattern = '\\Bmelon') # no word boundary, followed by 'melon' directly

# character classes
# characters can be specified via classes, inside square brakets, []
# find any countries, ended with 'ia', and the letter before 'ia' is 'n', 'l', or 's'
str_subset(countries, pattern = '[nls]ia$')
# find any countries, ended with 'ia', and the preceded letter is not from 'n', 'l', or 's'
str_subset(countries, pattern = '[^nls]ia$')

# escaping
# special meaning characters: $ * + . ? [ ] ^ { } | ( ) \
# prepend two backslashes to escape
# if only one backslach
cat("Do you use \"airquotes\" much?")
cat("before the newline\nafter the newline") # newline
cat("before the newline\tafter the newline") # a tab

# to find country names contain a period
str_subset(countries, pattern = '\\.')

# example 2: match an actual square bracket
(x <- c("whatever", "X is distributed U[0,1]"))
str_subset(x, pattern = '\\[')


# anchors: is to express where the expression must occur within the string
# hat ^ indicates the beginning of string
# dollar $ indicates the end
str_subset(countries, pattern = 'i.a$')
str_subset(countries, pattern = 'd')
str_subset(countries, pattern = 'd$')













