library(tidyverse)
library(nycflights13)
flights
airlines
airports
planes
weather

planes %>%
  count(tailnum) %>%
  filter(n > 1)
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)
flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)
flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)
flights %>%
  mutate(key = row_number()) %>%
  select(key, everything())

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
inner_join(x, y, by = "key")
left_join(x, y, by = "key")
right_join(x, y, by = "key")
full_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

left_join(flights2, weather)
left_join(flights2, planes, by = "tailnum")
left_join(flights2, airports, c("dest" = "faa"))
left_join(flights2, airports, c("origin" = "faa"))

semi_join(airports, flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

flights %>%
  group_by(tailnum) %>%
  count(ave_delay = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(planes, by = "tailnum") %>%
  select(ave_delay, year) %>%
  filter(!is.na(year)) %>%
  mutate(age = 2017 - year) %>%
  select(age, ave_delay) %>%
  ggplot(aes(age, ave_delay)) +
  geom_line()

# semi_join(x, y) keeps all observations in x that have a match in y.
# anti_join(x, y) drops all observations in x that have a match in y.
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest
flights %>%
  filter(dest %in% top_dest$dest)
flights %>%
  semi_join(top_dest)
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
flights %>%
  group_by(tailnum) %>%
  count(sort = TRUE) %>%
  filter(!is.na(tailnum), n >= 100)
anti_join(flights, airports, by = c("dest" = "faa"))
anti_join(airports, flights, by = c("faa" = "dest"))

airports %>% count(alt, lon) %>% filter(n > 1)

# intersect(x, y): return only observations in both x and y.
# union(x, y): return unique observations in x and y.
# setdiff(x, y): return observations in x, but not in y.
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)
intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)
