# dplyr: 6 functions:
# filter()     Pick observations by their values.
# arrange()    Reorder the rows.
# select()     Pick variables by their names.
# mutate()     Create new variables with functions of existing variables.
# summarise()  Collapse many values down to a single summary.
# group_by()   Group by one or more variables

# All verbs work similarly:
# The first argument is a data frame.
# The subsequent arguments describe what to do with the data frame, using the variable names (without quotes).
# The result is a new data frame.

# filter()
filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

# Comparisons
sqrt(2) ^ 2 == 2
1/49*49 == 1
near(sqrt(2) ^ 2, 2)
near(1 / 49 * 49, 1)

# Logical operators
filter(flights, month == 11 | month ==12)

nov_dec <- filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))

filter(flights, arr_delay <= 120, dep_delay <= 120)

# Missing values
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA
is.na(NA)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

# between()
filter(flights, between(month, 7, 9))

# arrange()
arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# select()
select(flights, year, month, day)

select(flights, year:day)

select(flights, -(year:day))

# There are a number of helper functions you can use within select():
# starts_with("abc"): matches names that begin with "abc"
# ends_with("xyz"): mathes names that end with "xyz"
# contains("ijk"): matches names that contain "ijk"
# matches("(.)\\1"): selects variables that match a regular expression
# num_range("x", 1:3): matches x1,x2,x3

# change column names
rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

select(flights, contains("TIME"))

# mutate()
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
                      )
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
       )
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
       )
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
          )
# %/%(integer division) %%(remainder)
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
          )

# log() log2() log10()
# lead() lag()
# cumsum() cumprod() cummin() cummax()
(x <- 1:10)
lag(x)
lead(x)
cumsum(x)
cummean(x)

# min_rank() row_number() dense_rank() percent_rank() cume_dist() ntile()
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

filter(flights,between(min_rank(desc(dep_delay)),1,10))
flights%>%filter(between(min_rank(desc(dep_delay)),1,10))%>%select(dep_delay)%>%arrange(desc(dep_delay))

# summarise()
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
                   )
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# %>% : x %>% f(y) %>% g(z) turns into g(f(x, y), z)
delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE),
    ) %>%
  filter(count > 20, dest != "HNL")

# Missing values
flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

# Counts n() sum(!is.na(x))
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

batting <- as.tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

batters %>% arrange(desc(ba))

# median()
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = median(arr_delay),
    avg_delay2 = median(arr_delay[arr_delay > 0])
  )

# sd() standard deviation IQR(), interquartile range mad(), median absolute deviation
not_cancelled %>%
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

# min(x), quantile(x, 0.25), max(x)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

# first(x), nth(X,2), last(x)
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

# n() the size of the current group
# sum(!is.na(x)) the number of non_missing values
# n_distinct() the number of distinct(unique) values
not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(dest)

not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 500))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(hour_perc = mean(arr_delay > 60))

# Grouping by multiple variables
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

# Ungrouping
daily %>%
  ungroup() %>%
  summarise(flights = n())

# count()
not_cancelled %>%
  count(dest)

not_cancelled %>%
  group_by(dest) %>%
  summarise(n=n())

not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

# Grouped mutates and filters
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

(popular_tests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365))

popular_tests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)
