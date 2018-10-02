library(magrittr)
library(tidyverse)
diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>%
  dplyr::mutate(price_per_carat = price / carat)
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)
diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

assign("x", 10)
x
"x" %>% assign(100)
x
env <- environment()
"x" %>% assign(100, envir = env)
x

tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>%
  tryCatch(error = function(e) "An error")

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()
mtcars %$%
  cor(disp, mpg)
mtcars <- mtcars %>%
  transform(cyl = cyl * 2)
mtcars%<>% transform(cyl = cyl * 2)





