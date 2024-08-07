##############################
##### mls match prediction ###
###########################

# simulating data and fitting a simple prediction of goals

# 1. Libraries and Data ----------------------
library(tidyverse)
library(brms)

matches <- 
  tibble(
    team = paste("team ", 1:10, sep = ""),
    home = rep(c("home", "away"), times = 5),
    game = rep(1:5, each = 2),
    goals = 
      sample(
        c(0,1,2,3), 
        size = 10,
        replace =  T,
        prob = c(.20,.50, .20, .10)
        )
  )

# 2. EMpty model ---------
m0 <- 
  brm(
    goals ~ 1 + (1|team),
    data = matches,
    warmup = 100,
    iter = 1000,
    seed = 3
  )

# 3. home field advantage ------
m1 <- 
  brm(
    goals ~ 1 + home + (1|team),
    data = matches,
    warmup = 100,
    iter = 1000,
    seed = 3
  )


