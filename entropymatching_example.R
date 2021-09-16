# R CODE
install.packages('ebal')
library(ebal); library(tidyverse)

br <- read_csv('https://github.com/LOST-STATS/lost-stats.github.io/raw/source/Model_Estimation/Matching/Data/broockman2013.csv')

# Outcome
Y <- br %>%
  pull(responded)
# Treatment
D <- br %>%
  pull(leg_black)
# Matching variables
X <- br %>%
  select(medianhhincom, blackpercent, leg_democrat) %>%
  # Add square terms to match variances if we like
  mutate(incsq = medianhhincom^2,
         bpsq = blackpercent^2) %>%
  as.matrix()

eb <- ebalance(D, X)

# Get weights for usage elsewhere
# Noting that this contains only control weights
br_treat <- br %>%
  filter(leg_black == 1) %>%
  mutate(weights = 1)
br_con <- br %>%
  filter(leg_black == 0) %>%
  mutate(weights = eb$w)
br <- bind_rows(br_treat, br_con)

# Compare outcome (responded) between groups after matching
m <- lm(responded ~ leg_black, data = br, weights = weights)
summary(m)
