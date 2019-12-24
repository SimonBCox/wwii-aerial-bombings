## Import required packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

## Loading the dataset "Aerial Bombing Operations in World War II" from www.kaggle.com
path <- "data.csv"
dat <- read.csv(path, header = TRUE, sep = ",")

ops <- dat %>% select(Theater.of.Operations)
ops
sdfsdf