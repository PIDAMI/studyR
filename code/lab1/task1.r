df <- read.csv("data/lab1_e1.csv",sep=',')

# helper function to process each value in df
fix_entry <- function(entry) {
  tryCatch(
    { as.numeric(gsub(" ", "", entry)) },
    error = function(cond) entry,
    warning = function(cond) entry,
    finally = {}
  ) 
}

fix_data <- function(df) {
  sapply(df, function(obs) lapply(obs, fix_entry))
}
fix_data(df)