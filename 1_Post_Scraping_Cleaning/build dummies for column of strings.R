# Create an example data frame with one column with strings:
df = data.frame(a = c("one, two, three",
                      "one, three",
                      "two, three, four, five",
                      "one, four, five",
                      "two"), stringsAsFactors = FALSE)
df
str(df$a)

# Split column 'a' into individual strings:
library(stringr)
split_list <- str_split(df$a, ", ")
split_list  # the result is a list of strings

# Grab unique values of all strings:
unique_strings <- sort(unique(unlist(split_list)))
unique_strings

# For each string in unique_strings create a variable with zeros:
df[unique_strings] <- 0
df

# Replace a zero with a 1 in a column if that row contains that string:
for(row in 1:nrow(df)){             # loop through rows
  for(string in split_list[[row]]){ # split a string; populate relevant columns
    df[row, string] <- 1
  }
}
df

##############################################

test <- data.frame(col = c('a; ff; cc; rr;', 'rr; a; cc; e; ff'))
test

# Base R:
x   <- strsplit(as.character(test$col), ";\\s?") # split the strings
lvl <- unique(unlist(x))                         # get unique elements
x   <- lapply(x, factor, levels = lvl)           # convert to factor
t(sapply(x, table))    

