# Defining my function for spreading:


# Split column 'a' into individual strings:

myspread = function(data, variable){
  library(stringr)
  df = data[variable]
  split_list <- str_split(df[[variable]], ", ")
  split_list  # the result is a list of strings
  
  # Grab unique values of all strings:
  unique_strings <- sort(unique(unlist(split_list)))
  unique_strings
  
  # For each string in unique_strings create a variable with zeros:
  df[unique_strings] <- 0
  
  # Replace a zero with a 1 in a column if that row contains that string:
  for (row in 1:nrow(df)) {             # loop through rows
    for (string in split_list[[row]]) { # split a string; populate relevant columns
      df[row, string] <- 1
    }
  }
  df[-1]
}


# Create an example data frame with one column with strings:
mydf = data.frame(a = c("one, two, three",
                      "one, three",
                      "two, three, four, five",
                      "one, four, five",
                      "two"), stringsAsFactors = FALSE)
mydf
myspread(mydf, 'a')

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




#----------------------------------------------------------------------

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


library(tidyverse)
spread_column = function(data_frame, column){
  mydf <- data_frame[column]
  names(mydf) <- 'col'
  result = rownames_to_column(mydf, 'grp') %>% 
    separate_rows(col) %>% 
    filter(col != "")  %>% 
    count(grp, col) %>%
    spread(col, n, fill = 0) %>%
    ungroup() %>% 
    select(-grp)
  return(result)
}


##############################################
# replace a string solutions:
# https://stackoverflow.com/questions/46852441/r-how-to-replace-substrings-within-a-longer-string-with-a-new-substring

test <- data.frame(a = c("str1_element_1_aaa, str1_element_2",
                         "str2_element_1",
                         "str3_element_1, str3_element_2_aaa, str3_element_3"),
                   stringsAsFactors = F)
test
str(test)

# Solution 1 -------------------------------
library(tidyverse)
# Small utility function
find_and_replace <- function(string, bad_string, replacement_string) {
  ifelse(str_detect(string, bad_string), replacement_string, string)
}

str_split(test$a, ", ") %>%                 
  map(find_and_replace, "aaa", "NEW") %>%   
  map_chr(paste, collapse = ", ") %>%
  unlist


test <- data.frame(a = c("str1_element_1_aaa, str1_element_2",
                         "str2_element_1",
                         "str3_element_1, str3_element_2_aaa, str3_element_3"),
                   stringsAsFactors = F)
string_replace(test, "_aaa", "NEW")

library(tidyverse)

# Small utility function
find_and_replace <- function(string, bad_string, replacement_string) {
  ifelse(str_detect(string, bad_string), replacement_string, string)
}
# function:
string_replace_n <- function(mystring, mybad_string, myreplacement){
  out <- str_split(mystring, ", ") %>%                 
    map(find_and_replace, mybad_string, myreplacement) %>%   
    map_chr(paste, collapse = ", ") %>% unlist
  out
}
string_replace_n(mystring = test, mybad_string ="_aaa", myreplacement ="NEW")

# Solution 2 -------------------------------

# does the operation for a string only. divide-and-conquer
replace_one = function(string, badstring, newstring) {
  # split it at ", "
  strs = str_split(string, ", ")[[1]]
  # an ifelse to find the ones containing badstring and replacing them
  strs = ifelse(grepl(badstring, strs, fixed = TRUE), newstring, strs)
  # join them again
  paste0(strs, collapse = ", ")
}

# vectorizes it
my_replace = Vectorize(replace_one, "string", USE.NAMES = FALSE)

# Solution 3 -------------------------------

new_str_replace <- function(strings_vector, badstring, newstring){
  split.dat <- strsplit(strings_vector,', ')[[1]]
  split.dat[grepl(badstring, split.dat)] <- newstring
  return(paste(split.dat, collapse = ', '))
}

results <- unname(sapply(test$a, new_str_replace, badstring = '_aaa', newstring = 'NEW'))
results
