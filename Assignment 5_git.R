## Assignment 5
library(tidyr) 
library(stringr)

## ----- TASK 1 ----- ##

#reading in the file
raw_file <- readLines(con = "suites_dw_Table1.txt", warn = F) #removing the warnign

# ecstracting the first two letters
substr(x = raw_file, start = 1, stop = 2)

# storing 
L <- 
  (substr(x = raw_file, start = 1, stop = 2) == "--") %>% 
  which %>% 
  min


#saving the variable descriptions in a text file for future referenve. 
cat(raw_file[1:(L-2)], sep = "\n", file = "Variable_descriptions.txt")


#splitting the string in raw_file(L-1) by the character "|" and trimming awat the lading and 
#trailing white spaces. The "|"-character has special
# meaning in R ("or"), so it must be *escaped*, meaning that we tell R that it
# should be interpreted as a normal character. We do that by adding two forward slashes
# in front of it. 

var_names <- 
  str_split(string = raw_file[L-1], pattern = "\\|") %>% 
  unlist() %>% #unlist to get out the vector
  str_trim() #gets rid of all the empty spaces

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

# Let us try the approach described above. It is quite transparent, but could
# probably be done quicker. We take the elements in "raw_file" containing data,
# replace all "|" with "," and remove all empty space. The gsub-function is
# super for this kind of search-and-replace. Replace the question mark below.

comma_separated_values <- 
  raw_file[L+1:810] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)
comma_separated_values

# We then just add the variable names (separated with commas) on top, and
# cat()-the whole ting to a .csv-file in the same way as we did with the
# variable descriptions above.

comma_separated_values_with_names <- 
  c(paste(var_names, collapse = ","),
    comma_separated_values)    

# Replace the question mark and come up with a file name
cat(comma_separated_values_with_names, sep = "\n", file = "Variable_names_comma_sep.csv")

# Read the file back in as a normal csv-file. The readr-package is part of
# tidyverse, so it is already loaded.
galaxies <- read.csv("Variable_names_comma_sep.csv")

galaxies <- subset(galaxies, subset = !is.na(galaxies$name)) # removing na from name variable
View(galaxies)

# The authors of the papers referenced above claim that their galaxy catalog is 
# approximately complete because it is a representative sample of a particular 
# volume in space (the ball of radius 11 megaparsecs centered on you and me). 
# There are, however, some signs that the smaller objects are under-represented 
# in the sample. Can you make a plot that reveals this tendency and a likely explanation?

library(ggplot2)
library(dplyr)

galaxies <- galaxies %>% 
  mutate(size_category = if_else(a_26 < 1, "small" , "big"))

galaxies %>% 
  ggplot(aes(x = size_category)) +
  geom_bar() +
  labs(x = "Size Category",
       y = "Frequency",
       title = "Frequency of Size Categories") +
  theme_minimal()

