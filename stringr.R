library(tidyverse)
df <- tibble(text = c("hej01", "HEJ:;_", "158c222e9b2ba8408") )
df

# Detect matches
# ------------------------------------------------------------------------------

# detect presence of pattern match in string:
str_detect(df$text, "hej")
# can be used to filter:
df %>% filter(str_detect(text, pattern = "hej"))

# find indexes of strings
str_which(df$text, "[a-z]" )

# count number of matches in a string:
str_count(df$text, "[0-9]")

# locate position of pattern matches in string
str_locate(df$text, "hej|HEJ") # stops after first match
str_locate(df$text, "[0-9]")
str_locate_all(df$text, "[0-9]") # shows all matches

# Subset strings
# ------------------------------------------------------------------------------

# extract substrings
str_sub(df$text, 1,2) # "he" "HE" "15"

# return strings containing pattern match
str_subset(df$text, "[0-9]") 
# "hej01"             "158c222e9b2ba8408"

# return first pattern match found in each string as a vector
str_extract(df$text, "[0-9]") 
# "0" NA  "1"

# return first pattern match found in each string as a matrix
str_match(df$text, "[0-9]")
# [1,] "0" 
# [2,] NA  
# [3,] "1"

# return all pattern matches found in each string as a matrix
# returns a list of matrices
str_match_all(df$text, "[0-9]")
# [[1]]
# [,1]
# [1,] "0" 
# [2,] "1" 
# etc

# a column gets created for each () group in pattern:
str_match(df$text, "(hej|HEJ)([0-9;:_])")
# [,1]   [,2]  [,3]
# [1,] "hej0" "hej" "0" 
# [2,] "HEJ:" "HEJ" ":" 
# [3,] NA     NA    NA  

# Manage lengths
# ------------------------------------------------------------------------------

# width of strings / number of code points.
# returns numeric vector
str_length(df$text)

# pad strings to constant width
str_pad(df$text, side = "both", width = 10, pad=" ")
# "  hej01   "        "  HEJ:;_  "        "158c222e9b2ba8408"

# truncate width of strings, replacing content with ... (ellipsis)
str_trunc(df$text, 5)
# [1] "hej01" "HE..." "15..."

# trim whitespace from start and/or end of string
str_trim("hej    ")

# Mutate strings
# ------------------------------------------------------------------------------

str_sub(df$text, 1,3) # "hej" "HEJ" "158"

# replace substring by assigning:
str_sub(df$text, 1,3) <- "nej"
str_sub(df$text, 1,3)
# [1] "nej" "nej" "nej"

# replace first matched pattern in each string:
str_replace(df$text, "n", "-") 
# [1] "-ej01"             "-ej:;_"            "-ejc222e9b2ba8408"

# replace all matched patterns in each string:
str_replace_all(df$text, "[0-9]", "-") 
# [1] "nej--"             "nej:;_"            "nejc---e-b-ba----"

# lower/uppercase:
str_to_lower(df$text)
str_to_upper(df$text)

# convert string to title case i.e. capitalized first letter
str_to_title(df$text, locale = "en")
# [1] "Nej01"             "Nej:;_"            "Nejc222e9b2ba8408"

# Join and Split
# ------------------------------------------------------------------------------

# join multiple string into one
str_c(df$text[1], df$text[2])
# [1] "nej01nej:;_"

# join/collapse a vector of strings into a single string
str_c(df$text, collapse = "")
# [1] "nej01nej:;_nejc222e9b2ba8408"

# repeat strings
str_dup(df$text,2)
#[1] "nej01nej01"      "nej:;_nej:;_"   "nejc222e9b2ba8408nejc222e9b2ba8408"

# split vector of strings into matrix of substrings,
# splitting at occurences of a pattern match
str_split_fixed(df$text, "e", n=1)
# [,1]               
# [1,] "nej01"            
# [2,] "nej:;_"           
# [3,] "nejc222e9b2ba8408"

str_split_fixed(df$text, "e", n=2)
# [,1] [,2]             
# [1,] "n"  "j01"            
# [2,] "n"  "j:;_"           
# [3,] "n"  "jc222e9b2ba8408"

str_split_fixed(df$text, "e", n=3)
# [,1] [,2]    [,3]       
# [1,] "n"  "j01"   ""         
# [2,] "n"  "j:;_"  ""         
# [3,] "n"  "jc222" "9b2ba8408"

# create a string from strings and {expressions} to evaluate
test <- "bike"
str_glue("My bike is a {test} and is {str_length(test)} characters long")
# My bike is a bike and is 4 characters long

# use dataframe, list or environment to create a string from strings:
str_glue_data(mtcars, "{rownames(mtcars)} has {hp} hp")
# Mazda RX4 has 110 hp
# Mazda RX4 Wag has 110 hp
# ...

# Order strings
# ------------------------------------------------------------------------------

# return vector of indices that sorts a charcater vector
str_order(c("cykel","bröd","ö"),locale = "se" )
# [1] 2 1 3 
# i.e. bröd first, cykel second, ö last

# sort a chracter vector
str_sort(c("cykel","bröd","ö"),locale = "se" )
# [1] "bröd"  "cykel" "ö" 

# Helpers
# ------------------------------------------------------------------------------

# override encoding of string
str_conv("hej", "utf-8")

# view html rendering of first regex match in each string
str_view(df$text, "[0-9]")

# view html rendering of all regex matches in each string
str_view_all(df$text, "[0-9]")

# wrap strings to nicely formatted paragrahps:
fruit_sentence <-  str_c(fruit, collapse = " ") 
str_wrap(fruit_sentence)

# type this to show help list for special characters like \", \n, etc
?"'"

# type \\. to mean \. in regex which matches .
# type \\\\ to mean \\ in regex which matches \
# type \\( to mean \( in regex which matches (
# \\b word boundaries
# \\w any word character
# \\W any non-word characters
# \\s any whitespace 

str_split("hej hej hej", boundary("word") )
str_split("hej hej hej", boundary("sentence") )
str_split("hej hej hej", boundary("character") )

pattern <- "a.b"
strings <- c("abb", "a.b")
str_detect(strings, pattern)
str_detect(strings, fixed(pattern))
str_detect(strings, coll(pattern))