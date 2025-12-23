library(tidyverse)
library(rvest)
library(jsonlite)
library(readr)

html <- tryCatch({
            read_html("https://pokemondb.net/type")
        }, error = function(e) {
            return(NULL) # Return NULL if there's an error
        })

json_data <- html %>%
    html_element("table") %>% # Adjust selector as needed
    html_text()

moveset <- json_data %>%
    str_extract_all('"move":".*?"') %>%
    unlist() %>%
    str_replace_all('^"move":"|"$', "") %>% # Remove the "move":" prefix and trailing "
    unique()

mega_x_moves <- json_data %>%
    str_extract('"items":\\["Charizardite Y"\\].*?"moveslots":\\[\\[.*?\\]\\]') %>%
    str_extract_all('"move":".*?"') %>%
    unlist() %>%
    str_replace_all('^"move":"|"$', "") %>% 
    unique()

url <- "https://www.smogon.com/dex/xy/types/"
html <- read_html(url)

# Extract the type effectiveness table
type_chart <- html %>%
    html_element("table") %>% # Locate the table
    html_table(fill = TRUE)   # Convert to a data frame

