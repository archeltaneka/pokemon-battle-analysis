### SETUP ###
# Install necessary libraries
cat('Installing dependencies...')
required_packages <- c(
  "tidyverse", "dplyr", "fmsb", "caret", "stats", "factoextra"
)
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}


library(dplyr)
library(stringi)
library(tidyverse)
library(rvest)
library(jsonlite)
library(readr)
library(fmsb)
library(caret)
library(stats)
library(factoextra)


# Create data directory
if (!dir.exists("data")) {
  dir.create("data")
}

# Download and unzip the entire dataset and save them to the data directory
system("kaggle datasets download -d n2cholas/competitive-pokemon-dataset -p data/competitive-pokemon-dataset")
system("kaggle datasets download -d terminus7/pokemon-challenge -p data/pokemon-challenge")
unzip("data/competitive-pokemon-dataset/competitive-pokemon-dataset.zip", exdir = "data/")
unzip("data/pokemon-challenge/pokemon-challenge.zip", exdir = "data/")

### END OF SETUP ###

### SCRAPE POKEMON MOVE LIST ###

# Scrape 4 moves for each pokemon in the data

# Helper functions
get_mega_stone <- function(pokemon_name) {
  if (is.na(pokemon_name)) return(NULL)
  
  mega_stone <- NULL
  
  if (str_detect(pokemon_name, "Mega")) {
    parts <- strsplit(pokemon_name, " ")[[1]]
    
    # Charizard and Mewtwo have X/Y megas
    if (length(parts) == 3) {
      pokemon <- parts[2]
      
      if (pokemon == "Charizard") {
        stone <- "Charizardite"
      } else if (pokemon == "Mewtwo") {
        stone <- "Mewtwonite"
      } else {
        return(NULL)
      }
      
      mega_stone <- paste0(stone, " ", parts[3])
    }
  }
  
  return(mega_stone)
}

handle_unique_pokemons <- function(pokemon_name) {
  if (is.na(pokemon_name) || pokemon_name == "") return(NA_character_)
  
  # Initial pokemon name preprocessing
  pokemon_name <- str_to_lower(pokemon_name)
  pokemon_name <- str_replace_all(pokemon_name, "[.']", "")
  pokemon_name <- stri_trans_general(pokemon_name, "Latin-ASCII")
  
  # Handle pokemon with female/male symbols (e.g. Nidoran)
  if(str_detect(pokemon_name, "♀")) {
    pokemon_name <- str_replace_all(pokemon_name, "♀", "-f")
  } else if(str_detect(pokemon_name, "♂")) {
    pokemon_name <- str_replace_all(pokemon_name, "♂", "-m")
  } else if(str_detect(pokemon_name, "female")) {
    pokemon_name <- str_replace_all(pokemon_name, "female", "f")
  } else if(str_detect(pokemon_name, "male")) {
    pokemon_name <- str_replace_all(pokemon_name, "male", "m")
  }
  
  # Handle pokemon with multiple forms
  keywords_to_remove <- c("forme", "mode", "cloak", "normal",
                          "primal", "average", "size")
  if(any(str_detect(pokemon_name, keywords_to_remove))) {
    pokemon_name <- str_replace_all(pokemon_name, paste(keywords_to_remove, collapse = "|"), "")
  }
  
  # Handle pokemon with multiple variants
  pokemon_name_split <- str_split(pokemon_name, " ")
  variant <- pokemon_name_split[[1]][2]
  if(variant %in% c("confined", "standard", "altered", "land", 
                    "incarnate", "ordinary", "aria", "plant",
                    "blade", "shield", "half")) {
    pokemon_name <- str_replace_all(pokemon_name, variant, "")
  }
  
  # Handle the word order specifically for "Rotom"
  if(str_detect(pokemon_name, "rotom")) {
    pokemon_name <- str_replace(pokemon_name, "^(\\w+)\\s+(\\w+)$", "\\2 \\1")
  }
  
  # Final name preprocessing steps
  pokemon_name <- str_trim(pokemon_name)
  pokemon_name <- str_replace_all(pokemon_name, "mega | x| y", "")
  
  # Concat pokemon names longer than 2 words with '-' for scraping purposes
  if(str_detect(pokemon_name, " ")) {
    pokemon_name_split <- str_split(pokemon_name, " ")
    pokemon_name <- paste0(pokemon_name_split[[1]][1], "-", pokemon_name_split[[1]][2])
  }
  
  return(pokemon_name)
}

scrape_pokemon_moves <- function(pokemon_name, gen = "xy") {
  
  # Check if pokemon name is null
  if (is.na(pokemon_name) || pokemon_name == "") {
    return(list(
      Original_Name = pokemon_name,
      Standardised_Name = NA,
      Move1 = NA, Move2 = NA, Move3 = NA, Move4 = NA,
      Smogon_URL = NA,
      Scraping_Is_Successful = FALSE
    ))
  }
  
  cat("Getting moves for ", pokemon_name, "...\n")
  
  mega_stone <- get_mega_stone(pokemon_name)
  scraping_name <- handle_unique_pokemons(pokemon_name)
  
  if (is.na(scraping_name)) {
    return(list(
      Original_Name = pokemon_name,
      Standardised_Name = NA,
      Move1 = NA, Move2 = NA, Move3 = NA, Move4 = NA,
      Smogon_URL = NA,
      Scraping_Is_Successful = FALSE
    ))
  }
  
  # Scrape otherwise
  url <- paste0(
    "https://www.smogon.com/dex/",
    gen,
    "/pokemon/",
    scraping_name,
    "/"
  )
  
  html <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(html)) {
    return(list(
      Original_Name = pokemon_name,
      Standardised_Name = scraping_name,
      Move1 = NA, Move2 = NA, Move3 = NA, Move4 = NA,
      Smogon_URL = url,
      Scraping_Is_Successful = FALSE
    ))
  }
  
  json_data <- html %>%
    html_element("script:contains('dex')") %>%
    html_text()
  
  if (is.null(json_data)) {
    return(list(
      Original_Name = pokemon_name,
      Standardised_Name = scraping_name,
      Move1 = NA, Move2 = NA, Move3 = NA, Move4 = NA,
      Smogon_URL = url,
      Scraping_Is_Successful = FALSE
    ))
  }
  
  # Extract moves
  if (!is.null(mega_stone)) {
    moveset <- json_data %>%
      str_extract(str_c('"items":\\["', mega_stone, '"\\].*?"moveslots":\\[\\[.*?\\]\\]')) %>%
      str_extract_all('"move":".*?"') %>%
      unlist()
  } else {
    moveset <- json_data %>%
      str_extract_all('"move":".*?"') %>%
      unlist()
  }
  
  moveset <- moveset %>%
    str_replace_all('^"move":"|"$', "") %>%
    unique()
  
  list(
    Original_Name = pokemon_name,
    Standardised_Name = scraping_name,
    Move1 = moveset[1],
    Move2 = moveset[2],
    Move3 = moveset[3],
    Move4 = moveset[4],
    Smogon_URL = url,
    Scraping_Is_Successful = TRUE
  )
}

convert_pokemon_moves_to_df <- function(pokemon_df, n_pokemon_sample = NULL) {
  if(is.null(n_pokemon_sample)) {
    moves_list <- map(pokemon_df$Name, scrape_pokemon_moves)
  } else {
    moves_list <- map(head(pokemon_df$Name, n = n_pokemon_sample), 
                      scrape_pokemon_moves)
  }
  moves_df <- moves_list %>%
    map_dfr(~ tibble(
      Original_Name = .x$Original_Name,
      Standardised_Name = .x$Standardised_Name,
      Move1 = .x$Move1,
      Move2 = .x$Move2,
      Move3 = .x$Move3,
      Move4 = .x$Move4,
      Smogon_URL = .x$Smogon_URL,
      Scraping_Is_Successful = .x$Scraping_Is_Successful # Include the success indicator
    )) %>%
    mutate(
      Is_Filled_Backwards = ifelse(
        is.na(Move1) & is.na(Move2) & is.na(Move3) & is.na(Move4), 
        TRUE, FALSE
      )
    )
  
  return(moves_df)
}

get_effectiveness_type_chart <- function() {
  return(data.frame(
    Attacking = c("Normal", "Fire", "Water", "Electric", "Grass", "Ice", "Fighting", "Poison", "Ground", "Flying", "Psychic", "Bug", "Rock", "Ghost", "Dragon", "Dark", "Steel", "Fairy"),
    Normal = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0, 1, 1, 0.5, 1),
    Fire = c(1, 0.5, 0.5, 1, 2, 2, 1, 1, 1, 1, 1, 2, 0.5, 1, 0.5, 1, 2, 1),
    Water = c(1, 2, 0.5, 1, 0.5, 1, 1, 1, 2, 1, 1, 1, 2, 1, 0.5, 1, 1, 1),
    Electric = c(1, 1, 2, 0.5, 0.5, 1, 1, 1, 0, 2, 1, 1, 1, 1, 0.5, 1, 1, 1),
    Grass = c(1, 0.5, 2, 1, 0.5, 1, 1, 0.5, 2, 0.5, 1, 0.5, 2, 1, 0.5, 1, 0.5, 1),
    Ice = c(1, 0.5, 0.5, 1, 2, 0.5, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 0.5, 1),
    Fighting = c(2, 1, 1, 1, 1, 2, 1, 0.5, 1, 0.5, 0.5, 0.5, 2, 0, 1, 2, 2, 0.5),
    Poison = c(1, 1, 1, 1, 2, 1, 1, 0.5, 0.5, 1, 1, 1, 0.5, 0.5, 1, 1, 0, 2),
    Ground = c(1, 2, 1, 2, 0.5, 1, 1, 2, 1, 0, 1, 0.5, 2, 1, 1, 1, 2, 1),
    Flying = c(1, 1, 1, 0.5, 2, 1, 2, 1, 1, 1, 1, 2, 0.5, 1, 1, 1, 0.5, 1),
    Psychic = c(1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 0.5, 1, 1, 1, 1, 0, 0.5, 1),
    Bug = c(1, 0.5, 1, 1, 2, 1, 0.5, 0.5, 1, 0.5, 2, 1, 1, 0.5, 1, 2, 0.5, 0.5),
    Rock = c(1, 2, 1, 1, 1, 2, 0.5, 1, 0.5, 2, 1, 2, 1, 1, 1, 1, 0.5, 1),
    Ghost = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 0.5, 1, 1),
    Dragon = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0.5, 0),
    Dark = c(1, 1, 1, 1, 1, 1, 0.5, 1, 1, 1, 2, 1, 1, 2, 1, 0.5, 1, 0.5),
    Steel = c(1, 0.5, 0.5, 0.5, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 0.5, 2),
    Fairy = c(1, 0.5, 1, 1, 1, 1, 2, 0.5, 1, 1, 1, 1, 1, 1, 2, 2, 0.5, 1)
  ))
}

# Scrape from smogon official website
start_time <- Sys.time()

pokemon_df <- read_csv("data/pokemon.csv")
moves_df <- convert_pokemon_moves_to_df(pokemon_df)
saveRDS(moves_df, file = "data/pokemon_moveset.rds")

end_time <- Sys.time()
execution_time <- end_time - start_time
cat("Scraping time:", round(as.numeric(execution_time, units = "secs"), 2), "seconds\n")

### END OF SCRAPE POKEMON MOVE LIST ###


### JOIN AND ASSEMBLE ###
pokemon_df <- read_csv("data/pokemon.csv") # Pokemon list
pokemon_moveset_df <- readRDS("data/pokemon_moveset.rds") # Pokemon moveset
move_data_df <- read_csv("data/move-data.csv") # Move data
pokemon_combat_df <- read_csv("data/combats.csv") # Combat history
effectiveness_df <- get_effectiveness_type_chart() # Type effectiveness

calculate_effectiveness <- function(move_type, opponent_type_1, opponent_type_2, effectiveness_df) {
  if(is.na(move_type)) {
    return(NA)
  }
  
  if(!is.na(opponent_type_1)) {
    type1_multiplier <- effectiveness_df %>%
      filter(Attacking == opponent_type_1) %>%
      pull(as.character(move_type)) %>%
      as.numeric()
  } else {
    type1_multiplier <- 1
  }
  
  if(!is.na(opponent_type_2)) {
    type2_multiplier <- effectiveness_df %>%
      filter(Attacking == opponent_type_2) %>%
      pull(as.character(move_type)) %>%
      as.numeric()
  } else {
    type2_multiplier <- 1
  }
  
  return(type1_multiplier * type2_multiplier)
}

preprocess_pokemon_data <- function(pokemon_df, pokemon_moveset_df, move_data_df, pokemon_combat_df, effectiveness_df) {
  
  # Preprocess pokedex data
  colnames(pokemon_df) <- c("No", "Name", "Type1", "Type2",
                            "HP", "Attack", "Defense", "Sp_Atk",
                            "Sp_Def", "Speed", "Generation", "Is_Legendary")
  
  # Preprocess move data
  move_data_df <- move_data_df %>%
    mutate(Power = ifelse(is.na(Power), 0, Power),
           Accuracy = ifelse(is.na(Accuracy), 100, Accuracy)) %>%
    select(-Index, -Contest, -Generation)
  
  pokemon_moveset_df <- pokemon_moveset_df %>%
    filter(!Is_Filled_Backwards) %>%
    select(Original_Name, Move1, Move2, Move3, Move4) %>%
    left_join(move_data_df, by = join_by(Move1 == Name)) %>%
    rename(
      Move1_Type = Type,
      Move1_Power = Power,
      Move1_Accuracy = Accuracy
    ) %>%
    left_join(move_data_df, by = join_by(Move2 == Name), suffix = c("_Move1", "_Move2")) %>%
    rename(
      Move2_Type = Type,
      Move2_Power = Power,
      Move2_Accuracy = Accuracy
    ) %>%
    left_join(move_data_df, by = join_by(Move3 == Name)) %>%
    rename(
      Move3_Type = Type,
      Move3_Power = Power,
      Move3_Accuracy = Accuracy
    ) %>%
    left_join(move_data_df, by = join_by(Move4 == Name), suffix = c("_Move3", "_Move4")) %>%
    rename(
      Move4_Type = Type,
      Move4_Power = Power,
      Move4_Accuracy = Accuracy
    )
  
  # Preprocess pokemon combat
  pokemon_combat_df <- pokemon_combat_df %>%
    # Join with pokedex table to find pokemon info details (stats)
    left_join(pokemon_df, by = join_by(First_pokemon == No)) %>%
    rename_with(~ paste0("First_", .), .cols = c("Name", "Type1", "Type2", "HP", "Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed", "Generation", "Is_Legendary")) %>%
    left_join(pokemon_df, by = join_by(Second_pokemon == No)) %>%
    rename_with(~ paste0("Second_", .), .cols = c("Name", "Type1", "Type2", "HP", "Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed", "Generation", "Is_Legendary")) %>%
    
    # Join with moveset table
    left_join(pokemon_moveset_df, by = join_by(First_Name == Original_Name)) %>%
    rename_with(~ paste0("First_", .), .cols = c(starts_with("Move"), starts_with("Category"), starts_with("PP"))) %>%
    left_join(pokemon_moveset_df, by = join_by(Second_Name == Original_Name)) %>%
    rename_with(~ paste0("Second_", .), .cols = c(starts_with("Move"), starts_with("Category"), starts_with("PP"))) %>%
    
    # Add type effectiveness calculation
    mutate(
      First_Type1_Effectiveness = mapply(calculate_effectiveness,
                                         First_Type1,
                                         Second_Type1,
                                         Second_Type2,
                                         MoreArgs = list(effectiveness_df = effectiveness_df)),
      First_Type2_Effectiveness = mapply(calculate_effectiveness,
                                         First_Type2,
                                         Second_Type1,
                                         Second_Type2,
                                         MoreArgs = list(effectiveness_df = effectiveness_df)),
      Second_Type1_Effectiveness = mapply(calculate_effectiveness,
                                          Second_Type1,
                                          First_Type1,
                                          First_Type2,
                                          MoreArgs = list(effectiveness_df = effectiveness_df)),
      Second_Type2_Effectiveness = mapply(calculate_effectiveness,
                                          Second_Type2,
                                          First_Type1,
                                          First_Type2,
                                          MoreArgs = list(effectiveness_df = effectiveness_df)),
      
      First_Move1_Effectiveness = mapply(calculate_effectiveness,
                                         First_Move1_Type,
                                         Second_Type1,
                                         Second_Type2,
                                         MoreArgs = list(effectiveness_df = effectiveness_df)),
      First_Move2_Effectiveness = mapply(calculate_effectiveness,
                                         First_Move2_Type,
                                         Second_Type1,
                                         Second_Type2,
                                         MoreArgs = list(effectiveness_df = effectiveness_df)),
      First_Move3_Effectiveness = mapply(calculate_effectiveness,
                                         First_Move3_Type,
                                         Second_Type1,
                                         Second_Type2,
                                         MoreArgs = list(effectiveness_df = effectiveness_df)),
      First_Move4_Effectiveness = mapply(calculate_effectiveness,
                                         First_Move4_Type,
                                         Second_Type1,
                                         Second_Type2,
                                         MoreArgs = list(effectiveness_df = effectiveness_df)),
      Second_Move1_Effectiveness = mapply(calculate_effectiveness,
                                          Second_Move1_Type,
                                          First_Type1,
                                          First_Type2,
                                          MoreArgs = list(effectiveness_df = effectiveness_df)),
      Second_Move2_Effectiveness = mapply(calculate_effectiveness,
                                          Second_Move2_Type,
                                          First_Type1,
                                          First_Type2,
                                          MoreArgs = list(effectiveness_df = effectiveness_df)),
      Second_Move3_Effectiveness = mapply(calculate_effectiveness,
                                          Second_Move3_Type,
                                          First_Type1,
                                          First_Type2,
                                          MoreArgs = list(effectiveness_df = effectiveness_df)),
      Second_Move4_Effectiveness = mapply(calculate_effectiveness,
                                          Second_Move4_Type,
                                          First_Type1,
                                          First_Type2,
                                          MoreArgs = list(effectiveness_df = effectiveness_df)),
    ) %>%
    
    # Fill the remaining columns with 0 for numeric and empty string "" for characters
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
    mutate(across(where(is.character), ~ replace_na(.x, "")))
  
  return(pokemon_combat_df)
}

start_time <- Sys.time()

final_df <- preprocess_pokemon_data(pokemon_df, pokemon_moveset_df, move_data_df, pokemon_combat_df, effectiveness_df)
saveRDS(final_df, file = "data/final_pokemon_data.rds")

end_time <- Sys.time()
execution_time <- end_time - start_time
cat("Execution time:", round(as.numeric(execution_time, units = "secs"), 2), "seconds\n")

### END OF JOIN AND ASSEMBLE ###
