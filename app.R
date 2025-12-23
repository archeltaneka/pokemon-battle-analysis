library(shiny)
library(plotly)
library(dplyr)
library(stats)
library(xgboost)
library(tidyverse)
library(rsconnect)

# Pokemon type colors (preserved as requested)
type_colors <- c(
  Normal = "#A8A77A", Fire = "#EE8130", Water = "#6390F0", Electric = "#F7D02C",
  Grass = "#7AC74C", Ice = "#96D9D6", Fighting = "#C22E28", Poison = "#A33EA1",
  Ground = "#E2BF65", Flying = "#A98FF3", Psychic = "#F95587", Bug = "#A6B91A",
  Rock = "#B6A136", Ghost = "#735797", Dragon = "#6F35FC", Dark = "#705746",
  Steel = "#B7B7CE", Fairy = "#D685AD"
)

# Enhanced Pokemon-themed color palette
poke_palette <- list(
  primary = "#FF6B6B",      # Pokemon Red
  secondary = "#4ECDC4",    # Teal
  accent = "#FFE66D",       # Yellow
  dark = "#2C3E50",         # Dark Blue-Gray
  light = "#F7FFF7",        # Off-white
  success = "#7AC74C",      # Grass Green
  info = "#6390F0",         # Water Blue
  warning = "#F7D02C",      # Electric Yellow
  danger = "#C22E28"        # Fighting Red
)

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

# Load data
pokemon_df <- read_csv("files/data/pokemon-challenge/pokemon.csv")
pokemon_moveset_df <- readRDS("files/data/pokemon-challenge/pokemon_moveset.rds")
move_data_df <- read_csv("files/data/competitive-pokemon-dataset/move-data.csv")
xgb_model <- xgb.load("files/models/pokemon_prediction_model.xgb")
effectiveness_df <- get_effectiveness_type_chart()

effectiveness_df2 <- get_effectiveness_type_chart()
rownames(effectiveness_df2) <- effectiveness_df$Attacking

# Join move data to include move types
pokemon_moveset_df <- pokemon_moveset_df %>%
  left_join(move_data_df %>% select(Move = Name, Type), by = c("Move1" = "Move")) %>%
  rename(Move1_Type = Type) %>%
  left_join(move_data_df %>% select(Move = Name, Type), by = c("Move2" = "Move")) %>%
  rename(Move2_Type = Type) %>%
  left_join(move_data_df %>% select(Move = Name, Type), by = c("Move3" = "Move")) %>%
  rename(Move3_Type = Type) %>%
  left_join(move_data_df %>% select(Move = Name, Type), by = c("Move4" = "Move")) %>%
  rename(Move4_Type = Type)

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

assemble_data <- function(pokemon_df, pokemon_moveset_df, move_data_df, effectiveness_df) {
  colnames(pokemon_df) <- c("No", "Name", "Type1", "Type2",
                            "HP", "Attack", "Defense", "Sp_Atk",
                            "Sp_Def", "Speed", "Generation", "Is_Legendary")
  pokemon_df <- pokemon_df %>%
    select(-No, -Generation)
  
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
  
  pokemon_df <- pokemon_df %>%
    left_join(pokemon_moveset_df, by = join_by(Name == Original_Name))
  
  return(pokemon_df)
}

choose_pokemon <- function(pokemon_df, pokemon1, pokemon2) {
  
  # Extract both Pokémon's stats
  p1_stats <- pokemon_df %>% filter(Name == pokemon1)
  p2_stats <- pokemon_df %>% filter(Name == pokemon2)
  
  p1_stats <- p1_stats %>%
    rename_with(~ paste0("First_", .), everything())
  
  p2_stats <- p2_stats %>%
    rename_with(~ paste0("Second_", .), everything())
  
  # Combine into one row for prediction
  combined_stats <- cbind(
    p1_stats,
    p2_stats
  )
  
  return(combined_stats)
}

calculate_effectiveness_categories <- function(pokemon_types, effectiveness_df) {
  # Initialize lists for each category
  immune_to <- c()
  strongly_resists <- c()
  resists <- c()
  weak_to <- c()
  very_weak_to <- c()
  
  new_effectiveness_df <- as.data.frame(t(effectiveness_df[-1]))  # Exclude the "Attacking" column
  colnames(new_effectiveness_df) <- effectiveness_df$Attacking
  
  # Loop through all attacking types
  for (attacking_type in rownames(new_effectiveness_df)) {
    # Calculate combined effectiveness for the attacking type against both of the Pokémon's types
    combined_effectiveness <- 1
    for (defender_type in pokemon_types) {
      if (!is.na(defender_type) && defender_type %in% colnames(new_effectiveness_df)) {
        combined_effectiveness <- combined_effectiveness * new_effectiveness_df[attacking_type, defender_type]
      }
    }
    
    # Categorize based on combined effectiveness
    if (combined_effectiveness == 0) {
      immune_to <- union(immune_to, attacking_type)
    } else if (combined_effectiveness == 0.25) {
      strongly_resists <- union(strongly_resists, attacking_type)
    } else if (combined_effectiveness == 0.5) {
      resists <- union(resists, attacking_type)
    } else if (combined_effectiveness == 2) {
      weak_to <- union(weak_to, attacking_type)
    } else if (combined_effectiveness == 4) {
      very_weak_to <- union(very_weak_to, attacking_type)
    }
  }
  
  return(list(
    immune_to = unique(immune_to),
    strongly_resists = unique(strongly_resists),
    resists = unique(resists),
    weak_to = unique(weak_to),
    very_weak_to = unique(very_weak_to)
  ))
}

preprocess_data <- function(chosen_pokemon_df, effectiveness_df) {
  preprocessed_df <- chosen_pokemon_df %>%
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
                                          MoreArgs = list(effectiveness_df = effectiveness_df))
    ) %>%
    
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
    mutate(across(where(is.character), ~ replace_na(.x, ""))) %>%
    
    rowwise() %>%
    mutate(
      # Pokemon's type effectiveness
      First_Type_Max_Effectiveness = coalesce(max(c_across(starts_with("First_Type") & ends_with("Effectiveness")), na.rm = TRUE), First_Type1_Effectiveness),
      Second_Type_Max_Effectiveness = coalesce(max(c_across(starts_with("Second_Type") & ends_with("Effectiveness")), na.rm = TRUE), Second_Type1_Effectiveness),
      # Pokemon's move effectiveness
      First_Move_Max_Effectiveness = max(c_across(starts_with("First_Move") & ends_with("Effectiveness")), na.rm = TRUE),
      Second_Move_Max_Effectiveness = max(c_across(starts_with("Second_Move") & ends_with("Effectiveness")), na.rm = TRUE),
      # Move combination count
      First_Physical_Move_Count = sum(c_across(starts_with("First_Category_Move")) == "Physical", na.rm = TRUE),
      First_Special_Move_Count = sum(c_across(starts_with("First_Category_Move")) == "Special", na.rm = TRUE),
      First_Status_Move_Count = sum(c_across(starts_with("First_Category_Move")) == "Status", na.rm = TRUE),
      Second_Physical_Move_Count = sum(c_across(starts_with("Second_Category_Move")) == "Physical", na.rm = TRUE),
      Second_Special_Move_Count = sum(c_across(starts_with("Second_Category_Move")) == "Special", na.rm = TRUE),
      Second_Status_Move_Count = sum(c_across(starts_with("Second_Category_Move")) == "Status", na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    mutate(across(ends_with("Power"), ~ ifelse(.x == "" | .x == "None", 0, as.numeric(.x))),
           across(ends_with("Accuracy"), ~ ifelse(.x == "" | .x == "None", 100, as.numeric(.x)))) %>%
    
    select(First_Type1, First_Type2, First_HP, First_Attack, First_Defense,
           First_Sp_Atk, First_Sp_Def, First_Speed, First_Is_Legendary,
           First_Type_Max_Effectiveness, First_Move_Max_Effectiveness,
           First_Physical_Move_Count, First_Special_Move_Count,
           First_Status_Move_Count,
           Second_Type1, Second_Type2, Second_HP, First_Attack, First_Defense,
           Second_Sp_Atk, Second_Sp_Def, Second_Speed, Second_Is_Legendary,
           Second_Type_Max_Effectiveness, Second_Move_Max_Effectiveness,
           Second_Physical_Move_Count, Second_Special_Move_Count,
           Second_Status_Move_Count) %>%
    
    mutate(across(where(is.character) | where(is.logical), ~ as.numeric(.)))
  
  return(preprocessed_df)
}

assembled_df <- assemble_data(pokemon_df, pokemon_moveset_df, move_data_df, effectiveness_df)

# Enhanced UI with Pokemon theme
css_styles <- tags$head(
  tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Roboto:wght@300;400;500;700&display=swap');
        
        :root {
            --poke-primary: #FF6B6B;
            --poke-secondary: #4ECDC4;
            --poke-accent: #FFE66D;
            --poke-dark: #2C3E50;
            --poke-light: #F7FFF7;
            --poke-success: #7AC74C;
            --poke-info: #6390F0;
            --poke-warning: #F7D02C;
            --poke-danger: #C22E28;
        }
        
        body {
            font-family: 'Roboto', sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            margin: 0;
            padding: 0;
        }
        
        .pokemon-title {
            font-family: 'Orbitron', monospace;
            font-weight: 900;
            font-size: 3.5rem;
            text-align: center;
            color: var(--poke-light);
            text-shadow: 3px 3px 6px rgba(0,0,0,0.5);
            margin-bottom: 2rem;
            letter-spacing: 2px;
            background: linear-gradient(45deg, var(--poke-primary), var(--poke-accent));
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
        }
        
        .pokemon-subtitle {
            text-align: center;
            color: var(--poke-light);
            font-size: 1.2rem;
            margin-bottom: 2rem;
            opacity: 0.9;
        }
        
        .pokemon-card {
            background: rgba(255, 255, 255, 0.95);
            border-radius: 20px;
            padding: 1.5rem;
            margin-bottom: 1.5rem;
            box-shadow: 0 15px 35px rgba(0,0,0,0.2);
            border: 3px solid var(--poke-accent);
            transition: all 0.3s ease;
        }
        
        .pokemon-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 20px 40px rgba(0,0,0,0.3);
        }
        
        .pokemon-card h4 {
            font-family: 'Orbitron', monospace;
            color: var(--poke-dark);
            text-align: center;
            margin-bottom: 1rem;
            font-weight: 700;
            font-size: 1.3rem;
        }
        
        .pokemon-select {
            width: 100%;
            padding: 12px 15px;
            border: 2px solid var(--poke-secondary);
            border-radius: 10px;
            font-size: 1rem;
            background: white;
            transition: all 0.3s ease;
            margin-bottom: 1rem;
        }
        
        .pokemon-select:focus {
            outline: none;
            border-color: var(--poke-primary);
            box-shadow: 0 0 0 3px rgba(255, 107, 107, 0.2);
        }
        
        .pokemon-image-container {
            text-align: center;
            margin: 1rem 0;
            padding: 1rem;
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            border-radius: 15px;
            border: 2px solid var(--poke-accent);
        }
        
        .pokemon-image-container img {
            border-radius: 10px;
            box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        }
        
        .pokemon-info {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 1rem;
            border-radius: 10px;
            margin-top: 1rem;
            font-size: 2.0rem;
            line-height: 1.6;
        }
        
        .pokemon-info strong {
            color: var(--poke-accent);
            font-weight: 700;
        }
        
        .battle-section {
            background: rgba(255, 255, 255, 0.95);
            border-radius: 20px;
            padding: 2rem;
            margin: 2rem 0;
            box-shadow: 0 15px 35px rgba(0,0,0,0.2);
            border: 3px solid var(--poke-primary);
        }
        
        .battle-title {
            font-family: 'Orbitron', monospace;
            font-weight: 900;
            font-size: 2.5rem;
            text-align: center;
            color: var(--poke-dark);
            margin-bottom: 1.5rem;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
        }
        
        .prediction-result {
            text-align: center;
            font-size: 1.5rem;
            font-weight: 700;
            padding: 1.5rem;
            border-radius: 15px;
            margin-bottom: 2rem;
            background: linear-gradient(135deg, var(--poke-primary), var(--poke-accent));
            color: white;
            text-shadow: 1px 1px 2px rgba(0,0,0,0.3);
            box-shadow: 0 10px 20px rgba(0,0,0,0.2);
        }
        
        .stats-plot-container {
            background: white;
            border-radius: 15px;
            padding: 1rem;
            box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        }
        
        .type-badge {
            display: inline-block;
            padding: 4px 8px;
            margin: 2px;
            border-radius: 12px;
            font-size: 2.0rem;
            font-weight: 600;
            color: white;
            text-shadow: 1px 1px 1px rgba(0,0,0,0.3);
        }
        
        .move-item {
            display: inline-block;
            margin: 2px;
            padding: 3px 8px;
            border-radius: 8px;
            font-size: 2.0rem;
            font-weight: 500;
            color: white;
            text-shadow: 1px 1px 1px rgba(0,0,0,0.3);
        }
        
        .effectiveness-section {
            margin: 0.5rem 0;
        }
        
        .battle-container {
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 2rem;
            margin: 2rem 0;
        }
        
        .vs-symbol {
            font-family: 'Orbitron', monospace;
            font-size: 3rem;
            font-weight: 900;
            color: var(--poke-primary);
            text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
            background: var(--poke-accent);
            border-radius: 50%;
            width: 80px;
            height: 80px;
            display: flex;
            align-items: center;
            justify-content: center;
            box-shadow: 0 5px 15px rgba(0,0,0,0.3);
        }
        
        .pokemon-name {
            font-family: 'Orbitron', monospace;
            font-weight: 700;
            font-size: 1.4rem;
            color: var(--poke-dark);
            text-align: center;
            margin-bottom: 0.5rem;
        }
        
        .legendary-indicator {
            background: linear-gradient(45deg, #FFD700, #FFA500);
            color: var(--poke-dark);
            padding: 2px 8px;
            border-radius: 10px;
            font-size: 0.7rem;
            font-weight: 700;
            margin-left: 0.5rem;
        }
        
        @media (max-width: 768px) {
            .pokemon-title {
                font-size: 2.5rem;
            }
            
            .battle-container {
                flex-direction: column;
                gap: 1rem;
            }
            
            .vs-symbol {
                width: 60px;
                height: 60px;
                font-size: 2rem;
            }
        }
        
        #welcome-notification {
          background-color: #ff0000; /* Pokémon Red */
          color: white;
          font-weight: bold;
          border: 2px solid #3b4cca; /* Pokémon Blue */
        }
    "))
)

# Define Enhanced UI
ui <- fluidPage(
  css_styles,
  
  # Application title
  tags$h1("Pokémon Battle Predictor", class = "pokemon-title"),
  tags$p("Advanced AI-Powered Battle Analysis", class = "pokemon-subtitle"),
  
  # Pokemon Selection Row
  fluidRow(
    column(5,
           div(class = "pokemon-card",
               h4("🎮 Your Pokémon"),
               # Wrap the input in a div to apply the custom class
               div(class = "pokemon-select",
                   selectInput("pokemon1", "", choices = unique(pokemon_df$Name))
               ),
               div(class = "pokemon-image-container",
                   imageOutput("first_pokemon_image")
               ),
               div(class = "pokemon-info",
                   uiOutput("first_pokemon_info")
               )
           )
    ),
    
    column(2,
           div(class = "battle-container",
               div(class = "vs-symbol", "VS")
           )
    ),
    
    column(5,
           div(class = "pokemon-card",
               h4("⚔️ Opponent's Pokémon"),
               # Wrap the input in a div to apply the custom class
               div(class = "pokemon-select",
                   selectInput("pokemon2", "", choices = unique(pokemon_df$Name))
               ),
               div(class = "pokemon-image-container",
                   imageOutput("second_pokemon_image")
               ),
               div(class = "pokemon-info",
                   uiOutput("second_pokemon_info")
               )
           )
    )
  ),
  
  # Battle Analysis Section
  div(class = "battle-section",
      h3("🔮 Battle Prediction", class = "battle-title"),
      div(class = "prediction-result",
          textOutput("prediction_result")
      ),
      div(class = "stats-plot-container",
          plotlyOutput("stats_plot", height = "500px")
      )
  )
)

# Define Enhanced Server
server <- function(input, output, session) {
  
  # Reactive expression to preprocess data and make predictions
  prediction <- reactive({
    pokemon1 <- input$pokemon1
    pokemon2 <- input$pokemon2
    
    # Preprocess the data
    chosen_pokemon_df <- choose_pokemon(assembled_df, pokemon1, pokemon2)
    preprocessed_data <- preprocess_data(chosen_pokemon_df, effectiveness_df)
    preprocessed_data <- as.matrix(preprocessed_data)
    
    # Make prediction
    pred_prob <- predict(xgb_model, preprocessed_data)
    pred_label <- ifelse(pred_prob > 0.5, "Winner", "Loser")
    
    list(probability = pred_prob, label = pred_label)
  })
  
  # Display the prediction result with enhanced styling
  output$prediction_result <- renderText({
    result <- prediction()
    win_prob <- round(result$probability * 100, 2)
    
    if (win_prob > 75) {
      message <- "🔥 Excellent Chance!"
    } else if (win_prob > 60) {
      message <- "⚡ Good Advantage"
    } else if (win_prob > 45) {
      message <- "⚖️ Close Match"
    } else if (win_prob > 30) {
      message <- "🛡️ Uphill Battle"
    } else {
      message <- "💀 Very Difficult"
    }
    
    paste0(message, " - Your win probability: ", win_prob, "%")
  })
  
  # Create enhanced stats comparison plot
  output$stats_plot <- renderPlotly({
    pokemon1 <- input$pokemon1
    pokemon2 <- input$pokemon2
    
    # Extract stats for both Pokémon
    p1_stats <- pokemon_df %>% filter(Name == pokemon1) %>% select(HP, Attack, Defense, `Sp. Atk`, `Sp. Def`, Speed)
    p2_stats <- pokemon_df %>% filter(Name == pokemon2) %>% select(HP, Attack, Defense, `Sp. Atk`, `Sp. Def`, Speed)
    
    # Combine stats into a single dataframe for plotting
    stats_df <- data.frame(
      Stat = rep(c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed"), 2),
      Value = c(as.numeric(t(p1_stats)), as.numeric(t(p2_stats))),
      Pokémon = rep(c(pokemon1, pokemon2), each = 6)
    )
    
    # Enhanced color scheme for the plot
    colors <- c(poke_palette$primary, poke_palette$info)
    
    # Create the enhanced plot
    plot_ly(stats_df,
            x = ~Value,
            y = ~Stat,
            color = ~Pokémon,
            type = "bar",
            orientation = "h",
            barmode = "group",
            colors = colors,
            text = ~Value,
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Value: %{x}<br>Pokémon: %{fullData.name}<extra></extra>") %>%
      layout(
        title = list(
          text = "⚔️ Pokémon Stats Comparison",
          font = list(size = 24, family = "Orbitron", color = poke_palette$dark)
        ),
        xaxis = list(
          title = "Stat Value",
          titlefont = list(size = 16, family = "Roboto"),
          gridcolor = "rgba(0,0,0,0.1)"
        ),
        yaxis = list(
          title = "Stat Category",
          titlefont = list(size = 16, family = "Roboto")
        ),
        legend = list(
          title = list(text = "Pokémon", font = list(size = 14)),
          font = list(size = 12)
        ),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Roboto"),
        margin = list(l = 80, r = 50, t = 80, b = 50)
      )
  })
  
  # Enhanced type formatting with badges
  format_types_with_colors <- function(types, type_colors) {
    if (length(types) == 0 || all(is.na(types))) {
      return("<span style='color: #999; font-style: italic;'>None</span>")
    }
    
    paste(
      sapply(types, function(type) {
        if (!is.na(type) && type %in% names(type_colors)) {
          sprintf('<span class="type-badge" style="background-color: %s;">%s</span>',
                  type_colors[type], type)
        } else {
          ""
        }
      }),
      collapse = " "
    )
  }
  
  # Enhanced move formatting
  format_moves_with_colors <- function(moves, move_types, type_colors) {
    if (length(moves) == 0 || all(is.na(moves))) {
      return("<span style='color: #999; font-style: italic;'>No moves</span>")
    }
    
    paste(
      sapply(seq_along(moves), function(i) {
        move <- moves[i]
        move_type <- move_types[i]
        if (!is.na(move_type) && move_type %in% names(type_colors)) {
          sprintf('<span class="move-item" style="background-color: %s;">%s</span>',
                  type_colors[move_type], move)
        } else {
          move
        }
      }),
      collapse = " "
    )
  }
  
  # Enhanced First Pokemon info display
  output$first_pokemon_info <- renderUI({
    pokemon1 <- input$pokemon1
    
    # Extract types
    types <- na.omit(c(assembled_df %>% filter(Name == pokemon1) %>% pull(Type1),
                       assembled_df %>% filter(Name == pokemon1) %>% pull(Type2)))
    
    # Calculate effectiveness categories
    effectiveness_categories <- calculate_effectiveness_categories(types, effectiveness_df)
    
    # Extract moves
    moves <- na.omit(c(
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move1),
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move2),
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move3),
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move4)
    ))
    move_types <- na.omit(c(
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move1_Type),
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move2_Type),
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move3_Type),
      assembled_df %>% filter(Name == pokemon1) %>% pull(Move4_Type)
    ))
    
    # Check if legendary
    is_legendary <- assembled_df %>% filter(Name == pokemon1) %>% pull(Is_Legendary) %>% first()
    legendary_badge <- if (is_legendary) "<span class='legendary-indicator'>★ LEGENDARY</span>" else ""
    
    HTML(paste(
      "<div class='pokemon-name' style=font-size: 24px;'>", pokemon1, legendary_badge, "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>🎯 Types:</strong><br>", format_types_with_colors(types, type_colors), "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>🛡️ Immune to:</strong><br>", format_types_with_colors(effectiveness_categories$immune_to, type_colors), "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>✨ Strongly resists:</strong><br>", format_types_with_colors(effectiveness_categories$strongly_resists, type_colors), "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>🔄 Resists:</strong><br>", format_types_with_colors(effectiveness_categories$resists, type_colors), "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>⚠️ Weak to:</strong><br>", format_types_with_colors(effectiveness_categories$weak_to, type_colors), "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>💥 Very weak to:</strong><br>", format_types_with_colors(effectiveness_categories$very_weak_to, type_colors), "</div>",
      "<div class='effectiveness-section' style=font-size: 24px;'><strong>🎮 Moves:</strong><br>", format_moves_with_colors(moves, move_types, type_colors), "</div>"
    ))
  })
  
  # Enhanced Second Pokemon info display
  output$second_pokemon_info <- renderUI({
    pokemon2 <- input$pokemon2
    
    # Extract types
    types <- na.omit(c(assembled_df %>% filter(Name == pokemon2) %>% pull(Type1),
                       assembled_df %>% filter(Name == pokemon2) %>% pull(Type2)))
    
    # Calculate effectiveness categories
    effectiveness_categories <- calculate_effectiveness_categories(types, effectiveness_df)
    
    # Extract moves
    moves <- na.omit(c(
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move1),
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move2),
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move3),
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move4)
    ))
    move_types <- na.omit(c(
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move1_Type),
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move2_Type),
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move3_Type),
      assembled_df %>% filter(Name == pokemon2) %>% pull(Move4_Type)
    ))
    
    # Check if legendary
    is_legendary <- assembled_df %>% filter(Name == pokemon2) %>% pull(Is_Legendary) %>% first()
    legendary_badge <- if (is_legendary) "<span class='legendary-indicator'>★ LEGENDARY</span>" else ""
    
    HTML(paste(
      "<div class='pokemon-name'>", pokemon2, legendary_badge, "</div>",
      "<div class='effectiveness-section'><strong>🎯 Types:</strong><br>", format_types_with_colors(types, type_colors), "</div>",
      "<div class='effectiveness-section'><strong>🛡️ Immune to:</strong><br>", format_types_with_colors(effectiveness_categories$immune_to, type_colors), "</div>",
      "<div class='effectiveness-section'><strong>✨ Strongly resists:</strong><br>", format_types_with_colors(effectiveness_categories$strongly_resists, type_colors), "</div>",
      "<div class='effectiveness-section'><strong>🔄 Resists:</strong><br>", format_types_with_colors(effectiveness_categories$resists, type_colors), "</div>",
      "<div class='effectiveness-section'><strong>⚠️ Weak to:</strong><br>", format_types_with_colors(effectiveness_categories$weak_to, type_colors), "</div>",
      "<div class='effectiveness-section'><strong>💥 Very weak to:</strong><br>", format_types_with_colors(effectiveness_categories$very_weak_to, type_colors), "</div>",
      "<div class='effectiveness-section'><strong>🎮 Moves:</strong><br>", format_moves_with_colors(moves, move_types, type_colors), "</div>"
    ))
  })
  
  # Render the image for the first Pokémon
  output$first_pokemon_image <- renderImage({
    pokemon1 <- input$pokemon1
    # Construct the file path for the image
    image_path <- normalizePath(file.path("files", "pokemon_images", paste0(pokemon1, ".jpg")))
    
    # Check if the file exists
    if (!file.exists(image_path)) {
      return(list(src = "path/to/default/image.jpg"))  # Provide a default image if the file doesn't exist
    }
    
    # Return the image details
    list(
      src = image_path,
      alt = paste("Image of", pokemon1),
      width = 250,
      height = 250
    )
  }, deleteFile = FALSE)
  
  # Render the image for the second Pokémon
  output$second_pokemon_image <- renderImage({
    pokemon2 <- input$pokemon2
    # Construct the file path for the image
    image_path <- normalizePath(file.path("files", "pokemon_images", paste0(pokemon2, ".jpg")))
    
    # Check if the file exists
    if (!file.exists(image_path)) {
      return(list(src = "path/to/default/image.jpg"))  # Provide a default image if the file doesn't exist
    }
    
    # Return the image details
    list(
      src = image_path,
      alt = paste("Image of", pokemon2),
      width = 250,
      height = 250
    )
  }, deleteFile = FALSE)
  
  # Show welcome notification
  showNotification(
    "Welcome to Pokémon Battle Predictor! Select two Pokémon to analyze their battle potential!",
    id = "welcome-notification", # Use id instead of class
    type = "default",
    duration = 5
  )
}

# Run the enhanced application
shinyApp(ui = ui, server = server)
