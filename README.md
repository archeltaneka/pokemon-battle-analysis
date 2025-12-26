# Pokémon Battle Analysis

[![R Version](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7+-brightgreen.svg)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Deployed on shinyapps.io](https://img.shields.io/badge/Deployed-shinyapps.io-blue)](https://your-username.shinyapps.io/your-app-name/)

## 📑 Table of Contents

* [Introduction](#-introduction)
* [Dataset](#-dataset)
  + [Data Sources](#-data-sources)
  + [Dataset Statistics](#-dataset-statistics)
* [Quick Start](#-quick-start)
  + [Option 1: View Live App](#-option-1-view-live-app)
  + [Option 2: Run Locally](#-option-2-run-locally)
* [Features](#-features)
* [How It Works](#-how-it-works)
* [Tech Stack](#%EF%B8%8F-tech-stack)
* [Requirements](#-requirements)
* [Installation](#-installation)
* [Usage](#-usage)
* [Project Structure](#-project-structure)
* [Screenshots](#-screenshots)
* [Limitations](#%EF%B8%8F-limitations)
* [Future Improvements](#-future-improvements)
* [Contributing](#-contributing)
* [License](#-license)
* [Acknowledgments](#-acknowledgments)

## 📚 Introduction

As an avid Pokémon fan and gamer myself, I found it difficult for me to find a tool to help me understand the type matchups of Pokémon. Understanding Pokémon type matchups is crucial for competitive battling and strategic team building. Thus, by combining my passion for Pokémon with my knowledge of data science, I created this interactive Shiny application to help players analyze type effectiveness, resistances, and weaknesses for any Pokemon.

The app combines:

* 🎯 **Type Effectiveness Analysis** - See what types your Pokemon is strong or weak against
* 💪 **Moveset Insights** - Analyze your Pokemon's available moves and their types
* 📊 **Visual Indicators** - Color-coded type badges for quick reference
* 🔍 **Search & Filter** - Easily find any Pokemon from the complete dataset

> 🎓 This project originated from one of my Master's unit, FIT5145 (Foundationals of Data Science) at Monash University and and I extended it with a simple cloud deployment using RShiny (shinyapps.io).

🔗 **Live App**: https://archeltaneka.shinyapps.io/pokemon-battle-analysis/

## 📂 Dataset

### 📊 Data Sources

* [Competitive Pokemon Dataset - Kaggle](https://www.kaggle.com/n2cholas/competitive-pokemon-dataset)
    + `combats.csv` - Contains matchups between two Pokémon and a "Winner" label to indicate which Pokémon wins the battle.
    + `pokemon.csv` - A comprehensive Pokédex listing all Pokémon up to the 7th generation (X/Y). It includes details such as name, type, stats, generation, and legendary status.
* [Pokemon Weedle's Cave - Kaggle](https://www.kaggle.com/datasets/terminus7/pokemon-challenge)
    + `move-data.csv` - A detailed information about Pokémon moves.

### 📈 Dataset Statistics

| Metric | Value |
| --- | --- |
| Total Pokemon | 800 |
| Total Moves | 700+ |
| Types Covered | 18 |
| Generations | 1-7 |
| Data Year | 2017 |

## ⚡ Quick Start

### Option 1: View Live App

Visit the deployed Shiny application: [archeltaneka.shinyapps.io/pokemon-battle-analysis](https://archeltaneka.shinyapps.io/pokemon-battle-analysis/)

### Option 2: Run Locally

```r
# Clone repository
git clone https://github.com/archeltaneka/pokemon-battle-analysis
cd pokemon-battle-analysis

# Download necessary data
source("download_data.R")
source("download_pokemon_images.R")

# Train the model
source("model.R")

# Run the app
library(shiny)
runApp()
```

## 🎮 Features

* **Pokemon Search** - Find any Pokemon by name with autocomplete
* **Type Effectiveness Visualization** 
    + See all resistances (0.25x, 0.5x damage)
    + Identify immunities (0x damage)
    + Spot weaknesses (2x, 4x damage)
* **Move Analysis** - View typical moves with type indicators
* **Color-Coded Types** - Instant visual recognition with standard Pokemon colors
* **Legendary Indicators** - Special badges for legendary Pokemon
* **Responsive Design** - Works on desktop and mobile devices

## 🔍 How It Works

1. **Select two Pokemons (your Pokemon and opponent's Pokemon)** from the dropdown menu
2. **View Type Information**:
    * Pokemon's primary and secondary types
    * All type matchups (immune, resistant, weak)
3. **Analyze Moves**:
    * See all moves the Pokemon can learn
    * Each move is color-coded by type
4. **Make Strategic Decisions**:
    * Counter opponent Pokemon
    * Choose optimal movesets
    * Build balanced teams

## 🛠️ Tech Stack

* **R 4.0+**
* **Shiny** - Interactive web framework
* **dplyr** - Data manipulation
* **tidyr** - Data tidying
* **stringr** - String operations
* **xgboost** - Prediction model

## 📦 Requirements

* R version 4.0 or higher
* RStudio (recommended)
* Active internet connection (for initial data download)
* Kaggle account (for downloading dataset)

## 💻 Installation

### Step 1: Clone the Repository

```bash
git clone https://github.com/archeltaneka/pokemon-battle-analysis.git
cd pokemon-battle-analysis
```

### Step 2: Install R Packages

```r
install.packages(c(
  "shiny",
  "dplyr", 
  "tidyr",
  "stringr",
  "kaggler",
  "xgboost"
))
```

### Step 3: Set Up Kaggle Credentials

Use the `kaggler` library
```r
library(kaggler)

# Get your API key from kaggle.com/settings
kgl_auth(username = "your_kaggle_username", 
         key = "your_api_key")
```

Or you can set the environment variables
```r
Sys.setenv(KAGGLE_USERNAME = "your_kaggle_username")
Sys.setenv(KAGGLE_KEY = "your_api_key")
```

Or save directly in a `kaggle.json` file in the %USERPROFILE%/.kaggle directory
```json
{
    "username": "your_kaggle_username",
    "key": "your_api_key"
}
```

### Step 4: Download Necessary Data

```r
# Run the download script
source("download_data.R")
```

This will download:
* `combats.csv`
* `move-data.csv`
* `pokemon.csv`

And save them inside the `data` directory.

```r
# Download Pokemon images
source("download_pokemon_images.R")
```

This will download all Pokemon images and save them inside the `files/pokemon_images` directory needed when running the app.

### Step 5: Train the Model

```r
source("train.R")
```

This will train the model and save it inside the `files/models` directory.

### Step 6: Run the App

```r
library(shiny)
runApp()
```

## 📖 Usage

### Running Locally

```r
# Option 1: From RStudio
# Open app.R and click "Run App"

# Option 2: From R Console
library(shiny)
runApp()
```

### Deploying to shinyapps.io

```r
library(rsconnect)

# First time: set up your account
rsconnect::setAccountInfo(
  name = "your-account-name",
  token = "your-shinyapps.io-token",
  secret = "your-shinyapps.io-secret"
)

# Deploy the app
deployApp(appName = "your-shinyapps.io-app-name")
```

## 🗂 Project Structure

```
pokemon-battle-analysis/
├── .gitignore
├── app.R                               # Main Shiny app
├── download_data.R                     # Kaggle data download and data preprocessing script
├── download_pokemon_images.R           # Pokemon images download script
├── pokemon_battle_eda.rmd              # Pokemon battle initial EDA
├── pokemon_clustering_analysis.R       # Pokemon clustering analysis script
├── train.R                             # Model training script
├── README.md
├── LICENSE
│
├── data/                               # Downloaded datasets
│   ├── combats.csv                     # Pokemon matchups
│   ├── move-data.csv                   # Pokemon moves
│   └── pokemon.csv                     # Pokemon data
│
├── files/                              # Downloaded files
│   ├── models/                # Trained models
│   └── pokemon_images/        # Pokemon images
```

## 📸 Screenshots


## ⚠️ Limitations

* Dataset covers Pokemon up to Generation 6 (X/Y)
* Does not include:
  + Ability effects on type matchups
  + Weather conditions
  + Stat calculations
  + Dynamax/Gigantamax forms
* Type effectiveness is based on standard battle mechanics
* Some regional variants may not be fully represented

## 🔮 Future Improvements

* 🎯 Add team builder feature (6 Pokemon team analysis)
* 📊 Include base stats visualization
* 🌦️ Weather and terrain effects
* 🎲 Damage calculator
* 📱 Progressive Web App (PWA) support
* 🔄 Real-time data updates from Pokemon Showdown API
* 🏆 Competitive tier information
* 🎨 Customizable themes

## 🤝 Contributing

All kinds of contributions are welcome!

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

* **Data Source**: 
  + [Competitive Pokemon Dataset](https://www.kaggle.com/n2cholas/competitive-pokemon-dataset) by N2Cholas
  + [Pokemon Weedle's Cave - Kaggle](https://www.kaggle.com/datasets/terminus7/pokemon-challenge) by Terminus7
* **Pokemon Company**: For creating this amazing franchise
* **RStudio Team**: For the incredible Shiny framework
* **Community**: Pokemon Showdown, Smogon, and the competitive Pokemon community
* **Type Colors**: Based on official Pokemon type color scheme

---