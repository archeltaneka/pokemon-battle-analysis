library(tidyverse)
library(rvest)
library(httr)


url <- "https://pokemondb.net/pokedex/national"
webpage <- read_html(url)

# Extract Pokémon data for Generations 1 to 7
gen_nodes <- webpage %>%
    html_nodes(xpath = "//h2[contains(@id, 'gen')][position() <= 7]/following-sibling::div[1]") # Limit to first 7 generations
print(gen_nodes)

# Initialize a list to store Pokémon data
pokemon_data <- list()

# Loop through each generation node
for (gen_node in gen_nodes) {
    pokemon_cards <- gen_node %>%
        html_nodes(".infocard") # Each Pokémon card

    for (card in pokemon_cards) {
        # Extract Pokémon name
        name <- card %>%
            html_node(".infocard-lg-img img") %>%
            html_attr("alt")

        # Extract image URL
        img_url <- card %>%
            html_node(".infocard-lg-img img") %>%
            html_attr("src")

        # Append to the list
        pokemon_data[[name]] <- img_url
    }
}
    
# Download images
for (name in names(pokemon_data)) {
    img_url <- pokemon_data[[name]]
    
    # Construct the file path
    file_path <- paste0("files/pokemon_images/", gsub("[^a-zA-Z0-9]", "_", name), ".jpg")
    
    # Download the image
    GET(img_url, write_disk(file_path, overwrite = TRUE))
    
    cat("Downloaded:", name, "\n")
}
