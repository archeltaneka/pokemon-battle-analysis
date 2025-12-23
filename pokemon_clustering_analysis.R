library(shiny)
library(plotly)
library(dplyr)
library(stats)
library(caret)


pokemon_data <- read_csv("files/data/pokemon-challenge/pokemon.csv")
clustering_data <- pokemon_data %>%
    mutate(
        across(where(is.character) | where(is.logical), ~ as.numeric(.))
    )

ui <- fluidPage(
    titlePanel("Pokémon KMeans Clustering Visualization"),
    sidebarLayout(
        sidebarPanel(
            h4("Instructions"),
            p("Hover over the points to view Pokémon details."),
            p("Clusters are represented by different colors.")
        ),
        mainPanel(
            plotlyOutput("clusterPlot")
        )
    )
)

server <- function(input, output) {
    set.seed(123)
    
    scaled_data <- scale(clustering_data)
    scaled_df <- as.data.frame(scaled_data)
    
    nzv_columns <- nearZeroVar(scaled_df, saveMetrics = TRUE)
    
    # Extract the names of constant or near-zero variance columns
    problematic_columns <- rownames(nzv_columns[nzv_columns$zeroVar | nzv_columns$nzv, , drop = FALSE])
    scaled_df <- scaled_df[, !colnames(scaled_df) %in% problematic_columns]
    
    pca_result <- prcomp(scaled_df, center = TRUE, scale. = TRUE)
    
    # Extract the first two principal components
    pca_data <- data.frame(
        PC1 = pca_result$x[, 1],
        PC2 = pca_result$x[, 2]
    )
    
    k <- 6
    kmeans_model <- kmeans(pca_data, centers = k, nstart = 25)
    
    # Add cluster assignments to the PCA data
    pca_data$Cluster <- as.factor(kmeans_model$cluster)
    
    pokemon_data$PC1 <- pca_data$PC1
    pokemon_data$PC2 <- pca_data$PC2
    pokemon_data$Cluster <- pca_data$Cluster
    
    cluster_labels <- c(
        "1" = "Physical Attackers",
        "2" = "Special Attackers",
        "3" = "Physically Defensive",
        "4" = "High Stats, Mega Evolutions, Legendaries",
        "5" = "Speedy",
        "6" = "Little Cup"
    )
    
    pokemon_data$Cluster_Label <- cluster_labels[as.character(pca_data$Cluster)]
    
    output$clusterPlot <- renderPlotly({
        # Create the plotly scatter plot
        plot_ly(pokemon_data, 
                x = ~PC1, 
                y = ~PC2, 
                color = ~Cluster_Label, 
                text = ~paste("Name:", Name,
                              "<br>Type 1:", `Type 1`,
                              "<br>Type 2:", `Type 2`,
                              "<br>HP:", HP,
                              "<br>Attack:", Attack,
                              "<br>Sp. Atk:", `Sp. Atk`,
                              "<br>Defense:", Defense,
                              "<br>Sp. Def:", `Sp. Def`,
                              "<br>Speed:", Speed),
                hoverinfo = "text",
                mode = "markers",
                marker = list(size = 10)) %>%
            layout(
                title = "Pokémon Clustering",
                xaxis = list(title = "", showticklabels = FALSE), # Remove x-axis label and tick labels
                yaxis = list(title = "", showticklabels = FALSE), # Remove y-axis label and tick labels
                legend = list(
                    title = "Cluster Type",       # Legend title
                    orientation = "h",            # Horizontal orientation
                    y = -0.2,                     # Position legend below the plot
                    x = 0.5,                      # Center the legend horizontally
                    xanchor = "center"            # Anchor the legend at the center
                )
            )
    })
}


shinyApp(ui = ui, server = server)