library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(bslib)


# Charger les données
data <- read_excel("laptops.xlsx")

# Définir l'interface utilisateur
ui <- fluidPage(
  
  theme = bs_theme(
    version = 4,
    bootswatch = "minty",
    primary = "#5bc0de",
    secondary = "#f7b731"
  ),
  titlePanel("Analyse des ordinateurs portables"),
  sidebarLayout(
    sidebarPanel(
      selectInput("brand", "Choisissez la marque:", choices = c("All", unique(data$Brand)), selected = "All"),
      sliderInput("priceRange", "Plage de prix:", min = min(data$`Price ($)`), max = max(data$`Price ($)`), value = c(min(data$`Price ($)`), max(data$`Price ($)`))),
      sliderInput("screenSizeRange", "Taille de l'écran (pouces):", min = min(data$`Screen Size (in.)`), max = max(data$`Screen Size (in.)`), value = c(min(data$`Screen Size (in.)`), max(data$`Screen Size (in.)`))),
      sliderInput("ramRange", "RAM (GB):", min = min(data$`RAM Memory (GB)`), max = max(data$`RAM Memory (GB)`), value = c(min(data$`RAM Memory (GB)`), max(data$`RAM Memory (GB)`))),
      actionButton("submit", "Appliquer les filtres")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Tableau", tableOutput("table")),
                  tabPanel("Résumé", verbatimTextOutput("summary")),
                  tabPanel("Graphique", plotOutput("plot"))
      )
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  filteredData <- eventReactive(input$submit, {
    data %>%
      filter(
        (input$brand == "All" | Brand == input$brand),
        `Price ($)` >= input$priceRange[1], `Price ($)` <= input$priceRange[2],
        `Screen Size (in.)` >= input$screenSizeRange[1], `Screen Size (in.)` <= input$screenSizeRange[2],
        `RAM Memory (GB)` >= input$ramRange[1], `RAM Memory (GB)` <= input$ramRange[2]
      )
  })
  
  output$table <- renderTable({
    filteredData()
  })
  
  output$summary <- renderPrint({
    summary(filteredData())
  })
  
  output$plot <- renderPlot({
    ggplot(filteredData(), aes(x = `Price ($)`, fill = Brand)) +
      geom_histogram(binwidth = 100, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Distribution des Prix", x = "Prix ($)", y = "Nombre d'ordinateurs")
  })
}

# Exécuter l'application
shinyApp(ui = ui, server = server)
