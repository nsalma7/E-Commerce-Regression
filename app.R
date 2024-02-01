###E-Commercee Companity Southeast Asian Contries###
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)

# Mengimport data
df <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(153000, 112000, 176000, 139000, 125000, 212000, 229000, 205000, 234000, 245000, 257000, 268000),
  x2 = c(8300, 9700, 10700, 12500, 13200, 9900, 13500, 12800, 14700, 15900, 18000, 21000),
  x3 = c(5, 6, 8, 4, 5, 7, 9, 5, 2, 5, 6, 5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(28000, 21000, 34000, 33000, 39000, 20000, 21000, 31000, 49000, 42000, 57000, 66000),
  y = c(100, 110, 150, 145, 190, 160, 180, 220, 210, 220, 390, 320))

# Load the regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = df) # Assuming 'data' is your dataset

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "E-commerce Sales"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Descriptive Statistics", tabName = "desc_stats", icon = icon("bar-chart")),
      menuItem("Relationship Each Variable", tabName = "relationship", icon = icon("line-chart")),
      menuItem("Dashboard Regression", tabName = "regression", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data_table",
        fluidPage(
          titlePanel(title = div("Data Table E-Commerce", style = "color: #9DC183; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
          dataTableOutput("data_table")
        )
      ),
      tabItem(
        tabName = "desc_stats",
        fluidPage(
          titlePanel(title = div("Descriptive Statistics", style = "color: #9DC183; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
          mainPanel(
            style = "margin: 0 auto; text-align: center; margin-left: 300px;",
            verbatimTextOutput("desc_stats_output"),
            plotlyOutput("barplot_output")
          )
        )
      ),
      tabItem(
        tabName = "relationship",
        fluidPage(
          titlePanel(title = div("Analysis Relationship Each Variable", style = "color: #9DC183; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
          sidebarLayout(
            sidebarPanel(
              selectInput("variable", "Select Variable:", choices = colnames(df)),
              actionButton("plot", "Generate Plot", class = "btn-primary")
            ),
            mainPanel(
              plotOutput("scatterbox_plot", height = "800px")
            )
          )
        )
      ),
      tabItem(
        tabName = "regression",
        fluidPage(
          titlePanel(title = div("Monthly Sales Volume Analysis Dashboard", style = "color: #9DC183; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
          sidebarLayout(
            sidebarPanel(
              numericInput("x1_input", "Number of Website Visitors:", value = 270000),
              numericInput("x2_input", "Number of Monthly Transactions:", value = 16000),
              numericInput("x3_input", "Average Items per Transaction:", value = 7),
              numericInput("x4_input", "Customer Satisfaction Rating (1-10):", value = 9.5),
              numericInput("x5_input", "Number of Online Advertisements:", value = 62000),
              actionButton("analyze_button", "Analyze Sales", class = "btn-primary")
            ),
            mainPanel(
              plotOutput("sales_plot"),
              verbatimTextOutput("analysis_output")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Placeholder data for Relationship Each Variable
  df_relationship <- data.frame(
    Month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
    Y = c(100, 110, 150, 145, 190, 160, 180, 220, 210, 220, 390, 320),
    X1 = c(153000, 112000, 176000, 139000, 125000, 212000, 229000, 205000, 234000, 245000, 257000, 268000)
  )
  # Render the data table
  output$data_table <- renderDataTable({
    df
  })
  
  # Descriptive Statistics
  output$desc_stats_output <- renderPrint({
    summary(df)
  })
  output$barplot_output <- renderPlotly({
    plots <- lapply(2:7, function(i) {
      plot_ly(df, x = ~df[, i], type = "histogram", color = I("#2596be")) %>%
        layout(showlegend = FALSE, title = colnames(df)[i], yaxis = list(title = "Frequency"))
    })
    subplot(plots, nrows = 2, margin = 0.05)
  })
  
  # Variable Relationship
  observeEvent(input$plot, {
    variable <- input$variable
    
    output$scatterbox_plot <- renderPlot({
      
      scatter_plot <- ggplot(df, aes_string(x = variable, y = "y")) +
        geom_point(color = "#2596be", size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "#FF5722") +
        labs(x = variable, y = "y", title = paste("Scatter Plot of", variable, "vs. y")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.position = "none")
      
      box_plot <- ggplot(df, aes_string(x = "y", y = variable)) +
        geom_boxplot(fill = "#2596be") +
        labs(x = "y", y = variable, title = paste("Boxplot of", variable)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12))
      
      grid.arrange(scatter_plot, box_plot, nrow = 2)
    })
  })
  
  # Prediction Dashboard
  observeEvent(input$analyze_button, {
    new_data <- data.frame(
      x1 = input$x1_input,
      x2 = input$x2_input,
      x3 = input$x3_input,
      x4 = input$x4_input,
      x5 = input$x5_input
    )
    
    # Predicted monthly sales volume
    predicted_sales <- predict(model, newdata = new_data)
    predicted_sales <- abs(predicted_sales)
    predicted_sales <- round(predicted_sales)
    
    output$sales_plot <- renderPlot({
      # Scatter plot for monthly sales volume prediction results
      plot(df$x1, df$y, col = "blue", xlab = "Number of Website Visitors", ylab = "Monthly Sales Volume (in thousands of USD)", main = "Scatterplot of Predicted Monthly Sales Volume", xlim = c(120000, 300000), ylim = c(0, 500))
      points(new_data$x1, predicted_sales, col = "red", pch = 16)
      legend("topright", legend = c("Actual Data", "Predicted Data"), col = c("blue", "red"), pch = c(1, 16))
    })
    
    output$analysis_output <- renderPrint({
      cat("Predicted Monthly Sales Volume: ", predicted_sales, " (in thousands of USD)\n")
      
      # Displaying regression coefficients
      coefficients <- coef(model)
      
      cat("\nRegression Coefficients:\n")
      for (i in seq_along(coefficients)) {
        cat(names(coefficients)[i], ": ", coefficients[i], "\n")
      }
      
      # Identifying the variable with the highest positive coefficient
      most_influential_variable <- names(coefficients)[which.max(coefficients)]
      
      cat("\nThe variable to focus on improving: ", most_influential_variable, "\n")
      
      # Adding interaction between x1 (number of website visitors) and x5 (number of ads)
      model_with_interaction <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x1:x5, data = df)
      
      # Evaluating significance of interaction
      interaction_p_value <- summary(model_with_interaction)$coefficients["x1:x5", "Pr(>|t|)"]
      
      cat("\nInteraction p-value between Number of Website Visitors (x1) and Number of Ads (x5): ", interaction_p_value, "\n")
      
      if (interaction_p_value < 0.05) {
        cat("There is a significant interaction between x1 and x5 affecting sales.\n")
      } else {
        cat("There is no significant interaction between x1 and x5 affecting sales.\n")
      }
    })
  })
}

# Run the app
shinyApp(ui,server)
