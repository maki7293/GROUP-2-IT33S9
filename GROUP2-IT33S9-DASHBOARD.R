# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(cluster)
library(factoextra)
library(DT)
library(modeest)
library(rvest)
library(stringr)
library(plotly)
library(gtrendsR)
library(lubridate)

# Load dataset
df <- read.csv("Students_Cleaned_Data.csv")

# Prepare clustering data
cluster_data <- df %>%
  select(Addicted_Score, Avg_Daily_Usage_Hours, Mental_Health_Score) %>%
  drop_na()
cluster_scaled <- scale(cluster_data)
set.seed(123)
kmeans_result <- kmeans(cluster_scaled, centers = 3, nstart = 25)

# Web scraping
url <- "https://headphonesaddict.com/social-media-addiction-statistics/"
page <- read_html(url)
data_raw <- page %>% html_elements("ul li") %>% html_text()
web_data <- data.frame(Stat = data_raw) %>%
  filter(str_detect(Stat, "%|students|average|hours")) %>%
  mutate(Stat = str_trim(Stat),
         Numeric = str_extract(Stat, "\\d+\\.*\\d*") %>% as.numeric())

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Student Social Media Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Avg Addiction by Country", tabName = "insight1", icon = icon("globe")),
      menuItem("Usage vs Academic", tabName = "insight2", icon = icon("book")),
      menuItem("Addiction vs Mental Health", tabName = "insight3", icon = icon("brain")),
      menuItem("Sleep vs Addiction", tabName = "insight4", icon = icon("bed")),
      menuItem("Addiction by Gender & Status", tabName = "insight5", icon = icon("venus-mars")),
      menuItem("Platform & Academic", tabName = "insight6", icon = icon("mobile")),
      menuItem("Student Clusters", tabName = "insight7", icon = icon("project-diagram")),
      menuItem("Academic Impact by Country", tabName = "insight8", icon = icon("chart-bar")),
      menuItem("Webscraped Stats", tabName = "webscrape", icon = icon("rss")),
      menuItem("Google Trends", tabName = "gtrends", icon = icon("chart-line")),
      menuItem("Student Data Table", tabName = "datatable", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("insight1", 
              fluidRow(
                column(12,
                       box(title = "Filter Options", width = 12, solidHeader = TRUE, status = "primary",
                           selectInput("selected_country", "Choose Country (Optional):",
                                       choices = unique(df$Country), selected = NULL, multiple = TRUE)
                       )
                ),
                column(12,
                       box(title = "Average Addiction Score by Country", width = 12, plotlyOutput("plot1"))
                ),
                column(12,
                       box(title = "Data Table", width = 12, DTOutput("table1"))
                ),
                column(12,
                       box(title = "Summary", width = 12, verbatimTextOutput("summary1"))
                )
              )
      ),
      tabItem("insight2", fluidRow(
        box(title = "Social Media Usage vs Academic Impact", width = 12, plotlyOutput("plot2")),
        box(title = "Data Table", width = 12, DTOutput("table2"))
      )),
      tabItem("insight3", fluidRow(
        box(title = "Addiction vs Mental Health", width = 12, plotlyOutput("plot3")),
        box(title = "Data Table", width = 12, DTOutput("table3"))
      )),
      tabItem("insight4", fluidRow(
        box(title = "Sleep Duration vs Addiction Score", width = 12, plotlyOutput("plot4")),
        box(title = "Data Table", width = 12, DTOutput("table4"))
      )),
      tabItem("insight5", fluidRow(
        box(title = "Addiction by Gender and Relationship Status", width = 12, plotlyOutput("plot5")),
        box(title = "Data Table", width = 12, DTOutput("table5"))
      )),
      tabItem("insight6", fluidRow(
        box(title = "Avg Addiction by Platform and Academic Impact", width = 12, plotlyOutput("plot6")),
        box(title = "Data Table", width = 12, DTOutput("table6"))
      )),
      tabItem("insight7", fluidRow(
        box(title = "Student Clusters", width = 12, plotOutput("plot7")),
        box(title = "Cluster Table", width = 12, DTOutput("table7"))
      )),
      tabItem("insight8", fluidRow(
        box(title = "% Reporting Academic Impact by Country", width = 12, plotlyOutput("plot8")),
        box(title = "Data Table", width = 12, DTOutput("table8")),
        box(title = "Summary", width = 12, verbatimTextOutput("summary8"))
      )),
      tabItem("webscrape", fluidRow(
        box(title = "Webscraped Global Social Media Stats", width = 12, DTOutput("web_table")),
        box(title = "Overview", width = 12, verbatimTextOutput("web_summary"))
      )),
      tabItem("gtrends", fluidRow(
        box(title = "Google Trends", width = 12, plotlyOutput("gtrends_plot")),
        box(title = "Google Trends Raw Data", width = 12, DTOutput("gtrends_table"))
      )),
      tabItem("datatable", fluidRow(
        box(width = 12, title = "Full Student Dataset", DTOutput("student_table"))
      ))
    )
  )
)

# Server
server <- function(input, output) {
  trend_df <- reactive({
    trend_data <- tryCatch({
      gtrends(
        keyword = c("social media addiction", "social media usage", "social media time"),
        geo = "",
        time = "now 1-H"
      )
    }, error = function(e) {
      message("Error fetching Google Trends data: ", e$message)
      NULL
    })
    
    if (!is.null(trend_data) && !is.null(trend_data$interest_over_time) && nrow(trend_data$interest_over_time) > 0) {
      trend_data$interest_over_time %>%
        mutate(
          date = with_tz(as.POSIXct(date, tz = "UTC"), tzone = "Asia/Manila"), # Changed to PHT
          hits = ifelse(hits == "<1", 0.5, as.numeric(hits)),
          keyword = as.factor(keyword)
        )
    } else {
      # Fallback with time series for 1 hour
      seq_time <- seq(from = Sys.time() - 3600, to = Sys.time(), by = "5 min")
      data.frame(
        date = with_tz(seq_time, tzone = "Asia/Manila"), # Changed to PHT
        hits = 0,
        keyword = "⚠️ No Google Trends data fetched. Try another keyword or time range."
      )
    }
  })
  
  filtered_df <- reactive({
    if (is.null(input$selected_country) || length(input$selected_country) == 0) return(df)
    df %>% filter(Country %in% input$selected_country)
  })
  
  output$summary1 <- renderPrint({
    scores <- filtered_df() %>% group_by(Country) %>% summarise(mean_score = mean(Addicted_Score, na.rm = TRUE)) %>% pull(mean_score)
    list(Summary = summary(scores), Mode = mfv(scores))
  })
  
  output$plot1 <- renderPlotly({
    p <- filtered_df() %>% group_by(Country) %>%
      summarise(Mean_Addicted_Score = mean(Addicted_Score, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Country, -Mean_Addicted_Score), y = Mean_Addicted_Score)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Country", y = "Mean Addiction Score")
    ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
    p <- ggplot(df, aes(x = Avg_Daily_Usage_Hours, fill = Affects_Academic_Performance)) +
      geom_histogram(binwidth = 1, position = "dodge") +
      labs(x = "Avg Daily Usage Hours", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot3 <- renderPlotly({
    p <- ggplot(df, aes(x = Addicted_Score, y = Mental_Health_Score)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot4 <- renderPlotly({
    p <- ggplot(df, aes(x = Sleep_Hours_Per_Night, y = Addicted_Score)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "purple") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot5 <- renderPlotly({
    p <- ggplot(df, aes(x = Relationship_Status, y = Addicted_Score, fill = Gender)) +
      geom_boxplot() +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot6 <- renderPlotly({
    p <- df %>% group_by(Most_Used_Platform, Affects_Academic_Performance) %>%
      summarise(Avg_Addiction = mean(Addicted_Score, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = reorder(Most_Used_Platform, -Avg_Addiction), y = Avg_Addiction, fill = Affects_Academic_Performance)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$plot8 <- renderPlotly({
    df$Affects_Academic_Performance <- as.logical(df$Affects_Academic_Performance)
    p <- df %>% group_by(Country) %>% summarise(Total = n(), Affected = sum(Affects_Academic_Performance, na.rm = TRUE),
                                                Percent = round((Affected / Total) * 100, 1)) %>%
      ggplot(aes(x = reorder(Country, -Percent), y = Percent)) +
      geom_bar(stat = "identity", fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "% Reporting Academic Impact")
    ggplotly(p)
  })
  
  output$plot7 <- renderPlot({
    fviz_cluster(kmeans_result, data = cluster_data, palette = "jco")
  })
  
  output$table1 <- renderDT({
    df %>% group_by(Country) %>% summarise(Mean_Addicted_Score = round(mean(Addicted_Score, na.rm = TRUE), 2)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$table2 <- renderDT({
    df %>% select(Country, Affects_Academic_Performance, Avg_Daily_Usage_Hours) %>% drop_na() %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$table3 <- renderDT({
    df %>% select(Country, Addicted_Score, Mental_Health_Score) %>% drop_na() %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$table4 <- renderDT({
    df %>% select(Country, Sleep_Hours_Per_Night, Addicted_Score) %>% drop_na() %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$table5 <- renderDT({
    df %>% select(Country, Relationship_Status, Gender, Addicted_Score) %>% drop_na() %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$table6 <- renderDT({
    df %>% select(Country, Most_Used_Platform, Affects_Academic_Performance, Addicted_Score) %>% drop_na() %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$table7 <- renderDT({
    cluster_data %>% mutate(Cluster = kmeans_result$cluster,
                            Country = df$Country[complete.cases(df[, c("Addicted_Score", "Avg_Daily_Usage_Hours", "Mental_Health_Score")])]) %>%
      select(Country, everything()) %>% datatable(options = list(pageLength = 10))
  })
  
  output$table8 <- renderDT({
    df %>% mutate(Affects_Academic_Performance = as.logical(Affects_Academic_Performance)) %>%
      group_by(Country) %>% summarise(Total = n(), Affected = sum(Affects_Academic_Performance, na.rm = TRUE),
                                      Percent = round((Affected / Total) * 100, 1)) %>% datatable(options = list(pageLength = 10))
  })
  
  # Plot Output
  output$gtrends_plot <- renderPlotly({
    df_plot <- trend_df()
    print(df_plot) # Debug: Print data to console to inspect
    
    # Check if data is valid for plotting
    if (is.null(df_plot) || nrow(df_plot) == 0 || all(is.na(df_plot$hits))) {
      return(plotly_empty(type = "scatter", text = "No data available or error fetching Google Trends data."))
    }
    
    p <- ggplot(df_plot, aes(x = date, y = hits, color = keyword, group = keyword)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Social Media Topics (Last 1 Hour)",
        x = "Time",
        y = "Search Interest"
      ) +
      theme_minimal() +
      scale_x_datetime(date_labels = "%H:%M") # Format x-axis as hours:minutes
    ggplotly(p)
  })
  
  output$gtrends_table <- renderDT({
    datatable(trend_df(), options = list(pageLength = 10))
  })
  
  output$web_table <- renderDT({
    datatable(web_data, options = list(pageLength = 10))
  })
  
  output$web_summary <- renderPrint({
    cat("This data was scraped from HeadphonesAddict.com and highlights key global statistics on social media usage and addiction.\n\n")
    cat("It includes figures like average screen time, addiction prevalence, student behavior trends, and more.\n\n")
    cat("This tab provides context by comparing international findings with our own student dataset.")
  })
  
  output$student_table <- renderDT({
    datatable(df, options = list(pageLength = 10, scrollY = "400px"))
  })
}

# Run the app
shinyApp(ui, server)