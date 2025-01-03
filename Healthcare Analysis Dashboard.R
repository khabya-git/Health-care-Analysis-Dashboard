# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(scales)
library(httr)
library(shinyjs)
library(DT)



# Load the dataset
healthcare_data <- read.csv("C:\\Users\\himank\\Downloads\\Cleaned_Healthcare_Dataset.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Healthcare Analysis"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Main Page", tabName = "main_page", icon = icon("home")),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Doctor", tabName = "doctor", icon = icon("user-md")),
                menuItem("Prediction", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("/* Global CSS Styling */
      body { font-family: 'Arial', sans-serif; }
      .welcome-section { text-align: center; padding: 30px; background: #003366; color: white; }
      .features-section { padding: 30px; background: linear-gradient(to bottom, #005f99, #007acc); color: white; }
      .contact-us-section { padding: 30px; background: #f8f9fa; color: #003366; }

      /* Custom Value Box Styling */
      .small-box { box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.2); transform: scale(1.02); border-radius: 10px; transition: all 0.3s ease-in-out; }
      .small-box:hover { transform: scale(1.05); box-shadow: 4px 4px 15px rgba(0, 0, 0, 0.3); }

      /* Table Styling */
      .doctor-table { font-size: 16px; color: #333; width: 100%; border-collapse: collapse; }
      .doctor-table th, .doctor-table td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
      .doctor-table th { background-color: #4CAF50; color: #fff; font-weight: bold; text-transform: uppercase; font-size: 18px; }
      .doctor-table tr:nth-child(even) { background-color: #f2f2f2; }
      .doctor-table tr:hover { background-color: #ddd; }
      .doctor-table td { font-size: 14px; color: #555; }

      /* Well Panel Styling */
      .well-panel-custom { background-color: #f7f7f7; border-radius: 10px; padding: 20px; }
      .well-panel-custom h3 { color: #003366; font-weight: bold; margin-bottom: 20px; }
      .well-panel-custom p { color: #555; }

      /* Button Styles */
      .btn {
        margin-top: 30px;
        padding: 15px 30px;
        font-size: 18px;
        background-color: #007bff;
        color: white;
        border-radius: 25px;
        border: none;
        box-shadow: 0 4px 10px rgba(0, 123, 255, 0.4);
        transition: background-color 0.3s, transform 0.3s;
      }

      .btn:hover {
        background-color: #0056b3;
        transform: scale(1.05);
        box-shadow: 0 6px 15px rgba(0, 123, 255, 0.6);
      }

      /* Center the button */
      .center-button {
        display: flex;
        justify-content: center;
        align-items: center;
      }
    "))
    ),
    tabItems(
      # Main Page
      tabItem(tabName = "main_page",
              fluidPage(
                div(class = "welcome-section",
                    h1("Welcome to the Healthcare Analysis Dashboard", style = "font-size: 36px; font-weight: bold;"),
                    p("Unlock actionable insights to improve patient outcomes and operational efficiency.")
                ),
                div(class = "features-section",
                    h3("Key Features of the Dashboard", style = "text-align: center; margin-bottom: 20px;"),
                    fluidRow(
                      column(4, div(style = "text-align: center;",
                                    icon("chart-bar", class = "fa-3x", style = "color: #f0ad4e;"),
                                    h4("Interactive Visualizations"),
                                    p("Gain insights with real-time metrics, charts, and predictive analytics.")
                      )),
                      column(4, div(style = "text-align: center;",
                                    icon("user-md", class = "fa-3x", style = "color: #5cb85c;"),
                                    h4("Doctor Performance"),
                                    p("Monitor and evaluate doctor efficiency and experience distribution.")
                      )),
                      column(4, div(style = "text-align: center;",
                                    icon("chart-line", class = "fa-3x", style = "color: #d9534f;"),
                                    h4("Predictive Analytics"),
                                    p("Predict insurance cover and compare billing with predictive models.")
                      ))
                    )
                ),
                div(class = "contact-us-section",
                    h3("Contact Us", style = "text-align: center; margin-bottom: 20px;"),
                    p("Have questions or need support? Reach out to us:", style = "text-align: center; font-size: 16px; margin-bottom: 10px;"),
                    fluidRow(
                      column(4, div(style = "text-align: center;",
                                    icon("envelope", class = "fa-2x", style = "color: #007acc;"),
                                    h4("Email"),
                                    p("chharshchoudhary@gmail.com")
                      )),
                      column(4, div(style = "text-align: center;",
                                    icon("phone", class = "fa-2x", style = "color: #28a745;"),
                                    h4("Phone"),
                                    p("+91 7976857105")
                      )),
                      column(4, div(style = "text-align: center;",
                                    icon("map-marker-alt", class = "fa-2x", style = "color: #dc3545;"),
                                    h4("Address"),
                                    p("SKIT Jaipur")
                      ))
                    )
                ),
                mainPanel(
                  div(class = "center-button", actionButton("start_button", "Start"))
                )
              )
      ),
      # Dashboard Page
      tabItem(tabName = "dashboard",
              fluidPage(
                h2("Dashboard Overview", style = "text-align:center; color: #003366; margin-bottom: 30px;"),
                fluidRow(
                  valueBoxOutput("total_patients", width = 3),
                  valueBoxOutput("total_doctors", width = 3),
                  valueBoxOutput("total_billing", width = 3),
                  valueBoxOutput("average_feedback", width = 3)
                ),
                tabsetPanel(
                  id = "charts_tab",
                  tabPanel("Pie Chart", plotlyOutput("pie_chart")),
                  tabPanel("Histogram", plotlyOutput("histogram")),
                  tabPanel("Bar Chart", plotlyOutput("bar_chart")),
                  tabPanel("Scatter Plot", plotlyOutput("scatter_plot")),
                  tabPanel("3D Chart", plotlyOutput("three_d_chart"))
                ),
                fluidRow(
                  column(6, actionButton("back_button_dashboard", "Back", class = "btn btn-secondary")),
                  column(6, actionButton("next_button_dashboard", "Next", class = "btn btn-primary"))
                )
              )
      ),
      # Doctor Page
      tabItem(tabName = "doctor",
              fluidPage(
                h2("Doctor Insights", style = "text-align:center; color: #003366; margin-bottom: 30px;"),
                fluidRow(
                  column(6, wellPanel(
                    h3("Top Performing Doctors", style = "color: #003366; font-weight: bold;"),
                    tableOutput("top_doctors_table")
                  )),
                  column(6, wellPanel(
                    h3("Doctor Experience Distribution", style = "color: #003366; font-weight: bold;"),
                    plotlyOutput("donut_chart")
                  ))
                ),
                div(style = "text-align: center; margin-top: 20px;",
                    actionButton("back_button_doctor", "Back", class = "btn btn-secondary"),
                    actionButton("next_button_doctor", "Next", class = "btn btn-primary")
                )
              )
      ),
      # Prediction Page
      tabItem(tabName = "prediction",
              fluidPage(
                h2("Predicted Healthcare Insurance Cover", style = "text-align:center; color: #003366; margin-bottom: 20px;"),
                fluidRow(
                  column(6, wellPanel(
                    h4("Enter Details", style = "color: #003366; font-weight: bold;"),
                    selectInput("doctor", "Doctor Name", choices = unique(healthcare_data$Doctor)),
                    numericInput("experience", "Years of Experience", value = 5, min = 0),
                    numericInput("billing", "Billing Amount", value = 10000, min = 0),
                    selectInput("diagnosis", "Diagnosis", choices = unique(healthcare_data$Diagnosis)),
                    selectInput("bed_occupancy", "Bed Occupancy", choices = unique(healthcare_data$Bed_Occupancy)),
                    dateRangeInput("admit_discharge", "Admit & Discharge Date", start = Sys.Date(), end = Sys.Date()),
                    actionButton("predict_button", "Predict", class = "btn btn-primary", style = "margin-top: 10px; width: 100%;")
                  )),
                  column(6, wellPanel(
                    h4("Results", style = "color: #003366; font-weight: bold;"),
                    div(strong("Predicted Insurance Cover:"), span(textOutput("predicted_cover"), style = "color: green; font-size: 20px;")),
                    div(strong("Comparison with Billing Amount:"), span(textOutput("comparison"), style = "color: blue; font-size: 16px;")),
                    plotlyOutput("comparison_chart")
                  ))
                ),
                div(style = "text-align: center;",
                    actionButton("back_button_prediction", "Back", class = "btn btn-secondary"),
                    actionButton("main_button_prediction", "Main Page", class = "btn btn-primary")
                )
              )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Navigation Buttons
  observeEvent(input$next_button, {
    updateTabItems(session, "tabs", "dashboard")
  })
  
  observeEvent(input$back_button_dashboard, {
    updateTabItems(session, "tabs", "main_page")
  })
  
  observeEvent(input$next_button_dashboard, {
    updateTabItems(session, "tabs", "doctor")
  })
  
  observeEvent(input$back_button_doctor, {
    updateTabItems(session, "tabs", "dashboard")
  })
  
  observeEvent(input$next_button_doctor, {
    updateTabItems(session, "tabs", "prediction")
  })
  
  observeEvent(input$back_button_prediction, {
    updateTabItems(session, "tabs", "doctor")
  })
  
  observeEvent(input$main_button_prediction, {
    updateTabItems(session, "tabs", "main_page")
  })
  
  # Start Button Functionality
  observeEvent(input$start_button, {
    updateTabItems(session, "tabs", "dashboard")
  })
  
  # Dashboard Overview Metrics
  output$total_patients <- renderValueBox({
    valueBox(
      value = scales::label_number(scale = 1e3, suffix = "K", big.mark = ",")(nrow(healthcare_data) / 1000),
      subtitle = "Total Patients",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$total_doctors <- renderValueBox({
    valueBox(
      value = length(unique(healthcare_data$Doctor)),
      subtitle = "Total Doctors",
      icon = icon("user-md"),
      color = "green"
    )
  })
  
  output$total_billing <- renderValueBox({
    valueBox(
      value = scales::label_number(scale = 1e-6, suffix = "M")(sum(healthcare_data$Billing.Amount, na.rm = TRUE)),
      subtitle = "Total Billing Amount",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$average_feedback <- renderValueBox({
    valueBox(
      value = round(mean(healthcare_data$Feedback, na.rm = TRUE), 1),
      subtitle = "Average Feedback",
      icon = icon("star"),
      color = "orange"
    )
  })
  
  # Visualization Plots
  output$pie_chart <- renderPlotly({
    pie_data <- healthcare_data %>% count(Diagnosis)
    plot_ly(pie_data, labels = ~Diagnosis, values = ~n, type = 'pie') %>%
      layout(title = "Diagnosis Distribution")
  })
  
  output$histogram <- renderPlotly({
    plot_ly(healthcare_data, x = ~Billing.Amount, type = "histogram") %>%
      layout(title = "Billing Amount Distribution")
  })
  
  output$bar_chart <- renderPlotly({
    bar_data <- healthcare_data %>% count(Doctor)
    plot_ly(bar_data, x = ~Doctor, y = ~n, type = "bar") %>%
      layout(title = "Number of Patients per Doctor")
  })
  
  output$scatter_plot <- renderPlotly({
    plot_ly(healthcare_data, x = ~Billing.Amount, y = ~Feedback, mode = "markers") %>%
      layout(title = "Feedback vs Billing Amount")
  })
  
  output$three_d_chart <- renderPlotly({
    plot_ly(healthcare_data, x = ~Billing.Amount, y = ~Consultation.Time, z = ~Feedback,
            type = "scatter3d", mode = "markers") %>%
      layout(title = "3D Chart: Feedback, Billing, and Consultation Time")
  })
  
  # Donut Chart
  output$donut_chart <- renderPlotly({
    experience_data <- data.frame(
      Experience = c("Less than 5 years", "5-10 years", "10-20 years", "20+ years"),
      Percentage = c(15, 35, 30, 20)
    )
    
    plot_ly(experience_data, labels = ~Experience, values = ~Percentage, type = 'pie',
            hole = 0.4, textinfo = 'label+percent',
            marker = list(
              colors = c('#4CAF50', '#FFC107', '#2196F3', '#9C27B0'),
              line = list(color = 'white', width = 2)
            )) %>%
      layout(title = "Doctor Experience Distribution")
  })
  
  # Top Doctors Table
  output$top_doctors_table <- renderTable({
    top_doctors <- data.frame(
      DoctorName = c("Dr. Smith", "Dr. Jones", "Dr. Lee", "Dr. Kumar"),
      Specialty = c("Cardiology", "Neurology", "Orthopedics", "General Practice"),
      Experience = c("15 years", "12 years", "10 years", "8 years"),
      Rating = c(4.7, 4.8, 4.5, 4.3)
    )
    top_doctors
  }, rownames = TRUE, striped = TRUE, hover = TRUE, class = "doctor-table")
  
  # Prediction Logic
  prediction <- reactiveVal(NULL)
  
  observeEvent(input$predict_button, {
    if (input$billing <= 0) {
      showNotification("Billing amount must be greater than zero.", type = "error")
      return()
    }
    if (input$experience < 0) {
      showNotification("Experience cannot be negative.", type = "error")
      return()
    }
    predicted_cover <- input$billing * 0.85
    prediction(list(predicted_cover = predicted_cover))
  })
  
  # Display Prediction Results
  output$predicted_cover <- renderText({
    req(prediction())
    paste("₹", round(prediction()$predicted_cover, 2))
  })
  
  output$comparison <- renderText({
    req(prediction())
    difference <- input$billing - prediction()$predicted_cover
    if (difference > 0) {
      paste("Billing exceeds insurance cover by ₹", round(difference, 2))
    } else {
      paste("Insurance cover exceeds billing by ₹", abs(round(difference, 2)))
    }
  })
  
  output$comparison_chart <- renderPlotly({
    req(prediction())
    predicted_cover <- prediction()$predicted_cover
    billing_amount <- input$billing
    comparison_data <- data.frame(
      Category = c("Billing Amount", "Predicted Insurance Cover"),
      Amount = c(billing_amount, predicted_cover)
    )
    
    plot_ly(comparison_data, x = ~Category, y = ~Amount, type = 'bar',
            text = ~paste0("₹", round(Amount, 2)), textposition = 'inside',
            marker = list(color = c('#FF7F0E', '#1F77B4'))) %>%
      layout(
        title = "Billing Amount vs Insurance Cover",
        yaxis = list(title = "Amount (₹)", showgrid = FALSE),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })
}

# Run the application
shinyApp(ui, server)
