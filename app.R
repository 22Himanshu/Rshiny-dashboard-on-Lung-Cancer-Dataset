library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Load data
data <- read_csv("survey lung cancer.csv")
data$GENDER <- ifelse(data$GENDER == 'M', 0, 1)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Lung cancer survey analysis",
             tabPanel("Introduction",
                      textOutput("introduction_title"),
                      textOutput("introduction"), 
                      textOutput("overview_title"),
                      textOutput("overview"),
                      textOutput("Tabular_title"),
                      textOutput("Tabular"),
                      textOutput("chart_title"),
                      textOutput("chart"),
                      textOutput("trend_title"),
                      textOutput("trend"),
                      textOutput("conclusion_title"),
                      textOutput("conclusion"),
                      
                      tags$head(tags$style("#introduction_title{ color:#1c1b1b; font-size: 24px; text-align: left;}
                                            #overview_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #Tabular_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #chart_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #trend_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #conclusion_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                           #introduction{text-align: justify;font-size: 14px;}
                                           #Tabular{text-align: justify;font-size: 14px;}
                                           #chart{text-align: justify;font-size: 14px;}
                                           #correlation{text-align: justify;font-size: 14px;}
                                           #conclusion{text-align: justify;font-size: 14px;}
                                           #trend{text-align: justify;font-size: 14px;}"))
             ),
             tabPanel("Exploratory Analysis",
                      fluidRow(
                        column(width = 12,
                               h4("Age vs smoking" , align = "left")
                        )),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("bins", "Number of Bins", min = 5, max = 50, value = 10),
                          selectInput("graphType", "Select Graph Type",
                                      choices = c("Stacked Bar Graph", "Density Plot"),
                                      selected = "Stacked Bar Graph"),
                          actionButton("generatePlot", "Generate")
                        ),
                        
                        mainPanel(
                          plotOutput("ageCountPlot")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               h4("Pie charts", align = "left")),
                        selectInput("pieChartType", "Select Pie Chart Type",
                                    choices = c("Smokers vs Non- Smokers",
                                                "Male vs Female",
                                                "Person with anxiety vs No anxiety",
                                                "Yellow fingers vs Non-Yellow fingers",
                                                "Peer Pressure",
                                                "Wheezing"),
                                    selected = "Smokers vs Non- Smokers"),
                        actionButton("generatePieChart", "Generate Pie Chart")
                        
                      ),
                      plotOutput("pieChart", height = "400px"),
                      fluidRow(
                        column(width = 12,
                               p("Note - These pie charts are used to visualise different aspect of our dataset, which can show biasness of this data."))
                      )
             )
             , tabPanel("Smoking vs Lung_cancer", fluidRow(column(width = 12,plotOutput("smokingVsLungCancerPlot")),
                        ),fluidRow(column(width = 12, plotOutput("nonSmokingVsLungCancerPlot")),
                                   fluidRow(column(width =12,
                                                   p("Here clearly we can see that number of people with lung cancer far exceeds to the number of people without it. so, this dataset might be little bias towards people having lung cancer.However there seems no evidence of relationship between smoking and lung cancer.")))
))))

# Server
server <- function(input, output) {
  output$introduction_title <- renderText({
    intro <- paste0("About the Project")
  })
  output$overview_title <- renderText(
    {text <- paste("1. Overview")}
  )
  output$overview <- renderText(
    {text <- paste("In this data, 1 represents NO for each value , 2 represents YES for each value. M & F are used for male and female respectively.")}
  )
  output$introduction <- renderText({
    text <- paste("Hello, My name is Himanshu, Student of Chennai Mathematical Institute.The 'Lung Cancer Analysis' app is a comprehensive and interactive tool built using R programming language and the Shiny framework. This app aims to provide insightful analysis and visualization of caughting Lung Cancer at its early stages.The effectiveness of cancer prediction system helps the people to know their cancer risk with low cost and it also helps the people to take the appropriate decision based on their cancer risk status. The data is collected from the website online lung cancer prediction system .")
  })
  output$Tabular_title <- renderText({
    text <- paste("2. Exploring the data")})
  output$Tabular <- renderText({
    text <- paste("In this we will try to explore our dataset, and see if there is any biasness towards any elements by relationship of age vs lung cancer and with the help of various pie charts which helps us to draw many conclusions.")
  })
  output$chart_title <- renderText({
    text <- paste("3. Relationship of smoking vs lung_cancer")})
  output$chart <- renderText({
    text <- paste("Lets try to understand whether smoking does really causes lung cancer with the help of some data analysis.This will be our main focus for now. This will help to make better policy for people and create healthy life-style for people.")
  })
  output$trend_title <- renderText({
    text <- paste("4. Sources")})
  output$trend <- renderText({
    text <- paste("This data can be found on kaggle.com. The link for data is https://www.kaggle.com/datasets/mysarahmadbhat/lung-cancer.  ")
  })
  output$conclusion_title <- renderText({
    text <- paste("5. Conclusion")})
  output$conclusion <- renderText({
    text <- paste('In conclusion, this app offers valuable insights into the complex interplay of smoking , age , other factors on lung cancer which can be extended much further than i have done here.Researchers and analysts can leverage various findings to better understand this issue. By identifying patterns, we can catch lung cancer at its early stages using various machine learning algorithms. Since , i dont know ml yet,but will add them future hopefully.')
  })
  
  output$ageCountPlot <- renderPlot({
    req(input$generatePlot)
    
    num_bins <- input$bins
    
    data$AGE_GROUP <- cut(data$AGE, breaks = num_bins)
    
    if (input$graphType == "Stacked Bar Graph") {
      ggplot(data, aes(x = AGE_GROUP, fill = LUNG_CANCER)) +
        geom_bar(position = "stack", color = "black") +
        scale_fill_manual(values = c("red", "green"), labels = c("Yes", "No")) +
        labs(title = "Age Distribution by Lung Cancer Status") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
        scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) 
    } else if (input$graphType == "Density Plot") {
      ggplot(data, aes(x = AGE, fill = LUNG_CANCER)) +
        geom_density(alpha = 0.5, aes(color = LUNG_CANCER), size = 1) +
        scale_fill_manual(values = c("red", "green"), labels = c("Yes", "No")) +
        scale_color_manual(values = c("red", "green"), labels = c("Yes", "No")) +
        labs(title = "Density Distribution by Lung Cancer Status") + 
        theme_minimal()
    }
  })
  output$smokingVsLungCancerPlot <- renderPlot({
    # Filter the data for people who smoke and plot their lung cancer status
    smokers_data <- data %>%
      filter(SMOKING == "1") %>%
      group_by(LUNG_CANCER) %>%
      summarize(count = n())
    
    ggplot(smokers_data, aes(x = LUNG_CANCER, y = count, fill = LUNG_CANCER)) +
      geom_bar(stat = "identity") +
      labs(title = "Smokers: Lung Cancer vs No Lung Cancer") +
      scale_fill_manual(values = c("green", "red"), labels = c("No", "Yes")) +
      theme_minimal()
  })
  
  output$nonSmokingVsLungCancerPlot <- renderPlot({
    # Filter the data for people who do not smoke and plot their lung cancer status
    non_smokers_data <- data %>%
      filter(SMOKING == "2") %>%
      group_by(LUNG_CANCER) %>%
      summarize(count = n())
    
    ggplot(non_smokers_data, aes(x = LUNG_CANCER, y = count, fill = LUNG_CANCER)) +
      geom_bar(stat = "identity") +
      labs(title = "Non-Smokers: Lung Cancer vs No Lung Cancer") +
      scale_fill_manual(values = c("green", "red"), labels = c("No", "Yes")) +
      theme_minimal()
  })
  
  output$pieChart <- renderPlot({
    req(input$generatePieChart)
    
    # Create the pie chart based on the selected type
    if (input$pieChartType == "Smokers vs Non- Smokers") {
      # Generate pie chart for smokers vs non-smokers
      smokers_data <- data %>%
        group_by(SMOKING) %>%
        summarize(count = n())
      
      #smokers_data$SMOKING <- factor(smokers_data$SMOKING, levels = c(1, 2), labels = c("Smoker", "Non-Smoker"))
      
      ggplot(smokers_data, aes(x = "", y = count, fill = SMOKING)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Smoking Status")
    } else if (input$pieChartType == "Male vs Female") {
      # Generate pie chart for male vs female
      gender_data <- data %>%
        group_by(GENDER) %>%
        summarize(count = n())
      
      gender_data$GENDER <- factor(gender_data$GENDER, levels = c(0, 1), labels = c("Female", "Male"))
      
      ggplot(gender_data, aes(x = "", y = count, fill = GENDER)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Gender")
    } else if (input$pieChartType == "Person with anxiety vs No anxiety") {
      # Generate pie chart for anxiety vs no anxiety
      anxiety_data <- data %>%
        group_by(ANXIETY) %>%
        summarize(count = n())
      
      anxiety_data$ANXIETY <- factor(anxiety_data$ANXIETY, levels = c(1, 2), labels = c("Anxiety", "No Anxiety"))
      
      ggplot(anxiety_data, aes(x = "", y = count, fill = ANXIETY)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Anxiety Status")
    }else if (input$pieChartType == "Yellow fingers vs Non-Yellow fingers") {
      yellow_fingers_data <- data %>%
        group_by(YELLOW_FINGERS) %>%
        summarize(count = n())
      
      yellow_fingers_data$YELLOW_FINGERS <- factor(yellow_fingers_data$YELLOW_FINGERS, levels = c(1, 2), labels = c("Yellow Fingers", "Non-Yellow Fingers"))
      
      ggplot(yellow_fingers_data, aes(x = "", y = count, fill = YELLOW_FINGERS)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Yellow Fingers Status")
    } else if (input$pieChartType == "Lung Cancer vs Not Lung Cancer") {
      lung_cancer_data <- data %>%
        group_by(LUNG_CANCER) %>%
        summarize(count = n())
      
      lung_cancer_data$LUNG_CANCER <- factor(lung_cancer_data$LUNG_CANCER, levels = c("Yes", "No"), labels = c("Lung Cancer", "Not Lung Cancer"))
      
      ggplot(lung_cancer_data, aes(x = "", y = count, fill = LUNG_CANCER)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Lung Cancer Status")
    }else if (input$pieChartType == "Peer Pressure") {
      # Generate pie chart for peer pressure
      peer_pressure_data <- data %>%
        group_by(PEER_PRESSURE) %>%
        summarize(count = n())
      
      peer_pressure_data$PEER_PRESSURE <- factor(peer_pressure_data$PEER_PRESSURE, levels = c(1, 2), labels = c("No Peer Pressure", "Peer Pressure"))
      
      ggplot(peer_pressure_data, aes(x = "", y = count, fill = PEER_PRESSURE)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Peer Pressure Status")
    } else if (input$pieChartType == "Wheezing") {
      # Generate pie chart for wheezing
      wheezing_data <- data %>%
        group_by(WHEEZING) %>%
        summarize(count = n())
      
      wheezing_data$WHEEZING <- factor(wheezing_data$WHEEZING, levels = c(1, 2), labels = c("No Wheezing", "Wheezing"))
      
      ggplot(wheezing_data, aes(x = "", y = count, fill = WHEEZING)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = "Wheezing Status")
    }
  })
}

shinyApp(ui = ui, server = server)
