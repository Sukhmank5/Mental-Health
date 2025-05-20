library(shiny)
library(tidyverse)
library(shinyWidgets)
library(fontawesome)
library(bslib)

# Load the dataset
load("Survey.RData")  # survey

# Clean and filter data
clean_data <- survey %>%
  filter(Age >= 18 & Age <= 25) %>%
  select(Gender, Age, self_employed, family_history, treatment, work_interfere, mental_health_consequence) %>%
  mutate(
    Gender = case_when(
      str_detect(str_to_lower(Gender), "female") ~ "Female",
      str_detect(str_to_lower(Gender), "male") ~ "Male",
      TRUE ~ "Other"
    )
  )

# --- UI ---
ui <- fluidPage(
    theme = bs_theme(bootswatch = "sandstone"), 
    titlePanel(
      div(
        icon("brain", lib = "font-awesome"),
        span("Mental Health Awareness Dashboard: College Edition", style = "margin-left: 10px; font-weight: bold;")
    )
  ),
  
  p("This dashboard looks at mental health trends in college-aged people (18–25) using real survey data. It brings attention to things like risk factors, getting help, and how stigma plays a role — all to help students better understand mental health and feel more comfortable talking about it."),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "gender", 
        label = "Select Gender:", 
        choices = c("All", "Male", "Female", "Other"),
        selected = "All",
        multiple = FALSE,
        # options = list(style = "btn-info") # optional
      ),
      sliderTextInput(
        inputId = "ageRange",
        label = "Select Age Range:",
        choices = 18:25,
        selected = c(18, 25),
        grid = TRUE,
        dragRange = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Treatment", plotOutput("treatmentPlot")),
        tabPanel("Risk Score", verbatimTextOutput("riskScore")),
        tabPanel("Employment & Treatment", plotOutput("employmentPlot")),
        tabPanel("Self-Employment & Gender", plotOutput("genderEmploymentPlot")),
        tabPanel("Work Interference", plotOutput("interferencePlot")),
        tabPanel("Perceived Consequences", plotOutput("consequencePlot")),
        
        tabPanel("Takeaway",
                 p("Mental health challenges are real, especially for college students navigating stress, transitions, and identity. This dashboard highlights how stigma, family background, and work pressure affect whether students seek help."),
                 br(),
                 p("What can you do?"),
                 tags$ul(
                   tags$li("Check in with yourself and your peers regularly."),
                   tags$li("Don’t wait — reaching out for help is a sign of strength."),
                   tags$li("Use campus counseling and wellness resources — they’re there for you."),
                   tags$li("Talk openly. Normalize mental health conversations."),
                   tags$li("Be the person who listens without judgment.")
                 ),
                 br(),
                 p(em("Remember: you're not alone — and your mental health matters."))
        ),
        
        tabPanel("Resources",
                 tags$h3("Mental Health Resources"),
                 tags$p("Here are some valuable resources for mental health support and information:"),
                 tags$ul(
                   tags$li(a(href = "https://www.nami.org/Home", target = "_blank", "National Alliance on Mental Illness (NAMI)")),
                   tags$li(a(href = "https://www.adaa.org/finding-help", target = "_blank", "Anxiety and Depression Association of America")),
                   tags$li(a(href = "https://www.counseling.org/", target = "_blank", "American Counseling Association")),
                   tags$li(a(href = "https://suicidepreventionlifeline.org/", target = "_blank", "Suicide Prevention Lifeline"))
                 ),
                 tags$p("If you or someone you know is struggling, please don't hesitate to reach out for help.")
        )
      )
    )
  )
)
# --- Server ---
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- clean_data
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    data <- data %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    return(data)
  })
  
  output$treatmentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = treatment, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("Male" = "#00BFFF", "Female" = "#FF69B4", "Other" = "#B0B0B0")) +
      labs(
        title = "Treatment Seeking Behavior Among College Students",
        x = "Sought Treatment",
        y = "Number of Students"
      ) +
      theme_minimal()
  })
  
  output$riskScore <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("No data available for selected filters.")
    score <- data %>%
      mutate(risk_points = ifelse(family_history == "Yes", 1, 0) +
               ifelse(work_interfere %in% c("Often", "Sometimes"), 1, 0) +
               ifelse(treatment == "No", 1, 0)) %>%
      summarise(avg_score = mean(risk_points, na.rm = TRUE)) %>%
      pull(avg_score)
    
    level <- case_when(
      score >= 2 ~ "High",
      score >= 1 ~ "Moderate",
      TRUE ~ "Low"
    )
    
    paste0("Estimated Mental Health Risk Level (College Age Group): ", level, 
           " (Avg Score: ", round(score, 2), " / 3)")
  })
  
  output$employmentPlot <- renderPlot({
    plot_data <- filtered_data() %>%
      mutate(self_employed = case_when(
        self_employed == "Yes" ~ "Self-Employed",
        self_employed == "No" ~ "Not Self-Employed",
        is.na(self_employed) | self_employed == "" ~ "Unknown"
      ))
    
    ggplot(plot_data, aes(x = self_employed, fill = treatment)) +
      geom_bar(position = "dodge", stat = "count") +
      scale_fill_manual(values = c("Yes" = "#00c0ef", "No" = "#d9534f")) +
      labs(
        title = "Treatment by Self-Employment Status (College Students)",
        x = "Employment Status",
        y = "Number of Students"
      ) +
      theme_minimal() +
      geom_text(
        aes(label = ..count..), 
        stat = "count", 
        position = position_dodge(width = 0.8), 
        vjust = -0.5
      )
  })
  
  output$genderEmploymentPlot <- renderPlot({
    plot_data <- filtered_data() %>%
      mutate(self_employed = case_when(
        self_employed == "Yes" ~ "Self-Employed",
        self_employed == "No" ~ "Not Self-Employed",
        is.na(self_employed) | self_employed == "" ~ "Unknown"
      ))
    
    ggplot(plot_data, aes(x = self_employed, fill = Gender)) +
      geom_bar(position = "dodge", stat = "count") +
      scale_fill_manual(values = c("Female" = "#f78fb3", "Male" = "#00c0ef")) +
      labs(
        title = "Self-Employment vs. Gender (College Students)",
        x = "Employment Status",
        y = "Number of Students"
      ) +
      theme_minimal() +
      geom_text(
        aes(label = ..count..), 
        stat = "count", 
        position = position_dodge(width = 0.8), 
        vjust = -0.5
      )
  })
  
  output$interferencePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = work_interfere)) +
      geom_bar(fill = "#FFCC66") +
      coord_flip() +
      labs(
        title = "How Often Mental Health Affects Academic or Work Responsibilities",
        x = "Work Interference Frequency",
        y = "Number of Students"
      ) +
      theme_minimal()
  })
  
  output$consequencePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = mental_health_consequence)) +
      geom_bar(fill = "#FF69B4") +
      coord_flip() +
      labs(
        title = "Perceived Consequences of Mental Health on Career Progression",
        x = "Perceived Consequences",
        y = "Number of Students"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)

