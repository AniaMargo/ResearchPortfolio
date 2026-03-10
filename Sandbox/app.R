#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(bslib)

ui <- navset_tab(
  title = "Ania Grudzien Research Portfolio",
  
  # ---- TAB 1: Overview ----
  nav_panel(
    "Overview",
    page_sidebar(
      sidebar = NULL,   # no sidebar needed
      card(
        card_header("Overview"),
        "Describe the project, datasets, and goals here."
      )
    )
  ),
  
  # ---- TAB 2: Experiments ----
  nav_panel(
    "Experiments 1–4",
    page_sidebar(
      sidebar = sidebar(
        selectInput("exp", "Choose experiment:", choices = 1:4)
      ),
      card(
        card_header("Experiment description"),
        uiOutput("exp_text")
      ),
      card(
        card_header("Experiment plots"),
        plotOutput("exp_plot")
      )
    )
  ),
  
  # ---- TAB 3: Pupil Dynamics ----
  nav_panel(
    "Pupil Dynamics",
    page_sidebar(
      sidebar = sidebar(
        selectInput("exp_pupil", "Experiment:", choices = 1:4),
        checkboxGroupInput(
          "cond_pupil",
          "Condition:",
          choices = c("Read", "Generate"),
          selected = c("Read", "Generate")
        )
      ),
      card(
        card_header("Pupil timecourse"),
        plotOutput("pupil_plot")
      )
    )
  ),
  
  # ---- TAB 4: Accuracy & Features ----
  nav_panel(
    "Accuracy & Features",
    page_sidebar(
      sidebar = sidebar(
        selectInput("exp_acc", "Experiment:", choices = 1:4),
        selectInput(
          "feature_x",
          "Feature:",
          choices = c("mean_dilation", "peak_dilation")
        )
      ),
      card(
        card_header("Accuracy"),
        plotOutput("acc_plot")
      ),
      card(
        card_header("Feature vs Accuracy"),
        plotOutput("feature_plot")
      )
    )
  ),
  
  # ---- TAB 5: Prediction Model ----
  nav_panel(
    "Prediction Model",
    page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "train_exps",
          "Train on experiments:",
          choices = 1:4,
          selected = 1:4
        ),
        checkboxInput("include_condition", "Include condition", TRUE),
        actionButton("fit_model", "Fit model")
      ),
      card(
        card_header("Model summary"),
        verbatimTextOutput("model_summary")
      ),
      card(
        card_header("Coefficients"),
        plotOutput("coef_plot")
      ),
      card(
        card_header("Predicted vs Observed"),
        plotOutput("pred_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$exp_text <- renderUI({
    paste("Experiment", input$exp, "description goes here.")
  })
  
  output$exp_plot <- renderPlot({
    plot(1:10, 1:10, main = paste("Experiment", input$exp))
  })
  
  output$pupil_plot <- renderPlot({
    plot(1:25, rnorm(25), type = "l",
         main = paste("Pupil dynamics – Exp", input$exp_pupil))
  })
  
  output$acc_plot <- renderPlot({
    plot(1:10, runif(10), main = paste("Accuracy – Exp", input$exp_acc))
  })
  
  output$feature_plot <- renderPlot({
    plot(1:10, 1:10, main = paste("Feature:", input$feature_x))
  })
  
  output$model_summary <- renderPrint({
    cat("Model will be fit when button is clicked.")
  })
  
  output$coef_plot <- renderPlot({
    barplot(c(0.2, 0.1, 0.05),
            names.arg = c("mean", "peak", "condition"))
  })
  
  output$pred_plot <- renderPlot({
    plot(runif(20), runif(20),
         xlab = "Observed", ylab = "Predicted")
    abline(0, 1, lty = 2)
  })
}

shinyApp(ui, server)
