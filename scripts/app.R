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
library(rio)
library(here)
library(tidyr)
library(dplyr)
library(tidyverse)
library(psych)
library(ggpubr)
library(ggplot2)
library(plyr)
library(ggrepel)
library(gganimate)
library(transformr)
library(jpeg)

# ---- IMPORT DATA (runs once when app starts) ----
data <- import(here::here("data", "AGGFinal.sav"))

P1 <- import(here::here("data", "E1Pupil.sav"))
P2 <- import(here::here("data", "E2Pupil.sav"))
P3 <- import(here::here("data", "E3Pupil.sav"))
P4 <- import(here::here("data", "E4Pupil.sav"))

A1<- import(here::here("data", "ACC1.sav"))
A2<- import(here::here("data", "ACC2.sav"))
A3<- import(here::here("data", "ACC3.sav"))
A4<- import(here::here("data", "ACC4.sav"))
#final pupil data from E3 long format 
data<- import(here::here("data", "AGGFinal.sav"))

#Data Wrangling



#writing a function to gather and organize pupil data for plotting

gathering_pupil <- function(df_pupil) {
  
  Pupil_Gather <- df_pupil %>%
    gather(key = "Bin", value = "Pupil", ebin1p_mean, ebin2p_mean, ebin3p_mean, ebin4p_mean, ebin5p_mean, ebin6p_mean, ebin7p_mean, ebin8p_mean, ebin9p_mean, ebin10p_mean, ebin11p_mean, ebin12p_mean, ebin13p_mean, ebin14p_mean, ebin15p_mean, ebin16p_mean, ebin17p_mean, ebin18p_mean, ebin19p_mean, ebin20p_mean, ebin21p_mean, ebin22p_mean, ebin23p_mean, ebin24p_mean, ebin25p_mean)
  
  
  exp1 <- Pupil_Gather %>%
    pivot_wider(names_from = Bin, values_from = Pupil)
  
  #Creating a bin by bin summary for plotting
  
  Pupil_Gather$Bin <- as.factor(Pupil_Gather$Bin)
  tg <- ddply(Pupil_Gather, c("TrialType", "Bin"), summarise,
              N=length(!is.na(Pupil)),
              mean=mean(Pupil),
              sd=sd(Pupil),
              se=sd/sqrt(N)) 
  
  tg$Bin <- ifelse(tg$Bin == "ebin1p_mean", 1,
                   ifelse(tg$Bin == "ebin2p_mean", 2, 
                          ifelse(tg$Bin == "ebin3p_mean", 3,
                                 ifelse(tg$Bin == "ebin4p_mean", 4,
                                        ifelse(tg$Bin == "ebin5p_mean", 5,
                                               ifelse(tg$Bin == "ebin6p_mean", 6,
                                                      ifelse(tg$Bin == "ebin7p_mean", 7,
                                                             ifelse(tg$Bin == "ebin8p_mean", 8,
                                                                    ifelse(tg$Bin == "ebin9p_mean", 9,
                                                                           ifelse(tg$Bin == "ebin10p_mean", 10,
                                                                                  ifelse(tg$Bin == "ebin11p_mean", 11,
                                                                                         ifelse(tg$Bin == "ebin12p_mean", 12,
                                                                                                ifelse(tg$Bin == "ebin13p_mean", 13,
                                                                                                       ifelse(tg$Bin == "ebin14p_mean", 14,
                                                                                                              ifelse(tg$Bin == "ebin15p_mean", 15,
                                                                                                                     ifelse(tg$Bin == "ebin16p_mean", 16,
                                                                                                                            ifelse(tg$Bin == "ebin17p_mean", 17,
                                                                                                                                   ifelse(tg$Bin == "ebin18p_mean", 18,
                                                                                                                                          ifelse(tg$Bin == "ebin19p_mean", 19,
                                                                                                                                                 ifelse(tg$Bin == "ebin20p_mean", 20,
                                                                                                                                                        ifelse(tg$Bin == "ebin21p_mean", 21,
                                                                                                                                                               ifelse(tg$Bin == "ebin22p_mean", 22,
                                                                                                                                                                      ifelse(tg$Bin == "ebin23p_mean", 23,
                                                                                                                                                                             ifelse(tg$Bin == "ebin24p_mean", 24,
                                                                                                                                                                                    ifelse(tg$Bin == "ebin25p_mean", 25, 999)))))))))))))))))))))))))
  
  tg$Bin = as.numeric(tg$Bin)
  
  tg$Bin <- tg$Bin*200
  
  tg$TrialType <- ifelse(tg$TrialType == "r", "Read",
                         ifelse(tg$TrialType == "g", "Generate",
                                ifelse(tg$TrialType == "ge", "Generate Easy",
                                       ifelse(tg$TrialType == "gd", "Generate Difficult",999))))
  
  return(tg)
  
}

#preparing the pupil
E1_pupil <- gathering_pupil(P1)
E2_pupil <- gathering_pupil(P2)
E3_pupil <- gathering_pupil(P3)
E4_pupil <- gathering_pupil(P4)

#Accuracy
detach(package:plyr)

#re-naming
A1 <- A1 %>% 
  rename(Condition = TrialType)

A2 <- A2 %>% 
  rename(Condition = TrialType)

A3 <- A3 %>% 
  rename(Condition = TrialType)

A1$Condition <- ifelse(A1$Condition == "r", "Read",
                       ifelse(A1$Condition == "g", "Generate",999))

A2$Condition <- ifelse(A2$Condition == "r", "Read",
                       ifelse(A2$Condition == "g", "Generate",999))

A3$Condition <- ifelse(A3$Condition == "r", "Read",
                       ifelse(A3$Condition == "ge", "Easy Generate",
                       ifelse(A3$Condition == "gd", "Difficult Generate", 999)))


A4 <- A4 %>%
  select(
    Subject,  
    RecallCued.ACC.VERB_meanGEN,
    RecallCued.ACC.VERB_meanREAD
  ) %>%
  pivot_longer(
    cols = c(
      RecallCued.ACC.VERB_meanGEN,
      RecallCued.ACC.VERB_meanREAD
    ),
    names_to = "Condition",
    values_to = "ACC_mean"
  ) %>%
  mutate(
    Condition = case_when(
      Condition == "RecallCued.ACC.VERB_meanGEN"  ~ "Generate",
      Condition == "RecallCued.ACC.VERB_meanREAD" ~ "Read"
    )
  )



library(data.table) 

#convert data frame to data table 
setDT(A1)
setDT(A2)
setDT(A3)
setDT(A4)


m1 <- tapply(A1$Acc_mean, A1$Condition, mean, na.rm = TRUE)
m2 <- tapply(A2$ACC_mean, A2$Condition, mean, na.rm = TRUE)
m3 <- tapply(A3$ACC_mean, A3$Condition, mean, na.rm = TRUE)
m4 <- tapply(A4$ACC_mean, A4$Condition, mean, na.rm = TRUE)

# Define UI for application that draws a histogram
ui <- navset_pill(
  
  # -----------------------
  # EXPERIMENT 1 TAB
  # -----------------------
  nav_panel(
    "Experiment 1",
    page_sidebar(
      sidebar = sidebar(
        radioButtons(
          "e1_plot_type",
          "View:",
          choices = c(
            "Accuracy" = "accuracy",
            "Pupil dynamics" = "pupil"
          ),
          selected = "accuracy"
        )
      ),
      card(
        card_header("Experiment 1: Description"),
        "40 University of Oregon undergraduates completed a paired associates (PA) learning task which required memorization of associated word pairs for a later test. 

An example of a word pair would be dark + room. In the task, there were two learning conditions, a read condition, which required participants to read the two words in the learning phase, and a generate condition, where participants had to mentally fill in missing vowels in the second word, for example dark + r_ _m. Throughout the experiment, pupil diameter was measured, and at the end of the task, participants were tested on their memory for the two words. "
        # or uiOutput("e1_text")
      ),

        conditionalPanel(
        condition = "input.e1_plot_type == 'accuracy'",
        card(
          card_header("Experiment 1: Memory Accuracy"),
        plotOutput("e1_acc_plot", height = 420)
      )
      ),
      conditionalPanel(
        condition = "input.e1_plot_type == 'pupil'",
      card(
        card_header("Experiment 1: Pupil Timecourse"),
        plotOutput("e1_pupil_plot", height = 480)
      )
    ),

      card(
        card_header("Experiment 1: Key takeaways"),
        tags$ul(
        tags$li("When people self-generate information they allocate more attention to it "),
        tags$li("when people self-generate information they remember it better"),
        tags$li("These results are consistent with a mental effort explanation of the generation effect. ")
        )
      ),   


    )
  ),
  # -----------------------
  # EXPERIMENT 2 TAB
  # -----------------------
nav_panel(
  "Experiment 2",
  page_sidebar(
    sidebar = sidebar(
      radioButtons(
        "e2_plot_type",
        "View:",
        choices = c(
          "Accuracy" = "accuracy",
          "Pupil dynamics" = "pupil"
        ),
        selected = "accuracy"
      )
    ),
    card(
      card_header("Experiment 2: Description"),
      "80 University of Oregon undergraduates completed a paired associates (PA) learning task. 
      In the second experiment, we separated the two learning conditions into two groups. 
      One group of 40 people only read word pairs, and the other group of people only generated word pairs."
# or uiOutput("e1_text")
    ),

conditionalPanel(
  condition = "input.e2_plot_type == 'accuracy'",
  card(
    card_header("Experiment 2: Memory Accuracy"),
    plotOutput("e2_acc_plot", height = 420)
  )
),
conditionalPanel(
  condition = "input.e2_plot_type == 'pupil'",
  card(
    card_header("Experiment 2: Pupil Timecourse"),
    plotOutput("e2_pupil_plot", height = 480)
  )
),

card(
  card_header("Experiment 2: Key takeaways"),
  tags$ul(
    tags$li("People pay more attention when generating words"),
    tags$li("This is not always accompanied by a boost in their memory performance")
  )
),   
  )

),
  # -----------------------
  # EXPERIMENT 3 TAB
  # -----------------------
nav_panel(
  "Experiment 3",
  page_sidebar(
    sidebar = sidebar(
      radioButtons(
        "e3_plot_type",
        "View:",
        choices = c(
          "Accuracy" = "accuracy",
          "Pupil dynamics" = "pupil"
        ),
        selected = "accuracy"
      )
    ),
    card(
      card_header("Experiment 3: Description"),
      "40 University of Oregon undergraduates completed a paired associates (PA) learning task. 
      In the third experiment, we varied the difficulty of the generated words, 
      to learn if words that were more difficult to generate required more effort. "
      # or uiOutput("e1_text")
    ),
    
    conditionalPanel(
      condition = "input.e3_plot_type == 'accuracy'",
      card(
        card_header("Experiment 3: Memory Accuracy"),
        plotOutput("e3_acc_plot", height = 420)
      )
    ),
    conditionalPanel(
      condition = "input.e3_plot_type == 'pupil'",
      card(
        card_header("Experiment 3: Pupil Timecourse"),
        plotOutput("e3_pupil_plot", height = 480)
      )
    ),
    
    card(
      card_header("Experiment 3: Key takeaways"),
      tags$ul(
        tags$li("We did not find a significant difference between the amount of attention in the two generate groups"),
        tags$li("The easy generate words seemed the easiest to remember. This could have been because the words were 
                easier, so people could quickly generate them at the test, and not during learning")
      )
    ),   
  )
  
),

# -----------------------
# EXPERIMENT 4 TAB
# -----------------------
nav_panel(
  "Experiment 4",
  page_sidebar(
    sidebar = sidebar(
      radioButtons(
        "e4_plot_type",
        "View:",
        choices = c(
          "Accuracy" = "accuracy",
          "Pupil dynamics" = "pupil"
        ),
        selected = "accuracy"
      )
    ),
    card(
      card_header("Experiment 4: Description"),
      "40 University of Oregon undergraduates completed a paired associates (PA) learning task. 
      In the third experiment, we varied the difficulty of the generated words, 
      to learn if words that were more difficult to generate required more effort. "
      # or uiOutput("e1_text")
    ),
    
    conditionalPanel(
      condition = "input.e4_plot_type == 'accuracy'",
      card(
        card_header("Experiment 4: Memory Accuracy"),
        plotOutput("e4_acc_plot", height = 420)
      )
    ),
    conditionalPanel(
      condition = "input.e4_plot_type == 'pupil'",
      card(
        card_header("Experiment 4: Pupil Timecourse"),
        plotOutput("e4_pupil_plot", height = 480)
      )
    ),
    
    card(
      card_header("Experiment 4: Key takeaways"),
      tags$ul(
        tags$li("We did not find a significant difference between the amount of attention in the two generate groups"),
        tags$li("The easy generate words seemed the easiest to remember. This could have been because the words were 
                easier, so people could quickly generate them at the test, and not during learning")
      )
    ),   
  )
  
),

  # -----------------------
  # PREDICTION MODEL TAB
  # -----------------------
  nav_panel(
    "Prediction Model",
    page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("train_exps", "Train on experiments:", choices = 1:4, selected = 1:4),
        checkboxInput("include_condition", "Include Read/Generate predictor", value = TRUE),
        actionButton("fit_model", "Fit / Refit model")
      ),
      card(
        card_header("Model Summary"),
        verbatimTextOutput("model_summary")
      ),
      card(
        card_header("Model Outputs"),
        plotOutput("coef_plot"),
        plotOutput("pred_plot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$e1_acc_plot <- renderPlot({
    
    req(input$e1_plot_type == "accuracy")
    
    ggplot(data = A1, aes(x = Condition, y = Acc_mean, color = Condition)) +
      geom_jitter(width = 0.25, alpha = 0.5, size = 3) +
      theme_bw() +
      labs(
        x = "Condition",
        y = "Mean Memory Accuracy (proportion correct)",
        color = "Condition",
        linetype = "Condition",
        title = "Mean Memory Accuracy (proportion correct)",
        subtitle = "Better memory for generated words"
      ) +
      scale_color_manual(
        name = "Condition",
        labels = c("Read", "Generate"),
        values = c("#FF66FF", "#0099FF")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Read", "Generate"),
        values = c(4, 1)
      ) +
      theme_minimal() +
      geom_segment(aes(x = 0.75, xend = 1.25, y = 0.5359079, yend = 0.5359079),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      geom_segment(aes(x = 1.75, xend = 2.25, y = 0.4898374, yend = 0.4898374),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      )
  })
  
  output$e2_acc_plot <- renderPlot({
    
    req(input$e2_plot_type == "accuracy")
    
    ggplot(data = A2, aes(x = Condition, y = ACC_mean, color = Condition)) +
      geom_jitter(width = 0.25, alpha = 0.5, size = 3) +
      theme_bw() +
      labs(
        x = "Condition",
        y = "Mean Memory Accuracy (proportion correct)",
        color = "Condition",
        linetype = "Condition",
        title = "Mean Memory Accuracy (proportion correct)",
        subtitle = "No difference in memory"
      ) +
      scale_color_manual(
        name = "Condition",
        labels = c("Read", "Generate"),
        values = c("#FF66FF", "#0099FF")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Read", "Generate"),
        values = c(4, 1)
      ) +
      theme_minimal() +
      geom_segment(aes(x = 0.75, xend = 1.25, y = m2["Generate"], yend = m2["Generate"]),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      geom_segment(aes(x = 1.75, xend = 2.25, y = m2["Read"], yend = m2["Read"]),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      )
  })
  
  output$e3_acc_plot <- renderPlot({
    
    req(input$e3_plot_type == "accuracy")
    
    ggplot(data = A3, aes(x = Condition, y = ACC_mean, color = Condition)) +
      geom_jitter(width = 0.25, alpha = 0.5, size = 3) +
      theme_bw() +
      labs(
        x = "Condition",
        y = "Mean Memory Accuracy (proportion correct)",
        color = "Condition",
        linetype = "Condition",
        title = "Mean Memory Accuracy (proportion correct)",
        subtitle = "Easy Generation words remembered best"
      ) +
      scale_color_manual(
        name = "Condition",
        labels = c("Read", "Easy Generate", "Difficult Generate"),
        values = c("#FF66FF", "#0099FF", "#8E6BBE")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Read", "Easy Generate", "Difficult Generate"),
        values = c(4, 1)
      ) +
      theme_minimal() +
      
      geom_segment(aes(x = 1.75, xend = 2.25, y = m3["Easy Generate"], yend = m3["Easy Generate"]),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      geom_segment(aes(x = 0.75, xend = 1.25, y = m3["Difficult Generate"], yend = m3["Difficult Generate"]),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      geom_segment(aes(x = 2.75, xend = 3.25, y = m3["Read"], yend = m3["Read"]),
                                                                                         linetype = "dashed", linewidth = 0.75, color = "black") +
      
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      )
  })
  
  
  output$e4_acc_plot <- renderPlot({
    
    req(input$e4_plot_type == "accuracy")
    
    ggplot(data = A4, aes(x = Condition, y = ACC_mean, color = Condition)) +
      geom_jitter(width = 0.25, alpha = 0.5, size = 3) +
      theme_bw() +
      labs(
        x = "Condition",
        y = "Mean Memory Accuracy (proportion correct)",
        color = "Condition",
        linetype = "Condition",
        title = "Mean Memory Accuracy (proportion correct)",
        subtitle = "Verbally generated words remembered better"
      ) +
      scale_color_manual(
        name = "Condition",
        labels = c("Read", "Generate"),
        values = c("#FF66FF", "#0099FF")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Read", "Generate"),
        values = c(4, 1)
      ) +
      theme_minimal() +
      geom_segment(aes(x = 0.75, xend = 1.25, y = m4["Generate"], yend = m4["Generate"]),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      geom_segment(aes(x = 1.75, xend = 2.25, y = m4["Read"], yend = m4["Read"]),
                   linetype = "dashed", linewidth = 0.75, color = "black") +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      )
  })
  
  output$e1_pupil_plot <- renderPlot({
   
    req(input$e1_plot_type == "pupil")
    
    ggplot(data = E1_pupil,
           aes(x = Bin, y = mean, color = TrialType, linetype = TrialType, label = TrialType)) +
      geom_point(size = 1.5) +
      geom_line(aes(linetype = TrialType, color = TrialType), linewidth = 0.75) +
      
      geom_label_repel(
        data = subset(E1_pupil, Bin == max(Bin)),
        aes(label = TrialType, y = mean + 0.02),
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = "grey50",
        segment.size = 0.2
      ) +
      
      theme_bw() +
      
      geom_ribbon(
        aes(ymin = mean - se, ymax = mean + se, fill = TrialType),
        alpha = 0.3, colour = NA, show.legend = FALSE
      ) +
      
      labs(
        x = "Time (ms)",
        y = "Change in Pupil Diameter (mm)",
        color = "Condition",
        linetype = "Condition",
        title = "Generating Words in a PA Memory Task Increases Attention",
        subtitle = "Pupil dilation in read versus generate learning conditions",
        caption = "Note: Highlighted bands represent one standard error of the mean"
      ) +
      
      scale_color_manual(
        name = "Condition",
        labels = c("Generate", "Read"),
        values = c("#FF66FF", "#0099FF")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Generate", "Read"),
        values = c(6, 1)
      ) +
      
      theme_minimal() +
      theme(
        # IMPORTANT: legend.position cannot be "".
        # Use "none" to remove it or "right"/"bottom" to show it.
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      ) +
      
      scale_x_continuous(
        limits = c(200, 5000),
        breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000)
      )
  })
  
  output$e2_pupil_plot <- renderPlot({
    
    req(input$e2_plot_type == "pupil")
    
    ggplot(data = E2_pupil,
           aes(x = Bin, y = mean, color = TrialType, linetype = TrialType, label = TrialType)) +
      geom_point(size = 1.5) +
      geom_line(aes(linetype = TrialType, color = TrialType), linewidth = 0.75) +
      
      geom_label_repel(
        data = subset(E2_pupil, Bin == max(Bin)),
        aes(label = TrialType, y = mean + 0.02),
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = "grey50",
        segment.size = 0.2
      ) +
      
      theme_bw() +
      
      geom_ribbon(
        aes(ymin = mean - se, ymax = mean + se, fill = TrialType),
        alpha = 0.3, colour = NA, show.legend = FALSE
      ) +
      
      labs(
        x = "Time (ms)",
        y = "Change in Pupil Diameter (mm)",
        color = "Condition",
        linetype = "Condition",
        title = "Generating Words in a PA Memory Task Increases Attention",
        subtitle = "Pupil dilation in read versus generate learning conditions",
        caption = "Note: Highlighted bands represent one standard error of the mean"
      ) +
      
      scale_color_manual(
        name = "Condition",
        labels = c("Generate", "Read"),
        values = c("#FF66FF", "#0099FF")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Generate", "Read"),
        values = c(6, 1)
      ) +
      
      theme_minimal() +
      theme(
        # IMPORTANT: legend.position cannot be "".
        # Use "none" to remove it or "right"/"bottom" to show it.
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      ) +
      
      scale_x_continuous(
        limits = c(200, 5000),
        breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000)
      )
  })
  
  output$e3_pupil_plot <- renderPlot({
    
    req(input$e3_plot_type == "pupil")
    
    ggplot(data = E3_pupil,
           aes(x = Bin, y = mean, color = TrialType, linetype = TrialType, label = TrialType)) +
      geom_point(size = 1.5) +
      geom_line(aes(linetype = TrialType, color = TrialType), linewidth = 0.75) +
      
      geom_label_repel(
        data = subset(E3_pupil, Bin == max(Bin)),
        aes(label = TrialType, y = mean + 0.02),
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = "grey50",
        segment.size = 0.2
      ) +
      
      theme_bw() +
      
      geom_ribbon(
        aes(ymin = mean - se, ymax = mean + se, fill = TrialType),
        alpha = 0.3, colour = NA, show.legend = FALSE
      ) +
      
      labs(
        x = "Time (ms)",
        y = "Change in Pupil Diameter (mm)",
        color = "Condition",
        linetype = "Condition",
        title = "Generating Words in a PA Memory Task Increases Attention",
        subtitle = "Pupil dilation in read versus generate learning conditions",
        caption = "Note: Highlighted bands represent one standard error of the mean"
      ) +
      
      scale_color_manual(
        name = "Condition",
        labels = c("Generate Difficult", "Generate Easy", "Read"),
        values = c("#FF66FF", "#0099FF", "#8E6BBE")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Generate Difficult", "Generate Easy", "Read"),
        values = c(3,1,8)
      ) +
      scale_fill_manual(
        values = c(
          "Generate Difficult" = "#FF66FF",
          "Generate Easy"      = "#0099FF",
          "Read"               = "#8E6BBE"
        ))+
      theme_minimal() +
      theme(
        # IMPORTANT: legend.position cannot be "".
        # Use "none" to remove it or "right"/"bottom" to show it.
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      ) +
      
      scale_x_continuous(
        limits = c(200, 5000),
        breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000)
      )
  })
  
  output$e4_pupil_plot <- renderPlot({
    
    req(input$e4_plot_type == "pupil")
    
    ggplot(data = E4_pupil,
           aes(x = Bin, y = mean, color = TrialType, linetype = TrialType, label = TrialType)) +
      geom_point(size = 1.5) +
      geom_line(aes(linetype = TrialType, color = TrialType), linewidth = 0.75) +
      
      geom_label_repel(
        data = subset(E4_pupil, Bin == max(Bin)),
        aes(label = TrialType, y = mean + 0.02),
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = "grey50",
        segment.size = 0.2
      ) +
      
      theme_bw() +
      
      geom_ribbon(
        aes(ymin = mean - se, ymax = mean + se, fill = TrialType),
        alpha = 0.3, colour = NA, show.legend = FALSE
      ) +
      
      labs(
        x = "Time (ms)",
        y = "Change in Pupil Diameter (mm)",
        color = "Condition",
        linetype = "Condition",
        title = "Generating Words in a PA Memory Task Increases Attention",
        subtitle = "Pupil dilation in read versus generate learning conditions",
        caption = "Note: Highlighted bands represent one standard error of the mean"
      ) +
      
      scale_color_manual(
        name = "Condition",
        labels = c("Generate", "Read"),
        values = c("#FF66FF", "#0099FF")
      ) +
      scale_linetype_manual(
        name = "Condition",
        labels = c("Generate", "Read"),
        values = c(6, 1)
      ) +
      
      theme_minimal() +
      theme(
        # IMPORTANT: legend.position cannot be "".
        # Use "none" to remove it or "right"/"bottom" to show it.
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        title = element_text(size = 10)
      ) +
      
      scale_x_continuous(
        limits = c(200, 5000),
        breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000)
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

