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
library(gifski)
library(shinycssloaders)
library(nlme)
library(DT)


# ---- IMPORT DATA (runs once when app starts) ----
data <- import(file.path("data", "AGGFinal.sav"))

P1 <- import(file.path("data", "E1Pupil.sav"))
P2 <- import(file.path("data", "E2Pupil.sav"))
P3 <- import(file.path("data", "E3Pupil.sav"))
P4 <- import(file.path("data", "E4Pupil.sav"))

A1<- import(file.path("data", "ACC1.sav"))
A2<- import(file.path("data", "ACC2.sav"))
A3<- import(file.path("data", "ACC3.sav"))
A4<- import(file.path("data", "ACC4.sav"))
#final pupil data from E3 long format 
data<- import(file.path("data", "AGGFinal.sav"))

#Data Wrangling

df_long1 <- P1 %>%
  pivot_longer(
    cols = matches("^ebin\\d+p_mean$"),
    names_to = "time_bin",
    values_to = "pupil_mean"
  ) %>%
  mutate(
    # Extract bin number from column names like ebin1p_mean → 1
    time_bin = as.numeric(str_extract(time_bin, "\\d+")),
    Subject = factor(Subject),
    TrialType = factor(TrialType)
  )

df_long2 <- P2 %>%
  pivot_longer(
    cols = matches("^ebin\\d+p_mean$"),
    names_to = "time_bin",
    values_to = "pupil_mean"
  ) %>%
  mutate(
    # Extract bin number from column names like ebin1p_mean → 1
    time_bin = as.numeric(str_extract(time_bin, "\\d+")),
    Subject = factor(Subject),
    TrialType = factor(TrialType)
  )


df_long3 <- P3 %>%
  pivot_longer(
    cols = matches("^ebin\\d+p_mean$"),
    names_to = "time_bin",
    values_to = "pupil_mean"
  ) %>%
  mutate(
    # Extract bin number from column names like ebin1p_mean → 1
    time_bin = as.numeric(str_extract(time_bin, "\\d+")),
    Subject = factor(Subject),
    TrialType = factor(TrialType)
  )

df_long4 <- P4 %>%
  pivot_longer(
    cols = matches("^ebin\\d+p_mean$"),
    names_to = "time_bin",
    values_to = "pupil_mean"
  ) %>%
  mutate(
    # Extract bin number from column names like ebin1p_mean → 1
    time_bin = as.numeric(str_extract(time_bin, "\\d+")),
    Subject = factor(Subject),
    TrialType = factor(TrialType)
  )

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

#for making GIFs
make_gif_file <- function(anim_plot, outfile, fps = 8, duration = 6, width = 700, height = 420, res = 96) {
  gganimate::animate(
    anim_plot,
    fps = fps,
    duration = duration,
    width = width,
    height = height,
    res = res,
    renderer = gganimate::gifski_renderer(outfile)
  )
}

make_anim_plot <- function(df, title_text, subtitle_text,
                           color_map, linetype_map,
                           x_limits = c(200, 5000),
                           x_breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000)) {
  
  ggplot(df, aes(x = Bin, y = mean, color = TrialType, linetype = TrialType, group = TrialType)) +
    geom_point(size = 1.5) +
    geom_line(linewidth = 0.75) +
    geom_ribbon(
      aes(ymin = mean - se, ymax = mean + se, fill = TrialType),
      alpha = 0.3, colour = NA, show.legend = FALSE
    ) +
    labs(
      x = "Time (ms)",
      y = "Change in Pupil Diameter (mm)",
      color = "Condition",
      linetype = "Condition",
      title = title_text,
      subtitle = subtitle_text,
      caption = "Note: Highlighted bands represent one standard error of the mean"
    ) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = color_map) +
    scale_linetype_manual(values = linetype_map) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 10),
      title = element_text(size = 10),
      plot.margin = margin(10, 10, 28, 10),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.text.x = element_text(margin = margin(t = 4))
    ) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    transition_reveal(Bin)
}

# Define UI for application 
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  # ---- GLOBAL CSS (Appears ONCE) ----
  tags$head(
    tags$style(HTML("
      .anim-gif img {
        max-width: 100% !important;
        width: 100% !important;
        height: auto !important;
        display: block;
      }
      .anim-gif {
        overflow: hidden;
      }
    ,
    .page-title { font-weight: 700; letter-spacing: .2px; }
    .page-subtitle { opacity: .75; margin-top: -6px; }
    
    .feature-card { padding: 16px; }
    .feature-icon { font-size: 28px; opacity: .9; margin-bottom: 8px; }
    
    .card { border-radius: 16px; }
    .nav-pills .nav-link { border-radius: 999px; } /* pill tabs more “pill-y” */

"))
  ),
  
  
  navset_card_pill(
  
    # -----------------------
    # BACKGROUND / OVERVIEW TAB
    # -----------------------
    nav_panel(
      div("Background", style = "margin-right: 12px;"),
      
      # ---- Background  ----
      tags$div(
        class = "mb-3",
        tags$h2(icon("brain"), " Background", class = "page-title"),
        tags$p("Welcome to a Generation Effect Deep Dive!", class = "page-subtitle"),
        style = "margin: 10px 0 18px 0;"
      ),
      
      card(
        fill = FALSE,
        card_header("The concept"),
        tags$p("In which case would you be more likely to remember a biology lecture on photosynthesis, a brand-new concept for you? If you re-read your notes, or if you generated a pictorial diagram of the process from start to finish? The growing body of literature in learning and memory has uncovered various memory strategies to enhance learning and subsequent recall. 
               These strategies can be useful when trying to remember your grocery shopping list, a new route to the library, or new material learned in the classroom. Using the biology lecture example, according to the body of research on a memory strategy referred to as the “generation effect,” you would likely have better memory for the photosynthesis content if you drew your own diagram representing the information.
               Intuitively, the generation effect refers to the phenomenon whereby people have enhanced memory for information which is self-generated than for information which is passively read (McCurdy et al., 2020)."),
      ),
      
      br(),
      
      layout_columns(
        card(
          fill = FALSE,
          card_header("Mental Effort Background"),
          tags$p("Throughout the years many theories have been proposed to explain the generation effect, one of which is the “mental effort theory” which suggests that more mental effort is allocated to self-generated information. In the following experiment we investigate the legitimacy of this theory. For the purposes of the following data and experiment, we operationalize mental effort as attention, and we use pupillometry as an online measurement of attention allocation during learning."),
        ),
        card(
          fill = FALSE,
          card_header("Core questions"),
          tags$ul(
            tags$li("Does generation increase attention during encoding?"),
            tags$li("Is attention reflected in pupil dilation dynamics?"),
            tags$li("Does increased attention relate to better memory?")
          )
        ),
        card(
          fill = FALSE,
          card_header("How to use this app"),
          tags$ol(
            tags$li("Experiments 1–4: accuracy + pupil plots + takeaways"),
            tags$li("Pupil Animations: dynamic timecourses"),
            tags$li("Model: linear mixed-effects model with an autoregressive feature")
          )
        ),
        col_widths = c(sm = 12, md = 6, lg = 6)
      ),
      
      layout_columns(
        card(class="feature-card",
             tags$div(icon("eye"), class="feature-icon"),
             tags$h5("Attention", class="mb-1"),
             tags$p("Pupil dilation as a proxy for effort during encoding.", class="mb-0")
        ),
        card(class="feature-card",
             tags$div(icon("brain"), class="feature-icon"),
             tags$h5("Memory", class="mb-1"),
             tags$p("Accuracy differences across read vs generate conditions.", class="mb-0")
        ),
        card(class="feature-card",
             tags$div(icon("film"), class="feature-icon"),
             tags$h5("Dynamics", class="mb-1"),
             tags$p("Animated timecourses to show condition separation over time.", class="mb-0")
        ),
        #col_widths = c(sm = 12, md = 4, lg = 4)
      )
      
    ),
    
    
  # -----------------------
  # EXPERIMENT 1 TAB
  # -----------------------
  nav_panel( div("Experiment 1", style = "margin-right: 12px;"),
    page_sidebar(
      title = "Experiment 1",
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
        tags$li("When people self-generate information they allocate more attention to it."),
        tags$li("When people self-generate information they remember it better."),
        tags$li("These results are consistent with a mental effort explanation of the generation effect.")
        )
      ),   


    )
  ),
  # -----------------------
  # EXPERIMENT 2 TAB
  # -----------------------
nav_panel( div("Experiment 2", style = "margin-right: 12px;"),
  page_sidebar(
    title = "Experiment 2",
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
    tags$li("People pay more attention when generating words."),
    tags$li("This is not always accompanied by a boost in their memory performance.")
  )
),   
  )

),
  # -----------------------
  # EXPERIMENT 3 TAB
  # -----------------------
nav_panel( div("Experiment 3", style = "margin-right: 12px;"),
  page_sidebar(
    title = "Experiment 3",
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
      In the third experiment, participants both read and generated word pairs. We also varied the difficulty of the generated words, 
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
        tags$li("We did not find a significant difference between the amount of attention in the two generate groups."),
        tags$li("The easy generate words seemed the easiest to remember. This could have been because the words were 
                easier, so people could quickly generate them at the test, and not during learning.")
      )
    ),   
  )
  
),

# -----------------------
# EXPERIMENT 4 TAB
# -----------------------
nav_panel( div("Experiment 4", style = "margin-right: 12px;"),
  page_sidebar(
    title = "Experiment 4",
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
      In the fourth experiment, participants both read and generated word pairs. Importantly, we asked participants to generate words verbally (out loud) to check for correctness."
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
        tags$li("People pay more attention when verbally generating words."),
        tags$li("Verbally generated words are remembered better than words read out loud."),
        tags$li("People correctly generate the words they are supposed to.")
      )
    ),   
  )
  
),

# -----------------------
# Pupil Animation TAB
# -----------------------
nav_panel( div("Pupil Animation", style = "margin-right: 12px;"),
  page_sidebar(
    title = "Pupil Animations",
    sidebar = sidebar(
      checkboxGroupInput(
        "anim_show",
        "Show plots:",
        choices = c(
          "Experiment 1" = "e1",
          "Experiment 2" = "e2",
          "Experiment 3" = "e3",
          "Experiment 4" = "e4"
        ),
        selected = c("e1", "e2", "e3", "e4")
      )
    ),
    
    # Description card (always visible)
      card(card_header("Animated pupil plots"),
      p("These animations show pupil dilation over time across encoding, by condition."),
      p("Use the checkboxes in the sidebar to display one or more experiments together.")
    ),
    
    # Dynamic container for plot cards
    uiOutput("anim_cards")
  
)),

  # -----------------------
  # PREDICTION MODEL TAB
  # -----------------------
nav_panel(
  div("Prediction Model", style = "margin-right: 12px;"),
  page_sidebar(
    title = "Models",
    sidebar = sidebar(
      checkboxGroupInput(
        "model_exps",
        "Show models:",
        choices = c("Experiment 1" = "e1", "Experiment 2" = "e2",
                    "Experiment 3" = "e3", "Experiment 4" = "e4"),
        selected = "e1"
      ),
      actionButton("fit_models", "Fit / Refit selected", class = "btn-primary")
    ),
    uiOutput("model_summaries")
  )
)
))

# Define server logic required to display plots
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
        subtitle = "Pupil dilation in verbal read versus verbal generate learning conditions",
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


  
  #----------The GIFS----------
  
  output$anim_cards <- renderUI({
    req(input$anim_show)
    
    meta <- list(
      e1 = list(title = "Experiment 1 (Within Subjects)", file = "e1_anim.gif"),
      e2 = list(title = "Experiment 2 (Between Subjects)", file = "e2_anim.gif"),
      e3 = list(title = "Experiment 3 (Within Subjects)", file = "e3_anim.gif"),
      e4 = list(title = "Experiment 4 (Within Subjects, Verbal)", file = "e4_anim.gif")
    )
    
    make_card <- function(k, size_class = "anim-small") {
      card(
        card_header(meta[[k]]$title),
        div(
          class = paste("anim-gif", size_class),
          tags$img(
            src = meta[[k]]$file,
            style = "width: 100%; height: auto; display: block;"
          )
        )
      )
    }
    
    n <- length(input$anim_show)
    
    # first two checked go to top row
    top_keys <- input$anim_show[seq_len(min(2, n))]
    
    # remaining checked go to bottom (preserves order)
    bottom_keys <- if (n > 2) input$anim_show[3:n] else character(0)
    
    tagList(
      # ---- TOP: two SMALL cards side-by-side ----
      if (length(top_keys) > 0) {
        fluidRow(
          lapply(top_keys, function(k) column(6, make_card(k, "anim-small")))
        )
      },
      
      br(),
      
      # ---- BOTTOM: LARGE cards stacked vertically (full width) ----
      if (length(bottom_keys) > 0) {
        tagList(
          lapply(bottom_keys, function(k) tagList(
            make_card(k, "anim-large"),
            br()
          ))
        )
      }
    )
  })
  
  
  output$anim_e1 <- renderImage({
    req("e1" %in% input$anim_show)
    
    # If we've already built the GIF once, reuse it
    if (is.null(gif_cache$e1)) {
      
      plot_anim <- ggplot(
        data = E1_pupil,
        aes(x = Bin, y = mean, color = TrialType, linetype = TrialType, label = TrialType)
      ) +
        geom_point(size = 1.5) +
        geom_line(linewidth = 0.75) +
        geom_ribbon(
          aes(ymin = mean - se, ymax = mean + se, fill = TrialType),
          alpha = 0.3, colour = NA, show.legend = FALSE
        ) +
        labs(
          x = "Time (ms)",
          y = "Change in Pupil Diameter (mm)",
          color = "Condition",
          linetype = "Condition",
          title = "Generation Increases Attention",
          subtitle = "Pupil dilation in read versus generate learning conditions",
          caption = "Note: Highlighted bands represent one standard error of the mean"
        ) +
        scale_color_manual(
          values = c("Generate" = "#FF66FF", "Read" = "#0099FF")
        ) +
        scale_fill_manual(  # <- IMPORTANT so ribbon matches line
          values = c("Generate" = "#FF66FF", "Read" = "#0099FF")
        ) +
        scale_linetype_manual(
          values = c("Generate" = 6, "Read" = 1)
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "bold", hjust = 0.5),
          axis.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10),
          title = element_text(size = 10)
        ) +
        scale_x_continuous(
          limits = c(200, 5000),
          breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3000, 3400, 3800, 4200, 4600, 5000)
        ) +
        transition_reveal(Bin)
      
      gif_cache$e1 <- make_gif(plot_anim, fps = 10, duration = 10)
    }
    
    list(src = gif_cache$e1, contentType = "image/gif")
  }, deleteFile = FALSE)
  
  output$anim_e2 <- renderImage({
    req("e2" %in% input$anim_show)
    
    if (is.null(gif_cache$e2)) {
      plot_anim <- make_anim_plot(
        df = E2_pupil,
        title_text = "Generation Increases Attention",
        subtitle_text = "Pupil dilation in read versus generate learning conditions",
        color_map = c("Generate" = "#FF66FF", "Read" = "#0099FF"),
        linetype_map = c("Generate" = 6, "Read" = 1)
      )
      gif_cache$e2 <- make_gif(plot_anim, fps = 10, duration = 10)
    }
    
    list(src = gif_cache$e2, contentType = "image/gif")
  }, deleteFile = FALSE)
  
  
  output$anim_e3 <- renderImage({
    req("e3" %in% input$anim_show)
    
    if (is.null(gif_cache$e3)) {
      # IMPORTANT: these names MUST match E3_pupil$TrialType exactly
      plot_anim <- make_anim_plot(
        df = E3_pupil,
        title_text = "Generation Increases Attention",
        subtitle_text = "Pupil dilation by generation difficulty",
        color_map = c(
          "Generate Difficult" = "#FF66FF",
          "Generate Easy"      = "#0099FF",
          "Read"               = "#8E6BBE"
        ),
        linetype_map = c(
          "Generate Difficult" = 3,
          "Generate Easy"      = 1,
          "Read"               = 8
        )
      )
      gif_cache$e3 <- make_gif(plot_anim, fps = 10, duration = 10)
    }
    
    list(src = gif_cache$e3, contentType = "image/gif")
  }, deleteFile = FALSE)
  
  
  output$anim_e4 <- renderImage({
    req("e4" %in% input$anim_show)
    
    if (is.null(gif_cache$e4)) {
      plot_anim <- make_anim_plot(
        df = E4_pupil,
        title_text = "Generation Increases Attention",
        subtitle_text = "Pupil dilation in read versus generate learning conditions",
        color_map = c("Generate" = "#FF66FF", "Read" = "#0099FF"),
        linetype_map = c("Generate" = 6, "Read" = 1)
      )
      gif_cache$e4 <- make_gif(plot_anim, fps = 10, duration = 10)
    }
    
    list(src = gif_cache$e4, contentType = "image/gif")
  }, deleteFile = FALSE)

  #----------------The Models--------------------
  
  
  # store fitted models here
  models_rv <- reactiveValues(fits = NULL)
  
  # When the button is clicked, fit the models for the checked experiments
  observeEvent(input$fit_models, {
    req(input$model_exps)
    
    dfs <- list(
      e1 = df_long1,
      e2 = df_long2,
      e3 = df_long3,
      e4 = df_long4
    )
    
    fits <- list()
    
    for (k in input$model_exps) {
      df <- dfs[[k]]
      
      fits[[k]] <- tryCatch({
        nlme::lme(
          pupil_mean ~ TrialType * time_bin,
          random = ~ 1 | Subject,
          correlation = nlme::corAR1(form = ~ time_bin | Subject/TrialType),
          data = df,
          method = "REML",
          na.action = na.omit,
          control = nlme::lmeControl(opt = "optim")
        )
      }, error = function(e) e)
    }
    
    models_rv$fits <- fits
  })
  

  # This creates the CARDS in the right panel
  output$model_summaries <- renderUI({
    
    # ---- NEW: top description card ----
    description_card <- card(
      card_header(
        tagList(
          icon("chart-line"),
          " How to interpret these models"
        )
      ),
      
      p("These models account for:"),
      
      tags$ul(
        tags$li(tags$b("Individual differences:"), 
                " People differ from one another in baseline pupil size."),
        tags$li(tags$b("Temporal dependence:"), 
                " Pupil size at one moment depends on the moment just before it (AR(1) structure).")
      ),
      
      
        p(tags$b("How to interpret the predictors:")),
      
      tags$ul(
        
        tags$li(
          tags$b("Intercept → "),
          "The estimated baseline pupil size at the beginning of the trial for the reference condition."
        ),
        
        tags$li(
          tags$b("Time → "),
          "Indicates whether pupil size systematically changes over time (for example, ramping up or down during encoding)."
        ),
        
        tags$li(
          tags$b("Generate vs Read → "),
          "Tests whether generating an answer leads to overall larger pupil dilation than simply reading."
        ),
        
        tags$li(
          tags$b("Easy generation vs Read → "),
          "Compares pupil dilation when generation is easy versus when participants simply read the word."
        ),
        
        tags$li(
          tags$b("Difficult generation vs Read → "),
          "Compares pupil dilation when generation is difficult versus when participants read the word."
        ),
        
        tags$li(
          tags$b("Generate × Time (or condition × Time) → "),
          "Tests whether the change in pupil size over time differs between conditions, indicating different trajectories of attention."
        ),
        br(),
        p(tags$em(
          "In the forest plot below, each point shows the estimated effect size and the horizontal bars represent 95% confidence intervals. 
  Effects whose confidence intervals do not cross zero indicate stronger evidence for a reliable effect."
        ))
      )
    )
    
    # Before first fit: show description + instructions
    if (is.null(models_rv$fits)) {
      return(
        tagList(
          description_card,
          card(
            card_header("Model summaries"),
            p("Select experiments on the left and click “Fit / Refit selected”.")
          )
        )
      )
    }
    
    titles <- c(e1 = "Experiment 1", e2 = "Experiment 2",
                e3 = "Experiment 3", e4 = "Experiment 4")
    
    # After fit: show description + the model summary cards
    tagList(
      description_card,
      lapply(names(models_rv$fits), function(k) {
        out_id <- paste0("summary_", k)
        
        card(
          card_header(paste0(titles[[k]], " — model predictors")),
          plotOutput(out_id, height = "260px")
        )
      })
    )
  })
  
  # This actually prints each summary into its card
  observe({
    fits <- models_rv$fits
    if (is.null(fits)) return()
    
    for (k in names(fits)) {
      out_id <- paste0("summary_", k)
      
      local({
        kk <- k
        oid <- out_id
        
        output[[oid]] <- renderPlot({
          
          fit <- models_rv$fits[[kk]]
          
          # If model failed, show a simple error plot
          if (inherits(fit, "error")) {
            plot.new()
            text(0.5, 0.5, paste("Model failed:\n", conditionMessage(fit)), cex = 1)
            return(invisible())
          }
          
          # Fixed effects table from nlme
          tab <- as.data.frame(summary(fit)$tTable)
          tab$Term <- rownames(tab)
          rownames(tab) <- NULL
          
          # 95% CI
          tab$lower <- tab$Value - 1.96 * tab$Std.Error
          tab$upper <- tab$Value + 1.96 * tab$Std.Error
          
          tab$Term <- dplyr::recode(tab$Term,
                                    "(Intercept)" = "Intercept",
                                    
                                    "TrialTyper" = "Generate vs Read",
                                    
                                    "TrialTypege" = "Easy generation vs Read",
                                    "TrialTypegd" = "Difficult generation vs Read",
                                    
                                    "time_bin" = "Time",
                                    
                                    "TrialTypeg:time_bin" = "Generate × Time",
                                    "TrialTyper:time_bin" = "Reading × Time",
                                    "TrialTypege:time_bin" = "Easy generation × Time",
                                    "TrialTypegd:time_bin" = "Difficult generation × Time"
          )
          
          ggplot(tab, aes(x = Value, y = reorder(Term, Value))) +
            
            # reference line
            geom_vline(
              xintercept = 0,
              linetype = "dashed",
              linewidth = 0.8,
              color = "#888888"
            ) +
            
            # confidence intervals
            geom_errorbarh(
              aes(xmin = lower, xmax = upper),
              height = 0.18,
              linewidth = 1.1,
              color = "#4A90E2"
            ) +
            
            # points
            geom_point(
              aes(color = Value > 0),
              size = 3.6
            ) +
            
            scale_color_manual(
              values = c(
                "TRUE" = "#18BC9C",   # Minty green
                "FALSE" = "#E74C3C"   # soft red
              ),
              guide = "none"
            ) +
            
            labs(
              title = "Model predictors",
              subtitle = "Effect estimates with 95% confidence intervals",
              x = "Effect size (estimate)",
              y = NULL
            ) +
            
            theme_minimal(base_size = 13) +
            
            theme(
              plot.title = element_text(face = "bold", size = 15),
              plot.subtitle = element_text(size = 11, color = "#555555"),
              axis.text.y = element_text(size = 11),
              axis.text.x = element_text(size = 10),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = margin(10, 20, 10, 20)
            )
          
        })
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

