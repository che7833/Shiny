##################################################
## Project: My Shiny App to get live AE counts
## Script purpose: Creates a Shiny app to get live AE counts
## Author: Chetan Chavan
##################################################


# app.R
# install.packages("DT")
library(shiny)
library(dplyr)
library(DT)

#Sample data
 
set.seed(123)

subjects <- sprintf("STDY-%03d", 1:40)

soc_map <- list(
  "Gastrointestinal disorders" = c("Nausea", "Vomiting", "Diarrhoea", "Abdominal pain"),
  "Nervous system disorders"   = c("Headache", "Dizziness", "Somnolence"),
  "Skin and subcutaneous tissue disorders" = c("Rash", "Pruritus", "Urticaria"),
  "General disorders and administration site conditions" = c("Fatigue", "Pyrexia", "Chills"),
  "Infections and infestations" = c("Nasopharyngitis", "Upper respiratory tract infection")
)

# Create  records 
n_ae <- 180
soc <- sample(names(soc_map), n_ae, replace = TRUE)
aeterm <- mapply(function(x) sample(soc_map[[x]], 1), soc)
aeterm <- as.character(aeterm)


aedecod <- toupper(gsub("[^A-Za-z0-9 ]", "", aeterm))

aeser <- sample(c("Y","N"), n_ae, replace = TRUE, prob = c(0.15, 0.85))
aesev <- sample(c("MILD","MODERATE","SEVERE"), n_ae, replace = TRUE, prob = c(0.55, 0.35, 0.10))

ae <- tibble(
  USUBJID = sample(subjects, n_ae, replace = TRUE),
  AESOC   = soc,
  AESER   = aeser,
  AESEV   = aesev,
  AETERM  = aeterm,
  AEDECOD = aedecod
) %>%
  arrange(USUBJID, AESOC)

# -----------------------------
# 2) Shiny app
# -----------------------------
ui <- fluidPage(
  titlePanel("(Test App by Chetan) Subjects with >=1 AE by SOC)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sev",
        label   = "Select severity (AESEV):",
        choices = c("All", sort(unique(ae$AESEV))),
        selected = "All"
      ),
      checkboxInput("ser_only", "Serious AEs only (AESER = 'Y')", value = FALSE),
      hr(),
      helpText("Metric: distinct USUBJID with ≥1 AE in each AESOC under current filters.")
    ),
    mainPanel(
      DTOutput("soc_table"),
      hr(),
      DTOutput("ae_table")
    )
  )
)

server <- function(input, output, session) {
  
  ae_filt <- reactive({
    df <- ae
    if (input$sev != "All") df <- df %>% filter(AESEV == input$sev)
    if (isTRUE(input$ser_only)) df <- df %>% filter(AESER == "Y")
    df
  })
  
  soc_counts <- reactive({
    ae_filt() %>%
      group_by(AESOC) %>%
      summarise(
        N_SUBJ = n_distinct(USUBJID),
        N_AE   = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(N_SUBJ), AESOC)
  })
  
  output$soc_table <- renderDT({
    datatable(
      soc_counts(),
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
  
  output$ae_table <- renderDT({
    datatable(
      ae_filt(),
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
}

shinyApp(ui, server)
