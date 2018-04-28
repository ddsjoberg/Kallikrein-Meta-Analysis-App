# this shiny app shows the results from the meta analysis 
# in any PSA, Age, DRE, contemporary cohort combination.
library(shiny)
library(dplyr)
library(ggplot2)

names = c("cohort.n",
          "table.one",
          "table.one.cohort",
          "meta.auc")

tempfile = c(tempfile(fileext = ".csv"),
             tempfile(fileext = ".csv"),
             tempfile(fileext = ".csv"),
             tempfile(fileext = ".csv"))
data.url = c("https://docs.google.com/spreadsheets/d/e/2PACX-1vSdISUp196c6nDgqytME57eCnorvOTfllBXPm2f5-xWZQotgYVhJun_tswo8mQHI61ytCYZtleKpMRP/pub?gid=0&single=true&output=csv",
             "https://docs.google.com/spreadsheets/d/e/2PACX-1vTfsNqWkNseFQQJWtvDdCXrigC2rNyZO7iNtvY6sz1ufYn-gI4itFR2IKxGD_DnmdDjfTwychsrG7i2/pub?gid=0&single=true&output=csv",
             "https://docs.google.com/spreadsheets/d/e/2PACX-1vQBSQVi4hDKbtkSnpzmtjxWVX4MIuPhmE5BWzDitB6IfNXk8gtLifSbXAAfpp5dEhm-vJQdiMdfEz8M/pub?gid=1144284133&single=true&output=csv",
             "https://docs.google.com/spreadsheets/d/e/2PACX-1vT4cMArJZVRFuTjacsdDGqCRjKa9Pi-FYktWaOO11oNrK5oBSS3oJRAIJV0qczgFrFlKbF0LFVdoHsx/pub?gid=0&single=true&output=csv")

# downloading temp csv files of data from Google Sheets (published as CSV files)
purrr::map2(data.url, tempfile, ~ download.file(.x, .y))

# loading downloaded files
data = purrr::map(tempfile, readr::read_csv)
names(data) = names

# creating a list of posstible outcomes and naming them after their values
filter.names = c("psa.range",
                 "age.range",
                 "dre.set",
                 "contemp.set")

filter.values = purrr::map(filter.names, ~ data$cohort.n %>% 
             select_(.x) %>%
             distinct)

filter.values %>%
  purrr::map(~ .x[[1]] %>%
               purrr::map(., ~ as.character(list(.x))))

psa.range.list = filter.values[[1]][[1]] %>%
  purrr::map(., ~ as.character(list(.x)))
names(psa.range.list) = filter.values[[1]][[1]]

age.range.list = filter.values[[2]][[1]] %>%
  purrr::map(., ~ as.character(list(.x)))
names(age.range.list) = filter.values[[2]][[1]]

dre.set.list = filter.values[[3]][[1]] %>%
  purrr::map(., ~ as.character(list(.x)))
names(dre.set.list) = filter.values[[3]][[1]]

contemp.set.list = list()
contemp.set.list$'All Cohorts' = 0
contemp.set.list$'Contemporary Cohorts Only' = 1

# Define UI for app 
ui <- fluidPage(
  # App title ----
  # titlePanel("Value of a statistical model based on four kallikrein markers in blood, commercially available as 4Kscore, in all reasonable prostate biopsy subgroups"),
  # h3("Supplemental Material"),
  # p("Words, blah, blah, select any PSA range you'd like, blah, blah"),
  
  selectInput("psa.range", h3("PSA Range"),
              choices = psa.range.list, selected = psa.range.list[[1]]),
  selectInput("age.range", h3("Age Range"),
              choices = age.range.list, selected = age.range.list[[1]]),
  selectInput("dre.set", h3("DRE Status"),
              choices = dre.set.list, selected = dre.set.list[[1]]),
  selectInput("contemp.set", h3("Contemporary Cohort"),
              choices = contemp.set.list, selected = contemp.set.list[[1]]),
  
  h3("Cohort Descriptions"),
  tableOutput("cohort.n"),

  h3("Base Model AUC Meta-analysis"),
  plotOutput("basemodel.meta"),

  h3("Kallikrein Model AUC Meta-analysis"),
  plotOutput("klkmodel.meta"),
  
  h3("Improvement of Kallikrein Model over Base Meta-analysis"),
  plotOutput("klkdelta.meta"),
  
  h3("Patient Characteristics"),
  tableOutput("table.one"),

  h3("Patient Characteristics, by cohort"),
  tableOutput("table.one.cohort")
)



# Define server logic 
server <- function(input, output) {
  
  # cohort descriptions
  output$cohort.n <- renderTable({
    data$cohort.n %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary >= input$contemp.set) %>%
    mutate(pctinclude.disp = sprintf("%1.0f%%", pctinclude)) %>%
    select(cohort.full, cohort, ntotal, ninclude, pctinclude.disp) %>%
      rename('Cohort' = cohort.full,
             'Cohort Abbreviation' = cohort,
             'Total N' = ntotal,
             'N in Subset' = ninclude,
             'Percent Included' = pctinclude.disp)
  },
  na = "",
  colnames = T)
  
  # tableone output
  output$table.one <- renderTable({
    data$table.one %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary >= input$contemp.set) %>%
      select(varname, value)
  },
  na = "",
  colnames = F)
  
  # tableone output by cohort
  output$table.one.cohort <- renderTable({
    data$table.one.cohort %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary >= input$contemp.set) %>%
      select(cohort.disp, varname, value)
  },
  na = "",
  colnames = F)  
  
  # base model meta
  output$basemodel.meta = renderPlot({
    data$meta.auc %>%
      filter(model == "Base Model") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      ggplot(aes(x = factor(cohort, rev(unique(.$cohort))),
                 y=esn, ymin=lbn, ymax=ubn)) +
      geom_pointrange() + 
      geom_hline(yintercept=0.5, lty=2) +  # add a dotted line at x=1 after flip
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab(" ") + ylab("AUC (95% CI)") +
      theme_bw()  # use a white background  
    })
    
  # klk model meta
  output$klkmodel.meta = renderPlot({
    data$meta.auc %>%
      filter(model == "Kallikrein Model") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      ggplot(aes(x = factor(cohort, rev(unique(.$cohort))),
                 y=esn, ymin=lbn, ymax=ubn)) +
      geom_pointrange() + 
      geom_hline(yintercept=0.5, lty=2) +  # add a dotted line at x=1 after flip
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab(" ") + ylab("AUC (95% CI)") +
      theme_bw()  # use a white background  
  })
  
  # klk improvement over base meta
  output$klkdelta.meta = renderPlot({
    data$meta.auc %>%
      filter(model == "Difference between Base and Ka") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      ggplot(aes(x = factor(cohort, rev(unique(.$cohort))),
                 y=esn, ymin=lbn, ymax=ubn)) +
      geom_pointrange() + 
      geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab(" ") + ylab("AUC (95% CI)") +
      theme_bw()  # use a white background  
  })
}



shinyApp(ui = ui, server = server)