# this shiny app shows the results from the meta analysis 
# in any PSA, Age, DRE, contemporary cohort combination.

here::here()

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

load(file = "data.RData")

# Define UI for app ----
ui <- fluidPage(
  # App title 
  # titlePanel("Value of a statistical model based on four kallikrein markers in blood, commercially available as 4Kscore, in all reasonable prostate biopsy subgroups"),
  # h3("Supplemental Material"),
  # p("Words, blah, blah, select any PSA range you'd like, blah, blah"),
  
  # ui: select functions ----
  selectInput("psa.range", h3("PSA Range"),
              choices = psa.range.list, selected = psa.range.list[[1]]),
  selectInput("age.range", h3("Age Range"),
              choices = age.range.list, selected = age.range.list[[1]]),
  selectInput("dre.set", h3("DRE Status"),
              choices = dre.set.list, selected = dre.set.list[[1]]),
  selectInput("contemp.set", h3("Contemporary Cohort"),
              choices = contemp.set.list, selected = contemp.set.list[[1]]),
  
  # ui: printing ui results ----
  # printing note if no analyses were performed in combination selected
  textOutput("N"),

  h3("Cohort Descriptions"),
  tableOutput("cohort.n"),

  h3("Base Model AUC Meta-analysis"),
  plotlyOutput("plotly.base.meta"),
  
  h3("Kallikrein Model AUC Meta-analysis"),
  plotlyOutput("plotly.klk.meta"),
  
  h3("Improvement of Kallikrein Model over Base Meta-analysis"),
  plotlyOutput("plotly.delta.meta"),

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



# server: Define server logic ----
server <- function(input, output) {
  
  # server: cohort descriptions ----
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
  colnames = T,
  digits = 0)
  
  # server: tableone output ----
  output$table.one <- renderTable({
    data$table.one %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary >= input$contemp.set) %>%
      select(varname, value)
  },
  na = "",
  colnames = F)
  
  # server: tableone output by cohort ----
  output$table.one.cohort <- renderTable({
    data$table.one.cohort %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary >= input$contemp.set) %>%
      select(cohort.disp, varname, value)
  },
  na = "",
  colnames = F)  
  
  # server: note for no analyses ----
  # this will display a message if there are no results 
  output$N = renderText({ 
    N = nrow(
      data$meta.auc %>%
        filter(model == "Base Model") %>%
        filter(psa.range == input$psa.range & age.range == input$age.range & 
                 dre.set == input$dre.set & contemporary == input$contemp.set)
    )
    
    ifelse(N>0, " ", "Too few data points for meta analysis. Cohorts with fewer than 20 high-grade cancers or fewer than 20 patients without high-grade cancer were excluded.")
  })
  
  # server: base model meta ----
  output$basemodel.meta = renderPlot({
    data$meta.auc %>%
      filter(model == "Base Model") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      ggplot(aes(x = factor(cohort.est, rev(unique(.$cohort.est))),
                 y=esn, ymin=lbn, ymax=ubn)) +
      geom_pointrange() + 
      geom_hline(yintercept=0.5, lty=2) +  # add a dotted line at x=1 after flip
      geom_vline(xintercept = 0.5) +
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab(" ") + ylab("AUC (95% CI)") +
      theme_bw() +  # use a white background  
      theme(axis.text=element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank())
    })
    
  # server: klk model meta ----
  output$klkmodel.meta = renderPlot({
    data$meta.auc %>%
      filter(model == "Kallikrein Model") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      ggplot(aes(x = factor(cohort.est, rev(unique(.$cohort.est))),
                 y=esn, ymin=lbn, ymax=ubn)) +
      geom_pointrange() + 
      geom_hline(yintercept=0.5, lty=2) +  # add a dotted line at x=1 after flip
      geom_vline(xintercept = 0.5) +
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab(" ") + ylab("AUC (95% CI)") +
      theme_bw() +  # use a white background  
      theme(axis.text=element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank())
    })
  
  # server: klk improvement over base meta ----
  output$klkdelta.meta = renderPlot({
    data$meta.auc %>%
      filter(model == "Difference between Base and Ka") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      ggplot(aes(x = factor(cohort.est, rev(unique(.$cohort.est))),
                 y=esn, ymin=lbn, ymax=ubn)) +
      geom_pointrange() + 
      geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
      geom_vline(xintercept = 0.5) +
      coord_flip() +  # flip coordinates (puts labels on y axis)
      xlab(" ") + ylab("Delta AUC (95% CI)") +
      theme_bw() +  # use a white background  
      theme(axis.text=element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank()) 
    })
  
  # server: PLOTLY base model meta ----
  output$plotly.base.meta <- renderPlotly({
    d = data$meta.auc %>%
      filter(model == "Base Model") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      mutate(es = str_replace(string = es, pattern = fixed(" ("), replacement = fixed(" (95% CI "))) %>%
      ggplot(aes(y = factor(cohort, rev(unique(.$cohort))),
                 x=esn, xmin=lbn, xmax=ubn, text = es)) +
      geom_vline(xintercept = 0.5, linetype=2, alpha=0.75) +
      geom_errorbarh(color="black", height = 0) +
      geom_point() +
      ylab(" ") + xlab("AUC (95% CI)") +
      theme_bw() +  # use a white background  
      theme(axis.text=element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank()) 
    ggplotly(d, tooltip="text") 
  })

  # server: PLOTLY klk model meta ----
  output$plotly.klk.meta <- renderPlotly({
    d = data$meta.auc %>%
      filter(model == "Kallikrein Model") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      mutate(es = str_replace(string = es, pattern = fixed(" ("), replacement = fixed(" (95% CI "))) %>%
      ggplot(aes(y = factor(cohort, rev(unique(.$cohort))),
                 x=esn, xmin=lbn, xmax=ubn, text = es)) +
      geom_vline(xintercept = 0.5, linetype=2, alpha=0.75) +
      geom_errorbarh(color="black", height = 0) +
      geom_point() +
      ylab(" ") + xlab("AUC (95% CI)") +
      theme_bw() +  # use a white background  
      theme(axis.text=element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank()) 
    ggplotly(d, tooltip="text") 
  })

  # server: PLOTLY klk improvement over base meta ----
  output$plotly.delta.meta <- renderPlotly({
    d = data$meta.auc %>%
      filter(model == "Difference between Base and Ka") %>%
      filter(psa.range == input$psa.range & age.range == input$age.range & 
               dre.set == input$dre.set & contemporary == input$contemp.set) %>%
      mutate(es = str_replace(string = es, pattern = fixed(" ("), replacement = fixed(" (95% CI "))) %>%
      ggplot(aes(y = factor(cohort, rev(unique(.$cohort))),
                 x=esn, xmin=lbn, xmax=ubn, text = es)) +
      geom_vline(xintercept = 0, linetype=2, alpha=0.75) +
      geom_errorbarh(color="black", height = 0) +
      geom_point() +
      ylab(" ") + xlab("Delta AUC (95% CI)") +
      theme_bw() +  # use a white background  
      theme(axis.text=element_text(size=12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks.y = element_blank()) 
    ggplotly(d, tooltip="text") 
  })
}



shinyApp(ui = ui, server = server)