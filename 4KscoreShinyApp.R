# this shiny app shows the results from the meta analysis 
# in any PSA, Age, DRE, contemporary cohort combination.

here::here()

library(shiny)
library(dplyr)
library(ggplot2)

load(file = "data.RData")

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
}



shinyApp(ui = ui, server = server)