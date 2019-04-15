library(shiny)

# Define UI for miles per gallon application
# shinyUI(pageWithSidebar(
ui <- pageWithSidebar(

  # Application title
  headerPanel("Stock Synthesis Selectivity (TEST PHASE)"),

  #Sidebar controls to select selectivity type and parameters
  sidebarPanel(
    selectInput("type", "Type:",
                choices = c("Logistic (1)",
                            "Double Normal (24)")),

    sliderInput("range", "Lengths:",
                min = 0, max = 100, value = c(0,50)),

    conditionalPanel(
       condition = "input.type == 'Logistic (1)'",
       sliderInput("par1", "Parameter 1:", 0,100,10,0.1),
       sliderInput("par2", "Parameter 2:", 0,100,1,0.1),
       numericInput("par2N","Parameter 2:",1)
    ),

#    uiOutput("DoubleNormPars")

    conditionalPanel(
      condition = "input.type == 'Double Normal (24)'",
      sliderInput("par.a", "PEAK:",0, 100, 25, 0.1),
      sliderInput("par.b", "TOP:", -5, 5, 0, 0.1),
      sliderInput("par.c", "ASC-WIDTH:", -5, 10, 3, 0.1),
      sliderInput("par.d", "DESC-WIDTH:", -5, 10, 3, 0.1),
      sliderInput("par.e", "INIT:", 0, 1, 0.1, 0.05),
      sliderInput("par.f", "FINAL:", 0, 1, 0.9, 0.05)#,
      # numericInput("par.eN", "LOGIT INIT", log(.1/.9)),
      # numericInput("par.fN", "LOGIT FINAL", log(.9/.1))
    )

  ),

  mainPanel(
    h3(textOutput("caption")),

    plotOutput("selPlot")
  )

)