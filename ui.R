library(shiny)

# Define UI for the application
shinyUI(fluidPage(
  
    # Error messages in red
    tags$head(
        tags$style(HTML("
                        .shiny-output-error-validation {
                        color: red;
                        }
                        "))
        ),    

    # Application title
    titlePanel("Confidence Intervals and Test for Two Proportions"),
    
    # Top panel with output: a plot and text
    plotOutput('plot'),  
    h3(textOutput("p_value")),
    p("(probability of the observed results under the hypothesis that the proportions are the same)"),
    p("Significance Codes: 0 = *** 0.001 = ** 0.01 = * 0.05 = ."),
      
    hr(),  
  
    # Bottom panel with two columns with inputs
    fluidRow(
        column(6,
             h3("Survey # 1", style = "color:blue"),
             numericInput('n1', 'Sample Size (>= 40)', 100, min = 40, step = 10),
             br(),
             sliderInput('p1', 'Proportion', 
                         min = 0.01, max = 0.99, value = 0.4, step = 0.01)
        ),
    
        column(6,
            h3("Survey # 2",style = "color:red"),
            numericInput('n2','Sample size (>= 40)', 100, min = 40, step = 10),
            br(),
            sliderInput('p2', 'Proportion',
                        min = 0.01, max = 0.99, value = 0.5, step = 0.01)
        )
    ),
    p("Values of proportions are bounded; set out off range, they are adjusted automatically."),    
    hr()
))

