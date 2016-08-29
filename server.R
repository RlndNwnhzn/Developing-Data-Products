library(shiny)

# Define server logic for the application
shinyServer(
    function(input, output, session) {

        # inputs are stored in reactive objects                    
        # input$n1 and input$n2 has to be numeric and larger than 40        
        survey1_p <- reactive({input$p1})
        survey1_n <- reactive({
            validate(need(input$n1 >= 40, 
                          message = "Sample size has to be numeric >= 40!"))
            round(input$n1)
        })
        
        survey2_p <- reactive({input$p2})
        survey2_n <- reactive({
            validate(need(input$n2 >= 40, 
                          message = "Sample size has to be numeric >= 40!"))
            round(input$n2)
        })

        # Controls for invalid inputs
        observe({
            # input$p1 can't be to small otherwise the normal approximation is not valid
            if(survey1_n() * survey1_p() * (1 - survey1_p()) < 10 & survey1_p() <= 0.5 ) 
            {
                updateNumericInput(session, "p1", value = survey1_p() + 0.01)
            }
            # input$p1 can't be to large  otherwise the normal approximation is not valid
            if(survey1_n() * survey1_p() * (1 - survey1_p()) < 10 & survey1_p() > 0.5 ) 
            {
                updateNumericInput(session, "p1", value = survey1_p() - 0.01)
            }
            # same controls for input$n2 and input$p2            
            if(survey2_n() * survey2_p() * (1 - survey2_p()) < 10 & survey2_p() <= 0.5 ) 
            {
                updateNumericInput(session, "p2", value = survey2_p() + 0.01)
            }
            if(survey2_n() * survey2_p() * (1 - survey2_p()) < 10 & survey2_p() > 0.5 ) 
            {
                updateNumericInput(session, "p2", value = survey2_p() - 0.01)
            }
            # rounded values for the sample sizes
            updateNumericInput(session, "n1", value = survey1_n())
            updateNumericInput(session, "n2", value = survey2_n())            
        })
        
        output$plot <- renderPlot({
            # standard deviation for both proportions
            survey1_sd <- sqrt((survey1_p() * (1 - survey1_p())) / 
                                   survey1_n())
            survey2_sd <- sqrt((survey2_p() * (1 - survey2_p())) / 
                                   survey2_n())
            
            # Values for the x-axis
            x <- seq(0, 1, length = 1000)
            
            # x-Values for both surveys (p +/- 2 sd's)
            survey1_x <- seq(survey1_p() - 2 * survey1_sd, 
                             survey1_p() + 2 * survey1_sd, length = 1000)
            survey2_x <- seq(survey2_p() - 2 * survey2_sd, 
                             survey2_p() + 2 * survey2_sd, length = 1000)
            
            # y-Values for both surveys (normal distribution)
            survey1_y <- dnorm(survey1_x, survey1_p(), survey1_sd)
            survey2_y <- dnorm(survey2_x, survey2_p(), survey2_sd)
            
            # y-values from the origin to the maximum y-value (for the definition of the plot area)
            y_max <- ceiling(max(survey1_y, survey2_y))
            y <- y_max/(x[length(x)] - x[1]) * (x - x[1])
            
            # definition of the tick marks (a mark every 0.01)
            tick <- seq(0,1,0.01)
            # definition of the labels (a label every 0.1)            
            label <- tick
            label[(100 * label) %% 10 != 0] <- NA
                        
            # plot an empty graph based on the maximum y-value
            plot(x, y, type = "n", xlab = "Proportions found in the surveys",
                 ylab = "", main = "The 95% Confidence Intervals",
                 xaxt = "n", yaxt = "n",
                 cex.main=1.5, cex.lab=1.5)

            # draw the distribution graphs
            lines(survey1_x, survey1_y,
                  xlab = "", ylab = "", col = "blue", lwd=4)
            lines(survey2_x, survey2_y,
                  xlab = "", ylab = "", col = "red", lwd = 4)
            
            # vertical lines for the proportions
            segments(survey1_p(), 0, survey1_p(),
                     max(survey1_y), col = "blue", lwd = 3, lty = 2)
            segments(survey2_p(), 0, survey2_p(), 
                     max(survey2_y), col = "red", lwd = 3, lty = 2)
            
            # apply the tick marks and labels            
            axis(1, at = tick, label = label, cex.axis = 1.3)            
        })

        output$p_value <- renderText({
            # pooled proportion
            pooled_p <- ((survey1_p() * survey1_n()) + (survey2_p() * survey2_n())) /
                        (survey1_n() + survey2_n())
            
            # pooled variance
            pooled_var <- (pooled_p * (1 - pooled_p)) * 
                          ((1 / survey1_n()) + (1 / survey2_n()))
            
            # normal approximation based on both proportions and pooled variance
            z_value <- abs(survey1_p() - survey2_p()) / sqrt(pooled_var)
            
            # corresponding p-value 
            p_value <- 2 * (1 - pnorm(z_value)) 
            
            # add significance code and print
            signif <- symnum(p_value, 
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                             symbols = c("***", "**", "*", ".", " ")) 
            paste("P-value: ",
                  sprintf("%1.1f%%", 100 * p_value),
                  signif)
        })
    }
)
