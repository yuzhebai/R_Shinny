shinyUI(bootstrapPage(

    sidebarPanel(
        sliderInput(inputId = "w",
                    label = "Accuracy:",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        
        sliderInput(inputId = "x",
                    label = "Sentivity",
                    min = 0, max = 1, value = 0.1, step = 0.01),

        sliderInput(inputId = "y",
                    label = "Area under the curve",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        
        sliderInput(inputId = "z",
                    label = "Specificity",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        
        actionButton("refreshButton", "Refresh")
        ),
    mainPanel(
        plotOutput(outputId = "main_plot", height = "800px", width = "800px")
        )
    ))

