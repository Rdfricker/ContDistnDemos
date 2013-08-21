library(shiny)

distns <- list("Uniform"="uniform",
               "Normal"="normal",
               "Gamma"="gamma",
               "Chi-square"="chisq",
               "Exponential"="exponential",
               "Beta"="beta")
               
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Continuous Distributions"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
  
  radioButtons("typeDist",label="",choices=distns),
    conditionalPanel(condition = "input.typeDist == 'uniform'",
        br(),
        numericInput("theta1", "Theta 1:", min = -9, max = 10, value = 0, step=0.1),
        numericInput("theta2", "Theta 2:", min = 1, max = 20, value = 1, step=0.1),
        br(),
        numericInput("min_y_unif", "Minimum y:", min = -10, max = 10, value = -1, step=0.1),         
        numericInput("max_y_unif", "Maximum y:", min = 2, max = 20, value = 2, step=0.1)),
        
    conditionalPanel(condition = "input.typeDist == 'normal'",
        br(),
        numericInput("mean", "Mean (mu):", min = -10, max = 10, value = 0, step=0.1),
        numericInput("sd", "Standard deviation (sigma):", min = 0.01, max = 10, value = 1, step=0.1),
        br(),
        numericInput("min_y_normal", "Minimum y:", min = -10, max = 0, value = -4),         
        numericInput("max_y_normal", "Maximum y:", min = 0, max = 10, value = 4)),

    conditionalPanel(condition = "input.typeDist == 'gamma'",
        br(),
        numericInput("alpha_gamma", "Alpha:", min = 0.01, max = 10, value = 1, step=0.1),
        numericInput("beta_gamma", "Beta:", min = 0.01, max = 20, value = 1, step=0.1),
        br(),   
        numericInput("max_y_gamma", "Maximum y:", min = 1, max = 100, value = 10)),

    conditionalPanel(condition = "input.typeDist == 'chisq'",
        br(),
        numericInput("df", "Degrees of freedom (df)", min = 1, max = 50, value = 1, step=1),
        br(),       
        numericInput("max_y_chisq", "Maximum y:", min = 1, max = 100, value = 10)),

    conditionalPanel(condition = "input.typeDist == 'exponential'",
        br(),
        numericInput("beta_exp", "Beta:", min = 0.01, max = 10, value = 1, step=0.1),
        br(),       
        numericInput("max_y_exp", "Maximum y:", min = 1, max = 100, value = 10)),

    conditionalPanel(condition = "input.typeDist == 'beta'",
        br(),
        numericInput("alpha_beta", "Alpha:", min = 0.1, max = 20, value = 2, step=0.1),
        numericInput("beta_beta", "Beta:", min = 0.1, max = 20, value = 2, step=0.1)),
        
        br(),
	    checkboxInput("showMean", "Show mean", FALSE),
	    checkboxInput("show1SD", "Show one standard deviation to either side of the mean", FALSE),
	   	checkboxInput("show2SD", "Show two standard deviations to either side of the mean", FALSE)
    ),

  mainPanel(
    tabsetPanel(
      tabPanel("Probability Density Function", plotOutput("pdf_plot")), 
      tabPanel("Cumulative Distribution Function", plotOutput("cdf_plot"))
    )
  )
))

