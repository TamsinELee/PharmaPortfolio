# 25 September 2024
# An app to allow the user to ecompare the different inputs and outputs for two different 
# portfolio choices. The user can select projects to keep and drop. 

# Load necessary libraries for the app
library(shiny)          # Core Shiny library for building interactive web apps
library(shinydashboard) # Provides dashboard layout capabilities
library(dplyr)          # Data manipulation library
library(reshape2)       # For reshaping data between wide and long formats
library(ggplot2)        # Popular plotting library for creating visualizations
library(plotly)         # Interactive plotting library built on top of ggplot2
library(shinyWidgets)   # Enhances UI components for Shiny
library(gridExtra)      # Provides grid functions for arranging multiple plots

## app.R ##
# Load data for the dashboard from a CSV file
data               <- read.csv("caseStudy_forDashboard_Public.csv")
indicationVec      <- data$Indication  # Extract the 'Indication' column for use in checkboxes

# Display the first few rows and structure of the data
head(data)  # Preview the data
str(data)   # Show the structure and data types

# Calculate various summary statistics for the portfolio
probPortfolio    <- prod(data$PoL)  # Calculate the overall probability of launch for the portfolio
MCHF_total       <- sum(data$MCHF_total)  # Sum of monetary values in millions of CHF
MY_total         <- sum(data$MY_total)    # Sum of full-time equivalent years (FTE)
Value_total      <- sum(data$Value)       # Total value (e.g., peak sales revenue)
profit_total     <- sum(data$profit)      # Total profit
expProfit_total  <- sum(data$expected.profit)  # Total expected profit
efficacy_mean    <- mean(data$Efficacy)   # Average efficacy score across projects
safety_mean      <- mean(data$Safety)     # Average safety score across projects
feasibility_mean <- mean(data$Operational.Feasibility)  # Average operational feasibility
competition_mean <- mean(data$Level.of.Competition)     # Average level of competition

# Define the User Interface (UI) layout using fluidPage
ui <- fluidPage(
  # Main header
  h2("Analysing and comparing investment portfolios for a fictional pharmaceutical company."),
  # Apply custom CSS styles for margin around box elements
  dashboardBody(tags$head(tags$style(HTML('.box {margin: 5px;}'))),
                fixedRow( # Layout rows and columns in a fixed position
                  column(12,
                         wellPanel(
                           fixedRow(  # Layout inner rows for input selection and plots
                             column(4,  # First column for selecting options and displaying smaller plots
                                    column(6,  # Checkbox group for "Option 1" projects
                                           h5(checkboxGroupInput(inputId = "option1", 
                                                                 label   = "Option 1", 
                                                                 choices = indicationVec,
                                                                 selected =  c("Ph0. Cardio D" = "Ph0. Cardio D", 
                                                                               "Ph0. Thyroid C" = "Ph0. Thyroid C", 
                                                                               "Ph1. Cardio C" = "Ph1. Cardio C", 
                                                                               "Ph1. Psych B" = "Ph1. Psych B", 
                                                                               "Ph1. Psych A" = "Ph1. Psych A", 
                                                                               "Ph1. Thyroid B" = "Ph1. Thyroid B",
                                                                               "Ph2. Thyroid A" = "Ph2. Thyroid A",
                                                                               "Ph2. Vision A" = "Ph2. Vision A",
                                                                               "Ph2. Cardio A" = "Ph2. Cardio A")))
                                    ), 
                                    column(6,  # Checkbox group for "Option 2" projects
                                           h5(checkboxGroupInput(inputId = "option2", 
                                                                 label   = "Option 2", 
                                                                 choices = indicationVec,
                                                                 selected = c("Ph0. Cardio D" = "Ph0. Cardio D", 
                                                                              "Ph1. Cardio C" = "Ph1. Cardio C",  
                                                                              "Ph1. Psych A" = "Ph1. Psych A",
                                                                              "Ph2. Thyroid A" = "Ph2. Thyroid A",
                                                                              "Ph2. Vision A" = "Ph2. Vision A",
                                                                              "Ph2. Cardio A" = "Ph2. Cardio A"))),
                                    ),
                                    # Add headings and plots for Probability of Launch, freed-up costs, and freed-up FTE
                                    h4("Probability of launch (PoL)"),
                                    plotOutput("plotPoL", height = 200),
                                    h4('Freed up costs'),
                                    h5('Immediate (red) and overall (blue)'),
                                    plotOutput("plotFreedMoney", height = 200),
                                    h4('Freed up FTE'),
                                    h5('Immediate (red) and overall (blue)'),
                                    plotOutput("plotFreedFTE", height = 200)),
                             # column(4, 
                             #        h4('Correlating TB incidence/mortality with health finance indicators.'),
                             #        h5('Indicators that correlate with TB incidence/mortality are below, in order of strength.'),
                             #        h5('The lag is given on each plot (the number of years used previously that correlate with the incidence/mortality).')
                             # ), 
                             column(8, # Second column for larger plots showing inputs, outputs, and mean scores
                                    h4('Estimated input (MCHF)'),
                                    h4('Benchmark denoted by triangle'),
                                    plotOutput("plotInput"),
                                    h4('Estimated output (MCHF)'),
                                    plotOutput("plotOutput"),
                                    h4('Estimated average qualities from ED qualitative assessment'),
                                    plotOutput("plotMeans")
                             )# close fixed Row# close fixed Row
                           ), # close wellPanel
                           #wellPanel(
                           #  fixedRow(   
                           #         column(6, 
                           #                h4('Indicators that can be used to forecast are below, IN ORDER OF ACCURACY (the best indicator is first).'),
                           #                h4('The lag is given on each plot (the number of years used previously for predicting/forecasting).'),
                           #                h5('Black dots are the data.'), 
                           #                h5('Green is the prediction/forecast.'),
                           #                h5('The two different forecasts are when the indicator in increasing/decreasing by 1% each year since 2020'),
                           #)
                           #), #close fixedRow
                           #),  #close wellPanel
                         ), #close column
                         column(9,
                                #wellPanel(
                                #h4('The finance indicators from GHED (green line) that correlate with the incidence/mortality. 
                                #Plots are ordered such that the first plot has the strongest correlation to the incidence/mortality.'),
                                #       plotOutput("plotInd", width = "100%")
                                #, height = 1200),#close WellPanel)
                         )
                  ) #close column
                ) # close fixedRow
  ) # close dashboardBody
) # close fluidPage

# Define the server logic that creates plots and calculations based on user inputs
server <- function(input, output, session) {
  
  
  # Generate plots for the PoL of the two options
  output$plotPoL  <-  renderPlot({
    # Filter data based on which projects were not selected in Option 1 and Option 2
    data1        <- data %>% filter(Indication %in% input$option1) 
    data2        <- data %>% filter(Indication %in% input$option2) 
    DF           <- data.frame(cbind(100 * prod(data$PoL), 100 * prod(data1$PoL), 100 * prod(data2$PoL)))
    colnames(DF) <- c("All", "Option 1", "Option 2")
    # Reformat dataframe to plot
    DFtoPlot     <- melt(DF)
    fig1 <- ggplot(DFtoPlot, aes(y = value, x = variable)) +
      geom_col(fill = "#75ABDC", , colour = "black", alpha = 0.5) +
      scale_y_continuous(name = "%", labels = ~ format(.x, scientific = FALSE)) +
      scale_x_discrete(name = "") +
      theme_bw(base_size = 16) 
    plot <- fig1  # Assign plot object
    plot
  }, height = 200)    #, height = 200, width = 400
  
  # Generate plot for freed-up monetary resources (MCHF)
  output$plotFreedMoney  <-  renderPlot({
    # Filter data based on which projects were not selected in Option 1 and Option 2
    data1        <- data %>% filter(!Indication %in% input$option1) 
    data2        <- data %>% filter(!Indication %in% input$option2) 
    
    nowvec       <- c(0,0)  # Initialize vectors to hold immediate values
    totalvec     <- c(0,0)  # Initialize vectors to hold overall values
    for (k1 in 1:2){  # Loop over the two options to calculate freed-up costs
      if (k1 == 1) {
        DF = data1  # Use data for Option 1
      } else {
        DF = data2  # Use data for Option 2
      }
      # Calculate immediate and total costs for Phases 0-2
      now   <- sum(DF$MCHF_Ph0[which(DF$Phase == "Ph0")]) + 
        sum(DF$MCHF_Ph1[which(DF$Phase == "Ph1")]) + 
        sum(DF$MCHF_Ph2[which(DF$Phase == "Ph2")])
      
      total <- sum(DF$MCHF_total[which(DF$Phase == "Ph0")]) + 
        sum(DF$MCHF_total[which(DF$Phase == "Ph1")]) + 
        sum(DF$MCHF_total[which(DF$Phase == "Ph2")])
      nowvec[k1]   <- now  # Store immediate costs
      totalvec[k1] <- total  # Store total costs
    }
    
    # Create a data frame for plotting the freed-up monetary resources
    DFtoPlot <- data.frame(cbind(rep(c("Option 1", "Option 2"), 2), c(nowvec,totalvec), c(rep("immediate", 2), rep("overall", 2))))
    colnames(DFtoPlot) <- c("X1", "X2", "timescale")
    DFtoPlot$X2 <- as.numeric(as.character(DFtoPlot$X2))
    maxy <- 1.2*max(DFtoPlot$X2)  # Set the maximum y-axis value for scaling
    # Create a bar plot showing freed-up money (immediate and overall)
    fig1 <-ggplot(DFtoPlot, aes(x = X1, y = X2, fill = timescale)) +
      geom_col(position = "dodge", colour = "black") +
      geom_text(aes(label = X2),  size =5,
                vjust = -1, position = position_dodge(.9)) +
      scale_fill_brewer(palette = "Pastel1") + 
      scale_y_continuous(name = "MCHF", limits = c(0, maxy)) +
      scale_x_discrete(name = "") +
      theme_bw(base_size = 16) +
      theme(legend.position = "none")
    plot <- fig1  # Assign plot object
    plot
  }, height = 200)    #, height = 200, width = 400
  
  # Generate plot for freed-up full-time equivalent (FTE) resources
  output$plotFreedFTE  <-  renderPlot({
    # Filter data based on which projects were not selected in Option 1 and Option 2
    data1        <- data %>% filter(!Indication %in% input$option1)  # Data excluding Option 1 selections
    data2        <- data %>% filter(!Indication %in% input$option2)  # Data excluding Option 2 selections
    
    nowvec       <- c(0,0)  # Initialize immediate FTE values
    totalvec     <- c(0,0)  # Initialize total FTE values
    for (k1 in 1:2){  # Loop over both options
      if (k1 == 1) {
        DF = data1  # Use data for Option 1
      } else {
        DF = data2  # Use data for Option 2
      }
      # Calculate immediate and total FTE values for Phases 0-2
      now   <- sum(DF$MY_Ph0[which(DF$Phase == "Ph0")]) + 
        sum(DF$MY_Ph1[which(DF$Phase == "Ph1")]) + 
        sum(DF$MY_Ph2[which(DF$Phase == "Ph2")])
      
      total <- sum(DF$MY_total[which(DF$Phase == "Ph0")]) + 
        sum(DF$MY_total[which(DF$Phase == "Ph1")]) + 
        sum(DF$MY_total[which(DF$Phase == "Ph2")])
      nowvec[k1]   <- now  # Store immediate FTE values
      totalvec[k1] <- total  # Store total FTE values
    }
    
    # Prepare data for plotting the freed-up FTE resources
    DFtoPlot <- data.frame(cbind(rep(c("Option 1", "Option 2"), 2), c(nowvec,totalvec), c(rep("immediate", 2), rep("overall", 2))))
    colnames(DFtoPlot) <- c("X1", "X2", "timescale")
    DFtoPlot$X2 <- as.numeric(as.character(DFtoPlot$X2))
    maxy <- 1.2*max(DFtoPlot$X2)  # Set the maximum y-axis value for scaling
    
    # Create a bar plot showing freed-up FTE (immediate and overall)
    fig1 <-ggplot(DFtoPlot, aes(x = X1, y = X2, fill = timescale)) +
      geom_col(position = "dodge", colour = "black") +
      geom_text(aes(label = X2),  size =5,
                vjust = -1, position = position_dodge(.9)) +
      scale_fill_brewer(palette = "Pastel1") + 
      scale_y_continuous(name = "MY (FTE)", limits = c(0, maxy)) +
      scale_x_discrete(name = "") +
      theme_bw(base_size = 16) +
      theme(legend.position = "none")
    plot <- fig1  # Assign plot object
    plot
  }, height = 200)    #, height = 200, width = 400
  
  # Generate plots for the outputs from the whole portfolio, and both options. 
  output$plotOutput  <-  renderPlot({
    # Filter data based on which projects were not selected in Option 1 and Option 2
    data1        <- data %>% filter(Indication %in% input$option1) 
    data2        <- data %>% filter(Indication %in% input$option2) 
    # Generate dataframe of output values
    DF           <- data.frame(cbind(sum(data$Value), sum(data1$Value), sum(data2$Value),
                                     sum(data$profit), sum(data1$profit), sum(data2$profit),
                                     sum(data$expected.profit), sum(data1$expected.profit), sum(data2$expected.profit)))
    colnames(DF) <- c("Value_all", "Value_1", "Value_2", "Profit_all", "Profit_1", "Profit_2", "ExpProfit_all", "ExpProfit_1", "ExpProfit_2")
    # Reformat dataframe to plot
    DFtoPlot     <- melt(DF)
    # Rename entries of dataframe to be more informative
    DFtoPlot$Variable <- c(rep("Value (global peak sales)", 3), rep("Profit (Value less costs from Input)", 3), rep("Expected profit (Estimated profit * PoL)", 3))
    DFtoPlot$option   <- rep(c("All", "Option 1", "Option 2"), 3)
    # Change levels so the ordering makes sense
    DFtoPlot$Variable <- factor(DFtoPlot$Variable, levels = (c("Value (global peak sales)", 
                                                               "Profit (Value less costs from Input)", 
                                                               "Expected profit (Estimated profit * PoL)")))
    
    
    fig1 <- ggplot(DFtoPlot) +
      geom_segment(aes(x=0, xend = value, y=option, yend=option), 
                   size=2, colour="gray") +
      geom_point(aes(x=value, y=option), size = 6) +
      geom_text(aes(x=value/2, y=option, label = round(value,1)), size = 6) +
      facet_grid(~Variable, scales = "free_x") + 
      scale_x_continuous(name = "One million Swiss Francs (MCHF)") +
      scale_y_discrete(name = "") + 
      # geom_text(
      #   data=axis_titles,
      #   aes(label=axis_title), 
      #   x=0.5,
      #   y=1, fontface='bold' 
      # ) + 
      theme_bw(base_size = 16) +
      theme( panel.spacing.x = unit(3, "lines")) + theme(legend.position="none")
    plot <- fig1  # Assign plot object
    plot
  }, height = 300)    #, height = 200, width = 400
  
  # Generate plots for the inputs from the whole portfolio, and both options. 
  output$plotInput  <-  renderPlot({
    # Filter data based on which projects were not selected in Option 1 and Option 2
    data1        <- data %>% filter(Indication %in% input$option1) 
    data2        <- data %>% filter(Indication %in% input$option2) 
    # Generate dataframe of input values
    DF           <- data.frame(cbind(sum(data$MCHF_total), sum(data1$MCHF_total), sum(data2$MCHF_total),
                                     sum(0.3 * data$MY_total), sum(0.3 * data1$MY_total), sum(0.3 * data2$MY_total)))
    # Add benchmark values to the dataframe
    DF_benchmark <- data.frame(cbind(sum(data$MCHF_benchmark), sum(data1$MCHF_benchmark), sum(data2$MCHF_benchmark),
                                     sum(0.3 * data$MY_benchmark), sum(0.3 * data1$MY_benchmark), sum(0.3 * data2$MY_benchmark)))
    colnames(DF) <- c("MCHF_all", "MCHF_1", "MCHF_2", "MY_all", "MY_1", "MY_2")
    colnames(DF_benchmark) <- c("MCHF_all", "MCHF_1", "MCHF_2", "MY_all", "MY_1", "MY_2")
    # Reformat dataframe to plot
    DFtoPlot1 <-  melt(DF)
    DFtoPlot1$Variable <- c(rep("Planned R&D Project Variable", 3), rep("0.3 * FTE expressed by year", 3))
    DFtoPlot1$option   <- rep(c("All", "Option 1", "Option 2"), 2)
    DFtoPlot     <- data.frame(rbind(melt(DF), melt(DF_benchmark)))
    # Rename entries of dataframe to be more informative
    DFtoPlot$Variable <- c(rep("Planned R&D Project Variable", 3), rep("0.3 * FTE expressed by year", 3),
                           rep("Planned R&D Project Variable", 3), rep("0.3 * FTE expressed by year", 3))
    DFtoPlot$option   <- rep(c("All", "Option 1", "Option 2"), 4)
    DFtoPlot$benchmark <- as.factor(c(0,0,0,0,0,0,1,1,1,1,1,1))
    # Change levels so the ordering makes sense
    DFtoPlot$Variable <- factor(DFtoPlot$Variable, levels = (c("Planned R&D Project Variable", 
                                                               "0.3 * FTE expressed by year")))
    DFtoPlot1$Variable <- factor(DFtoPlot1$Variable, levels = (c("Planned R&D Project Variable", 
                                                                 "0.3 * FTE expressed by year")))
    
    fig1 <- ggplot() +
      geom_segment(data = DFtoPlot1, aes(x=0, xend = value, y=option, yend=option), 
                   size=2, colour="gray") +
      geom_point(data = DFtoPlot, aes(x=value, y=option, shape = as.factor(benchmark)), size = 6 ) +
      geom_text(data = DFtoPlot1, aes(x=value/2, y=option, label = round(value,1)), size = 6) +
      facet_grid(~Variable, scales = "free_x") + 
      scale_x_continuous(name = "One million Swiss Francs (MCHF)") +
      scale_y_discrete(name = "")  +
      theme( panel.spacing.x = unit(3, "lines")) +
      theme_bw(base_size = 16)  + theme(legend.position="none")
    plot <- (fig1)  # Assign plot object
    plot
  }, height = 300)    #, height = 200, width = 400
  
  # Generate plots for the efficacy, safety, feasibility, and competition of the whole portfolio, and both options. 
  output$plotMeans  <-  renderPlot({
    # Filter data based on which projects were not selected in Option 1 and Option 2
    data1        <- data %>% filter(Indication %in% input$option1) 
    data2        <- data %>% filter(Indication %in% input$option2) 
    DF           <- data.frame(cbind(mean(data$Efficacy), mean(data1$Efficacy), mean(data2$Efficacy), 
                                     mean(data$Safety), mean(data1$Safety), mean(data2$Safety), 
                                     mean(data$Operational.Feasibility),  mean(data1$Operational.Feasibility), mean(data2$Operational.Feasibility),
                                     mean(data$Level.of.Competition), mean(data1$Level.of.Competition), mean(data2$Level.of.Competition)))
    colnames(DF) <- c("Efficacy_all", "Efficacy_1", "Efficacy_2", "_all", "Safety_1", "Safety_2",
                      "Operational.Feasibility_all", "Operational.Feasibility_1", "Operational.Feasibility_2",
                      "Level.of.Competition_all", "Level.of.Competition_1", "Level.of.Competition_2")
    # Reformat dataframe to plot
    DFtoPlot     <- data.frame(melt(DF))
    DFtoPlot$Variable <- c(rep("Efficacy", 3), rep("Safety", 3),
                           rep("Operational.Feasibility", 3), rep("Level.of.Competition", 3))
    DFtoPlot$option   <- rep(c("All", "Option 1", "Option 2"), 4)
    # Change levels so the ordering is consistent with order data given
    DFtoPlot$Variable <- factor(DFtoPlot$Variable, levels = (c("Efficacy", 
                                                               "Safety",
                                                               "Operational.Feasibility", "Level.of.Competition")))
    
    
    fig1 <- ggplot() +
      geom_segment(data = DFtoPlot, aes(x=0, xend = value, y=option, yend=option), 
                   size=2, colour="gray") +
      geom_point(data = DFtoPlot, aes(x=value, y=option), size = 6 ) +
      geom_text(data = DFtoPlot, aes(x=value/2, y=option, label = round(value,1)), size = 6) +
      facet_grid(~Variable, scales = "free_x") + 
      scale_x_continuous(name = "Scores 1-5: 1=low, 5=high", limits = c(0,5)) +
      scale_y_discrete(name = "")  +
      theme( panel.spacing.x = unit(3, "lines")) +
      theme_bw(base_size = 16)  + theme(legend.position="none")
    plot <- (fig1)  # Assign plot object
    plot
  }, height = 300)    #, height = 200, width = 400
  
  
  
  #   
}

shinyApp(ui, server)
