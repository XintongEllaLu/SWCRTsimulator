#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load necessary libraries
library(shiny)
library(dplyr)
library(survival)
library(lme4)
library(GLMMadaptive)
library(ggplot2)

library(bslib)
library(DT)

#####


cvp <- function(xx, var_type = "Continuous") {
  if (var_type == "Continuous") {
    card(
      card_header(
        paste0('Variable ', xx),
        class = 'bg-primary text-white'
      ),
      layout_columns(
        numericInput(
          paste0("var", xx, "_mean"), 
          label = "Mean", 
          value = 10, 
          width = '100%'
        ),
        numericInput(
           paste0("var", xx, "_sd"),  
          label = "SD", 
          value = 2.5, 
          width = '100%'
        )
      ),
      style = paste(
        "background-color: #f8f9fa;",
        "color: #333;",
        "padding: 15px;",
        "border-radius: 8px;",
        "box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);"
      )
    )
  } else if (var_type == "Binary") {
    card(
      card_header(
        paste0('Variable ', xx),
        class = 'bg-success text-white'
      ),
      layout_columns(
        numericInput(
          inputId = paste0("var", xx, "_prop"), 
          label = "Proportion (0 to 1)", 
          value = 0.5, 
          width = '100%'
        )
      ),
      style = paste(
        "background-color: #f8f9fa;",
        "color: #333;",
        "padding: 15px;",
        "border-radius: 8px;",
        "box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);"
      )
    )
  }
}


# Define UI for the app

ui<-page_navbar(
  
  title="Stepped Wedge Design Simulation",
  
  sidebar=sidebar(
    width=350,
    open=NA,
    
    actionButton("runSim", "Run Simulation", icon = icon("play")),
    
    
    accordion(
      open=c('Model Selection','Beta Coefficients'),
      accordion_panel(
        'Model Selection',
        radioButtons("model_choice",NULL, choices = c("glimmix", "glmmadaptive")),
        numericInput("significantvalue", "Significant Level", value = 0.05, min = 0.01),
        numericInput("sim_num", "Number of Simulations", value = 3, min = 1),
      ),
      
      accordion_panel(
        'Beta Coefficients',
        numericInput("beta", "Treatment Effect", value = 1.5),
        selectInput("num_vars", "Number of Covariates", choices = c(1, 2, 3)),
        uiOutput('bcv3')
      ),
      accordion_panel(
        'Sample Size and Cluster Information',
        numericInput("steps", "Steps", value = 4, min = 1),
        numericInput("clusters_per_step","Clusters per Step", value = 1, min = 1),
        numericInput("samplesize", "Cluster Sample Size", value = 200, min = 1, step = 50)
      ),
      accordion_panel(
        'Interval and Switch Parameters',
        numericInput("intv_length", "Interval Length (days)", value = 30, min = 1),
        numericInput("switch_interval", "Switch Interval (days)", value = 180, min = 1),
      ),
      accordion_panel(
        'Time Parameters',
        dateRangeInput("study_time", "Study Time Range", 
                       start = "2021-09-01", end = "2025-02-28"),
        dateInput("switch_begin", "Switch Begin Time", value = "2022-02-28"),
        dateInput("recruit_end", "Recruitment End Time", value = "2024-02-28"),
        numericInput("followup_length", "Follow-up Length (days)", value = 365, min = 0),
      ),
      accordion_panel(
        'Distribution Parameters',
        numericInput("scale", "Scale", value = 0.003),
        numericInput("shape", "Shape", value = 0.441),
      ),
      
      accordion_panel(
        'Standard Errors',
        numericInput("sigma_c", "Standard Deviation for Cluster ", value = 0.3, min = 0),
        # Conditional input for sigma_tau, only show if glmmadaptive is selected
        conditionalPanel(
          condition = "input.model_choice == 'glmmadaptive'",
          numericInput("sigma_tau", "Standard Deviation for Treatment within Cluster", value = 0.15, min = 0)
        ),
      ),
    ),
    
  ), ####end sidebar
  
  nav_panel(
    "User Guide",
    tags$div(
      style = "background-color: #f5f5f5; padding: 20px; border-radius: 8px; margin-top: 20px;",
      
      # Welcome message
      tags$h3("Welcome to the Stepped-Wedge Simulation Platform", 
              style = "color: #2C3E50; font-weight: bold; text-align: center;"),
      
      # Introduction paragraph
      tags$p(
        "This platform assists researchers and statisticians in conducting power and sample size calculations for stepped wedge cluster randomized trials (SWCRT) under various experimental conditions. Utilizing Monte Carlo simulations, the platform allows users to explore the influence of key parameters on the study’s power and outcomes. By simulating thousands of trial iterations, the platform provides robust estimates of power, enabling users to optimize trial design and evaluate statistical performance under realistic scenarios. Through an intuitive interface, users can easily manipulate input parameters and visualize the impact on study outcomes.",
        style = "color: #34495E; font-size: 16px; text-align: left; padding-bottom: 20px;"
      ),
      
      # How the platform works
      tags$h4("How the Platform Works", style = "color: #2980B9; font-weight: bold;"),
      tags$p("The app operates in three steps:", style = "color: #34495E;"),
      tags$ul(
        tags$li(strong("Weibull Distribution:"), " Generates continuous data using the Weibull distribution based on the input parameters."),
        tags$li(strong("Data Transformation:"), " Converts the continuous data into discrete data for modeling."),
        tags$li(strong("Model Fitting:"), " Fits a model to the discrete data.")
      ),
      
      # Model selection explanation
      tags$h4("Model Selection", style = "color: #2980B9; font-weight: bold;"),
      tags$p("The app provides two model choices:", style = "color: #34495E;"),
      tags$ul(
        tags$li(
          strong("Glimmix:"), 
          " Uses the ", tags$code("glmer()"), " function from the ", tags$code("lme4"), " package, accounting only for variability between clusters."
        ),
        tags$li(
          strong("GLMMadaptive:"), 
          " Uses the ", tags$code("mixed_model()"), " function from the ", tags$code("GLMMadaptive"), " package, which models both treatment and cluster-level random effects."
        )
      ),
      tags$p("Choose the appropriate model based on your experimental design.", style = "color: #34495E;"),
      
      # Setting up parameters
      tags$h4("Setting Up Parameters", style = "color: #2980B9; font-weight: bold;"),
      tags$p("In the input panel, you can configure the following parameters. Refer to the table below:", style = "color: #34495E;"),
      
      # Table of Parameters
      tags$div(
        style = "background-color: #EAF2F8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
        tags$table(
          style = "width: 100%;",
          tags$thead(
            tags$tr(
              tags$th("Parameter", style = "text-align: left; font-weight: bold; color: #34495E;"),
              tags$th("Description", style = "text-align: left; font-weight: bold; color: #34495E;")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("Simulation Numbers"), tags$td("Total number of simulations conducted for empirical power analysis.")),
            tags$tr(tags$td("Steps"), tags$td("Number of switches in the study")),
            tags$tr(tags$td("Cluster per Steps"), tags$td("Number of clusters per step in the study.")),
            tags$tr(tags$td("Cluster Sample Size"), tags$td("Number of individuals per cluster.")),
            tags$tr(tags$td("Time Interval"), tags$td("Time intervals between measurements for each cluster.")),
            tags$tr(tags$td("Switch Interval"), tags$td("Time interval for clusters to switch from control to intervention.")),
            tags$tr(tags$td("Study Begin Time"), tags$td("The start of the study.")),
            tags$tr(tags$td("Study End Time"), tags$td("The final time point of the study.")),
            tags$tr(tags$td("Switch Begin Time"), tags$td("The time when the first cluster switches from control to intervention.")),
            tags$tr(tags$td("Recruitment End Time"), tags$td("Time point after which no new participants are recruited.")),
            tags$tr(tags$td("Follow-up Time"), tags$td("The duration of time each individual is followed after entry into the study.")),
            tags$tr(tags$td("Treatment Effect"), tags$td("The treatment effect is presented in exponentiated form, as the clog-log link function are used.")),
            tags$tr(tags$td("Time-independent Covariates"), tags$td("Coefficients and Distributions for time-independent covariates such as age and gender.")),
            tags$tr(tags$td("Significance Level"), tags$td("The threshold for statistical significance.")),
            tags$tr(tags$td("Standard Deviation for ", withMathJax("\\(\\tau_{m\\phi}\\)")), tags$td("The variance of the random effect for treatment within each cluster.")),
            tags$tr(tags$td("Standard Deviation for ", withMathJax("\\(\\tau_{m}\\)")), tags$td("The variance of the random effect for each cluster.")),
            tags$tr(tags$td("Scale Parameter "), tags$td("Scale parameter for the Weibull distribution.")),
            tags$tr(tags$td("Shape Parameter"), tags$td("Shape parameter for the Weibull distribution."))
          )
        )
      ),
      
      # Running the simulation
      tags$h4("Running the Simulation", style = "color: #2980B9; font-weight: bold;"),
      tags$p(
        "After setting up your parameters, click the ", strong("Run Simulation"), " button to generate the results. You will see three progress bars indicating the simulation, data transformation, and model fitting steps, which track the progress of each process.",
        style = "color: #34495E;"
      ),
      
      # Viewing the results
      tags$h4("Viewing the Results", style = "color: #2980B9; font-weight: bold;"),
      tags$p("The output panel provides the following results:", style = "color: #34495E;"),
      tags$ul(
        tags$li(strong("Simulation Data:"), " Downloadable data generated by the simulation."),
        tags$li(strong("Density Plot:"), " A plot for visualizing event occurrences in the data."),
        tags$li(strong("Mean Treatment Effect and Power:"), " Displays the estimated power and the mean beta of the treatment effect for your study."),
        tags$li(strong("Histogram:"), " A histogram of the beta values of treatment effect to observe their distribution.")
      ),
      
      # Notes
      tags$h4("Notes", style = "color: #2980B9; font-weight: bold;"),
      tags$p(
        "For more detailed information about the models and platform, please refer to the accompanying research paper. To avoid interruptions during simulations, it is recommended to keep your screen active. The running time of the simulation will depend on the interval length, number of clusters, and the total number of simulations. Typically, 30 runs take about 5-15 minutes, while 100 runs may require up to an hour. If you encounter frequent server disconnections or experience slow performance during longer simulations, we suggest downloading the RShiny app’s source code and running it locally in your R environment to ensure smoother and more reliable execution for extended processes.",
        style = "color: #34495E;"
      )
    )
  ),
  
  nav_panel("Simulation Data",
            # UI container with a card-like style for simulation data
            div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1); margin-bottom: 20px;",
              
              # Title
              tags$h3("Simulation Data", style = "color: #2C3E50; font-weight: bold; text-align: center; margin-bottom: 20px;"),
              
              # UI outputs for simulation data details
              uiOutput('logDetail1'),
              uiOutput('logDetail2'),
              uiOutput('logDetail3'),
              uiOutput('simuResu'),
              
              # Checkbox to show or hide the density plot, with inline styling applied to the surrounding div
              div(
                style = "margin-top: 20px;",
                checkboxInput("showDensityPlot", "Show Density Plot", value = FALSE),
                
                # Conditionally display the density plot based on checkbox input
                conditionalPanel(
                  condition = "input.showDensityPlot == true",
                  div(
                    style = "border: 1px solid #ddd; padding: 10px; background-color: #fff; border-radius: 8px;",
                    plotOutput("densityPlot", height = "400px")
                  )
                )
              )
            )
  ),
  
  nav_panel("Mean Treatment Effect and Power",
            # UI container with a card-like style for mean beta and power results
            div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1); margin-bottom: 20px;",
              
              # Title
              tags$h3("Mean Beta and Power", style = "color: #2C3E50; font-weight: bold; text-align: center; margin-bottom: 20px;"),
              
              # UI output for simulation results (Mean Beta and Power)
              uiOutput("simulationResults"),
              
              # Checkbox to show or hide the histogram, with inline styling applied to the surrounding div
              div(
                style = "margin-top: 20px;",
                checkboxInput("showHistogram", "Show Histogram", value = FALSE),
                
                # Conditionally display the histogram plot based on checkbox input
                conditionalPanel(
                  condition = "input.showHistogram == true",
                  div(
                    style = "border: 1px solid #ddd; padding: 10px; background-color: #fff; border-radius: 8px;",
                    plotOutput("histogramPlot", height = "400px")
                  )
                )
              )
            )
  ),
  
  
  
  
)

# Begain Simulation
server <- function(input, output, session) {
  
 
  output$bcv3<-renderUI({
    input$num_vars->nv
    
    lapply(1:nv,function(xx)
    {
      tagList(
        selectInput(paste0("var",xx,"_type"), paste("Covariate",xx,"Type"), choices = c("Continuous", "Binary")),
        numericInput(paste0("beta",xx),paste0("Beta for Covariate ",xx), value = 0.5),
        uiOutput(paste0('bcvv',xx))
      )
    }
    )->inter.bcv
    
    tagList(inter.bcv)
  })
  
  ################
  output$bcvv1 <- renderUI({
    if (input$var1_type == "Continuous") {
      cvp(1, var_type = "Continuous")
    } else {
      cvp(1, var_type = "Binary")
    }
  })
  
  output$bcvv2 <- renderUI({
    if (input$num_vars >= 2 & input$var2_type == "Continuous") {
      cvp(2, var_type = "Continuous")
    } else {
      cvp(2, var_type = "Binary")
    }
  })
  
  output$bcvv3 <- renderUI({
    if (input$num_vars >= 3 & input$var3_type == "Continuous") {
      cvp(3, var_type = "Continuous")
    } else {
      cvp(3, var_type = "Binary")
    }
  })
  
  
  ########
  observeEvent(input$runSim, {
    set.seed(123)
    
    
    # Initialize an empty data frame to store results
    results <- data.frame()
    
    cluster_steps <- rep(1:input$steps, each = input$clusters_per_step) 
    cluster_num <- input$steps*input$clusters_per_step 
    
    # Function to generate variable values
    generate_variable <- function(var_type, var_mean, var_sd, var_prop) {
      if (var_type == "Continuous") {
        # Generate a continuous variable and round it
        var <- round(rnorm(1, mean = var_mean, sd = var_sd))
        return(var)
      } else {
        var <- rbinom(1, 1, prob = var_prop)
        return(var)  # Generates either 0 or 1 based on var_prob
      }
    }
    
    
    
    # Progress tracking
    start.sim_time <- Sys.time()
    
    progress <- Progress$new(session, min = 1, max = input$steps*input$sim_num)
    progress$set(message = paste('Simulation starts at:',format(start.sim_time,"%H:%M:%S"),'...in progress...'), value = 1)
    
    
    # Simulation loop
    for (sim in 1:input$sim_num) {
      for (cluster in 1:cluster_num) {
        # Determine the step for this cluster
        step <- cluster_steps[cluster]
        
        error <- rnorm(1, mean = 0, sd = input$sigma_c)
        tau_bti <- if (input$model_choice == "glmmadaptive") rnorm(1, mean = 0, sd = input$sigma_tau) else 0
        
        for (id in 1:input$samplesize) {
          # Generate variables based on the number of variables selected by the user
          var1 <- if (input$num_vars >= 1) {
            generate_variable(input$var1_type, input$var1_mean, input$var1_sd, input$var1_prop)
          } else {
            NA
          }
          
          if (input$num_vars >= 2) {
            var2 <- generate_variable(input$var2_type, input$var2_mean, input$var2_sd,input$var2_prop)
          } else {
            var2 <- NULL
          }
          
          if (input$num_vars == 3) {
            var3 <- generate_variable(input$var3_type, input$var3_mean, input$var3_sd, input$var3_prop)
          } else {
            var3 <- NULL
          }
          
          # Define study dates
          studybegin <- as.Date(input$study_time[1])
          recuitend <- as.Date(input$recruit_end)
          studyend <- as.Date(input$study_time[2])
          
          # Calculate entry/switch/drop date and switch time
          entertemp <- runif(1, min = 0, max = as.numeric(difftime(recuitend, studybegin, units = "days")))
          entrydate <- studybegin + as.integer(entertemp)
          
          switchdate <- as.Date(input$switch_begin) + as.integer((step) * input$switch_interval)
          switchtime <- as.numeric(pmax(switchdate - entrydate + 1, 0))
          
          c <- rexp(1, rate = exp(input$shape)) * as.numeric(difftime(studyend, studybegin, units = "days"))
          dropdate <- entrydate + as.integer(c)
          
          # Simulate survival time
          logu <- -log(runif(1))
          linpre <- sum(c(if (!is.null(var1)) var1 * input$beta1 else 0,
                          if (!is.null(var2)) var2 * input$beta2 else 0,
                          if (!is.null(var3)) var3 * input$beta3 else 0)) + error
          linpre2 <- linpre + tau_bti
          scalee <- input$scale * exp(linpre)
          scalee2 <- input$scale * exp(linpre2)
          
          nom <- logu - scalee * (switchtime^input$shape) + scalee2 * exp(input$beta) * (switchtime^input$shape)
          denom <- scalee2 * exp(input$beta)
          
          if (logu < scalee * (switchtime^input$shape)) {
            t <- (logu / scalee)^(1 / input$shape)
          } else {
            t <- (nom / denom)^(1 / input$shape)
          }
          
          disclosuredate <- entrydate + as.integer(t)
          
          # Determine censoring status and observed end date
          if (disclosuredate > pmin(dropdate, studyend, entrydate + input$followup_length)) {
            censor <- 0
            obenddate <- pmin(dropdate, studyend, entrydate + input$followup_length)
          } else {
            censor <- 1
            obenddate <- disclosuredate
          }
          
          # Calculate survival time
          disclosuretime <- as.numeric(difftime(obenddate, entrydate, units = "days")) + 1
          
          # Store the results
          results <- rbind(results, data.frame(
            sim, step, cluster, id, 
            var1 = if (!is.null(var1)) var1 else NA, 
            var2 = if (!is.null(var2)) var2 else NA, 
            var3 = if (!is.null(var3)) var3 else NA, 
            studybegin, entrydate, switchdate, dropdate, obenddate, censor, disclosuretime
          ))
        }
        progress$inc(1,message=paste('Simulating',scales::percent(progress$getValue()/(input$steps*input$sim_num),0.01)))
      }
      
    }
    
    progress$close()
    
    
    ####
    end.sim_time<-Sys.time()
    
    output$logDetail1 <- renderUI({
      total_time <- as.numeric(difftime(end.sim_time, start.sim_time, units = "secs"))
      formatted_time <- if (total_time > 60) {
        sprintf("%d min %.3f sec", floor(total_time / 60), total_time %% 60)
      } else {
        sprintf("%.3f sec", total_time)
      }
      
      tagList(
        tags$p(tags$em(tags$b('Simulating'),' starts at:'), start.sim_time,
               tags$em('ends at:'), end.sim_time,
               tags$em('takes:'), formatted_time)
      )
    })
    
    # Convert results to a data frame
    results <- as.data.frame(results)
    
    ###

    # Create interval data
    intvdata <- results %>%
      mutate(entryintv = as.integer(difftime(entrydate, studybegin, units = "days") / input$intv_length) + 1,
             switchintv = as.integer(difftime(switchdate, studybegin, units = "days") / input$intv_length) + 1,
             surveintv = as.integer(difftime(obenddate, studybegin, units = "days") / input$intv_length) + 1,
             survtimediscrete = surveintv - entryintv + 1)
    
    
    #### Density Plot
    output$densityPlot <- renderPlot({
      ggplot(intvdata, aes(x = survtimediscrete, fill = factor(censor, labels = c("Censor", "Event")))) +
        geom_density(alpha = 0.5) +
        labs(title = "Smoothed Density of Events and Censoring",
             x = "Individual Follow-Up (Py)",
             y = "Density",
             fill = "Outcome") +
        theme_minimal()
    })
    
    
    start.wrangle.time <- Sys.time()
    
    progress2 <- Progress$new(session, min = 1, max = nrow(intvdata))
    
    
    total <- nrow(intvdata)
    progress2$set(
      message = 'Wrangling data...',
      value = 1
    )
    # Event history data processing
    eventhistorydata <- intvdata %>% 
      rowwise() %>% 
      do({
        interval_data <- lapply(1:.$survtimediscrete, function(interval) {
          event <- ifelse(interval == .$survtimediscrete, .$censor, 0)
          trt <- ifelse(interval < (.$switchintv - .$entryintv + 1), 0, 1)
          data.frame(
            sim = .$sim, 
            cluster = .$cluster, 
            id = .$id, 
            var1 = if (!is.null(var1)) .$var1 else NA,
            var2 = if (!is.null(var2)) .$var2 else NA,
            var3 = if (!is.null(var3)) .$var3 else NA,
            disclosuretime = .$disclosuretime, 
            interval = interval, 
            event = event, 
            trt = trt
          )
        })
        progress2$inc(1)
        
        bind_rows(interval_data)
      }) %>% 
      ungroup()
    
    eventhistorydata <- eventhistorydata %>% 
      arrange(sim, cluster)
      # Close the progress bar is handled by on.exit(), so no need to call progress2$close() again
    progress2$close()
    
    end.wrangle.time <- Sys.time()
    
    output$logDetail2 <- renderUI({
      total_time <- as.numeric(difftime(end.wrangle.time, start.wrangle.time, units = "secs"))
      formatted_time <- if (total_time > 60) {
        sprintf("%d min %.3f sec", floor(total_time / 60), total_time %% 60)
      } else {
        sprintf("%.3f sec", total_time)
      }
      
      tagList(
        tags$p(tags$em(tags$b('Wrangling'),' starts at:'), start.wrangle.time,
               tags$em('ends at:'), end.wrangle.time,
               tags$em('takes:'), formatted_time)
      )
    })
    
    
    # Initialize the progress bar with a range between 1 and the number of simulations
    
    start.model.time <- Sys.time()
    progress3 <- Progress$new(session, min = 0, max = input$sim_num)
    progress3$set(
      message = paste('Modeling starts at:', format(start.model.time, "%H:%M:%S"), '... in progress...'), 
      value = 0
    )
    
    # Check for multicollinearity
    if (input$num_vars > 1) {
      selected_vars <- eventhistorydata %>% select(var1, var2, var3) %>% select_if(~ sum(!is.na(.)) > 0)
      corr_matrix <- cor(selected_vars, use = "complete.obs")
      
      if (any(abs(corr_matrix[lower.tri(corr_matrix)]) > 0.9)) {
        showModal(modalDialog(
          title = "Warning: Multicollinearity Detected",
          "High correlation between variables detected. Consider reducing the number of variables or modifying them.",
          easyClose = TRUE
        ))
      }
    }
    
    # Prepare model fitting
    variable_names <- c("var1", "var2", "var3")[1:input$num_vars]
    variable_names <- variable_names[!sapply(variable_names, function(x) all(is.na(eventhistorydata[[x]])))]
    
    # Initialize vectors to store betas and p-values for each simulation
    betas <- numeric(input$sim_num)
    pvalues <- numeric(input$sim_num)
    
    # Start the simulation loop
    for (sim in 1:input$sim_num) {
      
      
      # Dynamically update the progress bar for each simulation
      progress3$inc(1, message = paste('Modeling', sim, 'out of', input$sim_num, 'Simulations'))
      
      # Create the formula dynamically based on selected variables
      formula <- as.formula(paste("event ~", 
                                  paste(c(variable_names, "trt", "interval"), collapse = " + "), 
                                  "+ (1 | cluster)"))
      formula2 <- as.formula(paste("event ~", 
                                   paste(c(variable_names, "trt", "interval"), collapse = " + ")))
      
      if (input$model_choice == "glimmix") {
        # Fit the glimmix model
        glimmix_model <- glmer(formula, 
                               data = eventhistorydata %>% filter(sim == !!sim), 
                               family = binomial(link = "cloglog"), nAGQ = 1)
        
        betas[sim] <- fixef(glimmix_model)["trt"]
        pvalues[sim] <- summary(glimmix_model)$coefficients["trt", "Pr(>|z|)"]
        
      } else if (input$model_choice == "glmmadaptive") {
        # Fit the glmmadaptive model
        glmmadaptive_model <- mixed_model(
          fixed = formula2, 
          random = ~ trt | cluster, 
          family = binomial(link = "cloglog"), 
          data = eventhistorydata %>% filter(sim == !!sim),
          control = list(max_coef_value = 100, nAGQ = 1)
        )
        
        betas[sim] <- fixef(glmmadaptive_model)["trt"]
        summary_model <- summary(glmmadaptive_model)
        coefficients_table <- summary_model$coef_table
        p_value_trt <- coefficients_table["trt", "p-value"] 
        
        pvalues[sim] <- p_value_trt
      }
      
        }
    
    # Close the progress bar after all simulations are complete
    progress3$close()
   
    
    end.model.time <- Sys.time()
  
    
    ####
    output$logDetail3 <- renderUI({
      total_time <- as.numeric(difftime(end.model.time, start.model.time, units = "secs"))
      formatted_time <- if (total_time > 60) {
        sprintf("%d min %.3f sec", floor(total_time / 60), total_time %% 60)
      } else {
        sprintf("%.3f sec", total_time)
      }
      
      tagList(
        tags$p(tags$em(tags$b('Modelling'),' starts at:'), start.model.time,
               tags$em('ends at:'), end.model.time,
               tags$em('takes:'), formatted_time)
      )
    })
    
    
    # Calculate the mean of the beta coefficients and estimated power
    mean_beta <- mean(betas)
    power <- mean(pvalues < input$significantvalue)
    end_time<-Sys.time()
    
  
    
    # Output the mean beta and estimated power
    output$simulationResults <- renderUI({
      tagList(
        tags$h6("Simulation Results:"),
        tags$p("Mean of Beta Coefficients:",tags$em(mean_beta)),
        tags$p("Estimated Power:",tags$em(power)),
      )
    })
    
    # Convert betas to a data frame for plotting
    betas_df <- data.frame(beta = betas)
    
    # Render histogram plot
    output$histogramPlot <- renderPlot({
      ggplot(betas_df, aes(x = beta)) +
        geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
        geom_vline(aes(xintercept = mean_beta), color = "red", linetype = "dashed", size = 1) +
        labs(
          title = paste("Histogram of Treatment Effect Coefficients (Beta =", input$beta, ")"),
          x = "Beta (Treatment Effect)",
          y = "Frequency"
        ) +
        theme_minimal()
    })
    
    # Make results available for download
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("simulation_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        
        write.csv(results, file, row.names = FALSE)
      }
    )
    
    ################
    output$simuResu<-renderUI({
      tagList(
        layout_columns(col_widths=c(3),downloadButton("downloadData", "Download Simulation Data")),
        DTOutput('simdata')
      )
    })
    
    output$simdata<-renderDT({
      nocols<-''
      colss<-setdiff(names(results),nocols)
      datatable(results[,colss],options=list(dom='tp'))
    })
    
    
    
  })
  
  ############
  
}




# Run the application 
shinyApp(ui = ui, server = server)
