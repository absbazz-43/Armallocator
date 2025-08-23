library(shiny)
library(shinythemes)
library(bigrquery)
library(DT)
library(shinycssloaders)
library(dplyr)
library(cobalt)
library(ggplot2)
library(digest)
library(rlang)
library(shinyjs)
library(shinyalert)

source("func.R")

# Initialize shinyjs and shinyalert
useShinyjs()
useShinyalert()

# Add authentication UI at the beginning of your UI
ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  theme = shinytheme("flatly"),
  
  # Authentication UI (shown first)
  div(id = "auth_ui",
      style = "display: flex; justify-content: center; align-items: center; height: 100vh; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);",
      div(style = "background: white; padding: 40px; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.2); text-align: center; max-width: 400px;",
          h2("A/B Testing Platform", style = "color: #333;"),
          br(),
          p("Connect to your BigQuery data to start analyzing"),
          p("Only Pathao employees with @pathao.com emails can access this platform", 
            style = "color: #dc3545; font-weight: bold;"),
          br(),
          actionButton("auth_btn", "Sign in with Google",
                       icon = icon("google"),
                       style = "background-color: #4285F4; color: white; padding: 15px 30px; border: none; border-radius: 5px; font-size: 16px; width: 100%;"),
          br(), br(),
          p("Your credentials are stored locally and never shared"),
          # Add manual email input for Posit Connect
          div(style = "margin-top: 20px; padding-top: 20px; border-top: 1px solid #eee;",
              p("Having issues with Google sign-in?"),
              textInput("manual_email", "Enter your @pathao.com email:", placeholder = "your.name@pathao.com"),
              actionButton("manual_auth", "Continue with Email", style = "width: 100%;")
          )
      )
  ),
  
  # Main app UI (hidden initially)
  hidden(div(id = "main_ui",
             # YOUR EXISTING UI CODE STARTS HERE
             titlePanel("PEIRAMATISMOS"),
             tags$head(
               tags$head(
                 tags$style(HTML("
                            .split-container {
                              display: flex;
                              height: calc(100vh - 150px);
                              width: 100%;
                            }

                            .left-panel {
                              flex: 1;
                              padding: 15px;
                              border-right: 2px solid #ddd;
                              overflow-y: auto;
                              height: 100%;
                            }

                            .right-panel {
                              flex: 1;
                              padding: 15px;
                              overflow-y: auto;
                              height: 100%;
                            }

                            .sql-editor {
                              width: 100%;
                              height: calc(100% - 60px);
                              font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
                              font-size: 14px;
                              resize: none;
                              border: 1px solid #ccc;
                              border-radius: 4px;
                              padding: 10px;
                            }

                            .results-container {
                              height: calc(100% - 40px);
                              overflow-y: auto;
                              border: 1px solid #ddd;
                              border-radius: 4px;
                            }

                            .control-panel {
                              margin-bottom: 15px;
                              padding: 10px;
                              background-color: #f8f9fa;
                              border-radius: 5px;
                              border: 1px solid #e9ecef;
                            }

                            .tab-content {
                              height: calc(100vh - 150px);
                              overflow-y: auto;
                            }

                            .plot-container {
                              height: 400px;
                              margin-bottom: 20px;
                            }


                            .card {
                              margin-bottom: 20px;
                              border: 1px solid #ddd;
                              border-radius: 8px;
                              box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                            }
                            .card-header {
                              background-color: #f8f9fa;
                              padding: 15px;
                              border-bottom: 1px solid #ddd;
                              font-weight: bold;
                            }
                            .card-body {
                              padding: 20px;
                            }
                            .result-box {
                              background-color: #f8f9fa;
                              border: 1px solid #ddd;
                              border-radius: 5px;
                              padding: 15px;
                              margin-bottom: 15px;
                            }
                            .significant {
                              color: #28a745;
                              font-weight: bold;
                            }
                            .not-significant {
                              color: #dc3545;
                              font-weight: bold;
                            }

                          "))
               )
             ),
             tabsetPanel(
               id = "main_tabs",
               type = "tabs",
               
               tabPanel("SQL Runner",
                        div(class = "control-panel",
                            fluidRow(
                              column(6,
                                     textInput("project_id", "Project ID:",
                                               value = "your-project-id",
                                               placeholder = "Enter your BigQuery project ID")
                              ),
                              column(6,
                                     actionButton("run_query", "Run Query",
                                                  icon = icon("play"),
                                                  class = "btn-primary"),
                                     actionButton("clear_query", "Clear",
                                                  icon = icon("trash"),
                                                  class = "btn-warning"),
                                     downloadButton("download_data", "Download CSV",
                                                    class = "btn-success")
                              )
                            )
                        ),
                        
                        div(class = "split-container",
                            div(class = "left-panel",
                                h4("SQL Editor"),
                                tags$textarea(id = "sql_query", class = "sql-editor",
                                              placeholder = "SELECT * FROM `project.dataset.table` LIMIT 1000",
                                              rows = 10, ""),
                                br(),
                                numericInput("limit_rows", "Limit rows:",
                                             value = 1000, min = 1, max = 100000)
                            ),
                            
                            div(class = "right-panel",
                                h4("Query Results"),
                                div(class = "results-container",
                                    withSpinner(
                                      DTOutput("results_table"),
                                      type = 4,
                                      color = "#0dc5c1"
                                    )
                                ),
                                br(),
                                verbatimTextOutput("query_info")
                            )
                        )
               ),
               
               # Tab 2: Randomizer
               tabPanel("Randomizer",
                        fluidRow(
                          column(4,
                                 wellPanel(
                                   h4("Randomization Settings"),
                                   numericInput("arm_no", "Number of Arms:", value = 2, min = 2, max = 10),
                                   uiOutput("arm_proportion_ui"),
                                   uiOutput("arm_name_ui"),
                                   textInput("experiment_name", "Experiment Name:", value = "experiment_1"),
                                   checkboxInput("use_smd", "Use SMD Balancing", value = FALSE),
                                   uiOutput("confounders_ui"),
                                   numericInput("smd_threshold", "SMD Threshold:", value = 0.05, min = 0.01, max = 0.2, step = 0.01),
                                   actionButton("run_randomizer", "Run Randomization", class = "btn-primary")
                                 )
                          ),
                          column(8,
                                 h4("Randomized Data"),
                                 withSpinner(DTOutput("randomized_table"), type = 4),
                                 verbatimTextOutput("randomizer_info")
                          )
                        )
               ),
               
               # Tab 3: Covariate Balance Checker
               tabPanel("Balance Checker",
                        fluidRow(
                          column(4,
                                 wellPanel(
                                   h4("Balance Check Settings"),
                                   selectInput("treatment_var", "Treatment Variable:", choices = NULL),
                                   uiOutput("confounders_balance_ui"),
                                   actionButton("run_balance_check", "Run Balance Check", class = "btn-primary")
                                 )
                          ),
                          column(8,
                                 h4("Love Plot"),
                                 plotOutput("love_plot"),
                                 h4("SRM Check"),
                                 verbatimTextOutput("srm_result"),
                                 h4("Balance Table"),
                                 verbatimTextOutput("balance_table")
                          )
                        )
               )
               
               # Tab 4: NEW - Distribution Checker
               , tabPanel("Distribution Checker",
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("Distribution Check Settings"),
                                     selectInput("dist_treatment_var", "Treatment Variable:",
                                                 choices = NULL),
                                     selectInput("dist_target_var", "Target Variable:",
                                                 choices = NULL),
                                     radioButtons("plot_type", "Plot Type:",
                                                  choices = c("Density Plot (Continuous)" = "density",
                                                              "Bar Chart (Categorical)" = "bar"),
                                                  selected = "density"),
                                     actionButton("run_dist_check", "Generate Plot",
                                                  class = "btn-primary"),
                                     br(), br(),
                                     h5("Variable Information:"),
                                     verbatimTextOutput("var_info")
                                   )
                            ),
                            column(9,
                                   h4("Distribution Plot"),
                                   conditionalPanel(
                                     condition = "input.plot_type == 'density'",
                                     div(class = "plot-container",
                                         withSpinner(plotOutput("density_plot", height = "400px"),
                                                     type = 4)
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.plot_type == 'bar'",
                                     div(class = "plot-container",
                                         withSpinner(plotOutput("bar_plot", height = "400px"),
                                                     type = 4)
                                     )
                                   ),
                                   h4("Summary Statistics"),
                                   verbatimTextOutput("summary_stats")
                            )
                          )
               )
               ,
               tabPanel("A/B Test Results",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   h4("A/B Test Configuration"),
                                   selectInput("ab_treatment_var", "Treatment Variable:", choices = NULL),
                                   selectInput("ab_metric_var", "Metric Variable:", choices = NULL),
                                   textInput("control_group", "Control Group Name:", value = "control"),
                                   selectInput("test_type", "Test Type:",
                                               choices = c("t-test" = "t_test",
                                                           "z-test" = "z_test",
                                                           "Wilcoxon" = "wilcoxon",
                                                           "Bootstrap" = "bootstrap"),
                                               selected = "t_test"),
                                   sliderInput("confidence_level", "Confidence Level:",
                                               min = 0.8, max = 0.99, value = 0.95, step = 0.01),
                                   actionButton("run_ab_test", "Run A/B Test", class = "btn-primary")
                                 )
                          ),
                          column(9,
                                 h4("A/B Test Results"),
                                 
                                 # Summary Statistics Card
                                 div(class = "card",
                                     div(class = "card-header", "Summary Statistics"),
                                     div(class = "card-body",
                                         tableOutput("ab_summary_table")
                                     )
                                 ),
                                 
                                 # Results Card
                                 div(class = "card",
                                     div(class = "card-header", "Test Results"),
                                     div(class = "card-body",
                                         fluidRow(
                                           column(6,
                                                  div(class = "result-box",
                                                      h5("Effect Size (Cohen's d)"),
                                                      textOutput("effect_size_output")
                                                  ),
                                                  div(class = "result-box",
                                                      h5("Common Language Effect Size"),
                                                      textOutput("cle_output")
                                                  ),
                                                  div(class = "result-box",
                                                      h5("Test Power"),
                                                      textOutput("power_output")
                                                  )
                                           ),
                                           column(6,
                                                  div(class = "result-box",
                                                      h5("Difference (Treatment - Control)"),
                                                      textOutput("difference_output")
                                                  ),
                                                  div(class = "result-box",
                                                      h5("Relative Effect (%)"),
                                                      textOutput("relative_effect_output")
                                                  ),
                                                  div(class = "result-box",
                                                      h5("Confidence Interval"),
                                                      textOutput("ci_output")
                                                  )
                                           )
                                         ),
                                         
                                         # Hypothesis Test Result
                                         div(class = "result-box",
                                             h5("Hypothesis Test"),
                                             uiOutput("test_result_output")
                                         )
                                     )
                                 )
                          )
                        )
               ),
               
               
               tabPanel("Trend Analysis",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   h4("Trend Analysis Settings"),
                                   selectInput("trend_treatment_var", "Treatment Variable:", choices = NULL),
                                   selectInput("trend_metric_var", "Metric Variable:", choices = NULL),
                                   selectInput("date_var", "Date Variable:", choices = NULL),
                                   textInput("trend_control_group", "Control Group Name:", value = "control"),
                                   actionButton("run_trend_analysis", "Generate Trend Plot",
                                                class = "btn-primary")
                                 )
                          ),
                          column(9,
                                 h4("Metric Trend Over Time"),
                                 withSpinner(plotOutput("trend_plot", height = "500px"), type = 4),
                                 
                                 div(class = "card",
                                     div(class = "card-header", "Trend Summary"),
                                     div(class = "card-body",
                                         tableOutput("trend_summary_table")
                                     )
                                 )
                          )
                        )
               )
             )
  )
  )
)

server <- function(input, output, session) {
  
  # Authentication state
  is_authenticated <- reactiveVal(FALSE)
  user_email <- reactiveVal(NULL)
  
  # Function to verify Pathao email
  verify_pathao_email <- function(email) {
    is_valid <- grepl("@pathao\\.com$", tolower(email))
    return(is_valid)
  }
  
  # Check if we're running on Posit Connect
  is_posit_connect <- function() {
    Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps" || 
      Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect" ||
      !interactive()
  }
  
  # Google authentication handler
  authenticate_google <- function() {
    tryCatch({
      if (is_posit_connect()) {
        # On Posit Connect, use service account or manual authentication
        # For now, we'll rely on manual email input
        return(NULL)
      } else {
        # Local development - use normal Google auth
        bq_auth(
          cache = ".secrets",
          email = TRUE
        )
        return(TRUE)
      }
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Get user email from authentication (simplified for Posit Connect)
  get_user_email <- function() {
    # On Posit Connect, we can't easily get email from Google auth
    # So we'll use the manual input or a simulated approach
    if (is_posit_connect()) {
      # Return the manually entered email or a placeholder
      if (!is.null(input$manual_email) && nzchar(input$manual_email)) {
        return(input$manual_email)
      }
      return("user@pathao.com")  # Placeholder for demo
    } else {
      # Local development - try to get email from Google auth
      tryCatch({
        token <- gargle::token_fetch()
        if (!is.null(token$email)) return(token$email)
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    }
  }
  
  # Manual authentication handler for Posit Connect
  observeEvent(input$manual_auth, {
    email <- trimws(input$manual_email)
    
    if (nzchar(email)) {
      if (verify_pathao_email(email)) {
        # Success - Pathao email verified
        is_authenticated(TRUE)
        user_email(email)
        hide("auth_ui")
        show("main_ui")
        shinyalert("Welcome!", paste("Authenticated as:", email), type = "success")
      } else {
        # Not a Pathao email
        shinyalert("Access Denied", 
                   paste("Only @pathao.com emails are allowed.\n",
                         "Please use your Pathao email address."), 
                   type = "error")
      }
    } else {
      shinyalert("Error", "Please enter your @pathao.com email address.", type = "error")
    }
  })
  
  # Google authentication button handler
  observeEvent(input$auth_btn, {
    showModal(modalDialog(
      title = "Google Authentication",
      tags$div(
        if (is_posit_connect()) {
          tags$p("On Posit Connect, please use the manual email input below for authentication.")
        } else {
          tagList(
            tags$p("Please sign in with your Pathao Google account (@pathao.com)"),
            tags$p("1. A browser window will open for Google authentication"),
            tags$p("2. Sign in with your @pathao.com account"),
            tags$p("3. Grant BigQuery access permissions"),
            tags$p("4. Return to this window")
          )
        }
      ),
      footer = modalButton("Close"),
      easyClose = FALSE
    ))
    
    if (is_posit_connect()) {
      # On Posit Connect, suggest using manual authentication
      removeModal()
      shinyalert("Info", 
                 "For Posit Connect deployment, please use the manual email input below the Google sign-in button.", 
                 type = "info")
      return()
    }
    
    tryCatch({
      # Local development - use Google auth
      success <- authenticate_google()
      
      if (success) {
        email <- get_user_email()
        
        if (!is.null(email) && verify_pathao_email(email)) {
          # Success
          removeModal()
          is_authenticated(TRUE)
          user_email(email)
          hide("auth_ui")
          show("main_ui")
          shinyalert("Welcome!", paste("Authenticated as:", email), type = "success")
        } else {
          removeModal()
          shinyalert("Authentication Error", 
                     "Could not verify Pathao email. Please try again.", 
                     type = "error")
        }
      } else {
        removeModal()
        shinyalert("Authentication Failed", 
                   "Google authentication failed. Please try again or use manual email input.", 
                   type = "error")
      }
      
    }, error = function(e) {
      removeModal()
      shinyalert("Authentication Failed", 
                 paste("Error:", e$message), 
                 type = "error")
    })
  })
  
  # Display user email in the app
  output$user_info <- renderUI({
    req(user_email())
    tags$div(
      style = "position: absolute; top: 10px; right: 10px; background: #f8f9fa; padding: 8px 15px; border-radius: 20px; border: 1px solid #ddd;",
      tags$span(icon("user"), "Logged in as: ", tags$strong(user_email()))
    )
  })
  
  # Add user info to main UI
  insertUI(
    selector = "#main_ui",
    where = "afterBegin",
    ui = uiOutput("user_info")
  )
  
  # For Posit Connect, we need to handle BigQuery authentication differently
  # Use service account or other authentication methods
  
  # YOUR EXISTING SERVER CODE STARTS HERE
  # Reactive values
  query_results <- reactiveVal(NULL)
  randomized_data <- reactiveVal(NULL)
  
  # Modified data_loader function for Posit Connect
  data_loader <- function(project_id, query) {
    if (is_posit_connect()) {
      # On Posit Connect, you'll need to use service account authentication
      # This requires setting up a service account JSON key
      shinyalert("Info", "BigQuery access on Posit Connect requires service account setup. Please contact administrator.", type = "info")
      return(data.frame())  # Return empty dataframe
    } else {
      # Local development - use normal authentication
      bq_auth()
      job <- bq_project_query(project_id, query)
      results <- bq_table_download(job)
      return(results)
    }
  }
  
  # SQL Runner Tab
  observeEvent(input$run_query, {
    req(input$project_id, input$sql_query)
    
    if (!nzchar(input$sql_query)) {
      showNotification("Please enter a SQL query", type = "warning")
      return()
    }
    
    sql_query <- input$sql_query
    if (input$limit_rows > 0 && !grepl("\\bLIMIT\\s+\\d+\\s*$", sql_query, ignore.case = TRUE)) {
      sql_query <- paste(sql_query, "LIMIT", input$limit_rows)
    }
    
    showNotification("Running query...", type = "message", duration = NULL)
    
    tryCatch({
      results <- data_loader(input$project_id, sql_query)
      query_results(results)
      removeNotification("running")
      showNotification(sprintf("Query completed. %d rows returned.", nrow(results)),
                       type = "message", duration = 3)
    }, error = function(e) {
      removeNotification("running")
      showNotification(sprintf("Error: %s", e$message), type = "error")
      query_results(NULL)
    })
  })
  
  # ... [KEEP THE REST OF YOUR EXISTING SERVER CODE] ...
  output$results_table <- renderDT({
    req(query_results())
    datatable(query_results(), options = list(scrollX = TRUE, scrollY = 400))
  })
  
  output$query_info <- renderText({
    results <- query_results()
    if (is.null(results)) return("No query results to display. Run a query to see results here.")
    sprintf("Query returned %d rows and %d columns. Memory usage: %s",
            nrow(results), ncol(results),
            format(object.size(results), units = "MB"))
  })
  
  # Randomizer Tab
  output$arm_proportion_ui <- renderUI({
    arm_no <- input$arm_no
    proportions <- rep(1/arm_no, arm_no)
    lapply(1:arm_no, function(i) {
      numericInput(paste0("arm_prop_", i), paste0("Arm ", i, " Proportion:"),
                   value = proportions[i], min = 0, max = 1, step = 0.1)
    })
  })
  
  output$arm_name_ui <- renderUI({
    arm_no <- input$arm_no
    lapply(1:arm_no, function(i) {
      textInput(paste0("arm_name_", i), paste0("Arm ", i, " Name:"), value = paste0("arm_", i))
    })
  })
  
  output$confounders_ui <- renderUI({
    req(query_results())
    data <- query_results()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    selectInput("confounders", "Confounders (numeric columns):",
                choices = numeric_cols, multiple = TRUE)
  })
  
  observeEvent(input$run_randomizer, {
    req(query_results(), input$arm_no)
    
    # Collect arm proportions and names
    arm_proportion <- sapply(1:input$arm_no, function(i) {
      input[[paste0("arm_prop_", i)]]
    })
    
    arm_name <- sapply(1:input$arm_no, function(i) {
      input[[paste0("arm_name_", i)]]
    })
    
    if (sum(arm_proportion) != 1) {
      showNotification("Arm proportions must sum to 1!", type = "error")
      return()
    }
    
    tryCatch({
      randomized <- data_maker(
        experiment_data = query_results(),
        arm_proportion = arm_proportion,
        arm_no = input$arm_no,
        arm_name = arm_name,
        experiment_name = input$experiment_name,
        use_smd = input$use_smd,
        confounders = input$confounders,
        smd_threshold = input$smd_threshold
      )
      
      randomized_data(randomized)
      showNotification("Randomization completed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Randomization error:", e$message), type = "error")
    })
  })
  
  output$randomized_table <- renderDT({
    req(randomized_data())
    datatable(randomized_data(), options = list(scrollX = TRUE, scrollY = 400))
  })
  
  # Balance Checker Tab
  output$confounders_balance_ui <- renderUI({
    req(randomized_data())
    data <- randomized_data()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    selectInput("confounders_balance", "Confounders for Balance Check:",
                choices = numeric_cols, multiple = TRUE)
  })
  
  observe({
    req(randomized_data())
    data <- randomized_data()
    updateSelectInput(session, "treatment_var",
                      choices = names(data)[sapply(data, function(x) is.character(x) | is.factor(x))])
  })
  
  observeEvent(input$run_balance_check, {
    req(randomized_data(), input$treatment_var, input$confounders_balance)
    
    tryCatch({
      # SMD Check
      balance_result <- smd_checker(
        experiment_data = randomized_data(),
        treatment = input$treatment_var,
        confounders = input$confounders_balance
      )
      
      # SRM Check
      treatment_counts <- table(randomized_data()[[input$treatment_var]])
      expected_props <- prop.table(treatment_counts)
      srm_result <- check_srm(
        experiment_df = randomized_data(),
        allocated_arm_col = input$treatment_var,
        expected_proportions = expected_props
      )
      
      output$love_plot <- renderPlot({
        balance_result$love_plot
      })
      
      output$srm_result <- renderText({
        srm_result
      })
      
      output$balance_table <- renderPrint({
        balance_result$balance_tab
      })
      
    }, error = function(e) {
      showNotification(paste("Balance check error:", e$message), type = "error")
    })
  })
  
  # Distribution Checker Tab
  observe({
    req(randomized_data())
    data <- randomized_data()
    
    # Update treatment variable choices
    updateSelectInput(session, "dist_treatment_var",
                      choices = names(data)[sapply(data, function(x) is.character(x) | is.factor(x))])
    
    # Update target variable choices
    updateSelectInput(session, "dist_target_var",
                      choices = names(data))
  })
  
  # Variable information
  output$var_info <- renderText({
    req(randomized_data(), input$dist_target_var)
    data <- randomized_data()
    var <- data[[input$dist_target_var]]
    
    info <- paste(
      "Variable:", input$dist_target_var, "\n",
      "Type:", class(var), "\n",
      "Missing values:", sum(is.na(var)), "\n",
      "Unique values:", length(unique(var)), "\n"
    )
    
    if (is.numeric(var)) {
      info <- paste0(info,
                     "Range: [", min(var, na.rm = TRUE), ", ", max(var, na.rm = TRUE), "]\n",
                     "Mean: ", round(mean(var, na.rm = TRUE), 2), "\n",
                     "SD: ", round(sd(var, na.rm = TRUE), 2)
      )
    } else if (is.character(var) || is.factor(var)) {
      top_categories <- names(sort(table(var), decreasing = TRUE))[1:3]
      info <- paste0(info,
                     "Top categories: ", paste(top_categories, collapse = ", ")
      )
    }
    
    return(info)
  })
  
  observeEvent(input$run_dist_check, {
    req(randomized_data(), input$dist_treatment_var, input$dist_target_var)
    
    tryCatch({
      data <- randomized_data()
      target_var <- data[[input$dist_target_var]]
      
      # Auto-detect plot type if not specified
      plot_type <- input$plot_type
      if (is.null(plot_type)) {
        plot_type <- ifelse(is.numeric(target_var), "density", "bar")
      }
      
      if (plot_type == "density" && !is.numeric(target_var)) {
        showNotification("Density plots require numeric variables. Switching to bar chart.",
                         type = "warning")
        plot_type <- "bar"
        updateRadioButtons(session, "plot_type", selected = "bar")
      }
      
      if (plot_type == "bar" && is.numeric(target_var)) {
        # For numeric variables in bar charts, we can bin them
        data <- data %>%
          mutate(!!input$dist_target_var := cut(!!rlang::parse_expr(input$dist_target_var),
                                                breaks = 10, include.lowest = TRUE))
      }
      
      if (plot_type == "density") {
        output$density_plot <- renderPlot({
          density_checker(data, input$dist_treatment_var, input$dist_target_var)
        })
      } else {
        output$bar_plot <- renderPlot({
          bar_chart_checker(data, input$dist_treatment_var, input$dist_target_var)
        })
      }
      
      # Summary statistics
      output$summary_stats <- renderPrint({
        cat("Summary for:", input$dist_target_var, "\n")
        cat("By treatment group:", input$dist_treatment_var, "\n\n")
        
        if (is.numeric(target_var)) {
          summary_df <- data %>%
            group_by(!!rlang::parse_expr(input$dist_treatment_var)) %>%
            summarise(
              N = n(),
              Mean = mean(!!rlang::parse_expr(input$dist_target_var), na.rm = TRUE),
              SD = sd(!!rlang::parse_expr(input$dist_target_var), na.rm = TRUE),
              Min = min(!!rlang::parse_expr(input$dist_target_var), na.rm = TRUE),
              Max = max(!!rlang::parse_expr(input$dist_target_var), na.rm = TRUE)
            )
          print(summary_df, n = Inf)
        } else {
          summary_df <- data %>%
            group_by(!!rlang::parse_expr(input$dist_treatment_var),
                     !!rlang::parse_expr(input$dist_target_var)) %>%
            summarise(Count = n(), .groups = "drop") %>%
            group_by(!!rlang::parse_expr(input$dist_treatment_var)) %>%
            mutate(Percentage = round(Count / sum(Count) * 100, 1))
          print(summary_df, n = Inf)
        }
      })
      
    }, error = function(e) {
      showNotification(paste("Distribution check error:", e$message), type = "error")
    })
  })
  
  # Auto-update plot type based on variable type
  observe({
    req(randomized_data(), input$dist_target_var)
    data <- randomized_data()
    var <- data[[input$dist_target_var]]
    
    if (is.numeric(var)) {
      updateRadioButtons(session, "plot_type", selected = "density")
    } else if (is.character(var) || is.factor(var)) {
      updateRadioButtons(session, "plot_type", selected = "bar")
    }
  })
  
  
  observe({
    req(randomized_data())
    data <- randomized_data()
    
    updateSelectInput(session, "ab_treatment_var",
                      choices = names(data)[sapply(data, function(x) is.character(x) | is.factor(x))])
    
    updateSelectInput(session, "ab_metric_var",
                      choices = names(data)[sapply(data, is.numeric)])
  })
  
  observeEvent(input$run_ab_test, {
    req(randomized_data(), input$ab_treatment_var, input$ab_metric_var)
    
    tryCatch({
      results <- calculate_ab_test_results(
        data = randomized_data(),
        treatment_col = input$ab_treatment_var,
        metric_col = input$ab_metric_var,
        control_group = input$control_group,
        test_type = input$test_type,
        confidence_level = input$confidence_level
      )
      
      # Summary table
      output$ab_summary_table <- renderTable({
        results$summary
      }, digits = 3)
      
      # Individual result outputs
      output$effect_size_output <- renderText({
        sprintf("%.3f", results$effect_size)
      })
      
      output$cle_output <- renderText({
        sprintf("%.1f%%", results$common_language_es * 100)
      })
      
      output$power_output <- renderText({
        sprintf("%.1f%%", results$power * 100)
      })
      
      output$difference_output <- renderText({
        sprintf("%.3f", results$difference)
      })
      
      output$relative_effect_output <- renderText({
        sprintf("%.1f%%", results$relative_effect * 100)
      })
      
      output$ci_output <- renderText({
        sprintf("[%.3f, %.3f]", results$confidence_interval[1], results$confidence_interval[2])
      })
      
      # Hypothesis test result
      output$test_result_output <- renderUI({
        p_value <- results$test_result$p.value[1]
        statistic <- results$test_result$statistic[1]
        test_name <- switch(input$test_type,
                            "t_test" = "t-test",
                            "z_test" = "z-test",
                            "wilcoxon" = "Wilcoxon rank-sum test",
                            "bootstrap" = "Bootstrap (with t-test p-value)")
        
        significance <- ifelse(p_value < 0.05, "significant", "not significant")
        significance_class <- ifelse(p_value < 0.05, "significant", "not-significant")
        
        tagList(
          p(strong("Test:"), test_name),
          p(strong("Statistic:"), sprintf("%.3f", statistic)),
          p(strong("P-value:"), sprintf("%.4f", p_value)),
          p(strong("Conclusion:"),
            span(significance, class = significance_class))
        )
      })
      
    }, error = function(e) {
      showNotification(paste("A/B Test error:", e$message), type = "error")
    })
  })
  
  # Trend Analysis Tab
  observe({
    req(randomized_data())
    data <- randomized_data()
    
    updateSelectInput(session, "trend_treatment_var",
                      choices = names(data)[sapply(data, function(x) is.character(x) | is.factor(x))])
    
    updateSelectInput(session, "trend_metric_var",
                      choices = names(data)[sapply(data, is.numeric)])
    
    # Find date columns
    date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date") |
                                      inherits(x, "POSIXt"))]
    updateSelectInput(session, "date_var", choices = date_cols)
  })
  
  observeEvent(input$run_trend_analysis, {
    req(randomized_data(), input$trend_treatment_var, input$trend_metric_var, input$date_var)
    
    tryCatch({
      output$trend_plot <- renderPlot({
        create_trend_plot(
          data = randomized_data(),
          treatment_col = input$trend_treatment_var,
          metric_col = input$trend_metric_var,
          date_col = input$date_var,
          control_group = input$trend_control_group
        )
      })
      
      # Trend summary table
      output$trend_summary_table <- renderTable({
        randomized_data() %>%
          group_by(!!rlang::parse_expr(input$trend_treatment_var)) %>%
          summarise(
            Mean = mean(!!rlang::parse_expr(input$trend_metric_var), na.rm = TRUE),
            SD = sd(!!rlang::parse_expr(input$trend_metric_var), na.rm = TRUE),
            N = n(),
            .groups = "drop"
          )
      }, digits = 3)
      
    }, error = function(e) {
      showNotification(paste("Trend analysis error:", e$message), type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


