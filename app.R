library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  includeCSS("www/styles.css"),
  
  titlePanel(
    tags$h1("Nominal and Effective Interest Rate & Financial Calculator", id = "main_title")
  ),
  
  tags$head(
    tags$script(src = "script.js")
  ),
  
  tabsetPanel(
    # Interest Rate Calculator Tab
    tabPanel("Interest Rate Calculator",
             sidebarLayout(
               sidebarPanel(
                 h3("Instructions"),
                 helpText("Input any two of the following values to calculate the third:"),
                 tags$ul(
                   tags$li("Nominal Rate (%): The annual interest rate before considering compounding."),
                   tags$li("Effective Rate (%): The annual interest rate after compounding."),
                   tags$li("C/Y (Compounding per Year): Number of compounding periods per year.")
                 ),
                 helpText("Leave the field you want to calculate blank or as NA."),
                 
                 numericInput("nominal_rate", "Nominal Rate (%)", value = NA, min = 0, step = 0.01),
                 numericInput("effective_rate", "Effective Rate (%)", value = NA, min = 0, step = 0.01),
                 numericInput("compounding_frequency", "C/Y (Compounding per Year)", value = NA, min = 1, step = 1),
                 
                 actionButton("compute", "CPT", class = "btn-primary"),
                 actionButton("clear", "Clear", class = "btn-secondary")
               ),
               
               mainPanel(
                 h3("Results"),
                 verbatimTextOutput("result"),
                 hr(),
                 tableOutput("memory_table")
               )
             )
    ),
    
    # Memory Tab for Interest Rate Calculator
    tabPanel("Interest Rate Memory",
             tags$div(class = "memory-container",
                      h3("Interest Rate Calculation History"),
                      downloadButton("download_interest", "Download History"),
                      tableOutput("full_memory_table")
             )
    ),
    
    # Financial Calculator Tab
    tabPanel("Financial Calculator",
             sidebarLayout(
               sidebarPanel(
                 h3("Instructions"),
                 helpText("Input four of the following values to calculate the fifth:"),
                 tags$ul(
                   tags$li("Present Value (PV): Initial investment or loan amount (negative for money paid out)"),
                   tags$li("Future Value (FV): Final amount (negative if you're paying it)"),
                   tags$li("Number of Periods (N): Total number of compounding periods"),
                   tags$li("Interest per Year (I/Y %): Annual interest rate as a percentage"),
                   tags$li("Payment (PMT): Regular payment amount (negative for payments made)")
                 ),
                 helpText("Leave the field you want to calculate blank or as NA."),
                 helpText("Note: Follow the cash flow sign convention - money received is positive, money paid out is negative."),
                 
                 div(class = "input-group",
                     numericInput("pv", "Present Value (PV)", value = NA),
                     span(class = "input-help", "?",
                          title = "Enter as negative if it's money paid out, positive if received")
                 ),
                 div(class = "input-group",
                     numericInput("fv", "Future Value (FV)", value = NA),
                     span(class = "input-help", "?",
                          title = "Enter as negative if you're paying it, positive if receiving it")
                 ),
                 div(class = "input-group",
                     numericInput("n_periods", "Number of Periods (N)", value = NA, min = 1),
                     span(class = "input-help", "?",
                          title = "Must be a positive whole number")
                 ),
                 div(class = "input-group",
                     numericInput("interest_rate", "Interest per Year (I/Y %)", value = NA),
                     span(class = "input-help", "?",
                          title = "Enter as a percentage, e.g., 5 for 5%")
                 ),
                 div(class = "input-group",
                     numericInput("payment", "Payment (PMT)", value = NA),
                     span(class = "input-help", "?",
                          title = "Enter as negative for payments made, positive for payments received")
                 ),
                 
                 actionButton("calculate_financial", "CPT", class = "btn-primary"),
                 actionButton("clear_financial", "Clear", class = "btn-secondary")
               ),
               
               mainPanel(
                 h3("Calculation Result"),
                 verbatimTextOutput("financial_result"),
                 uiOutput("validation_message"),
                 hr(),
                 h3("Time Value of Money Graph"),
                 plotOutput("time_value_graph", height = "400px")
               )
             )
    ),
    
    # Memory Tab for Financial Calculator
    tabPanel("Financial Memory",
             tags$div(class = "memory-container",
                      h3("Financial Calculation History"),
                      downloadButton("download_financial", "Download History"),
                      tableOutput("financial_memory")
             )
    )
  ),
  
  uiOutput("hidden_mode")
)

# Define the server
server <- function(input, output, session) {
  # Reactive values for calculation history
  calculations <- reactiveValues(
    interest_history = data.frame(
      Nominal_Rate = numeric(),
      Effective_Rate = numeric(),
      Compounding_Frequency = numeric(),
      stringsAsFactors = FALSE
    ),
    financial_history = data.frame(
      PV = numeric(),
      FV = numeric(),
      N = numeric(),
      IY = numeric(),
      PMT = numeric(),
      Computed = character(),
      Computed_Value = numeric(),
      stringsAsFactors = FALSE
    )
  )
  # Add validation message
  output$validation_message <- renderUI({
    if (input$calculate_financial > 0) {  # Only show after first calculation attempt
      na_count <- sum(is.na(c(input$pv, input$fv, input$n_periods, input$interest_rate, input$payment)))
      if (na_count != 1) {
        div(class = "alert alert-warning",
            "Please leave exactly one field empty to calculate its value.")
      } else if (!is.na(input$n_periods) && input$n_periods <= 0) {
        div(class = "alert alert-warning",
            "Number of periods must be positive.")
      } else if (!is.na(input$interest_rate) && input$interest_rate <= 0) {
        div(class = "alert alert-warning",
            "Interest rate must be positive.")
      }
    }
  })
  
  # Download handlers
  output$download_interest <- downloadHandler(
    filename = function() {
      paste("interest-rate-history-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(calculations$interest_history, file, row.names = FALSE)
    }
  )
  
  output$download_financial <- downloadHandler(
    filename = function() {
      paste("financial-history-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(calculations$financial_history, file, row.names = FALSE)
    }
  )
  # Title click counter
  title_clicks <- reactiveVal(0)
  
  # Handle title clicks
  observeEvent(input$main_title_click, {
    title_clicks(title_clicks() + 1)
    if (title_clicks() >= 3) {
      output$hidden_mode <- renderUI({
        tags$div(
          style = "text-align: center; margin-top: 20px;",
          tags$p("How prepping for FM makes you feel... \U1F643", 
                 style = "font-size: 18px; color: dimgray;"),
          tags$img(src = "https://pics.granto.cloud/i/035320bf-5da8-4e9b-ab2f-5fded6d223ea.jpg",
                   style = "max-width: 500px;")
        )
      })
      title_clicks(0)
    }
  })
  
  # Interest Rate Calculator Logic
  observeEvent(input$compute, {
    req(input$compounding_frequency)
    
    if (!is.na(input$nominal_rate) && !is.na(input$effective_rate)) {
      try({
        cy <- uniroot(function(cy) {
          (1 + input$nominal_rate/100/cy)^cy - 1 - input$effective_rate/100
        }, c(1, 365))$root
        
        updateNumericInput(session, "compounding_frequency", value = round(cy, 2))
        
        calculations$interest_history <- rbind(
          calculations$interest_history,
          data.frame(
            Nominal_Rate = input$nominal_rate,
            Effective_Rate = input$effective_rate,
            Compounding_Frequency = round(cy, 2)
          )
        )
      })
    } else if (!is.na(input$nominal_rate) && !is.na(input$compounding_frequency)) {
      eff_rate <- (1 + input$nominal_rate/100/input$compounding_frequency)^input$compounding_frequency - 1
      updateNumericInput(session, "effective_rate", value = round(eff_rate * 100, 4))
      
      calculations$interest_history <- rbind(
        calculations$interest_history,
        data.frame(
          Nominal_Rate = input$nominal_rate,
          Effective_Rate = round(eff_rate * 100, 4),
          Compounding_Frequency = input$compounding_frequency
        )
      )
    } else if (!is.na(input$effective_rate) && !is.na(input$compounding_frequency)) {
      nom_rate <- input$compounding_frequency * ((1 + input$effective_rate/100)^(1/input$compounding_frequency) - 1)
      updateNumericInput(session, "nominal_rate", value = round(nom_rate * 100, 4))
      
      calculations$interest_history <- rbind(
        calculations$interest_history,
        data.frame(
          Nominal_Rate = round(nom_rate * 100, 4),
          Effective_Rate = input$effective_rate,
          Compounding_Frequency = input$compounding_frequency
        )
      )
    }
  })
  
  # Financial Calculator Logic
  observeEvent(input$calculate_financial, {
    req(input$n_periods, input$interest_rate)
    
    na_count <- sum(is.na(c(input$pv, input$fv, input$payment)))
    req(na_count == 1)
    
    i <- input$interest_rate/100
    n <- input$n_periods
    computed_value <- NA
    
    if (is.na(input$pv)) {
      pv <- (-input$fv/(1 + i)^n) - (input$payment * ((1 - 1/(1 + i)^n)/i))
      updateNumericInput(session, "pv", value = round(pv, 2))
      computed_field <- "PV"
      computed_value <- pv
    } else if (is.na(input$fv)) {
      fv <- -input$pv * (1 + i)^n - input$payment * ((1 + i)^n - 1)/i
      updateNumericInput(session, "fv", value = round(fv, 2))
      computed_field <- "FV"
      computed_value <- fv
    } else if (is.na(input$payment)) {
      pmt <- -(input$fv + input$pv * (1 + i)^n) * i/((1 + i)^n - 1)
      updateNumericInput(session, "payment", value = round(pmt, 2))
      computed_field <- "PMT"
      computed_value <- pmt
    }
    
    calculations$financial_history <- rbind(
      calculations$financial_history,
      data.frame(
        PV = input$pv,
        FV = input$fv,
        N = input$n_periods,
        IY = input$interest_rate,
        PMT = input$payment,
        Computed = computed_field,
        Computed_Value = round(computed_value, 2)
      )
    )
    # Generate time value of money graph
    output$time_value_graph <- renderPlot({
      years <- 0:n
      values <- numeric(length(years))
      
      for(t in years) {
        values[t + 1] <- input$pv * (1 + i)^t +
          input$payment * ((1 + i)^t - 1)/i
      }
      
      df <- data.frame(Year = years, Value = values)
      
      ggplot(df, aes(x = Year, y = Value)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "red", size = 3) +
        theme_minimal() +
        labs(title = "Time Value of Money",
             x = "Years",
             y = "Value ($)") +
        theme(plot.title = element_text(hjust = 0.5))
    })
  })
  
  # Clear buttons logic
  observeEvent(input$clear, {
    updateNumericInput(session, "nominal_rate", value = NA)
    updateNumericInput(session, "effective_rate", value = NA)
    updateNumericInput(session, "compounding_frequency", value = 12)
  })
  
  observeEvent(input$clear_financial, {
    updateNumericInput(session, "pv", value = NA)
    updateNumericInput(session, "fv", value = NA)
    updateNumericInput(session, "n_periods", value = NA)
    updateNumericInput(session, "interest_rate", value = NA)
    updateNumericInput(session, "payment", value = NA)
    output$time_value_graph <- renderPlot(NULL)
  })
  
  # Updated table outputs with formatting
  output$full_memory_table <- renderTable({
    calculations$interest_history
  }, digits = 4)
  
  output$financial_memory <- renderTable({
    calculations$financial_history
  }, digits = 2)
  
  # Results output
  output$result <- renderText({
    if (nrow(calculations$interest_history) > 0) {
      last_calc <- tail(calculations$interest_history, 1)
      paste0(
        "Nominal Rate: ", round(last_calc$Nominal_Rate, 4), "%\n",
        "Effective Rate: ", round(last_calc$Effective_Rate, 4), "%\n",
        "Compounding Frequency: ", round(last_calc$Compounding_Frequency, 2), " times per year"
      )
    }
  })
  
  output$financial_result <- renderText({
    if (nrow(calculations$financial_history) > 0) {
      last_calc <- tail(calculations$financial_history, 1)
      paste0(
        "PV: $", round(last_calc$PV, 2), "\n",
        "FV: $", round(last_calc$FV, 2), "\n",
        "N: ", last_calc$N, " years\n",
        "I/Y: ", last_calc$IY, "%\n",
        "PMT: $", round(last_calc$PMT, 2), "\n",
        "Computed: ", last_calc$Computed
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)