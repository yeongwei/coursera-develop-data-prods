require(shiny)
require(ggplot2)
require(rCharts)

CALC_NOW <- list(
  title = function() { "Calculate Now" },
  heading = function() { h1("Fill in Your Mortgage Information") },
  inputPropertyValue = function() { numericInput('propValI', 'Property Value ($):', value = 300000, min = 0) },
  inputDownPayment = function() { numericInput('downPayI', 'Down Payment ($):', value = 30000, min = 0) },
  inputPaymentStartDte = function() { dateInput("startDateI", "Repayment Start Date:", value = Sys.Date()) },
  inputInterestRate = function() { sliderInput('interestI', 'Loan Interest rate (%):', value = 5.00, min = 0.00, max = 30.00, step = 0.01) },
  inputYears2Complete = function() { numericInput('yearsPayI', 'Years To Complete:', value = 20, min = 0) }
  )

shinyUI(
  navbarPage("Mortgage Calculator",
    tabPanel("Calculate Now",
      fluidPage(
        CALC_NOW$heading(),
        fluidRow(
          column(6, align="left", 
              CALC_NOW$inputPropertyValue(), 
              CALC_NOW$inputDownPayment(),
              CALC_NOW$inputPaymentStartDte() ),
          column(6, align="left",
            CALC_NOW$inputInterestRate(),
            CALC_NOW$inputYears2Complete(),
            submitButton('Calculate', width = "100px")
          )
        ), hr(), br(),
        h1("Mortgage Calculation Results"), br(),
        tabsetPanel(
          tabPanel("Summary", br(),
            fluidRow(
              column(4, strong("Property Value ($):")), column(4, textOutput("propValO"))
            ),
            fluidRow(
              column(4, strong("Down Payment ($):")), column(4, textOutput("downPayO"))
            ),
            fluidRow(
              column(4, strong("Down Payment (%):")), column(4, textOutput("downPayPercentO"))
            ),
            fluidRow(
              column(4, strong("Principle ($):")), column(4, textOutput("principleO"))
            ),
            fluidRow(
              column(4, strong("Loan Interest rate (%):")), column(4, textOutput("interestO"))
            ),
            fluidRow(
              column(4, strong("Years to Complete Payment:")), column(4, textOutput("yearsPayO"))
            ),            
            fluidRow(
              column(4, strong("Payment Start Date:")), column(4, textOutput("startDateO"))
            ),
            fluidRow(
              column(4, strong("Payment End Date:")), column(4, textOutput("endDateO"))
            ),
            fluidRow(
              column(4, strong("Payment Per Month ($):")), column(4, textOutput("payPerMonthO"))
            ),
            fluidRow(
              column(4, strong("Total Interest Payment of Loan ($):")), column(4, textOutput("ttlInterestO"))
            ),
            fluidRow(
              column(4, strong("Total Payment of Loan ($):")), column(4, textOutput("ttlPaymentO"))
            ), hr()
          ), 
          tabPanel("Annual Amortization Table", br(), dataTableOutput("annualTbl")), 
          tabPanel("Monthly Amortization Table", br(), dataTableOutput("monthlyTbl")),
          tabPanel("Amortization Chart",
            h4("Mortgage Payment Chart by Month", align = "center"),
            h5("Hover over to see Interest and Principle values.", align ="center"),
            p("Payment ($)"),
            showOutput("pymtChart", "morris"),
            p("Date", align = "center")
          )
        )
      )
    ),
    tabPanel("How to Use",
      h1("Please use the reference below to operate the application"), br(),
      tags$ol(
        tags$li("Navigate into the main application page via the \"Calculate Now\" navigation tab."), br(),
        tags$li("Within the main application page, fill in the required input sections"), br(),
        tags$li("Click on the \"Calculate\" button for the application to perform necessary computations"), br(),
        tags$li("View the result tabs under the heading \"Mortgage Calculation Results\""),
        tags$ul(
          tags$li("\"Summary\" - A summary of the mortgage information with calculated monthly repayment value."),
          tags$li("\"Annual Armotization Table\" - Displays the estimated interest, principle payment values and remaining balance by YEAR."),
          tags$li("\"Monthly Armotization Table\" - Displays the estimated interest, principle payment values and remaining balance by MONTH."),
          tags$li("\"Amortization Chart\" -  Displays a Mortgage Payment Chart.")
        ), br(),
        tags$li("To repeat computation, please restart from step #1.")
      )
    ),
    tabPanel("About",
      h1("Overview"),
      p("This Shiny Application is a Course Project submission for the Data Science Specialization conducted by Cousera along with the John Hopkins University."),
      p("The main objective of this Application is to allow user to compute and view the mortgage repayments in a burnt-down fashion both annually and monthly."),
      p("The formula for the calculation is adopted from \"http://www.hughcalc.org/formula.php\"."),
      h1("Author"), 
      p("YeongWei <yeongwei@gmail.com>"),
      h1("Last Updated"), p("7th February 2016")
    )
  )  
)