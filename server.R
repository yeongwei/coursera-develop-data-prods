require(shiny)
require(lubridate)
require(rCharts)

calcMonthlyPayment <- function(principle, monthlyInterest, monthsToPay) {
  return(round(principle * (monthlyInterest/(1-(1+monthlyInterest)^(-monthsToPay))), digits = 2))
}

createMonthlyTable <- function(principle, monthlyInterest, monthlyPymt, startD, endD) { 
  # Calculate Mortage
  monthlyMortgage <- data.frame()
  balance <- principle
  while(round(balance, digits = 2) != 0.00) {
    cuMonthInterest <- balance * monthlyInterest
    cuMonthPrinciple <- monthlyPymt - cuMonthInterest
    balance <- balance - cuMonthPrinciple
    balance <- ifelse(balance < 0, 0.00, balance)
    monthlyMortgage <- rbind(monthlyMortgage, c(cuMonthInterest, cuMonthPrinciple, balance))
  }
  monthlyMortgage <- round(monthlyMortgage, digits = 2)
  
  # Create Date columns
  yearMonthSeq <- seq(startD, endD, by="month")
  if(length(yearMonthSeq)-1 == nrow(monthlyMortgage)) {
    yearMonthSeq <- yearMonthSeq[-length(yearMonthSeq)]
  }
  if(length(yearMonthSeq) != nrow(monthlyMortgage)) {
    stop(paste0("nrow for yearMonthSeq = ",
      length(yearMonthSeq),
      " and monthlyMortgage = ",
      nrow(monthlyMortgage),
      " are not the same"))
  } 
  yearMonthSeqStr <- do.call(rbind, strsplit(format(yearMonthSeq, "%Y-%b"), "-"))
  
  #Return MonthlyTable
  return(data.frame(
    DateByMonth = yearMonthSeq,
    Year = as.integer(yearMonthSeqStr[,1]),
    Month = as.character(yearMonthSeqStr[,2]),
    Interest = monthlyMortgage[,1],
    Principle = monthlyMortgage[,2],
    Balance = monthlyMortgage[,3],
    stringsAsFactors = F))
}

createAnnualTable <- function(monthlyTbl) {
  return(cbind(
    aggregate(. ~ Year, data = monthlyTbl[, c("Year", "Interest", "Principle")], sum),
    Balance = (aggregate(. ~ Year, data = monthlyTbl[, c("Year", "Balance")], min))$Balance))
}

filterTableByMonth <- function(monthlyTbl, dispYear) {
  return(monthlyTbl[monthlyTbl$Year == dispYear,])
}

shinyServer(
  function(input, output) {
    # Reactive variables
    propVal <- reactive({input$propValI})
    downPay <- reactive({input$downPayI})
    principle <- reactive({propVal() - downPay()})
    interest <- reactive({input$interestI})
    yearsToPay <- reactive({as.numeric(input$yearsPayI)})
    startD <- reactive({input$startDateI})
    endD <- reactive({startD() + years(yearsToPay())})
    monthlyInterest <- reactive({interest()/(12*100)})
    monthsToPay <- reactive({yearsToPay() * 12})
    monthlyPayment <- reactive({calcMonthlyPayment(principle(), monthlyInterest(), monthsToPay())})
    monthlyTable <- reactive({createMonthlyTable(principle(), monthlyInterest(), monthlyPayment(), startD(), endD())})
    annualTable <- reactive({createAnnualTable(monthlyTable())})
    
    ## Summary Output
    output$propValO <- renderText({propVal()})
    output$downPayO <- renderText({downPay()})
    output$downPayPercentO <- renderText({round(downPay()/propVal() * 100, digits = 2)})
    output$principleO <- renderText({principle()})
    output$interestO <- renderText({interest()})
    output$yearsPayO <- renderText({yearsToPay()})
    output$startDateO <- renderText({format(startD(), "%d %b %Y")})
    output$endDateO <- renderText({format(endD(), "%d %b %Y")})
    output$payPerMonthO <- renderText({monthlyPayment()})
    output$ttlInterestO <- renderText({sum(annualTable()$Interest)})
    output$ttlPaymentO <- renderText({sum(annualTable()$Interest) + sum(annualTable()$Principle)})
    
    ## Display Annual Table
    output$annualTbl <- renderDataTable({annualTable()})
    
    ## Display Monthly Table
    output$monthlyTbl <- renderDataTable({monthlyTable()[, -1]})
    
    ## Visualisation Output
    output$pymtChart <- renderChart({
      monthlyTableTrans <- monthlyTable()
      monthlyTableTrans$DateByMonth <- as.character(monthlyTableTrans$DateByMonth)
      p1 <- mPlot(x = "DateByMonth", y = c("Interest", "Principle"), type = "Line", data = monthlyTableTrans)
      p1$set(pointSize = 0, lineWidth = 1)
      p1$addParams(dom = 'pymtChart')
      return(p1)
    })
  }
)