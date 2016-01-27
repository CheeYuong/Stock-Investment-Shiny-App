shinyServer(
        function(input,output){
                output$EBITDA <- renderText({
                        calculateEBITDA(input$Revenues,input$Expenses)
                })
                output$EPS <- renderText({
                        calculateEPS(input$NetIncome,input$dividends, input$Preferred, input$Ordinary)
                })
                output$PE <- renderText({
                        calculatePE(input$Price,input$NetIncome, input$dividends, input$Preferred, input$Ordinary)
                })
                output$EarningsYield <- renderText({
                        calculateEY(input$Price,input$NetIncome, input$dividends, input$Preferred, input$Ordinary)
                })
                output$ROA <- renderText({
                        calculateROA(input$NetIncome, input$TotalAssets)
                })
                output$ROCE <- renderText({
                        calculateROCE(input$TotalAssets,input$CurrentLiabilities, input$Revenues, input$Expenses)
                })
                output$CashFlowPerShare <- renderText({
                        calculateCashFlowPerShare(input$OperatingCash,input$Preferred,input$Ordinary)
                })
                output$ExpectedReturn <- renderText({
                        calculateExpectedShareReturn(input$RiskFreeRate,input$StockBeta,input$MarketReturn)
                })
                output$PriceComparison <- renderText({
                        calculateReturn(input$PurchasePrice,input$Price,input$Fees,input$Amount)
                })
                output$ExpectedComparison <- renderText({
                        calculateExpectedComparison(input$RiskFreeRate,input$StockBeta,input$MarketReturn,input$PurchasePrice,input$Price,input$Fees, input$Amount)
                })
        }
)
calculateEBITDA <- function(Revenues,Expenses)
{
        EBITDA = Revenues - Expenses
}
calculateEPS <- function(NetIncome, dividends, Preferred, Ordinary)
{
        EPS = (NetIncome - dividends)/(Preferred + Ordinary)
        round(EPS, digit=5)
}
calculatePE <- function(Price, NetIncome, dividends, Preferred, Ordinary)
{
        checkEPS = (NetIncome - dividends)/(Preferred + Ordinary)
        PE = Price / checkEPS
        round(PE, digit=5)
}
calculateEY <- function(Price, NetIncome, dividends, Preferred, Ordinary)
{
        checkEPS = (NetIncome - dividends)/(Preferred + Ordinary)
        EY = checkEPS / Price
        round(EY, digit=5)
}
calculateROA <- function(NetIncome,TotalAssets)
{
        ROA = NetIncome / TotalAssets
        round(ROA, digit=5)
}
calculateROCE <- function(TotalAssets, CurrentLiabilites, Revenues, Expenses)
{
        ROCE = (Revenues - Expenses) /(TotalAssets - CurrentLiabilites)
        round(ROCE, digit=5)
}
calculateCashFlowPerShare <- function(OperatingCash,Preferred,Ordinary)
{
        CashFlowPerShare = (OperatingCash - Preferred)/Ordinary
        round(CashFlowPerShare, digit=5)
}
calculateExpectedShareReturn <- function(RiskFreeRate, StockBeta, MarketReturn)
{
        ExpectedShareReturn = RiskFreeRate + StockBeta*(MarketReturn - RiskFreeRate)
        round(ExpectedShareReturn, digit=3)
}
calculateReturn <- function(PurchasePrice, Price, Fees, Amount)
{
        Return <- ((Price - PurchasePrice)*Amount) - Fees
        if (Return >0){
                sprintf("Congratulation! You have made a profit of $%.2f.", Return)
        }
        else {
                sprintf("You have made a loss of $%.2f.", Return)
        }
        
}
calculateExpectedComparison <- function(RiskFreeRate, StockBeta, MarketReturn, PurchasePrice, Price, Fees, Amount)
{
        ExpectedSecurityReturn = RiskFreeRate + StockBeta*(MarketReturn - RiskFreeRate)
        
        ActualReturn <- ((Price - PurchasePrice)*Amount) - Fees
        RateReturn <- (ActualReturn / (PurchasePrice*Amount)) * 100
        RealRateReturn <- RateReturn - ExpectedSecurityReturn
        
        if (RateReturn > ExpectedSecurityReturn){
                sprintf("Congratulation! You have made an earning rate at %.2f %% which is more than expectation.", RealRateReturn)
        }
        else {
                sprintf("You have a loss of %.2f %% compare to the expected security return.", RealRateReturn)
        }
}