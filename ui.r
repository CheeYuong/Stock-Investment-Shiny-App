library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Stock Investment Calculator"),
        sidebarPanel(
                h4('Basic Information'),
                textInput('text','Company Name', value=''),
                numericInput('Price','Current Share Price ($)', 13.30, min = 0.00, max = 10000.00),
                numericInput('PurchasePrice','Purchase Price ($)', 11.29),
                numericInput('Amount','Amount of Share Purchase (Quantity)', 10000),
                numericInput('Fees','Total Transaction Fees ($)', 15.00),
                
                h4('Share Information'),
                numericInput('Preferred', 'Preferred Share Capital', 10000000),
                numericInput('Ordinary','Ordinary Share Capital', 44100000),
                numericInput('dividends','Dividends on Preferred Stock ($)', 200000),
                numericInput('RiskFreeRate','Risk Free Rate (%)', 3.81),
                numericInput('StockBeta','Beta of the share', 1.21),
                numericInput('MarketReturn','Expected Market Return (%)', 10),
                
                h4('Values'),
                numericInput('Revenues', 'Revenues ($ per year)', 30000000),
                numericInput('Expenses', 'Expenses ($ per year)', 1000000),
                numericInput('NetIncome', 'Net Income ($ per year)', 5000000),
                numericInput('TotalAssets', 'Total Assets ($ per year)', 80000000),
                numericInput('TotalLiabilities','Total Liabilities ($ per year)', 2000000),
                numericInput('CurrentLiabilities', 'Current Liabilities ($ per year)', 1500000),
                numericInput('OperatingCash','Net Operating Cash ($ per year)', 3000000)
        ),
        mainPanel(
                h3('Instructions'),
                hr(),
                helpText("The application is designed for the stock investor to calculate the ratios, to measure the market risks in order to know the expected share return and to estimate the profit or loss from the invested share. 
                          These information are not to be relied upon as authoritative or taken in substitution for the exercising of judgements by any users."),
                helpText("You are advised to consult and to discuss with your broker or any professionals to deal with the numbers before making any investment decision."),
                hr(),
                h3('Valuation with Financial Ratios'),
                hr(),
                h4('Earnings Before Interests, Taxes, Depreciation and Amortization (EBITDA)'),
                textOutput("EBITDA"),
                h4('Earnings Per Share (EPS)'),
                textOutput("EPS"),
                h4('Price-Earnings Ratio (P/E Ratio)'),
                textOutput("PE"),
                h4('Earnings Yield'),
                textOutput("EarningsYield"),
                h4('Return on Assets'),
                textOutput("ROA"),
                h4('Return on Common Equity'),
                textOutput("ROCE"),
                h4('Cash Flow Per Share'),
                textOutput("CashFlowPerShare"),
                hr(),
                h3('Measurement of Market Risks with Capital Asset Pricing Model (CAPM)'),
                hr(),
                h4('Expected Security Return'),
                textOutput("ExpectedReturn"),
                hr(),
                hr(),
                h3('Your Investment Return'),
                hr(),
                h4('Comparison between Purchase Price and Current Share Price'),
                hr(),
                textOutput("PriceComparison"),
                br(),
                h4('Comparison between Real Return and Expected Security Return'),
                hr(),
                textOutput("ExpectedComparison"),
                hr(),
                br(),
                br(),
                br(),
                br(),
                br(),
                helpText("Note:"),
                helpText("Risk Free rate is the yield on high quality government bonds.")
        )
        
))
