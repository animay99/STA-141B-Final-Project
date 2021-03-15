library(shiny)
library(usethis)
library(riingo)
library(tidyquant)
library(ggthemes)
library(tidyverse)
library(plotly)
riingo_set_token("467089ab4c396c06831550277fbf2d9bf325c1f0")
#usethis::edit_r_environ()
#Sys.getenv("RIINGO_TOKEN")


tickerlist = as_tibble(c("batusd", "ethusd", "btcusd", "ltcusd", "trxusd")) %>%
    setNames(., "Ticker List")

interval_list = as_tibble(c("Daily", "Intra-Day", "Return")) %>%
                setNames(., "Interval")


start1 = "2019-03-01"

ui <- fluidPage(
    
    # Application title
    titlePanel("Cryptocurrency Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("tickers",
                        "Select ticker:",
                        choices = tickerlist,
                        selected = tickerlist[1,1],
                        multiple = TRUE
            ),
            
            fluidRow(column(width = 4, wellPanel(
                radioButtons("plot.type", "Plot Type", 
                             c("Daily", "Intra-Day", "Volume"))
            )),
                        
            ),
            
            dateInput("start.date",
                      "Starting Date (Applicable for Daily Data Only):",
                      start1),
        ),
        
        # Show a plot
        mainPanel(
            plotlyOutput("distPlot"),
            
        )
    )
)

server <- function(input, output) {
        
    output$distPlot = renderPlotly({
        sym = input$tickers
        s1 = input$start.date
        
        crypto.data = riingo_crypto_prices(sym, 
                                           start_date = s1,
                                           end_date = today(), 
                                           resample_frequency = "1day") %>% 
            group_by(ticker) %>% 
            select(ticker, date, open, high, low, volume)
        

        crypto.intra = riingo_crypto_prices(sym ,
                                            resample_frequency = "5min") %>% 
            group_by(ticker) 
            
        
            
        p1 = ggplot(crypto.data, aes(x = date, y = high)) +
                        geom_line() +
                        xlab("Date") +
                        ylab("High") +
                        theme_economist_white()+
                        facet_wrap(~ticker)
        p1 = ggplotly(p1)
        
        
        
        p2 = ggplot(crypto.intra, aes(x = date, y=close))+
            geom_line()+
            theme_economist_white()+
            facet_wrap(~ticker)+
            xlab("Date")+
            ylab("Intra-Day Price")
        
        p2 = ggplotly(p2)
        
        p3 = ggplot(crypto.data, aes(x = date, y = volume)) +
            geom_line() +
            xlab("Date") +
            ylab("Volume") +
            theme_economist_white()+
            facet_wrap(~ticker)
        p3 = ggplotly(p3)
        
        if(input$plot.type == "Daily"){
            print((p1))}
            else if(input$plot.type == "Intra-Day"){
                 print((p2))}
        else{
            print(p3) }
                }
        
    )
    
    
}

shinyApp(ui = ui, server = server)