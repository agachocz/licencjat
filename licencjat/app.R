library(quantmod)
library(shiny)
library(GA)
library(dplyr)

#wczytanie listy nazw i symboli spółek WIG30
sym <- read.csv("symbols.csv", header=TRUE, sep=";", fileEncoding = "UTF-8-BOM")
names(sym) <- c("name", "symbol")


getData <- function(symbols){
  
      file <- read.csv("not.csv", header=TRUE, sep=",", fileEncoding = "UTF-8-BOM")
      data <- file[symbols]
  
  return(data)
  
}

getReturns <- function(data){
  #liczenie stop zwrotu
  nAct <- dim(data)[2] #liczba sp??ek
  nNot <- dim(data)[1] #liczba notowa?
  ret <- matrix(0, nrow=nNot-1, ncol=nAct)
  
  for(i in 1:nNot-1){
    for(j in 1:nAct){
      ret[i,j] <- (as.numeric(data[i+1, j])-as.numeric(data[i, j]))/as.numeric(data[i, j])
    }
  }
  
  return(ret)
}


#lambda - skłonność do ryzyka
#n - preferowana liczba akcji
compute <- function(lambda, n, data, r, stNames){
  
  
  ret <- getReturns(data)
  
  #warto?ci oczekiwane
  e <- vector()
  for(i in 1:ncol(ret)){
    e[i] <- mean(ret[,i]) 
  }
  
  #macierz kowariancji
  cov.mat <- cov(ret)
  
  #funkcja celu
  f <- function(w){     #w - udziały kolejnych akcji
    
    v <- w[w >= 0]
    
    if(length(v) == 0) return(0)
    else {

      e2 <- e[w >= 0]
      cov.mat2 <- cov.mat[w >= 0, w >= 0]
      
      #warto?? oczekiwana portfela
      ePort <- t(e2)%*%v
      
      #wariancja portfela
      varPort <- t(v)%*%cov.mat2%*%v
      
      #wynikowa warto?? portfela
      result <- (1-lambda)*ePort + lambda*varPort   #przemyśleć ten wzór / albo sprawdzić
      
      #funkcja kary
      sumPun <- abs(sum(v) - 100) #za złą sumę udziałów  
      lenPun <- abs(length(v)-n)  #za złą liczbę akcji
      
      #ostateczny wynik
      return(result - 3*sumPun - 10*lenPun)
      
    }
  }

  #ograniczenia
  minV <- rep(-100, length(stNames))
  maxV <- rep(100, length(stNames))
  
  #wykorzystanie algorytmu genetycznego
  GA <- ga(type = "real-valued", 
           fitness =  f,
           min = minV,
           max = maxV, 
           popSize = 100, maxiter = 600, run = 100)
  
  #wybór tylko pierwszego rozwiązania 
  #(jeśli jest kilka równie dobrych)
  x <- GA@solution
  x <- x[1,]
  
  formatted <- formatResult(x, r, stNames)
  
  return(data.frame(formatted, stats(formatted, e, cov.mat)))
  
}

#formatowanie wyniku
#skład ma się sumować do 100
#opcjonalne zaokrąglanie
formatResult <- function(x, r, stNames){
  
  dig <- 2
  x <- ifelse(x < 0, 0, x)
  if(r == TRUE) {dig <- 0}
  
  maxID <- 1
  for(i in 1:length(x)){
    if(x[i] > x[maxID]) maxID <- i
    x[i] <- round(x[i], digits=dig)
  }
  x[maxID] <- x[maxID] + 100-sum(x)
  
  frame <- as.data.frame(t(x))
  names(frame) <- stNames
  
  return(frame)
}

stats <- function(x, e, cov.mat){
  
  e2 <- e[x >= 0]
  cov.mat2 <- cov.mat[x >= 0, x >= 0]  
  x <- x[x >= 0]
  
  #warto?? oczekiwana portfela
  ePort <- t(e2)%*%x
  
  #wariancja portfela
  varPort <- t(x)%*%cov.mat2%*%x
  
  stats <- data.frame(ePort, varPort)
  names(stats) <- c("wartość oczekiwana", "wariancja")
  
  return(stats)
}


# Interfejs użytkownika
ui <- shinyUI(navbarPage( "Konstruowanie portfela inwestycyjnego",
  
  tabPanel("Dane",
      sidebarLayout(
        sidebarPanel(
               
               selectizeInput(
                 "stocks", "Spółki", choices = sym[,1], multiple = TRUE
               ),
               radioButtons("dataType", NULL, c("Ceny akcji", "Stopy zwrotu")),
               sliderInput("days", "Przedział czasowy",
                           min = 1, max = 250,
                           value = c(1,250))

           ),
        
        mainPanel(
          h2("Wykres stóp zwrotu"),
          plotOutput("stocksPlot")
        )
        )),
                          
                          
  tabPanel("Wyniki",
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("lamb",
                  "Skłonność do ryzyka: [%]",
                  min = 0,
                  max = 100,
                  value = 50),
      
      sliderInput("n",
                  "Preferowana liczba akcji:",
                  min = 2,
                  max = 6,
                  value = 3),     
      
      checkboxInput("checkbox", "Calkowite", value = FALSE),
      
      actionButton("start", "Oblicz")
      
    ),
    
    mainPanel(
      h1("Proponowany portfel inwestycyjny"),
      h3("Skłonność do ryzyka"),
      textOutput("lambda"),
      h3("Proporcje"),
      tableOutput("solution"),
      h3("Statystyki rozwiązania"),
      tableOutput("stats"),
      tableOutput("choice")
    )
  )
)

))

server <- function(input, output) {
  
  output$choice <- renderTable({
    data.frame(input$stocks)
  })
  
  #Wykresy danych
  output$stocksPlot <- renderPlot({
    
    stocks <- input$stocks
    if(!is.null(stocks)){
        symbols <- subset(sym, is.element(sym$name, stocks))$symbol
        data <- getData(symbols)
        
        nrDays <- input$days
        data <- data[(nrDays[1]):(nrDays[2]),]
        data <- as.data.frame(data)
        
        
        if(input$dataType == "Ceny akcji"){
          
                  
          data %>% gather(key = "company", value = "price") %>%
          cbind(day = nrDays[1]:nrDays[2]) %>%
          ggplot(aes(x = day, y = price, color = company)) +
          geom_line() +
          labs(title = "Ceny akcji w badanym okresie", 
               y = "Cena akcji", x = "Dzień", color = "Spółka")
          
        } else {
          
           ret_df <- as.data.frame(getReturns(data))
          
          ret_df %>% gather(key = "company", value = "returns") %>%
            cbind(day =  nrDays[1]:(nrDays[2]-1)) %>%
            ggplot(aes(x = day, y = returns, color = company)) +
            geom_line() +
            labs(title = "Stopy zwrotu w badanym okresie", 
                 y = "Stopy zwrotu", x = "Dzień", color = "Spółka")
        }


      }
      else return(NULL)
    
  })
  
  result <- reactive({
    
    if(input$start){
      stocks <- input$stocks
      symbols <- subset(sym, is.element(sym$name, stocks))$symbol #???
      
      data <- getData(symbols)
      lambda <- isolate(input$lamb/100)   #ustalona lambda
      n <- input$n           #preferowana liczba akcji
      res <- isolate(compute(lambda, n, data, input$checkbox, stocks))  
      
      return(res)}
    
    else return(NULL)
  })  
  
  output$solution <- renderTable({
    res <- result()
    #print(is.null(res))
    if(is.null(res)){
      if(input$start){
        "Obliczenia trwają..."
      }
      else  "Jeszcze nie ma wyników"
    }
    else {
      
      return(res)
    }
  })
  
  output$lambda <- renderText({
    
    paste0(input$lamb, "%")
  })
  
  output$stats <- renderTable({
    res <- result()
    if(is.null(res)){
      if(input$start){
        "Obliczenia trwają..."
      }
      else "Jeszcze nie ma wyników"
    }
    else {
      #v <- as.vector(res@solution)
      #stats(v)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

