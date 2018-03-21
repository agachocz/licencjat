library(quantmod)
library(shiny)
library(GA)
library(dplyr)

#wczytanie listy nazw i symboli spółek WIG30
sym <- read.csv("symbols.csv", header=TRUE, sep=";", fileEncoding = "UTF-8-BOM")
names(sym) <- c("name", "symbol")


getData <- function(symbols){
  
  #pobieranie symboli na podstawie nazw spółek
  #stNames <- symbols <- c("CDR", "KGH", "ING", "ACP")
  
  #pobieranie danych
  data <- getSymbols(paste0("WSE:",trimws(symbols[1])), verbose=TRUE, auto.assign = F, env = NULL, src='google')[,4]
  
  for(i in 2:length(symbols)){
    data <- cbind(data, getSymbols(paste0("WSE:",trimws(symbols[i])), verbose=TRUE, env = NULL, auto.assign = F, src='google')[,4])
  } 
  
  return(data)
  
}


#lambda - skłonność do ryzyka
#n - preferowana liczba akcji
compute <- function(lambda, n, data, r, stNames){
  
  #liczenie stop zwrotu
  nAct <- dim(data)[2] #liczba sp??ek
  nNot <- dim(data)[1] #liczba notowa?
  ret <- matrix(0, nrow=nNot-1, ncol=nAct)
  
  for(i in 1:nNot-1){
    for(j in 1:nAct){
      ret[i,j] <- (as.numeric(data[i+1, j])-as.numeric(data[i, j]))/as.numeric(data[i, j])
    }
  }
  
  
  #warto?ci oczekiwane
  e <- vector()
  for(i in 1:nAct){
    e[i] <- mean(ret[,i])
  }
  
  #macierz kowariancji
  cov.mat <- cov(ret)
  
  #funkcja celu
  f <- function(w){     #w - wagi kolejnych akcji
    
    v <- w[w >= 0]
    
    if(length(v) == 0) return(0)
    else {
      
      #kara za złą liczbę akcji
      lenPun <- abs(length(v)-n)
      
      e2 <- e[w >= 0]
      cov.mat2 <- cov.mat[w >= 0, w >= 0]
      
      
      #warto?? oczekiwana portfela
      ePort <- t(e2)%*%v
      
      #wariancja portfela
      varPort <- t(v)%*%cov.mat2%*%v
      
      #wynikowa warto?? portfela
      result <- (1-lambda)*ePort + lambda*varPort   #przemyśleć ten wzór / albo sprawdzić
      #funkcja kary
      pun <- max(0, sum(v) - 100)
      
      #ostateczny wynik
      return(result - 3*pun - 10*lenPun)
      
    }
  }
  #n <- 3
  minV <- rep(-100, length(stNames))
  maxV <- rep(100, length(stNames))
  
  #wykorzystanie algorytmu genetycznego
  GA <- ga(type = "real-valued", 
           fitness =  f,
           #min = c(0, 0, 0, 0, 0, 0), max = c(100, 100, 100, 100, 100, 100), - stara wersja
           min = minV,
           max = maxV, 
           popSize = 100, maxiter = 500, run = 50)
  
  #return(GA)
  
  print(length(GA@solution))
  print(length(stNames))
  
  x <- GA@solution
  
  formatted <- formatResult(x, r, stNames)
  
  #STATYSTYKI - TRZEBA UŻYWAĆ ZMODYFIKOWANYCH E I COV!!!!!!!
  return(data.frame(formatted, stats(formatted, e, cov.mat)))
  #return(data.frame(formatted, stats(formatted)))
  
}

#formatowanie wyniku
#skład ma się sumować do 100
#opcjonalne zaokrąglanie
formatResult <- function(x, r, stNames){
  
  #x <- GA@solution
  #x <- x[x >= 0]
  #x <- c(1, -7, 8, 4, 3, -5)
  #stNames <- c("a", "b", "c", "d", "r", "o")
  x <- ifelse(x < 0, 0, x)
  #r <- TRUE
  dig <- 2
  if(r == TRUE) {dig <- 0}
  
  maxID <- 1
  for(i in 1:length(x)){
    if(x[i] > x[maxID]) maxID <- i
    x[i] <- round(x[i], digits=dig)
  }
  x[maxID] <- x[maxID] + 100-sum(x)
  
  frame <- data.frame(x)
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
ui <- fluidPage(
  
  # Application title
  titlePanel("Konstruowanie portfela inwestycyjnego"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectizeInput(
        "stocks", "Spółki", choices = sym[,1], multiple = TRUE
      ),
      
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
    
    # Show a plot of the generated distribution
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$choice <- renderTable({
    data.frame(input$stocks)
    #subset(sym, is.element(sym$name, stocks))$name
    #subset(sym, is.element(sym$name, input$stocks))
    #symbols <- sym %>% select(symbols) %>% filter()
  })
  
  result <- reactive({
    
    if(input$start){
      stocks <- input$stocks
      symbols <- subset(sym, is.element(sym$name, stocks))$symbol #???
      
      data <- getData(symbols)
      #e <- getData(stocks)
      #e, cov.mat <- getData(stocks)
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


#s <- getSymbols('WSE:KGH', verbose=TRUE, src='google') 

# Run the application 
shinyApp(ui = ui, server = server)

