
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  library(readr)
  library(quanteda)
  library(R.utils)
  library(doParallel)
  library(data.table)
  library(dplyr)
  library(caTools)
  
  # setwd("c:/Ram/work/R/data/capstoneProject/project/")
  
  fetchNextNExp <- function (text, i, sep) {
    exp <- doTokenize(gsub(sep, " ", text),i)
    exp[length(exp)]
  }
  
  doBackOff <- function(text, n, dtsArr) {
    if (n == 0) {
      return(0)
    } 
    l <- rep(1, times = n)
    exp <- n
    for (i in n:1) {
      nexp <- fetchNextNExp(text, i, sep = "_")
      l[i] <- .4^exp * dtsArr[[i]][.(nexp)]$count
      exp <- exp + 1
    }
    return(sum(l))
  }
  
  doTokenize <- function(input, n = 1L) {
    tokenize(input, what = "word", remove_numbers = TRUE,
             remove_punct = TRUE, remove_separators = TRUE,
             remove_twitter = FALSE, remove_hyphens = TRUE,
             ngrams = n, simplify = TRUE)
  }
  
  findNgramMatch <- function (expn, ngm, dtsArr, est) {
    
    ngmHits <- NULL
    while(is.null(ngmHits) || (length(ngmHits) <= 0))
    {
      if (ngm == 0)
        break;
      
      ngmNext <- ngm + 1
      if (ngmNext > length(dtsArr))
        break

      incProgress(.20*ngm, detail=paste0("searching ",ngm, " gram..."))
      
      expnTks <- doTokenize(expn, ngm)
      if (length(expnTks) <= 0)
      {  
        expnTks <- doTokenize(expn)
        ngm <- 1
        ngmNext <- 2
      }  
      
      ngmRegex <- expnTks[length(expnTks)]
      print(paste0("nG: ", ngm, "Eval: ",ngmRegex))
      
      ngmDT <- dtsArr[[ngmNext]]
      ngmHits <- ngmDT[ngram %like% paste("^", ngmRegex, "_", sep = ""), ngram]    
      if (length(ngmHits) <= 0)
        ngm <- ngm - 1
    }  
    
    print(paste0("ngm: ", ngm))
    
    if (!is.null(ngmHits) && length(ngmHits) > 0)
    {
      baseCount <-dtsArr[[ngm]][.(ngmRegex)]$count
      print(paste0("basecount: ", baseCount))
      for (hit in ngmHits) {
        ngmDT[.(hit), ':=' (MaxLikelyhoodEst = count/baseCount, 
                            BackoffEst = count + 0.4*baseCount + doBackOff(hit, (ngm-1), dtsArr))]
      }
  
     if(est == "mle")
     {
       print(paste0("est------: ",est))
       return (ngmDT[ngmHits][order(-MaxLikelyhoodEst)])
     } else  
       return (ngmDT[ngmHits][order(-BackoffEst)])
    }  
    
    return (c(""))
  }
  
  withProgress(message = "Initializing app, Please wait!!!", value=0,{
    dt1 <- read.csv("data/dt1.csv")
    # dt1 <- read.csv("https://raw.githubusercontent.com/jolna/capstoneproject/master/data/dt1.csv")
    dt1 <- data.table(ngram = as.vector(dt1$ngram), count = as.vector(dt1$count), key = "ngram")
    
    incProgress(.10, detail=".")
    
    dt2 <- read.csv("data/dt2.csv")
    # dt2 <- read.csv("../final/en_US/clean/dt2.csv")
    # dt2 <- read.csv("https://raw.githubusercontent.com/jolna/capstoneproject/master/data/dt2.csv")
    dt2 <- data.table(ngram = as.vector(dt2$ngram), count = as.vector(dt2$count), key = "ngram")
    
    incProgress(.20, detail="..")
    
    dt3 <- read.csv("data/dt3.csv")
    # dt3 <- read.csv("https://raw.githubusercontent.com/jolna/capstoneproject/master/data/dt3.csv")
    dt3 <- data.table(ngram = as.vector(dt3$ngram), count = as.vector(dt3$count), key = "ngram")
    
    incProgress(.30, detail="...")
    
    dt4 <- read.csv("data/dt4.csv")
    # dt4 <- read.csv("https://raw.githubusercontent.com/jolna/capstoneproject/master/data/dt4.csv")
    dt4 <- data.table(ngram = as.vector(dt4$ngram), count = as.vector(dt4$count), key = "ngram")
    
    incProgress(.40, detail="....")
    
    dt5 <- read.csv("data/dt5.csv")
    # dt5 <- read.csv("https://raw.githubusercontent.com/jolna/capstoneproject/master/data/dt5.csv")
    dt5 <- data.table(ngram = as.vector(dt5$ngram), count = as.vector(dt5$count), key = "ngram")
    
    incProgress(.70, detail=".....")
  
    dtsI <- list(dt1,dt2,dt3,dt4,dt5)
    
  })
  
  
  iexp <- reactive({
    ie <- input$expression
    if (input$showTyped) {
      output$actualExp <- renderText(ie)
      output$actualExpTitle <- renderText("Word(s) entered: ")
    }
    else {
      output$actualExp <- renderText("")
      output$actualExpTitle <- renderText("")
    }  
    ie
  })
  
  sp <- reactive({input$showProgress})
  
  sugg <- reactive({
    if (iexp() == "")
      return
    exp <- tolower(iexp())
    initTokens <- doTokenize(exp)
    stN <- 4
    if (length(initTokens) <= 4)
      stN <- length(initTokens)
    
    estVal <- input$useEst
    if (sp())
    {
      withProgress(message = "working on suggestions...", value=20,{
        results <- findNgramMatch(exp, stN, dtsI,estVal)
      })
    } else {
      results <- findNgramMatch(exp, stN, dtsI, estVal)
    }
    
    if (length(results) == 0 || (length(results) == 1 && results == ""))
    { 
      if (iexp() == "")
        final <- ""
      else
        final <- "No suggestions :("
      
    } else if (length(results) > 1) 
    {
      results$ngram <- gsub("#", "", results$ngram)
      results$ngram <- gsub("_", " ", results$ngram)

      rs <- as.vector(results$ngram)
      final <- ""
      nSugg <- input$sliderNoOfSugg
      if (input$showDetails)
      {
        output$detailTitle <- renderText("N-Grams Summary")
        output$detailTable <- renderTable(head(results, nSugg))
      } else {
        output$detailTitle <- renderText("")
        output$detailTable <- renderText("")
      }
      print(nSugg)  
      nMax <- ifelse(length(rs) > nSugg,nSugg,length(rs))
      rs <- rs[1:nMax]
      # rs <- gsub("_", " ", rs)
      # print(rs)
      for (r in rs)
      {
        nr <- doTokenize(r)
        final <- paste0(final, " | ", nr[length(nr)])
      }
      final
      
    }   
  })
  
  output$suggestions <- renderText({sugg()})
  
})