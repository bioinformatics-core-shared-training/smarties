
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output,session) {
  df1 <- read.csv("frequencies.csv",stringsAsFactors = FALSE) #re-calculate the allele frequency to make sure they're correct
  df1$Allele.Freq <- 100*(df1$blue / (df1$not.blue + df1$blue))
  
  values <- reactiveValues()
  values$df2 <- data.frame(Number = numeric(0), not.blue=numeric(0),blue=numeric(0))
  
  newEntry <- observe({
    if(input$update > 0) {

      isolate(values$df2[nrow(values$df2) + 1,] <- c(as.numeric(input$Number), as.numeric(input$non_blue),as.numeric(input$blue)))
      
    }
  })
  
  newEntry <- observe({
    if(input$reset > 0) {
      
      isolate(values$df2 <- data.frame(Number = numeric(0), not.blue=numeric(0),blue=numeric(0)))
      
    }
  })

  output$table1 <- renderTable({
    
    df <- values$df2
#    df <- mutate(df, Allele.Freq = blue / (blue+not.blue))
    df
    })
  
  output$freq <- renderPlot({
    
    df2 <- values$df2
    df2 <- mutate(df2, Allele.Freq = 100*(blue / (blue+not.blue)),Type="Observed")
    df2 <- mutate(df2, Grade = df1$Grade[match(df2$Number,df1$Number)])
    
    print(df2)
    
    df <- bind_rows(df1,df2)
    
    if(!input$showExpected) {
      df <- filter(df,Type=="Observed")
    }
    

    if(nrow(df)==0) {
      p <- ggplot()
    } else{
      
      
      if(input$plotType == "scatter"){
      
        if(input$showExpected){
        obsVals <- unique(df$Number[which(df$Type == "Observed")])
        
        p <- ggplot(filter(df, Number %in% obsVals), aes(x = Number, y= Allele.Freq,col=Grade,alpha=Type)) + geom_point(size=5) + 
        scale_color_manual(values = c("High grade"=rgb(29,0,150,maxColorValue=255), "Low grade"=rgb(236,0,140,maxColorValue=255))) + scale_alpha_discrete(range=c(0.1,1)) + ylim(0,35) + scale_x_continuous(breaks=1:20, limits=c(0,20))
        } else{
          p <- ggplot(df, aes(x = Number, y= Allele.Freq,col=Grade)) + geom_point(size=5)  + 
            scale_color_manual(values = c("High grade"=rgb(29,0,150,maxColorValue=255), "Low grade"=rgb(236,0,140,maxColorValue=255))) + ylim(0,35) + scale_x_continuous(breaks=1:20, limits=c(0,20))
          
        }
        
        if(!input$showGroup) {
          p <- p + scale_color_manual(values = c("High grade"="black", "Low grade"="black")) 
        
          df <- filter(df,Type=="Observed")
          estMean <- mean(df$Allele.Freq)
          p <- p + geom_hline(yintercept=estMean,col="red") + theme(legend.position="none")
        } else{
          
          df <- filter(df,Type=="Observed") %>% group_by(Grade)
          estMean <- summarise(df, Mean=mean(Allele.Freq)) %>% select(-Grade)
          p <- p + geom_hline(yintercept=as.numeric(estMean$Mean)[1],col=c(rgb(29,0,150,maxColorValue=255))) + geom_hline(yintercept=as.numeric(estMean$Mean)[2],col=c(rgb(236,0,140,maxColorValue=255))) + theme(legend.position="none")
        }
      
      
      } else{
        
        obsVals <- unique(df$Number[which(df$Type == "Observed")])


        
        
        if(!input$showGroup) {
          df <- mutate(df, X="x")
          p <- ggplot(df, aes(x=X, y = Allele.Freq)) + geom_boxplot(fill="grey") + geom_point(size=5) + ylim(0,35)

          df <- filter(df,Type=="Observed")
          estMean <- mean(df$Allele.Freq)
          p <- p + geom_hline(yintercept=estMean,col="red") + theme(legend.position="none")
        } else{
          p <- ggplot(df, aes(x=Grade, y = Allele.Freq,col=Grade,fill=Grade)) + geom_boxplot() + geom_point(size=5) + 
            scale_color_manual(values = c("High grade"=rgb(29,0,150,maxColorValue=255), "Low grade"=rgb(236,0,140,maxColorValue=255))) + 
            scale_fill_manual(values = c("High grade"=rgb(29,0,150,maxColorValue=255), "Low grade"=rgb(236,0,140,maxColorValue=255))) + 
            ylim(0,35)
          df <- filter(df,Type=="Observed") %>% group_by(Grade)
          estMean <- summarise(df, Mean=mean(Allele.Freq)) %>% select(-Grade)
          p <- p + geom_hline(yintercept=as.numeric(estMean$Mean)[1],col=c(rgb(29,0,150,maxColorValue=255))) + geom_hline(yintercept=as.numeric(estMean$Mean)[2],col=c(rgb(236,0,140,maxColorValue=255))) + theme(legend.position="none")
        }
        
        
      }
      
      
    }
    

    p 

    })
  
  output$themean <- renderText({
    df2 <- values$df2
    if(nrow(df2) > 0){
    df2 <- mutate(df2, Allele.Freq = 100*(blue / (blue+not.blue)))
    paste("The current estimate of the mean is:",round(mean(df2$Allele.Freq)))
    }
    else paste("")
    
  })
  
})
