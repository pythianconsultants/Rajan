
shinyServer(function(input, output) {
  
  output$Age <- renderPlotly({
    BMT2 <- BMT%>%filter(Patient.s.Sex %in% input$fillvalue)
    p1 <- ggplot(BMT2, aes(x = Pts.age)) + 
      geom_histogram(aes(fill = ..count..), binwidth = 2) +
      scale_x_continuous(name = "Patient's Age",
                         breaks = seq(20, 80, 10),
                         limits=c(20, 80)) +
      scale_y_continuous(name = "Count") +
      ggtitle("Frequency histogram of Patient's Age") +
      scale_fill_gradient("Count", low = "blue", high = "red")
      # stat_function(fun = dnorm, colour = "black", 
      #               args = list(mean = mean(BMT2$Pts.age, na.rm = TRUE), 
      #                          sd = sd(BMT2$Pts.age, na.rm = TRUE)))
    ggplotly(p1)
  })
  
  output$bar <- renderPlotly({
    p2 <- ggplot(BMT,aes_string(x=input$xvalue))+geom_bar(aes(fill=BMT$Patient.s.Sex))
    ggplotly(p2+theme(axis.text.x=element_blank(),axis.ticks=element_blank()))
    
  })
  
  output$overall <- renderPlot({
    fit_overall <- survfit(Surv(surv_data$TD,surv_data$cenin_dead)~1,conf.type = "log-log")
    ggsurvplot(fit_overall,risk.table = T, risk.table.height = 0.4,pval = T)
  })
  
  
  #RF <- na.omit(select(surv_data, RenalFailure, TD, cenin_dead))
  

  
  # output$events <- renderPlot({
  #   if(input$analysis=='TD'){
  #   #RF <- na.omit(select_(rlp_data, .dots=c(input$event, "TD", "cenin_dead")))  
  #   fit_RF <- survfit(Surv(TD,cenin_dead)~.(input$event),data=rlp_data)
  #   ggsurvplot(fit_RF,risk.table = T,risk.table.height = 0.4,pval = T)
  #   } else{
  #     RF <- na.omit(select_(rlp_data, .dots=c(input$event, "TD_R", "relapse")))
  #     rlp_data[colnames(rlp_data)==input$event]
  #     fit_RF <- survfit(Surv(TD_R,relapse)~RF[,1],data=RF)
  #     ggsurvplot(fit_RF,risk.table = T,risk.table.height = 0.4,pval = T)
  #   }
  # })
  
  output$events <- renderPlot({
    print(str(input$event))
    if(input$analysis=='TD'){
      fit_RF <- survfit(Surv(TD,cenin_dead)~input$event,data=rlp_data)
      ggsurvplot(fit_RF,risk.table = T,risk.table.height = 0.4,pval = T)
    } else{
      fit_RF <- survfit(Surv(TD_R,relapse)~input$event,data=rlp_data)
      ggsurvplot(fit_RF,risk.table = T,risk.table.height = 0.4,pval = T)
    }
    
  })
  
})