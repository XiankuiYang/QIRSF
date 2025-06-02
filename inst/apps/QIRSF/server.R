library(shiny)
library(shinyFeedback)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(scatterplot3d)
library(doParallel)
library(foreach)
library(dplyr)
library(rsconnect)
library(doSNOW)
library(shinyjs)
library(DT)
source("function.R")

server <- function(input, output, session) {
  # download example data
  Data1 <- reactive(
    return(
      list(
        Regression_model = read.csv("Regression model candidate.csv",header = T),
        Regression_saddle = read.csv("Regression model with saddle point candidate.csv",header = T),
        Regression_irregular = read.csv("Regression model with irregular area candidate.csv",header = T),
        Exponential_model = read.csv("Exponential function model candidate.csv",header = T),
        Gramacy_Lee = read.csv("Gramacy and Lee function model candidate.csv",header = T),
        TITR = read.csv("Two inputs and two response model candidate.csv",header = T),
        TIOR = read.csv("Three inputs and one response model candidate.csv",header = T)
      )
    )
  )
  output$User_Guide <- downloadHandler(
    filename = "FIRSF user guide.pdf",
    content = function(file) {
      file.copy("FIRSF user guide.pdf", file)
    }
  )
  dataDownload <- reactive({
    switch(input$download,
           "2-D rectangular input with a maximum" = Data1()$Regression_model,
           "2-D rectangular input with a saddle point" = Data1()$Regression_saddle,
           "2-D non-rectangular input" = Data1()$Regression_irregular,
           "Exponential weight function" = Data1()$Exponential_model,
           "Gramacy & Lee function"=Data1()$Gramacy_Lee,
           "2 inputs & 2 resp."=Data1()$TITR,
           "3 inputs & 1 resp."=Data1()$TIOR)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$download, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataDownload(), file, row.names = FALSE)
    }
  )

  # Transfer the uploading data in R
  data <- reactive({
    req(input$Upload_file)
    ext <- tools::file_ext(input$Upload_file$name)
    switch(ext,
           csv = vroom::vroom(input$Upload_file$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })

  output$head <- DT::renderDataTable({
    K = head(data(), input$n)
    dat<-DT::datatable(K,options=list(dom='ltp',paging=TRUE,pageLength=10))
  })

  even <- reactive({
    (input$Dim_input+input$Dim_response) ==length(data()[1,])
  })

  Xmat <- reactive({
    req(even())
    return(as.matrix(data()[,1:(input$Dim_input)],ncol=input$Dim_input))
  })
  Ymat <- reactive({
    req(even())
    return(as.matrix(data()[,(input$Dim_input+1):(input$Dim_input+input$Dim_response)],ncol=input$Dim_response))
  })

  output$cand_plot <- renderPlot({
    even1 <- (input$Dim_input <= 3)
    even2 <- (input$Dim_response <= 2)
    req(even1&even2)
    if (input$Dim_input==1 & input$Dim_response==1) {
      plot(Xmat()[,1],Ymat()[,1],xlab="X",ylab="Y",pch = 16)
    }
    if (input$Dim_input==2 & input$Dim_response==1) {
      P1 <- ggplot(data = data.frame(a=Xmat()[,1],b=Xmat()[,2],c=Ymat()[,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      return(P1)
    }
    if (input$Dim_input==2 & input$Dim_response==2){
      P1 <- ggplot(data = data.frame(a=Xmat()[,1],b=Xmat()[,2],c=Ymat()[,1],d=Ymat()[,2]),aes(x=a,y=b))+
        geom_contour(aes(z=c,linetype = "Y1"),color = "black",bins = 20,size = 0.7)+
        geom_contour(aes(z=d,linetype = "Y2"),color = "blue",bins = 20,size = 0.7)+
        metR::geom_text_contour(aes(z = c)) +
        metR::geom_text_contour(aes(z = d),color = "blue") +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() + labs(linetype = "Legend")+
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(legend.position = "right") +
        scale_linetype_manual(labels = c(expression(Y[1]),expression(Y[2])),
                              breaks = c("Y1","Y2"),
                              values = c("Y1"=1,"Y2"=2)) +
        theme(legend.key.size = unit(0.5,"cm"),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      return(P1)
    }
    if (input$Dim_input==3 & input$Dim_response==1) {
      A = unique(Xmat()[,3])
      A = sort(A)
      if (length(A)<6) {
        A = c(A,rep(A[length(A)],6-length(A)))
        A1 = which(Xmat()[,3]==A[1])
        A2 = which(Xmat()[,3]==A[2])
        A3 = which(Xmat()[,3]==A[3])
        A4 = which(Xmat()[,3]==A[4])
        A5 = which(Xmat()[,3]==A[5])
        A6 = which(Xmat()[,3]==A[6])
      }else{
        B = sample(1:(length(A)-2),4,replace = F)
        B1 = A[-c(1,length(A))]
        A = c(A[1],sort(B1[B]),A[length(A)])
        A1 = which(Xmat()[,3]==A[1])
        A2 = which(Xmat()[,3]==A[2])
        A3 = which(Xmat()[,3]==A[3])
        A4 = which(Xmat()[,3]==A[4])
        A5 = which(Xmat()[,3]==A[5])
        A6 = which(Xmat()[,3]==A[6])
      }
      A = round(A,3)

      P1 <- ggplot(data = data.frame(a=Xmat()[A1,1],b=Xmat()[A1,2],c=Ymat()[A1,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) + labs(title = bquote(x[3]~"="~.(A[1])))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- ggplot(data = data.frame(a=Xmat()[A2,1],b=Xmat()[A2,2],c=Ymat()[A2,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) + labs(title = bquote(x[3]~"="~.(A[2])))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P3 <- ggplot(data = data.frame(a=Xmat()[A3,1],b=Xmat()[A3,2],c=Ymat()[A3,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) + labs(title = bquote(x[3]~"="~.(A[3])))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P4 <- ggplot(data = data.frame(a=Xmat()[A4,1],b=Xmat()[A4,2],c=Ymat()[A4,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) + labs(title = bquote(x[3]~"="~.(A[4])))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P5 <- ggplot(data = data.frame(a=Xmat()[A5,1],b=Xmat()[A5,2],c=Ymat()[A5,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) + labs(title = bquote(x[3]~"="~.(A[5])))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P6 <- ggplot(data = data.frame(a=Xmat()[A6,1],b=Xmat()[A6,2],c=Ymat()[A6,1]),aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) + labs(title= bquote(x[3]~"="~.(A[6])))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      return(plot_grid(P1, P2, P3, P4, P5, P6, labels = NULL))
    }
  },height = 600,width = 600)

  ## Tab showing
  observeEvent(input$Method,{
    if (input$Method=="ISF") {
      showTab("TABS",target = "ISF")
      hideTab("TABS",target = "RSF")
      hideTab("TABS",target = "IRSF")
    }else{
      if (input$Method=="RSF") {
        hideTab("TABS",target = "ISF")
        showTab("TABS",target = "RSF")
        hideTab("TABS",target = "IRSF")
      }else {
        if (input$Method=="IRSF") {
          hideTab("TABS",target = "ISF")
          hideTab("TABS",target = "RSF")
          showTab("TABS",target = "IRSF")
        }else{
          hideTab("TABS",target = "ISF")
          hideTab("TABS",target = "RSF")
          hideTab("TABS",target = "IRSF")
        }
      }
    }
  })
  # The number of runs reminder
  warning1 <- reactive({
    even.runs1 <- (input$Num_runs>0&is.integer(input$Num_runs))
    req(even())
    even.runs2 <- (input$Num_runs<=length(Xmat()[,1]))
    req(even.runs1&even.runs2)
    return(input$Num_runs)
  })
  warning2 <- reactive({
    even.runs1 <- (input$Num_runs>0&is.integer(input$Num_runs))
    shinyFeedback::feedbackWarning("Num_runs",!even.runs1,"Please enter an positive integer number!")
    req(even.runs1)
    even.runs2 <- (input$Num_runs<=length(Xmat()[,1]))
    shinyFeedback::feedbackWarning("Num_runs",!even.runs2,"The number of runs is over the candidate size!")
  })
  output$warningA <- renderText(warning2())
  ## ISF results showing
  result.ISF = eventReactive(input$work,{
    even.ISF = input$Method=="ISF"
    req(even.ISF&even())
    Result.A = irsf.cluster(Xmat(),Ymat(),warning1(),1,Method = "average")
    return(Result.A)
  })
  output$ISF_design <- DT::renderDataTable({
    Xdesign = result.ISF()$X.design
    Ydesign = result.ISF()$Y.design
    Output.ISF.design = data.frame(Xdesign, Ydesign)
    names(Output.ISF.design) = names(data())
    DT::datatable(Output.ISF.design, options = list(pageLength = 10))
  })

  output$download_ISF_design <- downloadHandler(
    filename = function() {
      paste0("ISF_Design_Table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      Xdesign = result.ISF()$X.design
      Ydesign = result.ISF()$Y.design
      Output.ISF.design = data.frame(Xdesign, Ydesign)
      names(Output.ISF.design) = names(data())
      write.csv(Output.ISF.design, file, row.names = FALSE)
    }
  )

  output$ISF_design_plot <- renderPlot({
    even1 <- (input$Dim_input <= 2)
    even2 <- (input$Dim_response <= 2)
    req(even1&even2)
    Cand_mat = data.frame(Xmat(),Ymat())
    Des_mat = data.frame(result.ISF()$X.design,result.ISF()$Y.design)
    N_des = warning1()
    if (input$Dim_input==1 & input$Dim_response==1) {
      plot(Xmat()[,1],Ymat()[,1],xlab="X",ylab="Y",pch = 16)
      points(Des_mat[,1], Des_mat[,2],col="darkgreen")
    }
    if (input$Dim_input==2 & input$Dim_response==1) {
      names(Cand_mat) = c("a","b","c")
      names(Des_mat) = c("a1","b1","c1")
      P1 <- ggplot(data = Cand_mat,aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P1 <- P1 + geom_point(data = Des_mat, aes(x=a1,y=b1),color="darkgreen",size = 2)
      P2 <-ggplot(data = Cand_mat,aes(x=c))+
        geom_histogram(bins = 15,color="black", fill="grey50")+
        xlab(expression("Y"))+ylab("frequency")+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- P2+geom_vline(data = Des_mat,aes(xintercept=c1),color = "darkgreen",size = 1)
      return(plot_grid(P1, P2, labels = NULL))
    }
    if (input$Dim_input==2 & input$Dim_response==2){
      names(Cand_mat) = c("a","b","c","d")
      names(Des_mat) = c("a1","b1","c1","d1")
      P1 <- ggplot(data = Cand_mat,aes(x=a,y=b))+
        geom_contour(aes(z=c,linetype = "Y1"),color = "black",bins = 20,size = 0.7)+
        geom_contour(aes(z=d,linetype = "Y2"),color = "black",bins = 20,size = 0.7)+
        metR::geom_text_contour(aes(z = c)) +
        metR::geom_text_contour(aes(z = d),color = "black") +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() + labs(linetype = "Legend")+
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(legend.position = "right") +
        scale_linetype_manual(labels = c(expression(Y[1]),expression(Y[2])),
                              breaks = c("Y1","Y2"),
                              values = c("Y1"=1,"Y2"=2)) +
        theme(legend.key.size = unit(0.5,"cm"),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P1 <- P1 + geom_point(data = Des_mat, aes(x=a1,y=b1),color="darkgreen",size = 2)

      P2 <-ggplot(data = Cand_mat,aes(x=c,y=d))+
        geom_point(color="black",shape=1)+
        xlab(expression(Y[1]))+ylab(expression(Y[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- P2+geom_point(data = Des_mat,aes(x=c1,y=d1),color = "darkgreen",size = 2)
      return(plot_grid(P1, P2, labels = NULL))
    }
  },height = 400,width = 800)
  output$ISF_3d_plot <- renderPlot({
    even1 <- (input$Dim_input == 3)
    req(even1)
    scatterplot3d(result.ISF()$X.design[,1:3],xlab = expression(X[1]),ylab = expression(X[2]),
                  zlab = expression(X[3]),xlim=c(0,1),ylim = c(0,1),zlim = c(0,1),color = "blue",pch = 16,cex.symbols = 0.8)
  },height = 400,width = 400)

  ## Response results showing
  result.RSF = eventReactive(input$work,{
    even.RSF = input$Method=="RSF"
    req(even.RSF&even())
    Result.A = irsf.cluster(Xmat(),Ymat(),warning1(),0,Method = "average")
    return(Result.A)
  })
  output$RSF_design <- DT::renderDataTable({
    Xdesign = result.RSF()$X.design
    Ydesign = result.RSF()$Y.design
    Output.RSF.design = data.frame(Xdesign, Ydesign)
    names(Output.RSF.design) = names(data())
    DT::datatable(Output.RSF.design, options = list(pageLength = 10))
  })

  output$download_RSF_design <- downloadHandler(
    filename = function() {
      paste0("RSF_Design_Table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      Xdesign = result.RSF()$X.design
      Ydesign = result.RSF()$Y.design
      Output.RSF.design = data.frame(Xdesign, Ydesign)
      names(Output.RSF.design) = names(data())
      write.csv(Output.RSF.design, file, row.names = FALSE)
    }
  )
  output$RSF_design_plot <- renderPlot({
    even1 <- (input$Dim_input <= 2)
    even2 <- (input$Dim_response <= 2)
    req(even1&even2)
    Cand_mat = data.frame(Xmat(),Ymat())
    Des_mat = data.frame(result.RSF()$X.design,result.RSF()$Y.design)
    N_des = warning1()
    if (input$Dim_input==1 & input$Dim_response==1) {
      plot(Xmat()[,1],Ymat()[,1],xlab="X",ylab="Y",pch = 16)
      points(Des_mat[,1], Des_mat[,2],col="purple")
    }
    if (input$Dim_input==2 & input$Dim_response==1) {
      names(Cand_mat) = c("a","b","c")
      names(Des_mat) = c("a1","b1","c1")
      P1 <- ggplot(data = Cand_mat,aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P1 <- P1 + geom_point(data = Des_mat, aes(x=a1,y=b1),color="purple",size = 2)
      P2 <-ggplot(data = Cand_mat,aes(x=c))+
        geom_histogram(bins = 15,color="black", fill="grey50")+
        xlab(expression("Y"))+ylab("frequency")+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- P2+geom_vline(data = Des_mat,aes(xintercept=c1),color = "purple",size = 1)
      return(plot_grid(P1, P2, labels = NULL))
    }
    if (input$Dim_input==2 & input$Dim_response==2){
      names(Cand_mat) = c("a","b","c","d")
      names(Des_mat) = c("a1","b1","c1","d1")
      P1 <- ggplot(data = Cand_mat,aes(x=a,y=b))+
        geom_contour(aes(z=c,linetype = "Y1"),color = "black",bins = 20,size = 0.7)+
        geom_contour(aes(z=d,linetype = "Y2"),color = "black",bins = 20,size = 0.7)+
        metR::geom_text_contour(aes(z = c)) +
        metR::geom_text_contour(aes(z = d),color = "black") +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() + labs(linetype = "Legend")+
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(legend.position = "right") +
        scale_linetype_manual(labels = c(expression(Y[1]),expression(Y[2])),
                              breaks = c("Y1","Y2"),
                              values = c("Y1"=1,"Y2"=2)) +
        theme(legend.key.size = unit(0.5,"cm"),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P1 <- P1 + geom_point(data = Des_mat, aes(x=a1,y=b1),color="purple",size = 2)

      P2 <-ggplot(data = Cand_mat,aes(x=c,y=d))+
        geom_point(color="black",shape=1)+
        xlab(expression(Y[1]))+ylab(expression(Y[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- P2+geom_point(data = Des_mat,aes(x=c1,y=d1),color = "purple",size = 2)
      return(plot_grid(P1, P2, labels = NULL))
    }
  },height = 400,width = 800)
  output$RSF_3d_plot <- renderPlot({
    even1 <- (input$Dim_input == 3)
    req(even1)
    scatterplot3d(result.RSF()$X.design[,1:3],xlab = expression(X[1]),ylab = expression(X[2]),
                  zlab = expression(X[3]),xlim=c(0,1),ylim = c(0,1),zlim = c(0,1),color = "blue",pch = 16,cex.symbols = 0.8)
  },height = 400,width = 400)




  ## IRSF Pareto Front

  Criterian1 <- eventReactive(input$work,{
    even.RSF = input$Method=="IRSF"
    req(even.RSF&even())
    n_cores <- 8
    cl <- makeCluster(n_cores)
    registerDoSNOW(cl)
    wt = seq(0,1,by = 1/input$Num_wt)
    xmat.r <- as.matrix(Xmat(),ncol=input$Dim_input)
    ymat.cand.est.full<-as.matrix(Ymat(),ncol=input$Dim_response)
    Ns0.ex1 <- warning1()
    iterations <- length(wt)

    CriterionA <- foreach(varid = 1:iterations, .combine = rbind) %dopar%
      {
        irsf.cluster <- function(xmat,ymat,N,wt,Method) {
          unitscale <- function(raw.mat,cand.mat=NULL)
          {
            raw.mat = as.matrix(raw.mat)
            p <- dim(raw.mat)[2]
            raw.mat.new <- raw.mat
            if(is.null(cand.mat))
            {
              cand.mat=raw.mat
            }
            cand.mat = as.matrix(cand.mat)
            for(i in 1:p)
            {
              raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
            }
            return(raw.mat.new)
          }
          Nd <- N
          wt <- wt
          nxq <- dim(xmat)[1]
          nxp <- dim(xmat)[2]
          nyp <- dim(ymat)[2]
          xmat.r <- xmat
          ymat.r <- ymat

          xmat.r.1 <- unitscale(xmat.r)
          ymat.r.1 <- unitscale(ymat.r)
          randmat.x <- as.matrix(xmat.r.1[,1:(nxp)])
          randmat.y <- as.matrix(ymat.r.1[,1:(nyp)])
          dx = stats::dist(randmat.x, method = "euclidean")
          dy = stats::dist(randmat.y, method = "euclidean")
          xmax = max(dx)
          xmin = min(dx)
          ymax = max(dy)
          ymin = min(dy)
          d1 = (dx-xmin)/(xmax-xmin)
          d2 = (dy-ymin)/(ymax-ymin)
          dc = wt*d1+(1-wt)*d2

          if (Method == "complete") {
            groupid <- stats::cutree(stats::hclust(d = dc, method = "complete"), k = Nd)
          }
          if (Method == "average"){
            groupid <- stats::cutree(stats::hclust(d = dc, method = "average"), k = Nd)
          }
          if (Method == "mcquitty"){
            groupid <- stats::cutree(stats::hclust(d = dc, method = "mcquitty"), k = Nd)
          }

          outmat.x <- matrix(0,nrow = Nd,ncol = nxp)
          outmat.y <- matrix(0,nrow = Nd,ncol = nyp)
          for (j in 1:Nd) {
            cluster.x = as.matrix(randmat.x[which(groupid == j),])
            cluster.y = as.matrix(randmat.y[which(groupid == j),])
            cluster.x.original = as.matrix(xmat.r[which(groupid == j),])
            cluster.y.original = as.matrix(ymat.r[which(groupid == j),])
            nd = dim(cluster.y)[1]
            np = dim(cluster.x)[2]
            nq = dim(cluster.y)[2]

            cluster1 = cbind(cluster.x,cluster.y)

            Dist.mat = apply(cluster1, 1, function(P1){
              max(wt*((sqrt(rowSums((cluster.x-matrix(rep(P1[1:np],nd),ncol=np,byrow = T))^2))-xmin)/(xmax-xmin))+
                    (1-wt)*((sqrt(rowSums((cluster.y-matrix(rep(P1[(np+1):(np+nq)],nd),ncol=nq,byrow = T))^2))-ymin)/(ymax-ymin)))
            })
            Minimax.point = which(Dist.mat==min(Dist.mat))
            if (length(Minimax.point)>1) {
              Minimax.point = sample(Minimax.point,1)
            }
            outmat.x[j,] = cluster.x.original[Minimax.point,]
            outmat.y[j,] = cluster.y.original[Minimax.point,]
          }

          result = list(X.design = outmat.x, Y.design = outmat.y, X.candidate = xmat.r, Y.candidate = ymat.r,
                        Cluster.num = groupid,weight.value = wt)

          return(result)
        }
        Cri_IRSF_cluster <- function(Des) {
          unitscale <- function(raw.mat,cand.mat=NULL)
          {
            raw.mat = as.matrix(raw.mat)
            p <- dim(raw.mat)[2]
            raw.mat.new <- raw.mat
            if(is.null(cand.mat))
            {
              cand.mat=raw.mat
            }
            cand.mat = as.matrix(cand.mat)
            for(i in 1:p)
            {
              raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
            }
            return(raw.mat.new)
          }
          xmat = as.matrix(Des[[1]])
          ymat = as.matrix(Des[[2]])
          Xcand = as.matrix(Des[[3]])
          Ycand = as.matrix(Des[[4]])
          wt = Des[[6]]

          xmat.scale = unitscale(xmat,Xcand)
          ymat.scale = unitscale(ymat,Ycand)
          qnx = dim(Xcand)[1]
          px = dim(xmat)[2]
          py = dim(ymat)[2]
          qx = dim(xmat)[1]

          xcand.scale = unitscale(Xcand,Xcand)
          ycand.scale = unitscale(Ycand,Ycand)
          X.max = max(as.numeric(lapply(1:qnx, function(i,xmat,xcand){
            min(as.numeric(lapply(1:qx, function(j,xmat,p){
              stats::dist(as.matrix(rbind(p[1:(px)],xmat[j,1:(px)])))
            },xmat = xmat,p = xcand[i,])))
          },xcand= xcand.scale,xmat =xmat.scale)))
          Y.max = max(as.numeric(lapply(1:qnx, function(i,ymat,ycand){
            min(as.numeric(lapply(1:qx, function(j,ymat,p){
              stats::dist(as.matrix(rbind(p[1:(py)],ymat[j,1:(py)])))
            },ymat = ymat,p = ycand[i,])))
          },ycand= ycand.scale,ymat =ymat.scale)))

          Xmat1 = as.data.frame(xmat[,1:(px)])
          names(Xmat1) = paste("x",c(1:(px)))
          Ymat1 = as.data.frame(ymat[,1:(py)])
          names(Ymat1) = paste("y",c(1:(py)))
          result = list(X.max=X.max,Y.max=Y.max,X.design=Xmat1,Y.design = Ymat1,weight.value = Des[[6]])
          return(result)
        }

        result.3 = irsf.cluster(xmat.r,ymat.cand.est.full,Ns0.ex1,wt[varid],Method = "complete")
        result.cri = Cri_IRSF_cluster(result.3)
        return(data.frame(X.max=result.cri[[1]],Y.max=result.cri[[2]],weight.x=wt[varid],Method="complete"))
      }
    gc()
    CriterionB <- foreach(varid = 1:iterations, .combine = rbind) %dopar%
      {
        irsf.cluster <- function(xmat,ymat,N,wt,Method) {
          unitscale <- function(raw.mat,cand.mat=NULL)
          {
            raw.mat = as.matrix(raw.mat)
            p <- dim(raw.mat)[2]
            raw.mat.new <- raw.mat
            if(is.null(cand.mat))
            {
              cand.mat=raw.mat
            }
            cand.mat = as.matrix(cand.mat)
            for(i in 1:p)
            {
              raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
            }
            return(raw.mat.new)
          }
          Nd <- N
          wt <- wt
          nxq <- dim(xmat)[1]
          nxp <- dim(xmat)[2]
          nyp <- dim(ymat)[2]
          xmat.r <- xmat
          ymat.r <- ymat

          xmat.r.1 <- unitscale(xmat.r)
          ymat.r.1 <- unitscale(ymat.r)
          randmat.x <- as.matrix(xmat.r.1[,1:(nxp)])
          randmat.y <- as.matrix(ymat.r.1[,1:(nyp)])
          dx = stats::dist(randmat.x, method = "euclidean")
          dy = stats::dist(randmat.y, method = "euclidean")
          xmax = max(dx)
          xmin = min(dx)
          ymax = max(dy)
          ymin = min(dy)
          d1 = (dx-xmin)/(xmax-xmin)
          d2 = (dy-ymin)/(ymax-ymin)
          dc = wt*d1+(1-wt)*d2

          if (Method == "complete") {
            groupid <- stats::cutree(stats::hclust(d = dc, method = "complete"), k = Nd)
          }
          if (Method == "average"){
            groupid <- stats::cutree(stats::hclust(d = dc, method = "average"), k = Nd)
          }
          if (Method == "mcquitty"){
            groupid <- stats::cutree(stats::hclust(d = dc, method = "mcquitty"), k = Nd)
          }

          outmat.x <- matrix(0,nrow = Nd,ncol = nxp)
          outmat.y <- matrix(0,nrow = Nd,ncol = nyp)
          for (j in 1:Nd) {
            cluster.x = as.matrix(randmat.x[which(groupid == j),])
            cluster.y = as.matrix(randmat.y[which(groupid == j),])
            cluster.x.original = as.matrix(xmat.r[which(groupid == j),])
            cluster.y.original = as.matrix(ymat.r[which(groupid == j),])
            nd = dim(cluster.y)[1]
            np = dim(cluster.x)[2]
            nq = dim(cluster.y)[2]

            cluster1 = cbind(cluster.x,cluster.y)

            Dist.mat = apply(cluster1, 1, function(P1){
              max(wt*((sqrt(rowSums((cluster.x-matrix(rep(P1[1:np],nd),ncol=np,byrow = T))^2))-xmin)/(xmax-xmin))+
                    (1-wt)*((sqrt(rowSums((cluster.y-matrix(rep(P1[(np+1):(np+nq)],nd),ncol=nq,byrow = T))^2))-ymin)/(ymax-ymin)))
            })
            Minimax.point = which(Dist.mat==min(Dist.mat))
            if (length(Minimax.point)>1) {
              Minimax.point = sample(Minimax.point,1)
            }
            outmat.x[j,] = cluster.x.original[Minimax.point,]
            outmat.y[j,] = cluster.y.original[Minimax.point,]
          }

          result = list(X.design = outmat.x, Y.design = outmat.y, X.candidate = xmat.r, Y.candidate = ymat.r,
                        Cluster.num = groupid,weight.value = wt)

          return(result)
        }
        Cri_IRSF_cluster <- function(Des) {
          unitscale <- function(raw.mat,cand.mat=NULL)
          {
            raw.mat = as.matrix(raw.mat)
            p <- dim(raw.mat)[2]
            raw.mat.new <- raw.mat
            if(is.null(cand.mat))
            {
              cand.mat=raw.mat
            }
            cand.mat = as.matrix(cand.mat)
            for(i in 1:p)
            {
              raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
            }
            return(raw.mat.new)
          }
          xmat = as.matrix(Des[[1]])
          ymat = as.matrix(Des[[2]])
          Xcand = as.matrix(Des[[3]])
          Ycand = as.matrix(Des[[4]])
          wt = Des[[6]]

          xmat.scale = unitscale(xmat,Xcand)
          ymat.scale = unitscale(ymat,Ycand)
          qnx = dim(Xcand)[1]
          px = dim(xmat)[2]
          py = dim(ymat)[2]
          qx = dim(xmat)[1]

          xcand.scale = unitscale(Xcand,Xcand)
          ycand.scale = unitscale(Ycand,Ycand)
          X.max = max(as.numeric(lapply(1:qnx, function(i,xmat,xcand){
            min(as.numeric(lapply(1:qx, function(j,xmat,p){
              stats::dist(as.matrix(rbind(p[1:(px)],xmat[j,1:(px)])))
            },xmat = xmat,p = xcand[i,])))
          },xcand= xcand.scale,xmat =xmat.scale)))
          Y.max = max(as.numeric(lapply(1:qnx, function(i,ymat,ycand){
            min(as.numeric(lapply(1:qx, function(j,ymat,p){
              stats::dist(as.matrix(rbind(p[1:(py)],ymat[j,1:(py)])))
            },ymat = ymat,p = ycand[i,])))
          },ycand= ycand.scale,ymat =ymat.scale)))

          Xmat1 = as.data.frame(xmat[,1:(px)])
          names(Xmat1) = paste("x",c(1:(px)))
          Ymat1 = as.data.frame(ymat[,1:(py)])
          names(Ymat1) = paste("y",c(1:(py)))
          result = list(X.max=X.max,Y.max=Y.max,X.design=Xmat1,Y.design = Ymat1,weight.value = Des[[6]])
          return(result)
        }

        result.3 = irsf.cluster(xmat.r,ymat.cand.est.full,Ns0.ex1,wt[varid],Method = "average")
        result.cri = Cri_IRSF_cluster(result.3)
        return(data.frame(X.max=result.cri[[1]],Y.max=result.cri[[2]],weight.x=wt[varid],Method="average"))
      }
    gc()
    CriterionC <- foreach(varid = 1:iterations, .combine = rbind) %dopar%
      {
        irsf.cluster <- function(xmat,ymat,N,wt,Method) {
          unitscale <- function(raw.mat,cand.mat=NULL)
          {
            raw.mat = as.matrix(raw.mat)
            p <- dim(raw.mat)[2]
            raw.mat.new <- raw.mat
            if(is.null(cand.mat))
            {
              cand.mat=raw.mat
            }
            cand.mat = as.matrix(cand.mat)
            for(i in 1:p)
            {
              raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
            }
            return(raw.mat.new)
          }
          Nd <- N
          wt <- wt
          nxq <- dim(xmat)[1]
          nxp <- dim(xmat)[2]
          nyp <- dim(ymat)[2]
          xmat.r <- xmat
          ymat.r <- ymat

          xmat.r.1 <- unitscale(xmat.r)
          ymat.r.1 <- unitscale(ymat.r)
          randmat.x <- as.matrix(xmat.r.1[,1:(nxp)])
          randmat.y <- as.matrix(ymat.r.1[,1:(nyp)])
          dx = stats::dist(randmat.x, method = "euclidean")
          dy = stats::dist(randmat.y, method = "euclidean")
          xmax = max(dx)
          xmin = min(dx)
          ymax = max(dy)
          ymin = min(dy)
          d1 = (dx-xmin)/(xmax-xmin)
          d2 = (dy-ymin)/(ymax-ymin)
          dc = wt*d1+(1-wt)*d2

          if (Method == "complete") {
            groupid <- stats::cutree(stats::hclust(d = dc, method = "complete"), k = Nd)
          }
          if (Method == "average"){
            groupid <- stats::cutree(stats::hclust(d = dc, method = "average"), k = Nd)
          }
          if (Method == "mcquitty"){
            groupid <- stats::cutree(stats::hclust(d = dc, method = "mcquitty"), k = Nd)
          }

          outmat.x <- matrix(0,nrow = Nd,ncol = nxp)
          outmat.y <- matrix(0,nrow = Nd,ncol = nyp)
          for (j in 1:Nd) {
            cluster.x = as.matrix(randmat.x[which(groupid == j),])
            cluster.y = as.matrix(randmat.y[which(groupid == j),])
            cluster.x.original = as.matrix(xmat.r[which(groupid == j),])
            cluster.y.original = as.matrix(ymat.r[which(groupid == j),])
            nd = dim(cluster.y)[1]
            np = dim(cluster.x)[2]
            nq = dim(cluster.y)[2]

            cluster1 = cbind(cluster.x,cluster.y)

            Dist.mat = apply(cluster1, 1, function(P1){
              max(wt*((sqrt(rowSums((cluster.x-matrix(rep(P1[1:np],nd),ncol=np,byrow = T))^2))-xmin)/(xmax-xmin))+
                    (1-wt)*((sqrt(rowSums((cluster.y-matrix(rep(P1[(np+1):(np+nq)],nd),ncol=nq,byrow = T))^2))-ymin)/(ymax-ymin)))
            })
            Minimax.point = which(Dist.mat==min(Dist.mat))
            if (length(Minimax.point)>1) {
              Minimax.point = sample(Minimax.point,1)
            }
            outmat.x[j,] = cluster.x.original[Minimax.point,]
            outmat.y[j,] = cluster.y.original[Minimax.point,]
          }

          result = list(X.design = outmat.x, Y.design = outmat.y, X.candidate = xmat.r, Y.candidate = ymat.r,
                        Cluster.num = groupid,weight.value = wt)

          return(result)
        }
        Cri_IRSF_cluster <- function(Des) {
          unitscale <- function(raw.mat,cand.mat=NULL)
          {
            raw.mat = as.matrix(raw.mat)
            p <- dim(raw.mat)[2]
            raw.mat.new <- raw.mat
            if(is.null(cand.mat))
            {
              cand.mat=raw.mat
            }
            cand.mat = as.matrix(cand.mat)
            for(i in 1:p)
            {
              raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
            }
            return(raw.mat.new)
          }
          xmat = as.matrix(Des[[1]])
          ymat = as.matrix(Des[[2]])
          Xcand = as.matrix(Des[[3]])
          Ycand = as.matrix(Des[[4]])
          wt = Des[[6]]

          xmat.scale = unitscale(xmat,Xcand)
          ymat.scale = unitscale(ymat,Ycand)
          qnx = dim(Xcand)[1]
          px = dim(xmat)[2]
          py = dim(ymat)[2]
          qx = dim(xmat)[1]

          xcand.scale = unitscale(Xcand,Xcand)
          ycand.scale = unitscale(Ycand,Ycand)
          X.max = max(as.numeric(lapply(1:qnx, function(i,xmat,xcand){
            min(as.numeric(lapply(1:qx, function(j,xmat,p){
              stats::dist(as.matrix(rbind(p[1:(px)],xmat[j,1:(px)])))
            },xmat = xmat,p = xcand[i,])))
          },xcand= xcand.scale,xmat =xmat.scale)))
          Y.max = max(as.numeric(lapply(1:qnx, function(i,ymat,ycand){
            min(as.numeric(lapply(1:qx, function(j,ymat,p){
              stats::dist(as.matrix(rbind(p[1:(py)],ymat[j,1:(py)])))
            },ymat = ymat,p = ycand[i,])))
          },ycand= ycand.scale,ymat =ymat.scale)))

          Xmat1 = as.data.frame(xmat[,1:(px)])
          names(Xmat1) = paste("x",c(1:(px)))
          Ymat1 = as.data.frame(ymat[,1:(py)])
          names(Ymat1) = paste("y",c(1:(py)))
          result = list(X.max=X.max,Y.max=Y.max,X.design=Xmat1,Y.design = Ymat1,weight.value = Des[[6]])
          return(result)
        }

        result.3 = irsf.cluster(xmat.r,ymat.cand.est.full,Ns0.ex1,wt[varid],Method = "mcquitty")
        result.cri = Cri_IRSF_cluster(result.3)
        return(data.frame(X.max=result.cri[[1]],Y.max=result.cri[[2]],weight.x=wt[varid],Method="mcquitty"))
      }
    gc()
    stopCluster(cl)
    return(rbind(CriterionA,CriterionB,CriterionC))
  })



  curpf <- reactive({
    Criterion2 = Criterian1()
    return(PF_max(Criterion2))
  })

  output$IRSF_table <- DT::renderDataTable({
    K = as.data.frame(curpf())
    dat<-DT::datatable(K,options=list(dom='ltp',paging=TRUE,pageLength=10))
  })

  output$download_IRSF_table <- downloadHandler(
    filename = function() {
      paste0("Criterion_Table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(as.data.frame(curpf()), file, row.names = FALSE)
    }
  )

  output$IRSFPF_plot <- renderPlot({
    plot(curpf()[,1],curpf()[,2],col="blue",pch = 16,xlab="X.cri",ylab="Y.cri",main="PF of maximum distance")
    lines(curpf()[,1],curpf()[,2],col="blue",lty = 2)
  })


  PF_Design<- eventReactive(input$plot_click,{
    A = nearPoints(data.frame(X.max=curpf()$X.max,Y.max=curpf()$Y.max,weight.x=curpf()$weight.x,Method = curpf()$Method),
                   input$plot_click,xvar = "X.max",yvar = "Y.max")
    design = irsf.cluster(Xmat(),Ymat(),warning1(),A$weight.x,Method = A$Method)
    return(design)
  })

  Design_Specific <- eventReactive(input$plot_click,{
    Xdesign = PF_Design()$X.design
    Ydesign = PF_Design()$Y.design
    Output.IRSF.design = data.frame(Xdesign,Ydesign)
    names(Output.IRSF.design) = names(data())
    return(Output.IRSF.design)
  })

  output$download_Specific_design <- downloadHandler(
    filename = function() {
      paste0("Design_Table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(as.data.frame(Design_Specific()), file, row.names = FALSE)
    }
  )

  output$Specific_design <- DT::renderDataTable({
    K = as.data.frame(Design_Specific())
    dat<-DT::datatable(K,options=list(dom='ltp',paging=TRUE,pageLength=10))
  })


  output$Specific_design_plot <- renderPlot({
    even1 <- (input$Dim_input <= 2)
    even2 <- (input$Dim_response <= 2)
    req(even1&even2)
    A = nearPoints(data.frame(X.max=curpf()$X.max,Y.max=curpf()$Y.max,weight.x=curpf()$weight.x,Method = curpf()$Method),
                   input$plot_click,xvar = "X.max",yvar = "Y.max")
    N_des = which(curpf()$weight.x==A$weight.x&curpf()$Method==A$Method)
    Cand_mat = data.frame(Xmat(),Ymat())
    Des_mat = data.frame(PF_Design()$X.design,PF_Design()$Y.design)
    if (input$Dim_input==1 & input$Dim_response==1) {
      plot(Xmat()[,1],Ymat()[,1],xlab="X",ylab="Y",pch = 16)
      points(Des_mat[,1], Des_mat[,2],col="red")
    }
    if (input$Dim_input==2 & input$Dim_response==1) {
      names(Cand_mat) = c("a","b","c")
      names(Des_mat) = c("a1","b1","c1")
      P1 <- ggplot(data = Cand_mat,aes(x=a,y=b))+
        geom_contour(aes(z=c),bins = 20,color = "black")+
        metR::geom_text_contour(aes(z = c)) +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P1 <- P1 + geom_point(data = Des_mat, aes(x=a1,y=b1),color="red",size = 2)
      P2 <-ggplot(data = Cand_mat,aes(x=c))+
        geom_histogram(bins = 15,color="black", fill="grey50")+
        xlab(expression("Y"))+ylab("frequency")+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- P2+geom_vline(data = Des_mat,aes(xintercept=c1),color = "red",size = 1)
      P <- plot_grid(P1, P2, labels = NULL)
      title <- ggdraw() + draw_label(paste("The",N_des,"design on PF with",A$Method,"method"), fontface='bold')
      return(plot_grid(title, P, ncol=1, rel_heights=c(0.1, 1)))
    }
    if (input$Dim_input==2 & input$Dim_response==2){
      names(Cand_mat) = c("a","b","c","d")
      names(Des_mat) = c("a1","b1","c1","d1")
      P1 <- ggplot(data = Cand_mat,aes(x=a,y=b))+
        geom_contour(aes(z=c,linetype = "Y1"),color = "black",bins = 20,size = 0.7)+
        geom_contour(aes(z=d,linetype = "Y2"),color = "black",bins = 20,size = 0.7)+
        metR::geom_text_contour(aes(z = c)) +
        metR::geom_text_contour(aes(z = d),color = "black") +
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() + labs(linetype = "Legend")+
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(legend.position = "right") +
        scale_linetype_manual(labels = c(expression(Y[1]),expression(Y[2])),
                              breaks = c("Y1","Y2"),
                              values = c("Y1"=1,"Y2"=2)) +
        theme(legend.key.size = unit(0.5,"cm"),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P1 <- P1 + geom_point(data = Des_mat, aes(x=a1,y=b1),color="red",size = 2)

      P2 <-ggplot(data = Cand_mat,aes(x=c,y=d))+
        geom_point(color="black",shape=1)+
        xlab(expression(Y[1]))+ylab(expression(Y[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2 <- P2+geom_point(data = Des_mat,aes(x=c1,y=d1),color = "red",size = 2)
      P <- plot_grid(P1, P2, labels = NULL)
      title <- ggdraw() + draw_label(paste("The",N_des,"design on PF with",A$Method,"method"), fontface='bold')
      return(plot_grid(title, P, ncol=1, rel_heights=c(0.1, 1)))
    }
  },height = 400,width = 800)
  output$Specific_design_3d_plot <- renderPlot({
    even1 <- (input$Dim_input == 3)
    req(even1)
    scatterplot3d(PF_Design()$X.design[,1:3],xlab = expression(X[1]),ylab = expression(X[2]),
                  zlab = expression(X[3]),xlim=c(0,1),ylim = c(0,1),zlim = c(0,1),color = "blue",pch = 16,cex.symbols = 0.8)
  },height = 400,width = 400)
  observeEvent(input$refresh, {
    js$refresh_page();
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}
