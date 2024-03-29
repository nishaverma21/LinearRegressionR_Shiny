rm(list=ls())

#************************** Environment Setup *******************************

if (!require(shiny)) install.packages('shiny')
if (!require(shinydashboard)) install.packages('shinydashboard')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(caTools)) install.packages('caTools')
if (!require(DT)) install.packages('DT')
if (!require(dplyr)) install.packages('dplyr')
if (!require(shinyalert)) install.packages('shinyalert')
if (!require(caret)) install.packages('caret')
if (!require(ERSA)) install.packages('ERSA')
if (!require(jtools)) install.packages('jtools')
if (!require(ggstance)) install.packages('ggstance')
if (!require(shinyjs)) install.packages('shinyjs')

#************************** LOAD ALL REQUIRED LIBRARIES *******************************

library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(caTools)
library(DT)
library(dplyr)
library(shinyalert)
library(caret)
library(ERSA)
library(jtools)
library(ggstance)
library(shinyjs)
#************************** USER INTERFASE *******************************************
ui <- 
  dashboardPage(
    #************************** DASHBOARD HEADER*******************************
    dashboardHeader(title = "Linear Regression Model"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Upload Data", tabName="data",icon=icon("upload")),
        menuItem("Table", tabName = "table", icon = icon("th")),
        menuItem("Summary", tabName = "summary", icon = icon("th")),
        menuItem("Plots", tabName = "plots", icon = icon("th")),
        menuItem("Clean Data", tabName = "cleanData", icon = icon("th")),
        menuItem("Model", tabName = "model", icon = icon("th")),
        menuItem("Predictions", tabName = "predict", icon = icon("th")),
        menuItem("Download Predictions",tabName="download",icon=icon("download"))
      )
    ),
    #************************** DASHBOARD BODY*******************************
    
    # NV_UI_DASHBOARD
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  h5("User Need to complete other tabs before clicking 'Click Here' below !!!",align = "center",style = "color:grey"),
                  box(
                    h2("Model Result",align = "center",style = "color:blue"),
                    actionButton("ModelSummary","Click Here"),
                    br(),br(),
                    h5("Target of Model : ",align = "Left",style = "color:black"),
                    textOutput("DisplayTarget"),
                    br(),br(),
                    h5("Predictors of Model :",align = "Left",style = "color:black"),
                    textOutput("DisplayPredictors"),
                    br(),br(),
                    tableOutput("ModelSumDashboard")
                  ),
                  box(
                    h2("Model Summary",align = "center",style = "color:blue"),
                    actionButton("ModelSummaryDashboard","Click Here"),
                    plotOutput(outputId = "ModelSummaryDashboard")
                  ),
                  box(
                    h2("Model F Static",align = "center",style = "color:blue"),
                    actionButton("FStaticDashboard","Click Here"),
                    plotOutput(outputId = "FStaticDashboard")
                  ),
                  box(
                    h2("Model T Static",align = "center",style = "color:blue"),
                    actionButton("TStaticDashboard","Click Here"),
                    br(),br(),
                    plotOutput("TStaticDashboard")
                  ),
                  box(
                    h2("Fitted Plot",align = "center",style = "color:blue"),
                    actionButton("FittedPlot","Click Here"),
                    br(),
                    uiOutput("PredictorSelect"),
                    plotOutput("PlotResult",height="350px", width = "100%", hover = hoverOpts(id = "plot_hover"))
                  )
                  
                )
                
        ),
        # NV_UI_UPLOADDATA
        tabItem(tabName="data",
                useShinyalert(),
                fileInput('file1', em('Upload test data in csv format'),accept=".csv")
        ),
        # NV_UI_TABLE
        tabItem(tabName = "table",
                h3("Few records of uploaded data"),
                mainPanel(tableOutput("dataTable"),style = "width: 75%")
        ),
        # NV_UI_SUMMARY
        tabItem(tabName = "summary",
                h3('Summary  of uploaded data'),
                mainPanel(verbatimTextOutput("sum"))
        ),
        
        # NV_UI_PLOTS
        tabItem(tabName = "plots",
                h3('Different plots of uploaded data'),
                sidebarLayout(
                  sidebarPanel(
                    uiOutput("PlotY"),
                    uiOutput("PlotX"),
                    uiOutput('TypeOfPlot'),
                    uiOutput('Legend')),
                  mainPanel("Plot", plotOutput("box"))
                )
        ),
        # NV_UI_CLEANDATA
        tabItem(tabName = "cleanData",
                h3('Clean Data before creating Model by selecting variable, arithmetic operator and
                   enter value'),
                sidebarLayout(
                  sidebarPanel(
                   # shinyjs::useShinyjs(),
                    #id="side-panel",
                    uiOutput("CleanRows"),
                    uiOutput("CleanRowsOption"),
                    textInput("Value", "Enter a Value", value = ""),
                    tags$head(tags$script(src = "message-handler.js")),
                    actionButton("CleanRowSubmit","Clean Data"),
                    br(),br(),
                    actionButton("CleanDataSummary","Data Summary")),
                  mainPanel(verbatimTextOutput("CleanDataSum")))
                ),
        # NV_UI_MODEL
        tabItem(tabName = "model",
                h3('Generate linear model from uploaded data'),
                sidebarLayout(
                  sidebarPanel(
                    shinyjs::useShinyjs(),
                    id="side-panel",
                    sliderInput("TrainTest", "Select % of Training Data:", 1, 100, 50),
                    uiOutput("Target"),
                    uiOutput("Predictor"),
                    actionButton("ModelSubmit","Submit"),
                    br(), br(), br(),
                    actionButton("reset","Reset"),
                    br(), br(), br()),
                  mainPanel(verbatimTextOutput(outputId = "ModelSum")
                  )
               )),
        # NV_UI_PREDICTIONS
        tabItem(tabName = "predict",
                h3("Prediction with linear model on uploaded data"),
                mainPanel(actionButton("predict","Prediction"),
                          br(),br(),
                          textOutput('TargetOfModel'),
                          br(),br(),
                          DT::dataTableOutput('mytable') )
        ),
        # NV_UI_DOWNLOADPREDICTIONS 
        tabItem(tabName = "download",
                h3("Download Predictions"),
                downloadButton("downloadPrediction", "Download")
        )
    ) # Tab Item Ends
  ) # Dashboard Body Ends
  ) # UI Ends
#************************** SERVER COMMUNICATION *******************************************
server <- function(input, output,session) { 
  set.seed(122)
  histdata <- rnorm(500)
 #************************** NV_SERVER_UPLOADDATA *******************************************
  # NV_SERVER_Uploadata
  dataset <- reactive({
    dataset<-read.csv(input$file1$datapath)
  })
  #************************** NV_SERVER_TABLE *******************************************
  # Uploaded file in table format
  output$dataTable <- renderTable({
    if(is.null(input$file1))     
      return(NULL) 
    data<-dataset()
    head(data,15)
  })
  #************************** NV_SERVER_SUMMARY *******************************************
 # Summary of loaded data
  output$sum <- renderPrint({
    if(is.null(input$file1)) {
      text<-'No file loaded'
      session$sendCustomMessage(type='jsCode', list(value = text))
    }
    else {
      data<-dataset()
      summary(data)
    }
  })
  #************************** NV_SERVER_CLEANDATA *******************************************
    # Select Data frame variable
  output$CleanRows <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("CleanRows","Select a Variable", choices = names(data[,!names(data) %in%  names(Filter(is.factor, data))]),multiple = FALSE)
    }
    )
  # Select Airthmetic Operator
  output$CleanRowsOption <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      selectInput("Options","Arithmetic Operator", choices = c("Equal","Greater","Less"),multiple = FALSE)
    }
   )
  # Enter value to remove rows with 
  Value<-reactive({input$Value})
  
  # Reactive Airthmetic operator selected
  SelectedOptions<- reactive({input$Options})
  
  # Clean data action button
  observeEvent(input$CleanRowSubmit, {
      dataset<-dataset()
      variabelToclean<-input$CleanRows
      if(SelectedOptions()=='Equal'){
          dataset<<-reactive({
          dataset<<-data.frame(dataset[!(dataset[paste(variabelToclean)]==as.double(Value())),])
        })
      }
      else if(SelectedOptions()=='Less'){
        dataset<<-reactive({
          dataset<<-data.frame(dataset[!(dataset[paste(variabelToclean)]<as.double(Value())),])
     })
      }
      else #(SelectedOptions()=='Greater')
        {
          dataset<<-reactive({
          dataset<<-data.frame(dataset[!(dataset[paste(variabelToclean)]>as.double(Value())),])
        })
      }
      
    })
  # Display summary on cleandata tab
  observeEvent(input$CleanDataSummary,{
    output$CleanDataSum<-renderPrint({
      summary(dataset())})
  })

  #************************** NV_SERVER_PLOTS *******************************************
  
  # Select Y variable 
  output$PlotY <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      selectInput("Y_ax","Variable For Y-axis", choices = names(dataset()))})
  
  # Select X variable 
  output$PlotX <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("X_ax","Variable For X-axis", choices =names(data[,!names(data) %in% input$Y_ax]))})
  
  # Select Type of Plot  
  output$TypeOfPlot <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      selectInput("PlotType","Plot Type", choices = c('Scatter Plot','Box Plot','Bar Graph'))})
  
  # Select Type of Legend  
  output$Legend <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Legend","Select Legend", choices = names(Filter(is.factor, data)))
      })
  
  # Generate Plot with selected X, Y, legend and Type of Plot
  output$box <- renderPlot({
    if(is.null(input$file1)) {
      return(NULL) 
    }
    if (is.null(input$Y_ax)) {
      return(NULL)
    }
    if (is.null(input$X_ax)) {
      return(NULL)
    } else{
      p<-ggplot(data = dataset(),mapping = aes(eval(as.name(input$X_ax)), eval(as.name(input$Y_ax))))+theme(legend.title = element_blank()) 
      if ((as.name(input$PlotType))=='Box Plot')
      {
        p+geom_boxplot(notch = FALSE,aes(fill=eval(as.name(input$Legend))))+xlab(input$X_ax)+ylab(input$Y_ax)
      }
      else if ((as.name(input$PlotType))=='Scatter Plot')
      {
        p+geom_jitter(aes(color=eval(as.name(input$Legend))))+xlab(input$X_ax)+ylab(input$Y_ax)
      }
      else
      {
        p+geom_bar(stat='Identity',notch = FALSE,aes(fill=eval(as.name(input$Legend))))+xlab(input$X_ax)+ylab(input$Y_ax)
      }
    }
  })
  
  #************************** NV_SERVER_MODEL *******************************************
  
  # Select Target  
  output$Target <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Target","Target", choices = names(data[,!names(data) %in%  names(Filter(is.factor, data))]),multiple = FALSE)}
    )
  # Select Multiple Predictors  
  output$Predictor <- 
    renderUI({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      data<-dataset()
      selectInput("Predictor","Predictor", choices = names(data[,!names(data) %in% input$Target]),multiple = TRUE)
    }
    
    )
  
  # Develop Linear Model , Generate Summary of Model and allow user to select train % from data
  model <- reactive({
    if (is.null(input$Target)) {
      return(NULL)
    }
    if (is.null(input$Predictor)) {
      return(NULL)
    } 
    set.seed(2)
    SampleSize<-reactive({SampleSize<-as.double(as.double(input$TrainTest)/100)})
    data<-dataset()
    sample <- sample.split(data[paste(input$Target)], SplitRatio=as.double(SampleSize()))
    train <- subset(data, sample==TRUE)
    test <- subset(data, sample==FALSE)
    model<- lm(as.formula(paste(input$Target," ~ ",paste(input$Predictor,collapse="+"))),data=train)
  })
  # Generate Model Summary
  observeEvent(input$ModelSubmit, {
    output$ModelSum<-renderPrint({summary(model())})
  })
  # Reset Model Tab
  observeEvent(input$reset,{
    shinyjs::reset("side-panel")
  })
  
  #************************** NV_SERVER_PREDICTIONS *******************************************
  
  # Display Target used for model
  output$TargetOfModel<-reactive({input$Target})
  
  # Generate Predictions from model
  prediction_df<-reactive({
    if (is.null(input$Target)) {
      return(NULL)
    }
    if (is.null(input$Predictor)) {
      return(NULL)
    } 
    prediction <- predict(model(), dataset())
    predict_df <- data.frame(Predictions = prediction)
    prediction_df <- cbind(predict_df,dataset())
    return(prediction_df)
  })
  
  # Display Predictions generated above  
  observeEvent(input$predict,{
    output$mytable <- DT::renderDataTable({prediction_df()}, selection='none',server = FALSE, escape = FALSE, options = list( 
      paging=TRUE,
      preDrawCallback = JS('function() { 
                           Shiny.unbindAll(this.api().table().node()); }'), 
      drawCallback = JS('function() { 
                        Shiny.bindAll(this.api().table().node()); } ') 
      ) )
})
  
  #************************** NV_SERVER_DOWNLOADPREDICTIONS *******************************************
  
  # Download Prediction on Uploaded data
  output$downloadPrediction <- downloadHandler(
    filename = function() {
      paste('LinearModelPrediction.csv')
    },
    content = function(file) {
      write.csv(prediction_df(), file,row.names = FALSE)
    }
  )
  
  #************************** NV_SERVER_DASHBOARD *******************************************
  
  # Prediction Value Summary with created model
  output$PredictedSummary <- renderPrint({
    if(is.null(input$file1)) {
      text<-'No file loaded'
      session$sendCustomMessage(type='jsCode', list(value = text))
    }
    else {
      predict<-prediction_df()
      summary(predict)
    }
  })
  
  # Select Variable to find diff points on fitted plot
  output$PredictorSelect <- renderUI({
    if(is.null(input$file1)) {
      return(NULL) 
    } else{
      selectInput("SelectVariable","Select Predictor", choices = names(dataset()))}})
  
  
  # Generate Plot after clicking action button for fitted values on dashboard
  observeEvent(input$FittedPlot,{
    
    output$PlotResult <- renderPlot({
      if(is.null(input$file1)) {
        return(NULL) 
      }
      if (is.null(input$SelectVariable)) {
        return(NULL)
      }
      ggplot(data = prediction_df(),mapping = aes(eval(as.name(input$Target)), Predictions,color=eval(as.name(input$SelectVariable))))+
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)+          
        xlab(input$Target)+
        ylab("Predicted Value")+
        theme(legend.position = "top",
              legend.direction = "horizontal")+
        theme(legend.title = element_blank())
    })
  })
  
  # Fetch model smmarry attributes and display on dashboard
  ModelSumDashboard<-reactive({
    model<-model()
    data.frame(Sigma=summary(model)$sigma,Degree_of_Freedom=model$df.residual,R_Squared=summary(model)$r.squared,Adjusted_R_Squared=summary(model)$adj.r.squared, stringsAsFactors=FALSE)
  })
  # Generate Model Summary on dashboard after clicking action button
  observeEvent(input$ModelSummary, {
    output$ModelSumDashboard<-renderTable({
      ModelSumDashboard()
    })
  
  # Dispay taget and predictors used in model creation
    output$DisplayTarget<-reactive({input$Target})
    
    output$DisplayPredictors<-reactive({list(input$Predictor)})
  })
  # Dispay T-Static plot
  observeEvent(input$TStaticDashboard, {
    output$TStaticDashboard<-renderPlot({
      model<-model()
      cols <- termColours(model)
      plottStats(model,cols)
    })
  })
  # Dispay Model plot
  observeEvent(input$ModelSummaryDashboard, {
    output$ModelSummaryDashboard<-renderPlot({
      model<-model()
      plot_summs(model, scale = TRUE,plot.distributions = TRUE,inner_ci_level = .9)
      
    })
  })
  
  # Dispay F-Static plot
  observeEvent(input$FStaticDashboard, {
    output$FStaticDashboard<-renderPlot({
      model<-model()
      cols <- termColours(model)
      plotAnovaStats(model,type="F")
    })
    
    
  })
  
      }

shinyApp(ui, server)