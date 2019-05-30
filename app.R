#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dr4pl)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
# Define UI for application that draws a histogram
parseFriendly <- function(x) {
  x <- ifelse(str_detect(string = x, pattern = " "), paste(sep = "", "`",x,"`"), x )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".sticky{
      position: sticky;
      top: 0;
      }
      "
    ))
    
    ),
  titlePanel("dr4pl Shiny Module"),
  fluidRow(column(2, class="well sticky",
                  tags$ul(class="nav nav-pills nav-stacked shiny-tab-input",
                          tags$li(class="active",tags$a(href="#tab-5482-1",
                                                        'data-toggle'="tab",
                                                        'data-value'="Data Input",
                                                        "Data Input")
                          ),
                          tags$li(tags$a(href="#tab-5482-2",
                                         'data-toggle'="tab",
                                         'data-value'="Subsetting",
                                         "Subsetting")
                          ),
                          tags$li(tags$a(href="#tab-5482-3",
                                         'data-toggle'="tab",
                                         'data-value'="Regression",
                                         "Regression")
                          )
                  )
  ),
  column(10,
         div(class="tab-content", 'data-tabsetid'="5482",
             div(class="tab-pane active",
                 'data-value'="Data Input",
                 id="tab-5482-1",
                 tabsetPanel(
                   tabPanel("csv",
                            inputPanel(fileInput(inputId = "file.csv",
                                                 label = "Choose csv file",
                                                 accept = c(".csv")),
                                       checkboxInput("header.csv", "Header", TRUE),
                                       actionButton("submit.csv", "Submit")
                            )
                   ),
                   tabPanel("xlsx",
                            inputPanel(fileInput(inputId = "file.xlsx",
                                                 label = "Choose xlsx file",
                                                 accept = c(".xls",".xlsx")),
                                       uiOutput("excel_sheet"),
                                       checkboxInput("colnames.xlsx", "Column Names", TRUE),
                                       actionButton("submit.xlsx", "Submit")
                            )
                   ),
                   tabPanel("tsv",
                            inputPanel(fileInput(inputId = "file.tsv",
                                                 label = "Choose tsv file",
                                                 accept = c(".tsv")),
                                       checkboxInput("header.tsv", "Header", TRUE)),
                            actionButton("submit.tsv", "Submit")),
                   tabPanel("table",
                            inputPanel(fileInput(inputId = "file.table",
                                                 label = "Choose a file"),
                                       checkboxInput("header.table", "Header", FALSE),
                                       textInput("sep.table", "Sep", ""),
                                       selectInput("dec.table", label = "Decimal character", choices = c(".",","),selected = "."),
                                       numericInput("skip.table", label = "Skip Number of Rows",value = 0),
                                       actionButton("submit.table", "Submit")))
                   
                 ),
                 hr(),
                 div(style = 'overflow-x: scroll',
                     dataTableOutput(outputId = "dr4pl.submit.data")
                 )
             ),
             div(class="tab-pane",
                 'data-value'="Subsetting",
                 id="tab-5482-2",
                 fluidRow(inputPanel(column(12,
                                            uiOutput("p.col.select")),
                                     column(12,
                                            uiOutput("p.facto.filt")),
                                     column(12,
                                            actionButton(inputId = "apply_sub", "Apply Subset"),
                                            actionButton(inputId = "apply_reset", "Reset Subset"))),
                          hr(),
                          h3("Subset Preview"),
                          dataTableOutput(outputId = "dat_preview"))
             ),
             div(class="tab-pane",
                 'data-value'="Regression",
                 id="tab-5482-3",
                 fluidRow(inputPanel(column(12,
                                            uiOutput("Dose.selection"),
                                            uiOutput("Response.selection")),
                                     column(12,
                                            radioButtons("dr4pl.trend",
                                                         "Curve Trend",
                                                         choices = c("auto",
                                                                     "increasing",
                                                                     "decreasing"),
                                                         selected = "auto"),
                                            radioButtons("dr4pl.method.init",
                                                         "Initialization Method",
                                                         choices = c("Mead", "logistic"),
                                                         selected = "Mead")),
                                     column(12,
                                            radioButtons("dr4pl.method.robust",
                                                         "Robust Estimation Method",
                                                         choices = c("squared",
                                                                     "absolute",
                                                                     "Huber",
                                                                     "Tukey"),
                                                         selected = "squared"),
                                            radioButtons("dr4pl.method.optim",
                                                         "Optimization Method",
                                                         choices = c("Nelder-Mead","BFGS",
                                                                     "CG","L-BFGS-B","SANN"),
                                                         selected = "Nelder-Mead")),
                                     column(12,
                                            checkboxInput("dr4pl.use.Hessian",
                                                          "Use Hessian Matrix",
                                                          value = TRUE),
                                            checkboxInput("use.init.param",
                                                          "Use Initialization Parameters",
                                                          value = F),
                                            checkboxInput("use.upper.lim",
                                                          "Impose Upper Limit",
                                                          value = F),
                                            checkboxInput("use.lower.lim",
                                                          "Impose Lower Limit",
                                                          value = F)),
                                     column(12,
                                            h5("Initialization Parameters"),
                                            textInput(inputId = "init.upperBound",
                                                      label = "Upper Bound",
                                                      value =  NULL),
                                            textInput(inputId = "init.lowerBound",
                                                      label = "Lower Bound",
                                                      value = NULL),
                                            textInput(inputId = "init.IC50",
                                                      label = "IC50",
                                                      value = NULL),
                                            textInput("init.slope",
                                                      label = "Slope",
                                                      value = NULL)),
                                     column(12,
                                            h5("Upper Limit Restraints"),
                                            textInput(inputId = "ubul",
                                                      label = "Upper Bound",
                                                      value =  NULL),
                                            textInput(inputId = "lbul",
                                                      label = "Lower Bound",
                                                      value = NULL),
                                            textInput(inputId = "icul",
                                                      label = "IC50",
                                                      value = NULL),
                                            textInput("slul",
                                                      label = "Slope",
                                                      value = NULL)),
                                     column(12,
                                            h5("Lower Limit Restraints"),
                                            textInput(inputId = "ubll",
                                                      label = "Upper Bound",
                                                      value =  NULL),
                                            textInput(inputId = "lbll",
                                                      label = "Lower Bound",
                                                      value = NULL),
                                            textInput(inputId = "icll",
                                                      label = "IC50",
                                                      value = NULL),
                                            textInput("slll",
                                                      label = "Slope",
                                                      value = NULL)))
                          
                          
                 ),
                 hr(),
                 plotOutput(outputId = "dr4pl.plot"),
                 hr(),
                 fluidRow(column(6,
                                 inputPanel(column(12,
                                                   textInput(inputId = "plot.title",
                                                             "Plot Title",
                                                             "Dose-Response Plot"),
                                                   uiOutput(outputId = "plot.x.text"),
                                                   uiOutput(outputId = "plot.y.text")),
                                            column(12,
                                                   checkboxInput(inputId = "ind.outlier", "Report Outliers"),
                                                   checkboxInput(inputId = "inc.curve", "Show Curve", value = TRUE)),
                                            column(12,
                                                   checkboxInput("IC50_include", "Include IC50", value = TRUE),
                                                   numericInput(inputId = "IC50_x", "IC50 x-axis", value = 100),
                                                   uiOutput(outputId = "IC50_yui"),
                                                   numericInput("text.size",label = "Label Size", value = 8)),
                                            column(12,
                                                   uiOutput(outputId = "dl.filename.opt"),
                                                   downloadButton(outputId = "get.dr4pl.plot"),
                                                   numericInput("width", "Plot Width (in)", value = 8),
                                                   numericInput("height", "Plot Height (in)", value = 6)))),
                          
                          column(6,
                                 verbatimTextOutput(outputId = "dr4pl.summary")))
             )  
         )
         
  )
)
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   values <- reactiveValues(submit.csv =0, submit.xlsx = 0, submit.tsv = 0, submit.table = 0, submit.preview = 0)
   
   observeEvent(input$submit.csv, {
     values$submit.csv <- 1
     values$submit.xlsx <- 0
     values$submit.tsv <- 0
     values$submit.table <- 0
     values$submit.preview <- 0
   })
   observeEvent(input$submit.xlsx, {
     values$submit.csv <- 0
     values$submit.xlsx <- 1
     values$submit.tsv <- 0
     values$submit.table <- 0
     values$submit.preview <- 0
   })
   observeEvent(input$submit.tsv, {
     values$submit.csv <- 0
     values$submit.xlsx <- 0
     values$submit.tsv <- 1
     values$submit.table <- 0
     values$submit.preview <- 0
   })
   observeEvent(input$submit.table, {
     values$submit.csv <- 0
     values$submit.xlsx <- 0
     values$submit.tsv <- 0
     values$submit.table <- 1
     values$submit.preview <- 0
   })
   subset_store <- reactiveValues()
   observeEvent(input$apply_sub, {
     #values$submit.preview <- 1
     subset_store$data <- pData()
   })
   observeEvent(input$apply_reset, {
     #values$submit.preview <- 0
     subset_store$data <- inputData()
   })
   output$excel_sheet <- renderUI({
     if(is.null(input$file.xlsx$datapath)){
       return(NULL)
     }
     o <- readxl::excel_sheets(path = input$file.xlsx$datapath)
     selectInput("sheet.xlsx", label = "Specify Sheet", choices = o, selected = o[1])
   })
   inputData <- reactive({
     if(input$submit.csv==0&&input$submit.xlsx==0&&input$submit.tsv==0&&input$submit.table==0){
       return(NULL)
     }
    isolate({
       if(values$submit.csv){
         data <- read.csv(file = input$file.csv$datapath,
                          header = input$header.csv)
       } else if(values$submit.xlsx){
         data <- readxl::read_xlsx(path = input$file.xlsx$datapath,
                                   col_names = input$colnames.xlsx, sheet = input$sheet.xlsx)
       } else if(values$submit.tsv){
         data <- read.delim(file = input$file.tsv$datapath,
                            header = input$header.tsv)
       } else if(values$submit.table){
         data <- read.table(file = input$file.table$datapath,
                            header = input$header.table,
                            sep = input$sep.table,
                            dec = input$dec.table,
                            skip = input$skip.table)
       }
       subset_store$data <- data
       return(data)
    })
   })
   dataName <- reactive({
     if(values$submit.csv){
       name <- input$file.csv$name
       name <- gsub(pattern = ".csv", replacement = "", x = name)
     } else if(values$submit.xlsx){
       name <- input$file.xlsx$name
       name <- gsub(pattern = ".xlsx", replacement = "", x = name)
     } else if(values$submit.tsv){
       name <- input$file.tsv$name
       name <- gsub(pattern = ".tsv", replacement = "", x = name)
     } else if(values$submit.table){
       name <- input$file.table$name
     }
     name
   })
   output$p.col.select <- renderUI({
     if(is.null(inputData())){
       return(NULL)
     } else {
       c.names <- colnames(inputData())
       selectInput("p.selected", "Select Column to Subset upon",choices = c.names, selected = c.names[1])
     }
   })
   output$p.facto.filt <- renderUI({
     if(is.null(inputData())){
       return(NULL)
     } else if(is.null(input$p.selected)){
       return(NULL)
     } else {   
       uni.c <- eval(parse(text = paste(sep = "","unique(inputData()$",parseFriendly(input$p.selected),")")))
       checkboxGroupInput(inputId = "p.facto.selected", "Select Factors to Include", choices = uni.c, selected = uni.c, inline = T)
     }
   })
   pData <- reactive({
     if(is.null(inputData())){
       return(NULL)
     } 
       pfs <- input$p.facto.selected
       data <- subset_store$data  %>% filter(eval(parse(text = paste(sep ="", parseFriendly(input$p.selected), " %in% pfs"))))
     
     return(data)
   })
   output$dat_preview <- renderDataTable({
     pData()
   })
   output$Dose.selection <- renderUI({
     if(is.null(inputData())){
       return(NULL)
     } else {
       c.names <- colnames(subset_store$data)
       selectInput("dr4pl.Dose", "Dose Column",choices = c.names)
     }
   })
   output$Response.selection <- renderUI({
     if(is.null(inputData())){
       return(NULL)
     } else {
       c.names <- colnames(subset_store$data)
       selectInput("dr4pl.Response", "Response Column",choices = c.names)
     }
   })
   output$dr4pl.submit.data <- renderDataTable({
     if(is.null(inputData())){
       return()
     }
     inputData()
   })
   dr4pl_reactive <- reactive({
     data <- subset_store$data
     if(is.null(input$dr4pl.Dose)|is.null(input$dr4pl.Response)){
       return(NULL)
     } else {
       colnames(data)[colnames(data)%in%input$dr4pl.Dose] <- "Dose"
       colnames(data)[colnames(data)%in%input$dr4pl.Response] <- "Response"
       init.parm <- NULL
       if(input$use.init.param){
         if(!any(c(is.na(as.numeric(input$init.upperBound)),
                  is.na(as.numeric(input$init.IC50)),
                  is.na(as.numeric(input$init.slope)),
                  is.na(as.numeric(input$init.lowerBound))))){
           init.parm <- c(as.numeric(input$init.upperBound),
                          as.numeric(input$init.IC50),
                          as.numeric(input$init.slope),
                          as.numeric(input$init.lowerBound))
         } 
       
       } 
       upperl.parm <- NULL
       if(input$use.upper.lim){
         if(!any(c(is.na(as.numeric(input$ubul)),
                   is.na(as.numeric(input$icul)),
                   is.na(as.numeric(input$slul)),
                   is.na(as.numeric(input$lbul))))){
           upperl.parm <- c(as.numeric(input$ubul),
                            as.numeric(input$icul),
                            as.numeric(input$slul),
                            as.numeric(input$lbul))
         } 
         
       } 
       lowerl.parm <- NULL
       if(input$use.lower.lim){
         if(!any(c(is.na(as.numeric(input$ubll)),
                   is.na(as.numeric(input$icll)),
                   is.na(as.numeric(input$slll)),
                   is.na(as.numeric(input$lbll))))){
           lowerl.parm <- c(as.numeric(input$ubll),
                            as.numeric(input$icll),
                            as.numeric(input$slll),
                            as.numeric(input$lbll))
         } 
         
       } 
       dr4plObj <- dr4pl(formula = Response~Dose,
                         data = data,
                         init.parm = init.parm,
                         trend = input$dr4pl.trend,
                         method.init = input$dr4pl.method.init,
                         method.robust = input$dr4pl.method.robust,
                         method.optim = input$dr4pl.method.optim,
                         use.Hessian = input$dr4pl.use.Hessian,
                         upperl = upperl.parm,
                         lowerl = lowerl.parm)
       return(dr4plObj)
     }
   })
   output$plot.x.text <- renderUI({
     if(is.null(inputData())){
       textInput(inputId = "text.x", "X-axis Label", value = "Dose")
     } else {
       textInput(inputId = "text.x", "X-axis Label", value = input$dr4pl.Dose)
     }
   })
   output$plot.y.text <- renderUI({
     if(is.null(inputData())){
       textInput(inputId = "text.y", "Y-axis Label", value = "Response")
     } else {
       textInput(inputId = "text.y", "Y-axis Label", value = input$dr4pl.Response)
     }
   })
   dr4pl_plot <- reactive({
     if(is.null(dr4pl_reactive())){
       return()
     } else {
       if(input$inc.curve){
         type.curve <- "all"
       } else {
         type.curve <- "data"
       }
       if(input$ind.outlier){
         indices.outlier <- "report"
       } else {
         indices.outlier <- NULL
       }
       
       
       p <- plot(dr4pl_reactive(),
                 text.title = input$plot.title,
                 text.x = input$text.x,
                 text.y = input$text.y,
                 indices.outlier = indices.outlier,
                 type.curve = type.curve,
                 color.vec = "blue",
                 labels = "dr4plObj") + ggplot2::guides(color = F)
       if(input$IC50_include){
         p <- p + geom_text(aes(label = paste("IC50:", round(dr4pl_reactive()$parameters[2],1)), x = input$IC50_x, y = input$IC50_y), size = input$text.size)
       }
       p
     }
   })
   output$IC50_yui <- renderUI({
     numericInput(inputId = "IC50_y", "IC50 y-axis", value = (max(dr4pl_reactive()$data$Response)-min(dr4pl_reactive()$data$Response))/2)
   })
   output$dr4pl.plot <- renderPlot({
     dr4pl_plot()
   })
   
   output$dr4pl.summary <- renderPrint({
     init.parm <- "NULL"
     if(input$use.init.param){
       if(!any(c(is.na(as.numeric(input$init.upperBound)),
                 is.na(as.numeric(input$init.IC50)),
                 is.na(as.numeric(input$init.slope)),
                 is.na(as.numeric(input$init.lowerBound))))){
         init.parm <- paste(sep = "", "c(",
                            input$init.upperBound,", ",
                            input$init.IC50,", ",
                            input$init.slope,", ",
                            input$init.lowerBound,")")
       } 
       
     } 
     upperl.parm <- "NULL"
     if(input$use.upper.lim){
       if(!any(c(is.na(as.numeric(input$ubul)),
                 is.na(as.numeric(input$icul)),
                 is.na(as.numeric(input$slul)),
                 is.na(as.numeric(input$lbul))))){
         upperl.parm <- paste(sep = "","c(",
                              input$ubul,", ",
                              input$icul,", ",
                              input$slul,", ",
                              input$lbul,")")
       } 
       
     } 
     lowerl.parm <- "NULL"
     if(input$use.lower.lim){
       if(!any(c(is.na(as.numeric(input$ubll)),
                 is.na(as.numeric(input$icll)),
                 is.na(as.numeric(input$slll)),
                 is.na(as.numeric(input$lbll))))){
         lowerl.parm <- paste(sep = "","c(",
                              input$ubll,", ",
                              input$icll,", ",
                              input$slll,", ",
                              input$lbll,")")
       } 
       
     } 
     call <- paste(sep = "",
                   "dr4pl.formula(formula = ",
                   parseFriendly(input$dr4pl.Response),
                   "~",
                   parseFriendly(input$dr4pl.Dose),
                   ", data = data, \n init.parm = ",
                   init.parm,
                   ", trend = \"", input$dr4pl.trend,
                   "\", \n method.init = \"", input$dr4pl.method.init,
                   "\", method.robust = \"", input$dr4pl.method.robust,
                   "\",\n method.optim = \"", input$dr4pl.method.optim,
                   "\", use.Hessian = ", input$dr4pl.use.Hessian,
                   ", \n upperl = ", upperl.parm, ", lowerl = ", lowerl.parm, ")")
     cat("Call\n")
     cat(call)
     cat("\n")
     stuff <- list(summary(dr4pl_reactive())$coefficients, dr4pl_reactive()$sample.size ,dr4pl_reactive()$message.diagnosis)
     names(stuff) <- c("Coefficients","Sample Size", "Message Diagnosis")
     stuff
   })
   output$dl.filename.opt <- renderUI({
     if(is.null(inputData())){
       textInput(inputId = "dl.filename",label = "File Name", value = "dr4pl_plot")
     } else {
       textInput(inputId = "dl.filename",label = "File Name", value = dataName())
     }
   })
   output$get.dr4pl.plot <- downloadHandler(filename = function() {paste(sep = "",input$dl.filename,".png")},
                                            content = function(file) {
                                              device <- function(..., width, height) {
                                                grDevices::png(..., width = input$width, height = input$height,
                                                               res = 300, units = "in")
                                              }
                                              ggplot2::ggsave(file , plot = dr4pl_plot(), device = device)
                                            })
}

# Run the application 
shinyApp(ui = ui, server = server)

