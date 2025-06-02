library(shiny)
library(shinyjs)
library(DT)

jscode <- "shinyjs.refresh_page = function() { history.go(0); }"
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "refresh_page"),
  # tags$style(HTML("
  #   body {
  #     background-color: #fff9c4; /* Light yellow */
  #   }
  #   .well {
  #     background-color: #ffecb3 !important; /* Peach sidebar */
  #     border: none; /* Optional: remove border for a cleaner look */
  #     box-shadow: none; /* Optional: remove shadow */
  #   }
  # ")),
  titlePanel(h1(strong("Quick Input-Response Space-Filling (QIRSF) Designs"),style = "font-family:'calibri';font-size:20pt",align = "center")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(10,h2(strong("Prepare Data:"), style = "font-family:'arial';font-size:14pt",align = "left")),
        column(2,actionButton("refresh",strong("Refresh",style = "font-family:'arial';font-size:12pt",align = "center")))
      ),
      h3(strong("Download Demonstration Materials:"), style = "font-family:'arial';font-size:11pt"),
      downloadButton("User_Guide",label = "User Guide",style = "width:260px;"),
      br(),
      selectInput("download", "Select data to download", choices = c("2-D rectangular input with a maximum", 
                                                                     "2-D rectangular input with a saddle point", 
                                                                     "2-D non-rectangular input",
                                                                     "Exponential weight function", 
                                                                     "Gramacy & Lee function",
                                                                     "2 inputs & 2 resp.",
                                                                     "3 inputs & 1 resp.")),
      downloadButton("downloadData"),
      
      br(),
      h3(strong("Upload Data:"), style = "font-family:'arial';font-size:11pt"),
      fileInput("Upload_file",NULL,buttonLabel = "Upload data",accept = c(".csv")),
      h3(strong("Data Recognition:"), style = "font-family:'arial';font-size:11pt"),
      p("Please enter the dimension of input space and response space",style = "font-family:'arial';font-size:12pt"),
      fluidRow(
        column(5,
               numericInput("Dim_input",strong("Input",style = "font-family:'arial';font-size:11pt"),value = NULL,min = 1,step = 1,width = "100%")
        ),
        column(5,
               numericInput("Dim_response",strong("Response",style = "font-family:'arial';font-size:11pt"),value = NULL,min = 1,step = 1,width = "100%")
        ),
      ),
      h2(strong("QIRSF:"), style = "font-family:'arial';font-size:14pt"),
      strong(tags$hr(style="border-top: 2px solid #000000;")),
      selectInput("Method",strong("Select Method(s):", style = "font-family:'arial';font-size:12pt"),choices = list("\n","QISF"="ISF",
                                                                                                                    "QRSF"="RSF",
                                                                                                                    "QIRSF - Minimax"="IRSF")),
      conditionalPanel(
        condition = "input.Method == 'IRSF'",
        numericInput("Num_wt",strong("Number of weights:", style = "font-family:'arial';font-size:11pt"),
                     value = 50,min = 1,step = 1),
        p(span(strong("Attention"),style = "color:red"),": This value heavily affects the running time and memory usage.",style = "font-family:'arial';font-size:12pt")
      ),
      shinyFeedback::useShinyFeedback(),
      numericInput("Num_runs",label = "Number of Runs:",value = 10,min = 1,step = 1),
      textOutput("warningA"),
      actionButton("work",strong("Generate",style = "font-family:'arial';font-size:11pt"))
    ),
    mainPanel(
      tabsetPanel(
        id = "TABS",
        tabPanel(strong("Instructions",style = "font-family:'arial';font-size:14pt"),
                 h2(strong("Step:",
                           style = "font-family:'arial';font-size:16pt")),
                 p("1. Complete the Prepare Data section to generate the Data Visualization tab.",
                   style = "font-family:'arial';font-size:11pt"),
                 p("2. Complete the QIRSF section to generate the minimax designs",
                   style = "font-family:'arial';font-size:11pt"),
                 h2(strong("Notes:",
                           style = "font-family:'arial';font-size:16pt")),
                 img(src='Data example.png',align = "right",height="20%", width="20%"),
                 p("1. The uploaded data file must be a .CSV file. Meanwhile, the format must match the right example. 
                   The right columns must be for the response variables After you upload a data file, please enter the number of columns 
                   for the input variables and response variables, respectively. Generally, the number columns in the file should be (dimension of input space) + (dimension of response space).",
                   style = "font-family:'arial';font-size:11pt"),
                 p("2. To start with new data, please click the 'Refresh' button to reset the environment.",
                   style = "font-family:'arial';font-size:11pt"),
                 em("For more details, please see User Guide (on left).",
                    style = "font-family:'arial';font-size:11pt")
        ),
        tabPanel( strong("Data Visualization",style = "font-family:'arial';font-size:14pt"),
                  numericInput("n", strong("Number of Rows",style = "font-family:'arial';font-size:12pt"), value = 5, min = 1, step = 1),
                  DT::dataTableOutput("head"),
                  plotOutput("cand_plot")
        ),
        tabPanel(strong("QISF",style = "font-family:'arial';font-size:14pt"),value = "ISF",
                 p(strong("ISF Design Table", style = "font-family:'arial';font-size:12pt")),
                 downloadButton("download_ISF_design", "Download ISF Design Table", style = "margin-bottom:10px"),
                 fluidRow(
                   column(5,
                          DT::dataTableOutput("ISF_design")
                   ),
                   column(5,
                          plotOutput("ISF_3d_plot", width = "100%")
                   )
                 ),
                 plotOutput("ISF_design_plot")),
        tabPanel(strong("QRSF",style = "font-family:'arial';font-size:14pt"),value = "RSF",
                 p(strong("RSF Design Table", style = "font-family:'arial';font-size:12pt")),
                 downloadButton("download_RSF_design", "Download RSF Design Table", style = "margin-bottom:10px"),
                 fluidRow(
                   column(5,
                          DT::dataTableOutput("RSF_design")
                   ),
                   column(5,
                          plotOutput("RSF_3d_plot", width = "100%")
                   )
                 ),
                 plotOutput("RSF_design_plot")),
        tabPanel(strong("QIRSF",style = "font-family:'arial';font-size:14pt"),value = "IRSF",
                 fluidRow(
                   column(5,
                          h2(strong("Pareto Front Plot:"), style = "font-family:'arial';font-size:14pt"),
                          plotOutput("IRSFPF_plot",click = "plot_click",width = "100%")
                   ),
                   column(5,
                          h2(strong("Criterion Table:"), style = "font-family:'arial';font-size:14pt"),
                          downloadButton("download_IRSF_table", "Download Criterion Table", style = "margin-bottom:10px"),
                          DT::dataTableOutput("IRSF_table")                   
                   )
                 ),
                 fluidRow(
                   column(5,
                          p(strong("Design table:"),style = "font-family:'arial';font-size:14pt"),
                          downloadButton("download_Specific_design", "Download Design Table", style = "margin-bottom:10px"),
                          DT::dataTableOutput("Specific_design")
                   ),
                   column(5,
                          plotOutput("Specific_design_3d_plot",width = "100%")
                   )
                 ),
                 plotOutput("Specific_design_plot")
        )
      )
      
    )
  )
)