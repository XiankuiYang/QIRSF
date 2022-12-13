library(shiny)

ui <- fluidPage(
  titlePanel(h1(strong("Input and Response Space Filling via Clustering Minimax Method"),style = "font-family:'calibri';font-size:20pt",align = "center")),
  sidebarLayout(
    sidebarPanel(
      h2(strong("Prepare Data:"), style = "font-family:'arial';font-size:14pt"),
      strong(tags$hr(style="border-top: 2px solid #000000;")),
      h3(strong("Download Demonstration Materials:"), style = "font-family:'arial';font-size:11pt"),
      downloadButton("Regression_data",label = "Regression model.csv",style = "width:260px;"),
      br(),
      downloadButton("RWS_data",label = "Regression with saddle point.csv",style = "width:260px;"),
      br(),
      downloadButton("RWI_data",label = "Regression with irregular area.csv",style = "width:260px;"),
      br(),
      downloadButton("Exponential_data",label = "Exponential function.csv",style = "width:260px;"),
      br(),
      downloadButton("GL_data",label = "Grmacy and Lee function.csv",style = "width:260px;"),
      br(),
      downloadButton("TITR_data",label = "Two inputs and two responses.csv",style = "width:260px;"),
      br(),
      downloadButton("TrIOR_data",label = "Three inputs and one response.csv",style = "width:260px;"),
      br(),
      h3(strong("Upload Data:"), style = "font-family:'arial';font-size:11pt"),
      fileInput("Upload_file",NULL,buttonLabel = "Upload data",accept = c(".csv")),
      p("Please enter the dimension of input space and response space",style = "font-family:'arial';font-size:12pt"),
      fluidRow(
        column(5,
               numericInput("Dim_input",strong("Input",style = "font-family:'arial';font-size:11pt"),value = NULL,min = 1,step = 1,width = "100%")
        ),
        column(5,
               numericInput("Dim_response",strong("Response",style = "font-family:'arial';font-size:11pt"),value = NULL,min = 1,step = 1,width = "100%")
        ),
      ),
      h2(strong("IRSF-clustering:"), style = "font-family:'arial';font-size:14pt"),
      strong(tags$hr(style="border-top: 2px solid #000000;")),
      selectInput("Method",strong("Select Method(s):", style = "font-family:'arial';font-size:12pt"),choices = list("\n","Input space filling (ISF)"="ISF",
                                                                                                                    "Response space filling (RSF)"="RSF",
                                                                                                                    "Input and response space filling (IRSF)"="IRSF")),
      conditionalPanel(
        condition = "input.Method == 'IRSF'",
        numericInput("Num_wt",strong("Number of weights:", style = "font-family:'arial';font-size:11pt"),
                     value = 50,min = 1,step = 1)
      ),
      numericInput("Num_runs",label = "Please enter the number of runs:",value = 10,min = 1,step = 1),
      actionButton("work",strong("Generate",style = "font-family:'arial';font-size:11pt"))
    ),
    mainPanel(
      tabsetPanel(
        id = "TABS",
        tabPanel(strong("Data Visualization",style = "font-family:'arial';font-size:14pt"),
                 h2(strong("Instructions:"), style = "font-family:'arial';font-size:14pt"),
                 br(),
                 br(),
                 p("1.Complete the Prepare Data section to generate the Data Vizualization and Candidate Data tabs",
                   style = "font-family:'arial';font-size:11pt"),
                 br(),
                 p("2.Complete the IRSF-clustering section to generate ISF, RSF, IRSF Pareto Front",
                   style = "font-family:'arial';font-size:11pt")
        ),
        tabPanel( strong("Candidate data",style = "font-family:'arial';font-size:14pt"),
                  numericInput("n", strong("Rows",style = "font-family:'arial';font-size:12pt"), value = 5, min = 1, step = 1),
                  tableOutput("head"),
                  plotOutput("cand_plot")
        ),
        tabPanel(strong("ISF",style = "font-family:'arial';font-size:14pt"),value = "ISF",
                 p(strong("ISF Design Table", style = "font-family:'arial';font-size:12pt")),
                 fluidRow(
                   column(5,
                          tableOutput("ISF_design")
                   ),
                   column(5,
                          plotOutput("ISF_3d_plot")
                   )
                 ),
                 plotOutput("ISF_design_plot")),
        tabPanel(strong("RSF",style = "font-family:'arial';font-size:14pt"),value = "RSF",
                 p(strong("RSF Design Table", style = "font-family:'arial';font-size:12pt")),
                 fluidRow(
                   column(5,
                          tableOutput("RSF_design")
                   ),
                   column(5,
                          plotOutput("RSF_3d_plot")
                   )
                 ),
                 plotOutput("RSF_design_plot")),
        tabPanel(strong("IRSF",style = "font-family:'arial';font-size:14pt"),value = "IRSF",
                 fluidRow(
                   column(5,
                          plotOutput("IRSFPF_plot",click = "plot_click")
                   ),
                   column(5,
                          tableOutput("IRSF_table")                   
                   )
                 ),
                 fluidRow(
                   column(5,
                          p("Design table",style = "font-family:'arial';font-size:14pt"),
                          tableOutput("Specific_design")
                   ),
                   column(5,
                          plotOutput("Specific_design_3d_plot")
                   )
                 ),
                 plotOutput("Specific_design_plot")
        )
      )
      
    )
  )
)