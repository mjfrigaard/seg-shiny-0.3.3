# =====================================================================#
# This is code to create: ui.R for The SEG Shiny app
# Authored by and feedback to mjfrigaard@gmail.com
# MIT License
# Version: 0.1.3.3 ----
# =====================================================================#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# Set the file limit request to larger -----
options(shiny.maxRequestSize = 30 * 1024^2)

# load the packages -------------------------------------------------------

library(colourpicker)
library(shiny)
library(shinythemes)
library(tidyverse)
library(styler)
library(magrittr)
library(heatmaply)
library(tools) # for tools
library(data.table) # for fread() function

# source helpers.R file -------------------------------------------------
source("helpers.R")
# this loads the lookup table, the riskpair data, and the SEG plot

# DEFINE THE UI ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ -----

ui <- fluidPage(
  # this is the pre-determined theme from shinythemes
  # check out the gallery here: https://rstudio.github.io/shinythemes/
  theme = shinytheme("spacelab"),
  
  # 0.0 ## ## ## ## ## ## ## START titlePanel( ----
  titlePanel(
    title = "Blood Glucose Monitoring System Surveillance Program",
    windowTitle = "SEG"
  ),
  
  # 0.1 ## ## ## ## ## START sidebarPanel( ----
  
  sidebarPanel(
    
    # 0.1.0 <TITLE TEXT> -----
    em("Welcome to the surveillance error graph!"),
    
    br(),
    
    # Horizontal line
    tags$hr(),
    
    # 0.1.1 - <SIDEPANEL> Download: sample file [downloadSampleData] ----
    
    h4("Download a sample CSV file:"),
    
    downloadButton(
      outputId = "downloadSampleData",
      label = "Download a sample CSV file"
    ),
    
    br(),
    # Horizontal line
    tags$hr(),
    
    # 0.1.2 - <SIDEPANEL> Upload: upload file [$file1] ----
    # ** ** ** This is where we define the input for the .csv file!!! ** ** ** ----
    h4("Import your own CSV file:"),
    
    fileInput(
      # ~~~~ CSV file inputId ~~~~ ~~~~ ----
      inputId = "file1",
      label = "Choose File to Upload:",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    
    # 0.1.3 - <SIDEPANEL> Select number of rows to display [disp] ----
    
    h4("Select number of rows to display in your .csv file:"),
    
    
    
    radioButtons(inputId = "disp", label = "Display",
                 choiceNames = list("Top 10", "Top 20"),
                 choiceValues = list("Top 10", "Top 20")
    ),
    
    # 0.1.4 - <SIDEPANEL> Table Output: display data file [contents] ----
    
    tableOutput(outputId = "contents"),
    
    # Horizontal line
    tags$hr(),
    
    # 0.1.5  - <SIDEPANEL> Title for summary tables  -----
    
    h4("Create Summary Tables:"),
    
    # 0.1.6 - <SIDEPANEL> [Button = SEGSummaryTableButton] for summary -------
    
    actionButton(
      inputId = "SEGSummaryTableButton",
      label = "Create Summary Tables"
    ),
    
    # Horizontal line
    tags$hr(),
    
    # 0.1.7 - <SIDEPANEL> [Button 2 = $CreateSEGTables] for SEG tables button  -----
    h4("Create SEG Tables:"),
    
    actionButton(
      inputId = "CreateSEGTables",
      label = "Create SEG Tables"
    ),
    
    # Horizontal line
    tags$hr(),
    
    # 0.1.8 - <SIDEPANEL> CUSTOMIZE SEG (h4) ----
    h4("Customize your SEG:"),
    
    # 0.1.9  - <SIDEPANEL> INPUTS for [alpha] ----
    sliderInput(
      inputId = "alpha",
      label = "Alpha (opacity) of points:",
      min = 0,
      max = 1,
      value = 0.5
    ),
    
    # 0.2.0  - <SIDEPANEL> INPUTS for point [color] ----
    colourInput(
      inputId = "color",
      label = "Point color",
      value = "white"
    ),
    
    # 0.2.1  - <SIDEPANEL> INPUTS for point [size] ----
    numericInput(
      inputId = "size",
      label = "Point size",
      value = 2,
      min = 1
    ),
    
    # 0.2.2  -<SIDEPANEL> INPUTS text for plot title [plot_title] -----
    textInput(
      inputId = "plot_title",
      label = "Plot title",
      placeholder = "Enter text to be used as plot title"
    ),
    
    # 0.2.3  - <SIDEPANEL> INPUTS SEG plot file download [heatmap_file] -----
    radioButtons(
      inputId = "heatmap_file",
      label = "Select the file type for your SEG",
      choices = list("png", "pdf")
    ),
    
    br(),
    # download id
    downloadButton(
      outputId = "heatmap_file_download",
      label = "Download SEG"
    ),
    
    # 0.2.4 - <SIDEPANEL> INPUTS MOD BA plot file download -----
    radioButtons(
      inputId = "modba_plot_file",
      label = "Select the file type for your Bland Altman Plot",
      choices = list("png", "pdf")
    ),
    
    br(),
    
    # download id
    downloadButton(
      outputId = "modba_plot_download",
      label = "Download Mod-BA plot"
    ),
    
    # add some space
    br(),
    # Horizontal line
    tags$hr(),
    
    # 0.2.5 -<SIDEPANEL> Shiny and RStudio logo -----
    # these have been moved to the Github repo in the www folder 
    h5(
      "Built by",
      # Quesgen logo
      img(
        src = "QuesGenLogo.png",
        height = "25%",
        width = "25%"
      ),
      "using",
      # Shiny logo
      img(
        src = "shiny.png",
        height = "25%",
        width = "25%"
      ),
      "by",
      # RStudio Logo
      img(
        src = "RStudio-Logo-Blue-Gray.png",
        height = "25%",
        width = "25%"
      )
    ),
    # 0.2.6 -<SIDEPANEL> support email -----
    h6("Questions? Email support@quesgen.com")
    
  ), # end sidebarPanel ) <- Do not forget a comma here! ----
  
  
  # 1.0 ------------- START mainPanel() ----
  
  mainPanel(
    
    # 2.0 -- START tabsetPanel( ----
    
    tabsetPanel(
      
      # 2.1 PANEL 1 < INSTRUCTIONS PANEL > ----
      
      tabPanel(
        # 2.1.0 < INSTRUCTIONS PANEL (Title)> ----
        title = "Instructions",
        
        # 2.1.1 text1 ** **  ** ** ** < UPLOAD .CSV FILE TEXT > text1 ----
        
        h5(textOutput(outputId = "text1")),
        
        #  "Upload your data in a comma separated values (CSV) file by clicking on the 'Browse' button in the left sidebar panel. 
        # Please refer to the image below. Your CSV file should contain only two columns. The blood glucose monitor (BGM) readings 
        # should be in the leftmost column under the heading 'BGM'. These are the meter readings or point-of-care readings. The reference
        # values should be in the next column under the label 'REF'. Reference values might come from simultaneously obtained plasma 
        # specimens run on a laboratory analyzer such as the YSI Life Sciences 2300 Stat Plus Glucose Lactate Analyzer. 
        # All glucose concentrations should be in mg/dL and rounded to the nearest integer.  
        # If you have any questions about how your CSV data file should look before uploading it, please download the sample data set we 
        # have provided. **Again, all glucose concentrations should be in mg/dL and rounded to the nearest integer.**."
        
        # 2.1.2 < INSTRUCTIONS PANEL > (save_as_csv.png IMAGE) ----
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        img(
          src = "save-as-csv.png",
          width = "80%",
          height = "80%"
        ),
        
        # 2.1.3 < INSTRUCTIONS PANEL > Text .csv/click buttons (text2) ----
        
        # 2.1.3 text2 ** **  ** ** ** < SUMMARY TABLE TEXT > text2 ----
        h5(textOutput(outputId = "text2")),
        
        # "After you have uploaded your .csv file, click on the Create Summary 
        # Tables and the Create SEG Tables button in the left-hand panel. The 
        # results can be viewed on the 'Summary Tables' tab and 'SEG' tab."
        
        
        # # 2.1.4 < INSTRUCTIONS PANEL > SEG Text Tab (text3) ----
        
        # 2.1.4 text3 ** **  ** ** ** < GRAPH TEXT > text3 ----
        h5(textOutput(outputId = "text3")),
        
        # "When your .csv file finishes uploading, a static graph will be 
        # generated from the BGM and REF values. You may customize your 
        # static graph parameters in the left sidebar panel and download the
        # graph to your computer (as either a .png or .pdf). See the example 
        # provided below:"
        
        img(
          src = "heat_map_2.0.png",
          width = "95%",
          height = "95%"
        )
      ),
      
      
      # # 2.2 PANEL 2 < SUMMARY TABLES > ----
      
      tabPanel(
        title = "Summary Tables",
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.1 < SUMMARY PairTypeTable1 > Tab 2 Text (text4.1-4.4) ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # 2.2.1.1 text4.1 ** **  ** ** ** < PairTypeTable1 > text4.1 ----
        h4(textOutput(outputId = "text4.1")),
        #     "The Surveillance Error Grid Analysis Tool Output:"
        
        # 2.2.1.2 text4.2 ** **  ** ** ** < PairTypeTable1 > text4.2 ----
        h5(textOutput(outputId = "text4.2")),
        #      "BGM = Blood Glucose Monitor"
        
        # 2.2.1.3 text4.3 ** **  ** ** ** < PairTypeTable1 > text4.3 ----
        h5(textOutput(outputId = "text4.3")),
        #       "REF = Reference"
        
        # 2.2.1.4 text4.4 ** **  ** ** ** < PairTypeTable1 > text4.4 ----
        h5(textOutput(outputId = "text4.4")),
        
        # "This contains the number of BGM values that were 1) less than the REF
        # values, 2) equal to the REF values, and 3) greater than the REF 
        # values. Note that REF values < 21 mg/dL or >600 mg/dL will not be 
        # plotted on the SEG. This tab also stratifies the values across eight
        # clinical risk levels."
        
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.2 < SUMMARY TABLES > define PairTypeTable1 table outputId ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # add some space
        br(),
        tableOutput(outputId = "PairTypeTable1"),
        # add some space
        br(),
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.3 < SUMMARY MARD text > Tab 2 Text (text6.1 - text6.5) ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        
        # 2.2.3.1 text6.1 ** **  ** ** ** < MARDTable2 > text6.1 ----
        h5(textOutput(outputId = "text6.1")),
        
        # "Bias: Mean relative difference between BGM and REF 
        # ( BGM-REF )/ REF.)"
        
        # 2.2.3.2 text6.2 ** **  ** ** ** < MARDTable2 > text6.2 ----
        h5(textOutput(outputId = "text6.2")),
        # MARD: Mean Absolute Relative Difference. | BGM-REF | / REF.)
        
        # 2.2.3.3 text6.3 ** **  ** ** ** < MARDTable2 > text6.3 ----
        h5(textOutput(outputId = "text6.3")),
        # CV: Standard Deviation of Mean Relative Difference between BGM and REF.
        
        # 2.2.3.4 text6.4 ** **  ** ** ** < MARDTable2 > text6.4 ----
        h5(textOutput(outputId = "text6.4")),
        # Lower 95% Limit of Agreement: Bias - 1.96 x CV
        
        # 2.2.3.5 text6.5 ** **  ** ** ** < MARDTable2 > text6.5 ----
        h5(textOutput(outputId = "text6.5")),
        # Upper 95% Limit of Agreement: Bias +1.96 x CV
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.4 < SUMMARY TABLES > MARDTable2 ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        tableOutput(outputId = "MARDTable2"),
        
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        # add some space
        br(),
        # add some space
        br(),
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.5 <SUMMARY TAB> define RiskGradeTable3 table DTOutput ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        DT::DTOutput(outputId = "RiskGradeTable3"),
        # add some space
        br(),
        # add some space
        br(),
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.6 <SUMMARY TAB> define SEGRiskCategoryTable4 table outputId ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        DT::DTOutput(outputId = "SEGRiskCategoryTable4"),
        br(),
        
        # add some space
        br(),
        
        # Horizontal line
        tags$hr(),
        
        # 2.2.6.1 text7.0 ** **  ** ** ** text7.0 ----
        p(textOutput(outputId = "text7.0")),
        # "Models indicate that a device with >= 97% pairs inside the SEG
        # no-risk 'green' zone would meet the International Organization
        # Standardization (ISO) requirements of ≤ 5% data pairs outside the
        # 15 mg/dL (0.83 mmol/L) / 15% standard limits, while higher
        # percentages outside the SEG no-risk zone would indicate
        # noncompliance with the standard."
        
        # 2.2.6.2 text7.0.1 ** **  ** ** ** text7.0.1 ----
        em(textOutput(outputId = "text7.0.1")),
        # "Source: J Diabetes Sci Technol 8: 673-684, 2014. PMID: 25562887."
        # add some space
        br(),
        # add some space
        br(),
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.7 <SUMMARY TAB> define ISORangeTable5 outputId ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tableOutput(outputId = "ISORangeTable5"),
        
        # 2.2.7.1 text7.1 ** **  ** ** ** text7.1 ----
        em(textOutput(outputId = "text7.1")),
        # *Difference between BGM and REF as % of REF for REF >100 mg/dL
        # and in mg/dL for REF ≤ 100 mg/dL*
        # add some space
        br(),
        # add some space
        br(),
        # add some space
        br(),
        # Horizontal line
        tags$hr(),
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 2.2.8 <SUMMARY TAB> define BinomialTest6 outputId ----
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tableOutput(outputId = "BinomialTest6"),
        
        # 2.2.8.1 text7.2 ** **  ** ** ** text7.2  ----
        em(textOutput(outputId = "text7.2"))
        # "Klonoff, D. C., et al. 'Investigation of the Accuracy of 18 Marketed 
        # Blood Glucose Monitors.' Diabetes Care. June 13, 2018. Epub ahead of 
        # print."
        
      ),
      
      # 3.0 PANEL < SEG PANEL > ----
      
      tabPanel(
        title = "SEG",
        
        plotOutput(
          outputId = "heatmap_plot",
          width = 900, height = 500
        ) # heatmap outputId
      ),
      
      # 4.0 PANEL < MODIFIED BLAND-ALTMAN PLOT > ----
      
      tabPanel(
        title = "Modified Bland-Altman Plot",
        plotOutput(
          outputId = "modba_plot"
        ),
        width = 780,
        height = 600
      )
    ) # ~ END tabsetPanel()
  ) # ~ END mainPanel()
) # ~ END fluidPage()


# DEFINE THE SERVER ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ ~~~~ -----

server <- shinyServer(function(input, output) {
  
  # 2.1 - INSTRUCTIONS PANEL <text output> -----
  
  # 2.1.1 text1 ** **  ** ** ** RENDER text1 -----
  
  output$text1 <- renderText({
    "Upload your data in a comma separated values (CSV) file by clicking on the 'Browse' button in the left sidebar panel. 
    Please refer to the image below. Your CSV file should contain only two columns. The blood glucose monitor (BGM) readings 
    should be in the leftmost column under the heading 'BGM'. These are the meter readings or point-of-care readings. The reference
    values should be in the next column under the label 'REF'. Reference values might come from simultaneously obtained plasma 
    specimens run on a laboratory analyzer such as the YSI Life Sciences 2300 Stat Plus Glucose Lactate Analyzer. 
    All glucose concentrations should be in mg/dL and rounded to the nearest integer.  
    If you have any questions about how your CSV data file should look before uploading it, please download the sample data set we 
    have provided. **Again, all glucose concentrations should be in mg/dL and rounded to the nearest integer.**"
  })
  
  # 2.1.3 text2 ** **  ** ** ** RENDER text2 -----
  output$text2 <- renderText({
    "After you have uploaded your .csv file, click on the Create Summary Tables and the Create SEG Tables button in the left-hand panel. The results can be viewed on the 'Summary Tables' tab and 'SEG' tab."
  })
  
  
  # 2.1.4 text3 ** **  ** ** ** RENDER text3 -----
  output$text3 <- renderText({
    "When your .csv file finishes uploading, a static graph will be generated from the BGM and REF values. You may customize your static graph parameters in the left sidebar panel and download the graph to your computer (as either a .png or .pdf). See the example provided below:"
  })
  
  
  # 2.2.1.1 text4.1 ** **  ** ** ** RENDER text4.1 -----
  output$text4.1 <- renderText({
    "The Surveillance Error Grid Analysis Tool Output:"
  })
  
  # 2.2.1.2 text4.2 ** **  ** ** ** RENDER text4.2 -----
  output$text4.2 <- renderText({
    "BGM = Blood Glucose Monitor"
  })
  
  # 2.2.1.3 text4.3 ** **  ** ** ** RENDER text4.3 -----
  output$text4.3 <- renderText({
    "REF = Reference"
  })
  
  # 2.2.1.4 text4.4 ** **  ** ** ** RENDER text4.4 -----
  output$text4.4 <- renderText({
    "This contains the number of BGM values that were 1) less than the REF values, 2) equal to the REF values, and 3) greater than the REF values. Note that REF values >600 mg/dL will not be plotted on the SEG. This tab also stratifies the values across eight clinical risk levels."
  })
  
  # 2.2.3.1 text6.1 ** **  ** ** ** RENDER text6.1 ----
  
  output$text6.1 <- renderText({
    "Bias: Mean relative difference between BGM and REF ( BGM-REF )/ REF"
  })
  
  # 2.2.3.2 text6.2 ** **  ** ** ** RENDER text6.2 ----
  
  output$text6.2 <- renderText({
    "MARD: Mean Absolute Relative Difference. | BGM-REF | / REF"
  })
  
  # 2.2.3.3 text6.3 ** **  ** ** ** RENDER text6.3 ----
  
  output$text6.3 <- renderText({
    "CV: Standard Deviation of Relative Difference between BGM and REF"
  })
  
  # 2.2.3.4 text6.4 ** **  ** ** ** RENDER text6.4 ----
  
  output$text6.4 <- renderText({
    "Lower 95% Limit of Agreement: Bias - 1.96 * CV"
  })
  
  # 2.2.3.5 text6.5 ** **  ** ** ** RENDER text6.5 ----
  
  output$text6.5 <- renderText({
    "Upper 95% Limit of Agreement: Bias + 1.96 * CV"
  })
  
  
  # 0.1.1 - downloadHandler (sample file download) -----
  
  # this uses the downloadSampleData input from the downloadButton above
  
  output$downloadSampleData <- downloadHandler(
    filename = function() {
      paste("Sample", "Data", "File", ".csv", sep = "")
    },
    content = function(file) {
      file.copy("AppSampleDataFile.csv", file)
    },
    contentType = "text/csv"
  )
  
  # 5.0.0 - # # # # # # reactive({}) CREATE datasetInput() ---- 
  datasetInput <- reactive({
    validate(
      need(
        expr = input$file1 != 0,
        message = "import a .csv file & click 'Create Summary Tables' or 'Create SEG Tables'"
      )
    )
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data.table::fread(inFile$datapath)
  })
  
  # 5.1.0 - # # # # # # reactive({}) CREATE datasetSEG() with segTable() ----
  
  datasetSEG <- reactive({
    validate(
      need(
        expr = input$file1 != 0,
        message = "import a .csv file & click 'Create Summary Tables' or 'Create SEG Tables'"
      )
    )
    # rename inFile
    inFile <- input$file1
    # prepare data file
    if (is.null(inFile)) return(NULL)
    segTable(inFile$datapath)
  })
  
  # 0.1.4 - RENDER contents on sidebar ---- ---- ---- ----
  
  output$contents <- renderTable({
    # require the dataset before executing
    req(datasetInput())
    # define df
    df <- datasetInput()
    
    if (input$disp == "Top 10") {
      return(head(df, 10))
    }
    else {
      return(head(df, 20))
    }
  })
  
  # 5.2.0 - # # # # # # reactive({}) CREATE REACTIVE DATA [PairType()] ----
  
  PairType <- reactive({
    # validate with message
    validate(
      need(
        expr = input$file1 != 0,
        message = "import a .csv file & click 'Create Summary Tables' or 'Create SEG Tables'"
      )
    )
    # rename inFile
    inFile <- input$file1
    # prepare data file
    if (is.null(inFile)) return(NULL)
    pairtypeTable(inFile$datapath)
  })
  
  # 2.2.2 <SUMMARY TAB> RENDER PairTypeTable1 TABLE ---- ---- ---- ----
  # this presents the output from the reactive above
  
  output$PairTypeTable1 <- renderTable({
    
    input$SEGSummaryTableButton
    
    # req(datasetSEG()) # ensure availablity of value before proceeding
    # isolate thre presentation of this table
    isolate({
      PairType()
    })
  },
  striped = TRUE,
  hover = TRUE,
  spacing = "m",
  align = "l",
  digits = 1)
  
  
  # 2.2.4 <SUMMARY TAB> RENDER [MARDTable2] TABLE ---- ---- ----
  
  
  output$MARDTable2 <- renderTable({
    # Add the button as a dependency to
    # cause the MARDTable2 to render on click
    input$SEGSummaryTableButton
    
    isolate({
      (data.frame(
        Total = c(nrow(datasetSEG())),
        Bias = c(mean(datasetSEG()$rel_diff)),
        MARD = c(mean(datasetSEG()$abs_rel_diff)),
        CV = c(sd(datasetSEG()$rel_diff)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ) %>%
        add_column(
          `Lower 95% Limit of Agreement` = .$Bias - 1.96 * .$CV
        ) %>%
        add_column(
          `Upper 95% Limit of Agreement` = .$Bias + 1.96 * .$CV
        ) %>%
        dplyr::mutate(
          Bias = base::paste0(base::round(
            100 * Bias,
            digits = 1
          ), "%"),
          
          MARD = base::paste0(base::round(
            100 * MARD,
            digits = 1
          ), "%"),
          
          CV = base::paste0(base::round(
            100 * CV,
            digits = 1
          ), "%"),
          
          `Lower 95% Limit of Agreement` = base::paste0(base::round(
            100 * `Lower 95% Limit of Agreement`,
            digits = 1
          ), "%"),
          
          `Upper 95% Limit of Agreement` = base::paste0(base::round(
            100 * `Upper 95% Limit of Agreement`,
            digits = 1
          ), "%")
        )
      )
    })
  },
  striped = TRUE,
  hover = TRUE,
  spacing = "m",
  align = "c",
  digits = 1)
  
  
  # 2.2.5 <SUMMARY TAB> RENDER [RiskGradeTable3] TABLE ---- ---- ----
  
  output$RiskGradeTable3 <- DT::renderDT({
    # Add the CreateSEGTables button as a dependency to
    # cause the table to re-render on click
    input$CreateSEGTables
    # create the isolate function
    isolate({
      # this is the RiskGradeTable3() reactive
      datasetSEG() %>%
        dplyr::count(risk_grade, sort = TRUE) %>%
        dplyr::full_join(x = .,
                         y = lkpRiskGrade,
                         by = "risk_grade") %>%
        # change lkp table variables
        dplyr::mutate(
          risk_grade_id = as.numeric(risk_grade_id),
          Percent = base::paste0(base::round(n / nrow(datasetSEG()) * 100,
                                             digits = 1),
                                 if_else(condition = is.na(n),
                                         true = "", false = "%"))) %>%
        # rename variables
        dplyr::select(ID = risk_grade_id,
                      `Risk Grade` = risk_grade,
                      `Number of Pairs` = n,
                      Percent,
                      # `REF Range` = REF
                      `Risk Factor Range` = REF) %>% # pipe to color format
        
        # 2.2.5.1 <SUMMARY TAB> Color for Risk Grades ---- ---- ----
      
      DT::datatable(.,options = list(lengthChange = FALSE,
                                     dom = 't',
                                     rownames = TRUE )) %>%
        # select numerical reference
        DT::formatStyle('ID', # column
                        target = "row", # reference rows
                        backgroundColor =
                          DT::styleEqual(levels =  # five levels/labels
                                           c(1, 2, 3, 4, 5),
                                         values = c("limegreen",
                                                    "greenyellow",
                                                    "yellow",
                                                    "orange",
                                                    "red")
                          )
        )
    })
  })
  
  
  # 2.2.6 <SUMMARY TAB> RENDER [SEGRiskCategoryTable4] TABLE ---- ---- ----
  
  output$SEGRiskCategoryTable4 <- DT::renderDT({
    # prompted when user clicks on 'Summary Table' button
    input$CreateSEGTables
    # isolate this function
    isolate({
      datasetSEG() %>%
        dplyr::count(risk_cat, sort = TRUE) %>%
        dplyr::full_join(x = .,
                         y = lkpSEGRiskCat4, by = "risk_cat") %>%
        dplyr::mutate(
          risk_cat = as.numeric(risk_cat),
          Percent = base::paste0(base::round(n / nrow(datasetSEG()) * 100,
                                             digits = 1),
                                 if_else(condition = is.na(n),
                                         true = "", false = "%"))) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::select(
          `SEG Risk Level` = risk_cat,
          `SEG Risk Category` = risk_cat_txt,
          `Number of Pairs` = n,
          Percent) %>%
        
        DT::datatable(.,options = list(lengthChange = FALSE,
                                       dom = 't',
                                       rownames = FALSE )) %>%
        # select numerical reference
        DT::formatStyle('SEG Risk Level',
                        target = "row",
                        backgroundColor = DT::styleEqual(
                          levels =  # eight levels/labels
                            c(0, 1, 2, 3,
                              4, 5, 6, 7),
                          values = c("#00EE00", "#ADFF2F", "#FFFF00",
                                     "#FFD700", "#FFA500","#EE7600",
                                     "#FF4500", "#FF0000")))
    })
  })
  
  # 2.2.6.1 text7.0 ** ** **  ** ** RENDER text7.0 ----
  output$text7.0 <- renderText({
    "Models indicate that a device with ≥ 97% pairs inside the SEG no-risk 'green' zone would meet the requirements of ≤ 5% data pairs outside the 15 mg/dL (0.83 mmol/L) / 15% standard limits, while higher percentages outside the SEG no-risk zone would indicate noncompliance with the standard. The Diabetes Technology Society Blood Glucose Monitor System (BGMS) Surveillance Program confirmed these ranges on 18 blood glucose monitoring systems using pre-determined analytical accuracy criteria agreed upon by the DTS-BGMS Surveillance Committee."
  })
  # 2.2.6.2 text7.0.1 ** ** **  ** ** RENDER text7.0.1 ----
  output$text7.0.1 <- renderText({
    "Source: J Diabetes Sci Technol 8: 673-684, 2014. PMID: 25562887."
  })
  
  # 2.2.7 <SUMMARY TAB> RENDER [ISORangeTable5] TABLE ----- ----- -----
  
  output$ISORangeTable5 <- renderTable({
    # prompted when user clicks on 'SEG Table' button
    input$CreateSEGTables
    # isolate this function
    isolate({
      datasetSEG() %>%
        dplyr::count(iso_range, sort = TRUE) %>%
        dplyr::full_join(x = .,
                         y = lkpISORanges,
                         by = "iso_range") %>%
        dplyr::mutate(
          Percent = base::paste0(base::round(n / nrow(datasetSEG()) * 100,
                                             digits = 1),
                                 dplyr::if_else(condition = is.na(n),
                                                true = "",
                                                false = "%"))) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::select(ID,
                      `ISO range` = iso_range,
                      N = n,
                      Percent)
    }) # close off the isolate function
  },
  striped = TRUE,
  hover = TRUE,
  spacing = "m",
  align = "c",
  digits = 1)
  
  # 2.2.7.1 text7.1 ** **  ** ** ** RENDER text7.1 ----
  output$text7.1 <- renderText({
    "ISO range = difference between BGM and REF as percent of REF for REF >
      100 mg/dL and in mg/dL for REF <= 100 mg/dL."
  })
  
  # 5.3.0 - # # # # # # reactive({}) CREATE REACTIVE DATA [Binomial()] ----- 
  # this table is created with a function like the datasetSEG() reactive
  
  Binomial <- reactive({
    validate(
      need(
        expr = input$file1 != 0,
        message = "import a .csv file & click 'Create Summary Tables' or 'Create SEG Tables'"
      ))
    # use binomialTable in helpers.R file
    inFile <- input$file1
    # prepare data file
    if (is.null(inFile)) return(NULL)
    binomialTable(inFile$datapath)
  })
  
  # 2.2.8 <SUMMARY TAB> RENDER BinomialTest6 TABLE ---- ---- ---- ----
  # this presents the output from the reactive above
  
  output$BinomialTest6 <- renderTable({
    # prompt when clicking on SEG Tables button
    input$CreateSEGTables
    # isolate this function (which is the data set for the binomial table)
    isolate({
      Binomial()
    })
  },
  striped = TRUE,
  hover = TRUE,
  spacing = "m",
  align = "c",
  digits = 0)
  
  # 2.2.8.1 text7.2 ** **  ** ** ** RENDER text7.2 ----
  output$text7.2 <- renderText({
    "Klonoff, D. C., et al. 'Investigation of the Accuracy of 18 Marketed Blood Glucose Monitors.' Diabetes Care. June 13, 2018. Epub ahead of print."
  })
  
  # 6.0 <SEG TAB> RENDER SEG [heatmap_plot] ----- ----- -----
  output$heatmap_plot <- renderPlot({
    
    # (6.1 - 6.5) use layers imported fromthe helpers.R file ----- 
    seg_gaussian_layer_shiny +
      
      # 6.6 - plot heat map ----
    geom_point(
      data = datasetSEG(), # 6.6.1 - plot SAMPLE DATA GOES HERE! ----
      aes(
        x = datasetSEG()$REF,
        y = datasetSEG()$BGM
      ),
      shape = 21,
      alpha = input$alpha,
      fill = input$color,
      size = input$size, # 6.6.2 - plot use input for size value ----
      stroke = 0.4
    ) +
      theme(plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)) +
      # add title
      ggtitle(input$plot_title)
  }) # end renderPlot() function -----
  
  
  # 7.0 - <Mod Bland-Altman Tab> RENDER MOD-BA ----- ----- -----
  
  output$modba_plot <- renderPlot({
    datasetSEG() %>%
      # 7.1 calculate LN of REF and BGM -----
    dplyr::mutate( 
      lnREF = log(REF),
      lnBGM = log(BGM),
      lnDiff = lnBGM - lnREF,
      rel_perc_diff = exp(lnDiff) - 1
    ) %>%
      # 7.2 create points layer ----- 
    ggplot2::ggplot(aes(x = REF, y = rel_perc_diff)) +
      ggplot2::geom_point(alpha = 0.5, color = "royalblue") +
      ggplot2::scale_y_continuous(
        name = "% Error",
        limits = c(-0.50, 0.50)
      ) +
      # 7.3 use SEGBlandAltmanRefVals from helpers.R file -----
    ggplot2::geom_line(aes(x = Ref, y = UB),
                       data = SEGBlandAltmanRefVals,
                       linetype = "dotted",
                       color = "red",
                       size = 1.5
    ) +
      ggplot2::geom_line(aes(x = Ref, y = LB),
                         data = SEGBlandAltmanRefVals,
                         linetype = "dotted",
                         color = "red",
                         size = 1.5
      ) +
      theme(plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)) +
      ggplot2::labs(
        x = "Reference (mg/dL)",
        title = "Modified Bland-Altman Plot",
        subtitle = "Blood Glucose Monitoring System Surveillance Program"
      )
  })
  
  # 8.0 - SEG DOWNLOAD ---- ---- ----- ----- ----- -----
  # downloadHandler contains 2 arguments as functions (filename & content)
  
  # 8.1 - downloadHandler for heatmap  ---- 
  output$heatmap_file_download <- downloadHandler(
    filename = function() {
      paste("SEG_file", input$heatmap_file, sep = ".")
    },
    # content is a function with argument 'file.'
    # content writes the plot to the device
    
    # 8.2 - contents for heatmap download ---- 
    # open the png device
    content = function(file) {
      if (input$heatmap_file == "png") {
        png(filename = file, 
            units = "in",
            res = 96,
            width = 9, 
            height = 7.7)
      } 
      
      # open the pdf device
      else {
        pdf(file = file, 
            paper = "USr", 
            width = 9, 
            height = 7.7)
      } 
      
      print(
        
        seg_gaussian_layer_shiny +
          
          # 8.3 - plot heat map ----
        geom_point(
          data = datasetSEG(), # 8.4 - plot SAMPLE DATA GOES HERE! ----
          aes(
            x = datasetSEG()$REF,
            y = datasetSEG()$BGM
          ),
          shape = 21, # 8.5 - plot inputs for size, color, & alpha ----
          alpha = input$alpha,
          fill = input$color,
          size = input$size, 
          stroke = 0.4
        ) +
          
          # 8.6 - title input for the SEG graph for download ---- ---- 
        ggtitle(input$plot_title)
        
      ) # end print ----
      
      dev.off() # 8.7 - turn the device off ----
    }
  )
  
  # 9.0 - MOD Bland Altman DOWNLOAD ---- ---- ---- ---- ---- ---- ----
  
  # 9.1 - downloadHandler for modba_plot ---- 
  output$modba_plot_download <- downloadHandler(
    filename = function() {
      paste("modba_plot", input$modba_plot_file, sep = ".")
    },
    
    # 9.2 - contents for heatmap download ---- 
    # content is a function with argument 'file.'
    # content writes the plot to the device
    content = function(file) {
      # open the png device
      if (input$modba_plot_file == "png") {
        png(filename = file, 
            units = "in",
            res = 72,
            width = 9, 
            height = 7)
      } 
      # open the pdf device
      else {
        pdf(file = file, 
            width = 9, 
            height = 7)
      } 
      
      # 9.3 - plot mod bland-altman ----
      print(
        datasetSEG() %>%
          dplyr::mutate( # calculate LN of REF and BGM
            lnREF = log(REF),
            lnBGM = log(BGM),
            lnDiff = lnBGM - lnREF,
            rel_perc_diff = exp(lnDiff) - 1
          ) %>%
          ggplot2::ggplot(aes(x = REF, y = rel_perc_diff)) +
          ggplot2::geom_point(alpha = 0.5, color = "royalblue") +
          ggplot2::scale_y_continuous(
            name = "% Error",
            limits = c(-0.50, 0.50)
          ) +
          ggplot2::geom_line(aes(x = Ref, y = UB),
                             data = SEGBlandAltmanRefVals,
                             linetype = "dotted",
                             color = "red",
                             size = 1.5
          ) +
          ggplot2::geom_line(aes(x = Ref, y = LB),
                             data = SEGBlandAltmanRefVals,
                             linetype = "dotted",
                             color = "red",
                             size = 1.5
          ) +
          
          ggplot2::labs(
            x = "Reference (mg/dL)",
            title = "Modified Bland-Altman Plot",
            subtitle = "Blood Glucose Monitoring System Surveillance Program"
          )
      )
      dev.off() # 9.4 - turn the device off ----
    }
  )
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END ----
})


# LAUNCH APPLICATION ------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
