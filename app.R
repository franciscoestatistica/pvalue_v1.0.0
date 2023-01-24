detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()


rm(list=ls())
options(encoding='native.enc')
BaseEmR_Seg <<- data.frame(0,2,2)
BaseEmR_BACKUP <<- data.frame(0,1,1)

FOI_SEGMENTADO <<- FALSE
POSICAO_SPL_NUM1_DTB_GLOBAL <<- 1
POSICAO_SPL_CAT1_DTB_GLOBAL <<- 1

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(skimr)
library(DataCombine)
library(GGally)
library(shinythemes)
library(plotly)
library(DT)
library(shinycssloaders)
library(pROC)
library(multcompView)
library(stats)
library(lawstat)
library(shiny.i18n)
#library(shinyBS) # for modal 
#library(shinyLP)



flags <- c("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
           "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/br.svg",
           "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg")

lang <- c("English", "Português", "Español")

ui <- shinyUI( 
  navbarPage( theme = shinytheme("cerulean"), 
              title = span(id="logo", tags$a(href="https://pvalue.site", span(id="logo", "pvalue.site"))), 
              windowTitle="pvalue.site", responsive = NULL, collapsible = TRUE,
              
              tabPanel(textOutput("Upload_MENU", inline = TRUE), icon = icon("cloud-upload-alt", lib = "font-awesome"),
                       
                       tags$head(
                         
                         tags$link(rel="shortcut icon", href="http://estatistica.info/examples/images/favicon/favicon.ico"),
                         tags$link(rel="shortcut icon", href="http://estatistica.info/examples/images/favicon/favicon-16x16.png"),
                         tags$link(rel="shortcut icon", href="http://estatistica.info/examples/images/favicon/favicon-32x32.png"),
                         tags$link(rel="shortcut icon", href="http://estatistica.info/examples/images/favicon/favicon-96x96.png"),
                         
                         tags$style(HTML("@import url('//fonts.googleapis.com/css2?family=MuseoModerno:wght@500&display=swap');")),
                         tags$style(HTML("@import url('//fonts.googleapis.com/css2?family=Bebas+Neue&display=swap');")),
                         tags$style( HTML("#logo { font-family: MuseoModerno; color: white; font-size: 17px; }")),
                         tags$style( HTML('#logotxt { font-family: MuseoModerno; }')) ),
                       tags$style( HTML("#segment_alert { font-family: 'Bebas Neue'; color: green ; font-size: 17px; background-color:#DC143C }")),
                       
                       uiOutput('upload_server') %>% withSpinner() ),
              
              navbarMenu(textOutput("Data_viewer_MENU", inline = TRUE), icon = icon("database", lib = "font-awesome"),
                         tabPanel(textOutput("Descriptive_statistics_SUBMENU"),
                                  uiOutput('descriptive_statistics_server')  %>% withSpinner()  ),
                         tabPanel(textOutput("View_download_data_SUBMENU"),
                                  (uiOutput("view_download_server") %>% withSpinner() ) ),
                         tabPanel(textOutput("Data_segmentation_SUBMENU"), 
                                  (uiOutput('data_segmentation_server')  %>% withSpinner() ) 
                         )
              ),
              
              navbarMenu(textOutput("Custom_graphics_MENU", inline = TRUE), icon = icon("chart-pie", lib = "font-awesome"),
                         tabPanel(textOutput("Histogram_SUBMENU"),
                                  uiOutput('histogram_server') %>% withSpinner()  ),
                         tabPanel(textOutput("Boxplot_SUBMENU"),
                                  uiOutput('boxplot_server') %>% withSpinner()  ),
                         tabPanel(textOutput("Scatterplot_SUBMENU"),
                                  uiOutput('scatterplot_server') %>% withSpinner()  ),
                         tabPanel(textOutput("twofactors_x_twonumbers_SUBMENU"),
                                  uiOutput('twofactors_twonumbers_server') %>% withSpinner()  ) ),
              
              navbarMenu(textOutput("Tests_MENU", inline = TRUE), icon = icon("vials", lib = "font-awesome"),
                         tabPanel(textOutput("KW_test_SUBMENU"),
                                  uiOutput('kruskal_wallis_test_server') %>% withSpinner()  ),             
                         tabPanel(textOutput("ANOVA_test_SUBMENU"),
                                  uiOutput('anova_test_server') %>% withSpinner()  ),             
                         tabPanel(textOutput("MW_test_SUBMENU"),
                                  uiOutput('mann_whitney_test_server') %>% withSpinner()  ),             
                         tabPanel(textOutput("T_test_SUBMENU"),
                                  uiOutput('students_t_test_server') %>% withSpinner()  ),             
                         tabPanel(textOutput("CORRELATION_test_SUBMENU"),
                                  uiOutput('correlation_test_server') %>% withSpinner()  ),             
                         tabPanel(textOutput("FISHER_test_SUBMENU"),
                                  uiOutput('fisher_exact_test_server') %>% withSpinner()  ),             
                         tabPanel(textOutput("ROC_test_SUBMENU"),
                                  uiOutput('roc_curve_test_server') %>% withSpinner()  ) ),
              
              navbarMenu(textOutput("Citation_MENU", inline = TRUE), icon = icon("graduation-cap", lib = "font-awesome"),
                         tabPanel(textOutput("How_cite_SUBMENU"),
                                  uiOutput('cite_this_web_site_server') %>% withSpinner()  ) )
  ) ) 

translator <- Translator$new(translation_csvs_path = "translations/")

server <- shinyServer(function(input, output, session) {
  
  output$upload_server <- renderUI({
    tagList(
      
      sidebarLayout(
        sidebarPanel(
          pickerInput('selected_language', i18n()$t("Translate to"), multiple = FALSE,
                      choices = c("en","pt","es"), selected = input$selected_language,
                      choicesOpt = list(content =  
                                          mapply(lang, flags, FUN = function(lang, flagUrl) {
                                            HTML(paste(
                                              tags$img(src=flagUrl, width=20, height=15),
                                              lang
                                            ))
                                          }, SIMPLIFY = FALSE, USE.NAMES = FALSE ))),
          
          fileInput("file", i18n()$t("CSV File upload (max 5MB)"), multiple = TRUE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          radioButtons(inputId = 'sep', label = i18n()$t("CSV separated by"), 
                       choiceNames = c(i18n()$t("comma") , i18n()$t("semicolon") , i18n()$t("tab") , i18n()$t("space")),
                       choiceValues =c(',' , ';' , '\t' , ''), selected = ';'),
          
          radioButtons(inputId = 'decimals', label = i18n()$t("Decimals are separated by"), 
                       choiceNames = c(i18n()$t("point") , i18n()$t("comma")),
                       choiceValues =c('.' , ','), selected = ',', inline=TRUE),
          
          actionButton("btn_analyze" ,i18n()$t("ANALYZE MY DATA"), icon("refresh")),
          uiOutput("selectfile")
        ),
        mainPanel(
          
          uiOutput("tb"),
          h2(tags$strong(i18n()$t("WELCOME")), align="center"),
          h3(tags$strong(span(id="logotxt", "pvalue.site ")),i18n()$t("is a free statistical analysis service. DIY!")),
          h4(i18n()$t("Just upload a CSV file:")),
          tags$ul(
            tags$li(i18n()$t("Factors or categorical variables must be standardized and typed in full. Example: (Married - Single - Other) OR (Success - Failure)")), 
            tags$li(i18n()$t("Missing values must be registered with"), tags$strong(" NA.")), 
            tags$li(i18n()$t("The first column and the first row are reserved for identification of patients and variables"),", ", i18n()$t("respectively.") ), 
            tags$li(i18n()$t("Use the side panel to adjust other CSV settings and click on")," ", icon("refresh"),tags$strong(i18n()$t("ANALYZE MY DATA"))), 
            tags$li(i18n()$t("If you have any difficulties"),", ",i18n()$t("try to insert your data in our example database:")," ", tags$a(href="http://estatistica.info/examples/DataBase/diamonds.csv", "diamonds.csv")
            ) ),
          h6(actionButton("LegalDisclaimer", i18n()$t("IMPORTANT: Legal Disclaimer")), align="center")
        ) )
      
    )})
  
  output$descriptive_statistics_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition="input.tabselected==1",
                           selectInput('spl_SUMM1', i18n()$t("Split by"), choices = names(var_cat()) , selected = names(var_cat())[1]),
                           checkboxGroupInput("VAR_NUM_DESC_STATS", i18n()$t("What measures do you need?"),
                                              choiceNames = c( i18n()$t("sample size"),
                                                               i18n()$t("mean"),
                                                               i18n()$t("standard deviation"),
                                                               i18n()$t("maximum"),
                                                               i18n()$t("minimum"),
                                                               i18n()$t("range"),
                                                               i18n()$t("median"),
                                                               i18n()$t("first quartile"),
                                                               i18n()$t("third Quartile"),
                                                               i18n()$t("interquartile range")),
                                              choiceValues = c('(n):',
                                                               'mean:',
                                                               'sd:',
                                                               'max:',
                                                               'min:',
                                                               'range:',
                                                               'median:',
                                                               'q1:',
                                                               'q3:',
                                                               'IQR:'),
                                              selected = c('(n):',
                                                           'mean:',
                                                           'sd:',
                                                           'max:',
                                                           'min:',
                                                           'range:',
                                                           'median:',
                                                           'q1:',
                                                           'q3:',
                                                           'IQR:') ),
                           selectInput('VAR_NUM_DESC_NAMES', i18n()$t("Numerical variables"), 
                                       choices = names(var_num()) , selected = names(var_num()), multiple = TRUE ),
                           sliderInput("DCMP_DESC_NUM", i18n()$t("Decimal places"), min = 0, max = 10, value = 3)
          ),
          conditionalPanel(condition="input.tabselected==2",
                           selectInput('spl_SUMM2', i18n()$t("Split by"), choices = names(var_cat()) , selected = names(var_cat())[1]),
                           checkboxGroupInput("VAR_CAT_DESC_STATS", i18n()$t("What measures do you need?"),
                                              choiceNames = c( i18n()$t("Line percentage (%lin)"),
                                                               i18n()$t("Column percentage (%col)")),
                                              choiceValues = c('(%lin)',
                                                               '(%col)'),
                                              selected = c('(%lin)',
                                                           '(%col)') ),
                           selectInput('VAR_CAT_DESC_NAMES',i18n()$t("Categorical variables"), 
                                       choices = names(var_cat()) , selected = names(var_cat()), multiple = TRUE),
                           sliderInput("DCMP_DESC_CAT", i18n()$t("Decimal places"), min = 0, max = 10, value = 3) ) ),
        
        mainPanel(
          uiOutput('DISPLAY_SEGMENTACAO_DESCRIPTIVE'),
          tabsetPanel( 
            tabPanel(i18n()$t("Numerical variables"), value=1, helpText(i18n()$t("Descriptive statistics calculated for numerical variables")),
                     tableOutput("summ_NUMERICAL") %>% withSpinner() ),
            tabPanel(i18n()$t("Categorical variables"), value=2, helpText(i18n()$t("Descriptive statistics calculated for categorical variables")),
                     tableOutput("summ_CATEGORICAL") %>% withSpinner() ),
            id = "tabselected") ) )
    )})
  
  #CRIA OS ALERTAS DE SEGMENTA??O EM TODO O SITE, AO CRIAR ABAS NOVAS DEVE-SE INCLUIR DADOS AQUI
  
  output$DISPLAY_SEGMENTACAO_DESCRIPTIVE    <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_DESCRIPTIVE"))  )})
  output$DISPLAY_SEGMENTACAO_VIEW_DOWNLOAD  <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_VIEW_DOWNLOAD")) )})
  output$DISPLAY_SEGMENTACAO_HISTOGRAM      <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_HISTOGRAM"))  )})
  output$DISPLAY_SEGMENTACAO_BOXPLOT        <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_BOXPLOT"))  )})
  output$DISPLAY_SEGMENTACAO_SCATTERPLOT    <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_SCATTERPLOT"))  )})
  output$DISPLAY_SEGMENTACAO_2F2N           <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_2F2N"))  )})
  output$DISPLAY_SEGMENTACAO_KW             <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_KW"))  )})
  output$DISPLAY_SEGMENTACAO_ANOVA          <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_ANOVA"))  )})
  output$DISPLAY_SEGMENTACAO_MW             <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_MW"))  )})
  output$DISPLAY_SEGMENTACAO_TESTT          <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_TESTT")) )})
  output$DISPLAY_SEGMENTACAO_CORRELATION    <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_CORRELATION"))  )})
  output$DISPLAY_SEGMENTACAO_FISHER         <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_FISHER")) )})
  output$DISPLAY_SEGMENTACAO_ROC            <- renderUI({ tagList( span(id="segment_alert", textOutput("TXT_DISPLAY_SEGMENTACAO_ROC"))  )})
  
  output$TXT_DISPLAY_SEGMENTACAO_DESCRIPTIVE    <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_VIEW_DOWNLOAD  <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_HISTOGRAM      <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_BOXPLOT        <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_SCATTERPLOT    <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_2F2N           <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_KW             <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_ANOVA          <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_MW             <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_TESTT          <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_CORRELATION    <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_FISHER         <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  output$TXT_DISPLAY_SEGMENTACAO_ROC            <- renderText({ if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) { paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " ", i18n()$t("to")," ", paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) ) } else { paste0("") } })
  
  
  #######################################################################################P?GINA DE SEGMENTA??O
  #Estrutura da p?gina de segmenta??o
  output$data_segmentation_server <- renderUI({
    #req(dim(BaseEmR_BACKUP)[1]>1)
    tagList(
      h4(i18n()$t( i18n()$t("Enter parameters to segment the base"))),
      #span(id="segment_alert", textOutput("TXT_DE_SEGMENTACAO_ALERTA_ABA_SEGMENTACAO")),
      uiOutput("BOTAO_SHOW_OPTIONS_PARTE2"),br(),
      uiOutput("view_data_segmentation_server")
    )})
  
  output$TXT_DE_SEGMENTACAO_ALERTA_ABA_SEGMENTACAO <- renderText({ 
    if ( dim(BaseEmR_BACKUP)[1] != dim(BaseEmR())[1] ) {
      paste0( i18n()$t("The base is segmented!"), " - " , dim(data.frame(BaseEmR_BACKUP))[1],
              " ", i18n()$t("lines")," ", i18n()$t("to")," ",paste0(dim(data.frame(BaseEmR()))[1]," ", i18n()$t("lines")) )
    } else { paste0("") } })
  
  output$BOTAO_SHOW_OPTIONS_PARTE2 <- renderUI({
    tagList( 
      
      div(style="display:inline-block; vertical-align: top; padding-left: 30px; width:250px;",
          uiOutput('segmentacao_da_base_DTB_NUM1') %>% withSpinner(),
          uiOutput('segmentacao_da_base_RANGE_NUM1') %>% withSpinner() ),  
      
      div(style="display:inline-block; vertical-align: top; padding-left: 30px; width:250px;",
          uiOutput('segmentacao_da_base_DTB_CAT1') %>% withSpinner(),
          uiOutput('segmentacao_da_base_RANGE_CAT1') %>% withSpinner() ),
      
      div(style="text-align: center; width:500px; padding-left: 30px; float: center;",
          actionButton("SEGMENTAR_BASE" ,  i18n()$t("SPLIT"), icon = icon("divide", lib = "font-awesome")), 
          actionButton("RETORNAR_BASE" ,  i18n()$t("RESTORE"), icon = icon("history", lib = "font-awesome")), br(),br(),
          span(id="segment_alert", textOutput("texto_linhas_base"))
      )) 
  })
  
  output$view_data_segmentation_server <- renderUI({
    tagList(
      DT::renderDataTable( 
        datatable(data = BaseEmR(),
                  extensions = 'Buttons',
                  options = list(
                    dom = "Blfrtip",
                    buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf") , text = "download") ),
                    lengthMenu = list( c(10, 20, 40, 60, 80, 100, -1) , c(10, 20, 40, 60, 80, 100,  "All") ) # end of lengthMenu customization
                    , pageLength = 10 ) )
      ) 
    )})
  
  #######################################################################################################################  
  
  output$histogram_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          sliderInput('b', i18n()$t("Number of bins"), min = 3, max = 30, value = 10),
          selectInput('spl', i18n()$t("Split by"),  names(var_cat()) , selected = names(var_cat())[1] ),
          selectInput('analise', i18n()$t("The value of the variable"),  names(var_num()) , selected = names(var_num())[1] ),
          sliderInput("DCMP_HIST_NUM", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
        ),
        mainPanel(
          uiOutput('DISPLAY_SEGMENTACAO_HISTOGRAM'),
          h4(i18n()$t("Histogram")),  (plotlyOutput("plot") %>% withSpinner()), 
          h4(i18n()$t("Shapiro-Wilk normality test")),
          tableOutput("testes_normalidade") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("the data is NOT normally distributed"),".")))
      )
    )})
  
  
  output$boxplot_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('spl_BXP0',i18n()$t("Split by"), names(var_cat()) , selected = names(var_cat())[1] ),
          selectInput('spl_BXP',i18n()$t("Subgroup"),  names(var_cat()) , selected = names(var_cat())[2] ),
          selectInput('analise_BXP', i18n()$t("The value of the variable"), names(var_num()) , selected = names(var_num())[1] ),
          sliderInput("DCMP_BXP_NUM", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
        ),
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_BOXPLOT'),
          h4(i18n()$t("Boxplot")),  (plotlyOutput("plot_BXP") %>% withSpinner() ), 
          h4(i18n()$t("Shapiro-Wilk normality test")),
          tableOutput("testes_normalidade_BXP") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("the data is NOT normally distributed"),".")))
      )
    )})
  
  
  
  output$scatterplot_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('analise_SCTP1',i18n()$t("X axis"),  names(var_num()) , selected = names(var_num())[1] ),
          selectInput('analise_SCTP2',i18n()$t("Y axis"),  names(var_num()) , selected = names(var_num())[1] ),
          selectInput('analise_SCTP3',i18n()$t("Colors"),   names(BaseEmR()) , 
                      selected = names(var_cat())[2]),
          selectInput('analise_SCTP4',i18n()$t("Size"),   names(var_num()) , selected = names(var_num())[2] ),
          sliderInput("DCMP_SCTP_NUM",i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
          
        ),
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_SCATTERPLOT'),
          h4(i18n()$t("Scatterplot")),  (plotlyOutput("plot_SCTP") %>% withSpinner() ),
          h4(i18n()$t("Shapiro-Wilk normality test")),
          tableOutput("testes_normalidade_SCTP") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("the data is NOT normally distributed"),".")))
      )
    )})
  
  
  output$twofactors_twonumbers_server <- renderUI({
    tagList(
      div(style="display:inline-block; vertical-align: top; padding-left: 30px; width:250px;", 
          selectInput("CORR_2NUM_2FAT_1FAT", i18n()$t("First factor"), 
                      choices = names(var_cat()), selected = names(var_cat())[2], 
                      multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
          selectInput("CORR_2NUM_2FAT_2FAT", i18n()$t("Second factor"), 
                      choices = names(var_cat()), selected = names(var_cat())[3], 
                      multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
      
      div(style="display:inline-block; vertical-align: top; padding-left: 30px; width:250px;", 
          selectInput("CORR_2NUM_2FAT_1NUM", i18n()$t("First numerical variable"), 
                      choices = names(var_num()), selected = names(var_num())[1], 
                      multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
          selectInput("CORR_2NUM_2FAT_2NUM", i18n()$t("Second numerical variable"), 
                      choices = names(var_num()), selected = names(var_num())[2], 
                      multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
      br(),
      mainPanel( 
        uiOutput('DISPLAY_SEGMENTACAO_2F2N'),
        h4(i18n()$t("Insights - Graphical analysis: 2 factors vs. 2 numeric variables")), (plotOutput("plot_CORR_2NUM_2FAT") %>% withSpinner()  
                                                                                           
        ) )
    )})
  
  output$segmentacao_da_base_DTB_NUM1 <- renderUI({
    selectInput("SPL_NUM1_DTB",  i18n()$t("by numeric variable"),
                choices = names(var_num()), 
                selected = names(var_num())[POSICAO_SPL_NUM1_DTB_GLOBAL],
                multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
  })
  
  output$segmentacao_da_base_DTB_CAT1 <- renderUI({
    selectInput("SPL_CAT1_DTB",  i18n()$t("by categorical variable"),
                choices = names(var_cat()), 
                selected = names(var_cat())[POSICAO_SPL_NUM1_DTB_GLOBAL],
                multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
  })
  
  output$segmentacao_da_base_RANGE_NUM1 <- renderUI({ 
    sliderInput("SPL_NUM1_DTB_RANGE",  label = "choose the interval", #"delimit range",
                min = min(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                max = max(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                value = if( FOI_SEGMENTADO==TRUE && (SPL_NUM1_DTB_GLOBAL == as.vector(as.character(input$SPL_NUM1_DTB))) ) {
                  as.vector(SPL_NUM1_DTB_RANGE_GLOBAL) } else {
                    c(min(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                      max(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])))
                  }
    )})
  
  output$segmentacao_da_base_RANGE_CAT1 <- renderUI({ 
    checkboxGroupInput("SPL_CAT1_DTB_LVLS",  i18n()$t("choose levels"),
                       choiceNames = levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB]),
                       choiceValues = levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB]),
                       selected = if(FOI_SEGMENTADO == TRUE && (SPL_CAT1_DTB_GLOBAL == as.vector(as.character(input$SPL_CAT1_DTB))) ) { 
                         as.vector(as.character(SPL_CAT1_DTB_LVLS_GLOBAL)) } else {
                           levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB])
                         }
                       ,inline = TRUE) })  
  
  #SEGMENTA BASE DE DADOS
  observeEvent(input$SEGMENTAR_BASE, {
    FOI_SEGMENTADO <<- TRUE
    SPL_NUM1_DTB_GLOBAL <<- as.vector(as.character(input$SPL_NUM1_DTB))
    POSICAO_SPL_NUM1_DTB_GLOBAL <<- as.numeric(match(SPL_NUM1_DTB_GLOBAL, names(var_num())))
    SPL_CAT1_DTB_GLOBAL <<- as.vector(as.character(input$SPL_CAT1_DTB))
    POSICAO_SPL_CAT1_DTB_GLOBAL <<- as.numeric(match(SPL_CAT1_DTB_GLOBAL, names(var_cat())))
    SPL_NUM1_DTB_RANGE_GLOBAL <<- as.vector(as.numeric(input$SPL_NUM1_DTB_RANGE))
    SPL_CAT1_DTB_LVLS_GLOBAL <<- as.vector(as.character(input$SPL_CAT1_DTB_LVLS))
    
    vetor_escolhas <- as.character(input$SPL_CAT1_DTB_LVLS)
    for (i in 1:length(input$SPL_CAT1_DTB_LVLS)) {
      if (as.character(input$SPL_CAT1_DTB_LVLS[i])=="") { 
        vetor_escolhas[i] <- "NA" 
      } else { vetor_escolhas[i] <- as.character(input$SPL_CAT1_DTB_LVLS[i])  }
    }
    categorico_temporario <- 0 
    for (i in 1:length(BaseEmR_BACKUP[,input$SPL_CAT1_DTB])) {
      if (is.na(BaseEmR_BACKUP[,input$SPL_CAT1_DTB][i])) {
        categorico_temporario[i] <- "NA"
      } else { categorico_temporario[i] <- as.character(BaseEmR_BACKUP[,input$SPL_CAT1_DTB])[i] }
    }
    BaseEmR_Seg <- BaseEmR_BACKUP[categorico_temporario %in% vetor_escolhas,]
    
    LSUP_BACKUP <- max(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])]))
    LINF_BACKUP <- min(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])]))
    LSUP_ESCOLHA <- max(as.numeric(input$SPL_NUM1_DTB_RANGE))
    LINF_ESCOLHA <- min(as.numeric(input$SPL_NUM1_DTB_RANGE))
    
    if ((LSUP_BACKUP != LSUP_ESCOLHA) || (LINF_BACKUP != LINF_ESCOLHA)) {
      BaseEmR_Seg <-  BaseEmR_Seg[!is.na(BaseEmR_Seg[,input$SPL_NUM1_DTB]),]
      BaseEmR_Seg <-  BaseEmR_Seg[BaseEmR_Seg[,input$SPL_NUM1_DTB] <= LSUP_ESCOLHA,]
      BaseEmR_Seg <-  BaseEmR_Seg[BaseEmR_Seg[,input$SPL_NUM1_DTB] >= LINF_ESCOLHA,]
    }
    
    BaseEmR_Seg <<- BaseEmR_Seg
    #DEBUG
    #write.csv2(BaseEmR(), file = "confere.csv", sep=";")
    
    output$texto_linhas_base = renderText(
      paste0( i18n()$t("The original base had"),": ", dim(data.frame(BaseEmR_BACKUP))[1]," ", i18n()$t("lines"), " / ",
              paste0( i18n()$t("The last base has"),": ", dim(data.frame(BaseEmR_Seg))[1]," ", i18n()$t("lines"))  )
    )
    
    
    #COLOCANDO VALORES ONDE FOI FEITO A SEGMENTA??O
    updateSelectInput(session, "SPL_NUM1_DTB",  i18n()$t("by numeric variable"),
                      choices = names(var_num()), selected = names(var_num())[POSICAO_SPL_NUM1_DTB_GLOBAL])
    updateSelectInput(session, "SPL_CAT1_DTB",  i18n()$t("by categorical variable"),
                      choices = names(var_cat()), selected = names(var_cat())[POSICAO_SPL_CAT1_DTB_GLOBAL])
    updateSliderInput(session, "SPL_NUM1_DTB_RANGE",   label = "choose the interval",
                      min = min(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                      max = max(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                      value = as.vector(SPL_NUM1_DTB_RANGE_GLOBAL))
    updateCheckboxGroupInput(session, "SPL_CAT1_DTB_LVLS",  i18n()$t("choose levels"),
                             choiceNames = levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB]),
                             choiceValues = levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB]),
                             selected = as.vector(as.character(SPL_CAT1_DTB_LVLS_GLOBAL)), inline = TRUE)
    
    
  })
  
  #RESTAURA BASE DE BACKUP
  observeEvent(input$RETORNAR_BASE, {
    FOI_SEGMENTADO <<- FALSE
    SPL_NUM1_DTB_GLOBAL <<-  names(var_num())
    POSICAO_SPL_NUM1_DTB_GLOBAL <<- 1
    SPL_CAT1_DTB_GLOBAL <<-  names(var_cat())
    POSICAO_SPL_CAT1_DTB_GLOBAL <<- 1
    SPL_NUM1_DTB_RANGE_GLOBAL <<- c(min(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                                    max(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])))
    SPL_CAT1_DTB_LVLS_GLOBAL <<- as.vector(unique(as.character(BaseEmR_BACKUP[,input$SPL_CAT1_DTB])))
    
    
    
    BaseEmR_Seg <<- BaseEmR_BACKUP
    output$texto_linhas_base = renderText(
      paste0( i18n()$t("The base has been restored to the original version")," :", dim(data.frame(BaseEmR_Seg))[1]," ", i18n()$t("lines"))
    )
    
    #VOLTANDO OS SELETORES PARA OS VALORES DEFAULT (MESMO C?DIGO DA ATUALIZA??O DOS VALORES)
    updateSelectInput(session, "SPL_NUM1_DTB",  i18n()$t("by numeric variable"),
                      choices = names(var_num()), selected = names(var_num())[POSICAO_SPL_NUM1_DTB_GLOBAL])
    updateSelectInput(session, "SPL_CAT1_DTB",  i18n()$t("by categorical variable"),
                      choices = names(var_cat()), selected = names(var_cat())[POSICAO_SPL_CAT1_DTB_GLOBAL])
    updateSliderInput(session, "SPL_NUM1_DTB_RANGE",   label = "choose the interval",
                      min = min(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                      max = max(as.numeric(BaseEmR_BACKUP[,input$SPL_NUM1_DTB][!is.na(BaseEmR_BACKUP[,input$SPL_NUM1_DTB])])),
                      value = as.vector(SPL_NUM1_DTB_RANGE_GLOBAL))
    updateCheckboxGroupInput(session, "SPL_CAT1_DTB_LVLS",  i18n()$t("choose levels"),
                             choiceNames = levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB]),
                             choiceValues = levels(BaseEmR_BACKUP[,input$SPL_CAT1_DTB]),
                             selected = as.vector(as.character(SPL_CAT1_DTB_LVLS_GLOBAL)), inline = TRUE)
  })
  
  
  
  
  output$kruskal_wallis_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('spl_BXP1_TST', i18n()$t("Compare Groups by"),  names(var_cat()) , selected = names(var_cat())[2] ),
          selectInput('analise_BXP_TST', i18n()$t("The value of the variable"), names(var_num()) , selected = names(var_num())[1] ),
          sliderInput("DCMP_KW_TST", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
        ),
        #MOSTRA O PAINEL DE SELE??O
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_KW'),
          h4(i18n()$t("Boxplot (Kruskal-Wallis Test)")),  (plotlyOutput("plot_BXP_TST") %>% withSpinner() ), 
          h4(i18n()$t("Result of the Kruskal-Wallis Test")),
          tableOutput("testes_de_comparacao"),
          h4(i18n()$t("Post hoc analysis (p-value for multiple comparisons)")),
          tableOutput("testes_de_comparacao_posthoc") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("there is a difference between groups"),"."))
          )
      ) ) })
  
  output$texto_linhas_base <- renderText({
  })
  
  
  output$anova_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('spl_ANOVA1_TST', i18n()$t("Compare Groups by"),  names(var_cat()) , selected = names(var_cat())[2] ),
          selectInput('analise_ANOVA_TST', i18n()$t("The value of the variable"), names(var_num()) , selected = names(var_num())[1] ),
          sliderInput("DCMP_ANOVA_TST", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
          sliderInput("DCMP_ANOVA_TSTpvalue", i18n()$t("Decimal places (p-value)"), min = 0, max = 15, value = 5),
        ),
        
        mainPanel(
          uiOutput('DISPLAY_SEGMENTACAO_ANOVA'),
          h4(i18n()$t("Boxplot (ANOVA Test)")),  (plotlyOutput("plot_ANOVA_TST") %>% withSpinner() ), 
          h4(i18n()$t("Result of the ANOVA Test")),
          tableOutput("testes_de_comparacaoANOVA"),
          h4(i18n()$t("Post hoc analysis (differences between means and p-value)")),
          tableOutput("testes_de_comparacao_posthocANOVA") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("there is a difference between groups"),".")) 
          )
      ) ) })
  
  
  output$mann_whitney_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('spl_BXP1_TST_MW',i18n()$t("Compare Groups by"),  names(var_cat_dic()) , selected = names(var_cat_dic())[1] ),
          selectInput('analise_BXP_TST_MW',i18n()$t("The value of the variable"), names(var_num()) , selected = names(var_num())[1] ),
          radioButtons("MW_ALTERNATIVE_HIP", i18n()$t("Alternative hypothesis"),
                       choiceNames= c(i18n()$t("two-tailed"),
                                      i18n()$t("greater"),
                                      i18n()$t("less")), 
                       choiceValues =c("two.sided", "greater", "less"), selected =c("two.sided")),
          sliderInput("DCMP_MW_TST", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
        ),
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_MW'),
          h4(i18n()$t("Boxplot (Mann-Whitney Test)")),  (plotlyOutput("plot_BXP_TST_MW") %>% withSpinner()  ), 
          h4(i18n()$t("Result of the Mann-Whitney Test")), tableOutput("testes_de_comparacao_MW") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("there is a difference between groups"),".")) 
          )
          ) ) })
  
  
  output$students_t_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('spl_BXP1_TST_TESTET',i18n()$t("Compare Groups by"),  names(var_cat_dic()) , selected = names(var_cat_dic())[1] ),
          selectInput('analise_BXP_TST_TESTET',i18n()$t("The value of the variable"), names(var_num()) , selected = names(var_num())[1] ),
          radioButtons("TESTET_ALTERNATIVE_HIP", i18n()$t("Alternative hypothesis"),
                       choiceNames= c(i18n()$t("two-tailed"),
                                      i18n()$t("greater"),
                                      i18n()$t("less")), 
                       choiceValues =c("two.sided", "greater", "less"), selected =c("two.sided")),
          sliderInput("DCMP_TESTET_TST", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
          sliderInput("DCMP_TESTET_TSTpvalue", i18n()$t("Decimal places (p-value)"), min = 0, max = 15, value = 5),
        ),
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_TESTT'),
          h4(i18n()$t("Boxplot (Student's t-test)")),  (plotlyOutput("plot_BXP_TST_TESTET") %>% withSpinner()  ), 
          h4(i18n()$t("Result of Student's t-test")), tableOutput("testes_de_comparacao_TESTET") %>% withSpinner(),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("there is a difference between groups"),".")) 
          )
          ) ) })
  
  
  output$correlation_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('COR_TST_X', i18n()$t("X axis"), names(var_num()) , selected = names(var_num())[1] ),
          selectInput('COR_TST_Y', i18n()$t("Y axis"), names(var_num()) , selected = names(var_num())[2] ),
          radioButtons("CORR_ALTERNATIVE_HIP", i18n()$t("Alternative hypothesis"),
                       choiceNames= c(i18n()$t("two-tailed"),
                                      i18n()$t("greater"),
                                      i18n()$t("less")), 
                       choiceValues =c("two.sided", "greater", "less"), selected =c("two.sided")),
          sliderInput("DCMP_CORR_TST", i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
        ),
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_CORRELATION'),
          h4(i18n()$t("Scatterplot (Correlation test)")),  (plotlyOutput("plot_COR_TST") %>% withSpinner()), 
          div(style="display:inline-block; vertical-align: top; padding-left: 1px; width:250px;", 
              h4(i18n()$t("Pearson correlation")), tableOutput("testes_de_correlacaopearson") %>% withSpinner() , br()),
          div(style="display:inline-block; vertical-align: top; padding-left: 1px; width:250px;", 
              h4(i18n()$t("Spearman's rank correlation")), tableOutput("testes_de_correlacaospearman") %>% withSpinner() , br()),
          div(style="display:inline-block; vertical-align: top; padding-left: 1px; width:250px;", 
              h4(i18n()$t("Kendall rank correlation")),  tableOutput("testes_de_correlacaokendall") %>% withSpinner() , br()),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("the correlation is significant"),".")) 
        ) )
    )})
  
  output$fisher_exact_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('FISHER_TST_LIN',  i18n()$t("lines"), names(var_cat()) , selected = names(var_cat())[2] ),
          selectInput('FISHER_TST_COL', i18n()$t("Columns"), names(var_cat()) , selected = names(var_cat())[2] ),
          radioButtons("FISHER_ALTERNATIVE_HIP", i18n()$t("Alternative hypothesis"),
                       choiceNames= c(i18n()$t("two-tailed"),
                                      i18n()$t("greater"),
                                      i18n()$t("less")), 
                       choiceValues =c("two.sided", "greater", "less"), selected =c("two.sided")),
          radioButtons("FISHER_TST_PERCENTAGE", i18n()$t("What measures do you need?"),
                       choiceNames= c(i18n()$t("Line percentage (%lin)"), i18n()$t("Column percentage (%col)")), 
                       choiceValues =c('(%lin)', '(%col)'), selected =c('(%col)')),
          
          sliderInput("DCMP_FISHER_PERCENT", i18n()$t("Decimal places"), min = 0, max = 10, value = 1),
          sliderInput("DCMP_FISHER_TST",  i18n()$t("Decimal places (p-value and CI)"), min = 0, max = 15, value = 10),
        ),
        
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_FISHER'),
          h4(i18n()$t("Fisher's exact test - tabulated data")), (tableOutput("table_fisher_test") %>% withSpinner() ), 
          h4(i18n()$t("Results for the Fisher's exact test")), 
          helpText(i18n()$t("In tables greater than 2x2 the p-value is estimated through 20x1000 Monte Carlo simulations")),
          (tableOutput("result_fisher_test") %>% withSpinner() ),
          helpText(paste0(i18n()$t("Legend"),": * ", i18n()$t("statistically significant values"),".")) 
          )
      ) ) })
  
  output$roc_curve_test_server <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('ROC_DESFECHO', i18n()$t("Dichotomous outcome"), names(var_cat_dic()) , selected = names(var_cat_dic())[1] ),
          radioButtons('ROC_DESFECHO_LVL', i18n()$t("Outcome of interest"), 
                       choices=  levels(as.factor(BaseEmR()[names(var_cat_dic())][,1])),
                       selected = levels(as.factor(BaseEmR()[names(var_cat_dic())][,1]))[1]),
          selectInput('ROC_VA', i18n()$t("The value of the variable"), names(var_num()) , selected = names(var_num())[2] ),
          sliderInput("NUMBER_ROC_BOOTSTRAP_REP", i18n()$t("Bootstrap repetitions"), min = 100, max = 5000, value = 500),
          sliderInput("DCMP_ROC_CURVE",  i18n()$t("Decimal places"), min = 0, max = 10, value = 3),
        ),
        
        mainPanel( 
          uiOutput('DISPLAY_SEGMENTACAO_ROC'),
          h4(i18n()$t("ROC Curve - The ROC Curve Calculation can take up to 5 minutes")),  (plotlyOutput("plot_ROC_CURVE") %>% withSpinner()), 
          h4( textOutput("TEXT_ROC_BOOTSTRAP_REP", inline = TRUE) ), (tableOutput("testes_da_ROC_Curve") %>% withSpinner() ) )
      )
    )})
  
  output$cite_this_web_site_server <- renderUI({  
    today_year <- format(Sys.Date(), format="%Y")
    today_month <- format(Sys.Date(), format="%B")
    today_day <- format(Sys.Date(), format="%d")
    
    tagList(
      
      navlistPanel(
        widths = c(2, 10),
        tabPanel(i18n()$t("How can I cite this web site in my publication?"), 
                 h4(i18n()$t("How can I cite this web site in my publication?")), br(),
                 br(), tags$strong("APA style"), br(), 
                 "The format is", ": Website Name. (Year, Month Day). Retrieved from [URL].", br(),
                 "For example", ": ", span(id="logotxt", "pvalue.site"), ". (",paste0(today_year),", ",paste0(today_month)," ",paste0(today_day),"). Retrieved from ",tags$a(href="https://pvalue.site", "https://pvalue.site"),".", br(),
                 "For in text citations you can shorten to: (Website Name, Year). For example, (", span(id="logotxt", "pvalue.site"),", ",paste0(today_year),").",
                 br(), tags$strong("Chicago Style"), br(),
                 "The format is: Website Owner. Website Name. URL (accessed date).", br(),
                 "For example: Lopes-Neto, F.D.N. ", span(id="logotxt", "pvalue.site"),". ",tags$a(href="https://pvalue.site", "https://pvalue.site")," (accessed ",paste0(today_month)," ",paste0(today_day),", ",paste0(today_year),").",
                 br(), tags$strong("MLA Style"), br(),
                 "The format is: Website Name, Publisher Name, URL.", br(),
                 "For example, ", span(id="logotxt", "pvalue.site"),", Lopes-Neto, F.D.N., ", tags$a(href="https://pvalue.site", "https://pvalue.site"), ".", br(),
                 tags$em(i18n()$t("If you have any questions"),", ",i18n()$t("or need more information to cite")," ", span(id="logotxt", "pvalue.site"),", ",i18n()$t("send us an email")," (",
                         tags$a(href="mailto:contact@pvalue.site", "", span(id="logotxt", "contact@pvalue.site"),"") ,")."), br(),
                 h4(i18n()$t("Other important information!")),
                 i18n()$t("Soon we will make a page to publicize projects that used the")," ", span(id="logotxt", "pvalue.site"),".", br(),
                 i18n()$t("So"),", ",i18n()$t("if you want"), ", ", i18n()$t("we can publicize your"), tags$strong(i18n()$t("article - post - YouTube course")),
                 ", ",i18n()$t("or"),"  ", tags$strong(i18n()$t("any production"))," ",i18n()$t("that uses the services of")," ", span(id="logotxt", "pvalue.site"),".", br(),
                 i18n()$t("Send us an email"),": ", tags$a(href="mailto:contact@pvalue.site", "", span(id="logotxt", "contact@pvalue.site"),"") ),
        
        tabPanel(i18n()$t("Contact"), h4(i18n()$t("Contact")),
                 
                 i18n()$t("We are waiting for your contact"), ". ", br() ,
                 i18n()$t("if you have any suggestions"), " ", br() ,
                 i18n()$t("question or compliment"), ",", br() ,
                 i18n()$t("send an email to"), ": ",br() ,
                 tags$a(href="mailto:contact@pvalue.site", span(id="logotxt", "contact@pvalue.site") ), br() ,
                 i18n()$t("Regards"), ",", br() ,
                 span(id="logotxt", "pvalue.site") ),
        
        tabPanel(
          i18n()$t("Legal Disclaimer"), h4(i18n()$t("Legal Disclaimer")),
          
          span(i18n()$t("The owners of this website"), ", ",
               tags$strong(span(id="logotxt", "pvalue.site")),", ",
               i18n()$t("are not responsible for"), ", ",
               i18n()$t("and expressly disclaim all liability for"), ", ",
               i18n()$t("damages of any kind arising out of use"), ", ",
               
               i18n()$t("reference to"), ", ",
               i18n()$t("or reliance on any information"), ", ",
               i18n()$t("including all results from all statistical calculators"), ", ", 
               i18n()$t("contained within this website"), ". ",
               
               i18n()$t("Please make sure that you understand that the information provided here"), ", ", 
               i18n()$t("including the results of statistical calculations"), ", ",
               i18n()$t("is being provided freely"), ", ",
               i18n()$t("and that no kind of agreement or contract is created between you and the owners of this site"), ", ",
               i18n()$t("the owners of the servers upon which it is housed"), ", ",
               i18n()$t("or anyone else who is in any way connected with this project"), ". ",
               
               i18n()$t("In short"), ", ",
               i18n()$t("the information within this website is presented as is"), ", ",
               i18n()$t("and you use this site"), ", ",
               i18n()$t("including its statistical calculators"), ", ",
               i18n()$t("at your own risk"), ". ",
               
               i18n()$t("While we make our best efforts to ensure the accuracy of the calculations performed here"), ", ",
               i18n()$t("we cannot rule out the possibility of error"), ", ",
               i18n()$t("and we will not be held liable in the unfortunate circumstance that such an error arises"), ". ",
               
               i18n()$t("By using this website"), ", ",
               i18n()$t("you accept the terms and conditions of this disclaimer"), "."), br(),
          tags$strong(span(id="logotxt", "pvalue.site"))
        ) )
    )})
  
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  output$view_download_server <- renderUI({
    tagList(
      uiOutput('DISPLAY_SEGMENTACAO_VIEW_DOWNLOAD'),
      DT::renderDataTable( 
        datatable(data = BaseEmR(),
                  extensions = 'Buttons',
                  options = list(
                    dom = "Blfrtip",
                    buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf") , text = "download") ),
                    lengthMenu = list( c(10, 20, 40, 60, 80, 100, -1) , c(10, 20, 40, 60, 80, 100,  "All") ) # end of lengthMenu customization
                    , pageLength = 10 ) )
      ) 
    ) })
  
  ### IN?CIO Plot Histogram
  output$plot <- renderPlotly({
    if (  match(input$spl, names(BaseEmR())) == 2 ) {
      plot <- plot_ly(BaseEmR(), x = ~get(input$analise), type = "histogram", nbinsx = input$b ) %>% 
        layout( xaxis = list(title = names(BaseEmR()[input$analise]) ),#, titlefont = f), 
                yaxis = list(title = i18n()$t("Frequency") ),#, titlefont = f), 
                barmode = "stack", margin = list(b = 100))
      plot <- plot %>%
        add_annotations( input$spl, x = 1, y = 1, xref = "paper", yref = "paper", showarrow = FALSE )
      plot
    } else {
      one_plot <- function(d) {
        plot <- plot_ly(d, x = ~get(input$analise), type = "histogram", nbinsx = input$b ) %>% 
          layout( barmode = "stack", margin = list(b = 100))
        plot <- plot %>%
          add_annotations( d[1,match(input$spl, names(BaseEmR()))], x = 1, y = 1, xref = "paper", yref = "paper", showarrow = FALSE )
        plot
      }
      splitted_list <- split(BaseEmR(), BaseEmR()[input$spl])
      lapply(splitted_list, one_plot) %>% 
        subplot(nrows = 2, shareX = TRUE, titleX = FALSE) %>% 
        hide_legend() 
      
    }
  })
  
  ### IN?CIO Plot Boxplot
  output$plot_BXP <- renderPlotly({
    
    plot_BXP <- plot_ly(BaseEmR(), x = ~get(input$spl_BXP0), y = ~get(input$analise_BXP), color = ~get(input$spl_BXP), type = "box")  %>% 
      layout(xaxis = list(title = if (input$spl_BXP0 == "OneGroup_pvalue") { paste0(input$spl_BXP)} else { 
        if(input$spl_BXP == "OneGroup_pvalue"){ paste0(input$spl_BXP0) } else {
          paste0(input$spl_BXP0," by ",input$spl_BXP)}} ),
        yaxis = list(title = input$analise_BXP  ),
        barmode = "stack", margin = list(b = 100))
    plot_BXP <- plot_BXP %>% layout(boxmode = "group")
    plot_BXP 
    
  })
  
  ### IN?CIO Scatterplot
  output$plot_SCTP <- renderPlotly({
    
    x_SCTP <- list( title = input$analise_SCTP1 )
    y_SCTP <- list( title = input$analise_SCTP2 )
    
    plot_SCTP <- plot_ly(BaseEmR(), x = ~get(input$analise_SCTP1), y = ~get(input$analise_SCTP2),
                         text = ~paste(input$analise_SCTP1, ": ", get(input$analise_SCTP1), '<br>' ,  input$analise_SCTP2, ": ", get(input$analise_SCTP2), '<br>' ,  input$analise_SCTP3, ": ", get(input$analise_SCTP3), '<br>' ,  input$analise_SCTP4, ": ", get(input$analise_SCTP4)),
                         color = ~get(input$analise_SCTP3), size = ~get(input$analise_SCTP4) )  
    
    plot_SCTP <- plot_SCTP %>% layout(xaxis = x_SCTP, yaxis = y_SCTP)
    
    plot_SCTP
  })
  
  ### IN?CIO CORR 2 numbers 2 factors
  output$plot_CORR_2NUM_2FAT <- renderPlot({  
    pm <- ggpairs(
      data.frame(BaseEmR())[ , c(match(input$CORR_2NUM_2FAT_1NUM, names(data.frame(BaseEmR()))),
                                 match(input$CORR_2NUM_2FAT_1FAT, names(data.frame(BaseEmR()))),
                                 match(input$CORR_2NUM_2FAT_2FAT, names(data.frame(BaseEmR()))),
                                 match(input$CORR_2NUM_2FAT_2NUM, names(data.frame(BaseEmR())))) ][ , c(1,2,3,4) ],
      upper = list(continuous = "density", combo = "box_no_facet"),
      lower = list(continuous = "points", combo = "dot_no_facet") )
    pm
    
  })
  
  ### IN?CIO Plot BoxPlot - TESTE KW
  output$plot_BXP_TST <- renderPlotly({
    
    x_KW <- list( title = input$spl_BXP1_TST )
    y_KW <- list( title = input$analise_BXP_TST )
    
    plot_BXP_TST <- plot_ly(BaseEmR(), x = ~get(input$spl_BXP1_TST), y = ~get(input$analise_BXP_TST), type = "box") 
    plot_BXP_TST <- plot_BXP_TST  %>% layout(boxmode = "group")
    
    plot_BXP_TST <- plot_BXP_TST %>% layout(xaxis = x_KW, yaxis = y_KW)
    
    plot_BXP_TST
    
  })
  
  
  
  
  ### IN?CIO Plot BoxPlot - TESTE ANOVA
  output$plot_ANOVA_TST <- renderPlotly({
    
    x_ANOVA <- list( title = input$spl_ANOVA1_TST )
    y_ANOVA <- list( title = input$analise_ANOVA_TST )
    
    plot_ANOVA_TST <- plot_ly(BaseEmR(), x = ~get(input$spl_ANOVA1_TST), y = ~get(input$analise_ANOVA_TST), type = "box") 
    plot_ANOVA_TST <- plot_ANOVA_TST %>% layout(boxmode = "group")
    
    plot_ANOVA_TST <- plot_ANOVA_TST %>% layout(xaxis = x_ANOVA, yaxis = y_ANOVA)
    
    plot_ANOVA_TST
    
  })
  
  
  ### IN?CIO gr?fico DE CORRELA??O
  output$plot_COR_TST <- renderPlotly({
    
    xx <- list( title = input$COR_TST_X )
    yy <- list( title = input$COR_TST_Y )
    
    plot_COR_TST <- BaseEmR() %>%
      plot_ly(., x = ~get(input$COR_TST_X), y = ~get(input$COR_TST_Y), type='scatter', name='data set',
              marker = list(size = 10,
                            color = 'rgba(255, 182, 193, .9)',
                            line = list(color = 'rgba(152, 0, 0, .8)',
                                        width = 2)))
    
    plot_COR_TST <- plot_COR_TST %>% layout(title = '',
                                            yaxis = list(zeroline = FALSE),
                                            xaxis = list(zeroline = FALSE))
    
    plot_COR_TST <- plot_COR_TST %>% layout(xaxis = xx, yaxis = yy)
    
    BaseTemp = cbind(data.frame(BaseEmR())[input$COR_TST_Y], data.frame(BaseEmR())[input$COR_TST_X])
    BaseTemp <- BaseTemp[complete.cases(BaseTemp[,c(1,2)]),]
    aa <- lm(BaseTemp[,1]~BaseTemp[,2])$coefficients[2]
    bb <- lm(BaseTemp[,1]~BaseTemp[,2])$coefficients[1]
    
    p_abline <- function(x, a, b) { 
      y <-  a*x + b
      return(y) 
    }
    
    p<-add_trace(plot_COR_TST,  x=c(min(BaseTemp[,2]), max(BaseTemp[,2])), 
                 y=c(p_abline(min(BaseTemp[,2]), aa, bb), p_abline(max(BaseTemp[,2]), aa, bb)), 
                 type="scatter", mode= i18n()$t("lines"), name='linear fit', showlegend = FALSE, hoverinfo='none',
                 line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'),
                 marker = list(size = 0,
                               color = 'rgba(0, 0, 0, 0)',
                               line = list(color = 'rgba(0, 0, 0, 0)',
                                           width = 0)))
    p
    
    
  })
  
  ############################################################################################################################ INICIO grafico curva roc
  output$plot_ROC_CURVE <- renderPlotly({
    
    BaseEmR_T <- data.frame(BaseEmR())[complete.cases(data.frame(BaseEmR())[input$ROC_DESFECHO]),c(1:length(data.frame(BaseEmR())[1,]))]
    BaseEmR_T <- BaseEmR_T[complete.cases(BaseEmR_T[input$ROC_VA]),c(1:length(BaseEmR_T[1,]))]
    var_cat_dic_ins <- input$ROC_DESFECHO
    #BaseEmR_T[,var_cat_dic_ins] <- as.factor(BaseEmR_T[,var_cat_dic_ins])
    var_num_analisada <- input$ROC_VA
    vetor_variavel_analisada <- data.frame(BaseEmR_T[,var_num_analisada])[,1]
    nivel_escolhido <- input$ROC_DESFECHO_LVL
    desfecho <- matrix(0,length(vetor_variavel_analisada),1)
    for (i in 1:length(BaseEmR_T[,var_num_analisada])) { 
      if(  nivel_escolhido == BaseEmR_T[var_cat_dic_ins][,1][i]  ) { 
        desfecho[i,1] <- 1} else { desfecho[i,1] <- 0} }
    desfecho <- data.frame(desfecho)
    names(desfecho) <- "Outcome"
    vetor_variavel_analisada <- data.frame(vetor_variavel_analisada)
    names(vetor_variavel_analisada) <- var_num_analisada
    
    BaseEmR_ROC <- cbind(desfecho , vetor_variavel_analisada)
    
    BaseEmR_ROC <- BaseEmR_ROC[complete.cases(BaseEmR_ROC[,1:length(BaseEmR_ROC[1,])]),]
    
    roc1 <- roc(BaseEmR_ROC[,1], BaseEmR_ROC[,2])
    
    sensibilidade <- roc1$sensitivities
    um_menos_especificidade <- matrix(0,1,length(roc1$specificities))
    for (i in 1:length(roc1$specificities)) { um_menos_especificidade[1,i] <- 1 - roc1$specificities[i] }
    um_menos_especificidade <- as.numeric(um_menos_especificidade)
    Cutoffs <- roc1$thresholds
    
    False_positive_rate <- data.frame(um_menos_especificidade)
    names(False_positive_rate) <- "False_positive_rate"
    True_positive_rate <- data.frame(sensibilidade)
    names(True_positive_rate) <- "True_positive_rate"
    Cutoff <- data.frame(Cutoffs)
    names(Cutoff) <- "Cutoff"
    #ENTRA COMO VARI?VEL GLOBAL PARA SER REAPROVEITADO NA CONFEC??O DA TABELA
    AUC_10000 <<- ci.auc(smooth(roc1, method="density"), boot.n = input$NUMBER_ROC_BOOTSTRAP_REP)
    AUC_Curve <- data.frame(rep(AUC_10000[2], length(roc1$thresholds)))
    names(AUC_Curve) <- "AUC"
    V_NAME <- data.frame(rep(var_num_analisada, length(roc1$thresholds)))
    names(V_NAME) <- var_num_analisada
    
    ROC_CURVE <- data.frame(cbind(False_positive_rate, True_positive_rate, Cutoff, AUC_Curve, V_NAME ))
    
    ROC_PLOT <- plot_ly(ROC_CURVE, x = ~False_positive_rate, y = ~True_positive_rate, type='scatter', mode='lines', name="ROC Curve",
                        text = ~paste("Variable:", V_NAME[1,1] , '<br>',
                                      "Cutoff:", round(Cutoff, digits = input$DCMP_ROC_CURVE) , '<br>', 
                                      "Sensitivity: ", round(True_positive_rate, digits = input$DCMP_ROC_CURVE) , '<br>',
                                      "1-Specificity: ", round(False_positive_rate, digits = input$DCMP_ROC_CURVE) , '<br>',
                                      "AUC: ", round(AUC_Curve[1,1], digits = input$DCMP_ROC_CURVE) ) )
    p <- add_trace( ROC_PLOT , x=seq(0, 1, length.out = length(ROC_CURVE[,1])), y=seq(0, 1, length.out = length(ROC_CURVE[,1])), 
                    type="scatter", mode= i18n()$t("lines"), name='Reference', mode= i18n()$t("lines"), showlegend = FALSE, hoverinfo='none',
                    line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot') )
    p
    
  })
  
  ### IN?CIO Plot BoxPlot - TESTE MW
  output$plot_BXP_TST_MW <- renderPlotly({
    
    x_MW <- list( title = input$spl_BXP1_TST_MW )
    y_MW <- list( title = input$analise_BXP_TST_MW )
    
    plot_BXP_TST_MW <- plot_ly(BaseEmR(), x = ~get(input$spl_BXP1_TST_MW), y = ~get(input$analise_BXP_TST_MW), type = "box") 
    plot_BXP_TST_MW <- plot_BXP_TST_MW %>% layout(boxmode = "group")
    plot_BXP_TST_MW <- plot_BXP_TST_MW %>% layout(xaxis = x_MW, yaxis = y_MW)
    
    plot_BXP_TST_MW
    
  })
  ### FINAL Plot BoxPlot - TESTE MW
  
  ### IN?CIO Plot BoxPlot - TESTET
  output$plot_BXP_TST_TESTET <- renderPlotly({
    
    x_TESTET <- list( title = input$spl_BXP1_TST_TESTET )
    y_TESTET <- list( title = input$analise_BXP_TST_TESTET )
    
    plot_BXP_TST_TESTET <- plot_ly(BaseEmR(), x = ~get(input$spl_BXP1_TST_TESTET), y = ~get(input$analise_BXP_TST_TESTET), type = "box") 
    plot_BXP_TST_TESTET <- plot_BXP_TST_TESTET %>% layout(boxmode = "group")
    plot_BXP_TST_TESTET <- plot_BXP_TST_TESTET %>% layout(xaxis = x_TESTET, yaxis = y_TESTET)
    
    plot_BXP_TST_TESTET
    
  })
  ### FINAL Plot BoxPlot - TESTET
  
  
  ### INICIO comparacao_KW
  output$testes_de_comparacao <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_KW_TST
    
    matriz <- matrix("", 2, 3)
    colnames(matriz) <- c(i18n()$t("Parameter"),i18n()$t("Value"),i18n()$t("Conclusion"))
    matriz[1,1]<-i18n()$t("p-value")
    matriz[1,2]<-try(round(kruskal.test(get(input$analise_BXP_TST) ~ get(input$spl_BXP1_TST), data = BaseEmR())$p.value, digits = DECIMAL_PLACES), silent=TRUE)[1]
    matriz[1,3]<- if (!is.na(as.numeric(matriz[1,2]))) { if (as.numeric(matriz[1,2])<0.05) {
      i18n()$t("there is a statistically significant difference between the groups")} else {
        i18n()$t("it was not possible to prove differences between groups")}  } else {matriz[1,2]}
    
    matriz[1,2]<- if (!is.na(as.numeric(matriz[1,2]))) { if (as.numeric(matriz[1,2])<0.05) {
                                                        paste0(matriz[1,2],"*")} else {
                                                          matriz[1,2]}  } else {matriz[1,2]}

    matriz[2,1]<-i18n()$t("statistic(K-W)")
    matriz[2,2]<-try(round(kruskal.test(get(input$analise_BXP_TST) ~ get(input$spl_BXP1_TST), data = BaseEmR())$statistic, digits = DECIMAL_PLACES), silent=TRUE)[1]
    matriz
  })
  
  
  ### INICIO comparacao_ANOVA
  output$testes_de_comparacaoANOVA<- renderTable({
    
    
    DECIMAL_PLACES <- input$DCMP_ANOVA_TST
    
    DECIMAL_PLACESpvalue <- input$DCMP_ANOVA_TSTpvalue
    teste <- summary(aov(get(input$analise_ANOVA_TST) ~ get(input$spl_ANOVA1_TST), data = data.frame(BaseEmR())))
    matriz <- matrix("", 2, 7)
    colnames(matriz) <- c(i18n()$t("Parameter"),i18n()$t("Df"), i18n()$t("Sum sq"), i18n()$t("Mean sq"), paste0(i18n()$t("Statistic"),"(F)"), "Pr(>F)", i18n()$t("Conclusion"))
    matriz[1,1]<-i18n()$t("Between groups")
    matriz[2,1]<-i18n()$t("Within groups")
    matriz[1,2]<-round(teste[[1]][1][1,1], digits = DECIMAL_PLACES)
    matriz[2,2]<-round(teste[[1]][1][2,1], digits = DECIMAL_PLACES)
    matriz[1,3]<-round(teste[[1]][2][1,1], digits = DECIMAL_PLACES)
    matriz[2,3]<-round(teste[[1]][2][2,1], digits = DECIMAL_PLACES)
    matriz[1,4]<-round(teste[[1]][3][1,1], digits = DECIMAL_PLACES)
    matriz[2,4]<-round(teste[[1]][3][2,1], digits = DECIMAL_PLACES)
    matriz[1,5]<-round(teste[[1]][4][1,1], digits = DECIMAL_PLACES)
    matriz[2,5]<-""
    matriz[1,6]<-round(teste[[1]][5][1,1], digits = DECIMAL_PLACESpvalue)
    matriz[2,6]<-""
    matriz[1,7]<-if (!is.na(as.numeric(matriz[1,6]))) { if (as.numeric(matriz[1,6])<0.05) {
                      i18n()$t("there is a statistically significant difference between the groups")} else {
                        i18n()$t("it was not possible to prove differences between groups")}  } else {matriz[1,6]}
    matriz[1,6]<- if (!is.na(as.numeric(matriz[1,6]))) { if (as.numeric(matriz[1,6])<0.05) {
                      paste0(matriz[1,6],"*")} else {
                        matriz[1,6]}  } else {matriz[1,6]}
    
    matriz
  })
  
  ### INICIO comparacao_MANN WHITNEY
  output$testes_de_comparacao_MW <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_MW_TST
    HIP_ALTERNATIVA <- input$MW_ALTERNATIVE_HIP
    
    matriz <- matrix("", 2, 3)
    colnames(matriz) <- c(i18n()$t("Parameter"),i18n()$t("Value"),i18n()$t("Conclusion"))
    matriz[1,1]<-i18n()$t("p-value")
    matriz[1,2]<-try(round(wilcox.test(get(input$analise_BXP_TST_MW) ~ get(input$spl_BXP1_TST_MW), data = BaseEmR(),
                                       alternative = HIP_ALTERNATIVA)$p.value, digits = DECIMAL_PLACES), silent=TRUE)[1]
    
    matriz[1,3] <- if (!is.na(as.numeric(matriz[1,2]))) { if (as.numeric(matriz[1,2])<0.05) {
                        i18n()$t("there is a statistically significant difference between the groups")} else {
                          i18n()$t("it was not possible to prove differences between groups")}  } else {matriz[1,2]}
    
    matriz[1,2]<- if (!is.na(as.numeric(matriz[1,2]))) { if (as.numeric(matriz[1,2])<0.05) {
                        paste0(matriz[1,2],"*")} else {
                          matriz[1,2]}  } else {matriz[1,2]}
    
    matriz[2,1]<-i18n()$t("statistic(M-W)")
    matriz[2,2]<-try(round(wilcox.test(get(input$analise_BXP_TST_MW) ~ get(input$spl_BXP1_TST_MW), data = BaseEmR(),
                                       alternative = HIP_ALTERNATIVA)$statistic, digits = DECIMAL_PLACES), silent=TRUE)[1]
    matriz
  })
  
  
  
  ### INICIO comparacao_TEST T
  output$testes_de_comparacao_TESTET <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_TESTET_TST
    DECIMAL_PLACESpvalue <- input$DCMP_TESTET_TSTpvalue
    HIP_ALTERNATIVA <- input$TESTET_ALTERNATIVE_HIP
    BaseEmR_T <- data.frame(BaseEmR())
    
    
    TESTE_VAR_EQ <- t.test(BaseEmR_T[,input$analise_BXP_TST_TESTET] ~  BaseEmR_T[,input$spl_BXP1_TST_TESTET], 
                           alternative = HIP_ALTERNATIVA, var.equal=TRUE)
    
    TESTE_VAR_DIFF <- t.test(BaseEmR_T[,input$analise_BXP_TST_TESTET] ~  BaseEmR_T[,input$spl_BXP1_TST_TESTET], 
                             alternative = HIP_ALTERNATIVA, var.equal=FALSE)
    
    
    #LEVENE AJUSTADO    
    YY <- BaseEmR_T[,input$analise_BXP_TST_TESTET]
    GROUPS <- BaseEmR_T[,input$spl_BXP1_TST_TESTET]
    REMOVE <- 0
    for (i in 1:dim(BaseEmR_T)[1]) {
      if ( is.na(YY[i]) || is.na(GROUPS[i]) ) { REMOVE[i] <- FALSE } else { REMOVE[i] <- TRUE }
    }
    YY_GROUPS <- cbind(YY, GROUPS)
    YY_GROUPS <- YY_GROUPS[REMOVE==TRUE,]
    LEVENE <- levene.test(YY_GROUPS[,1], as.factor(YY_GROUPS[,2]))
    
    
    matrizTT <- matrix("", 2, 12)
    colnames(matrizTT) <- c(i18n()$t("Assumption"),"|",
                            i18n()$t("F(Levene's test)"), i18n()$t("Pr(>F)"),"|",
                            i18n()$t("T-test"),i18n()$t("Df"),i18n()$t("p-value"),"|", 
                            i18n()$t("Mean(diff)"), i18n()$t("Sd(diff)"), i18n()$t("CI95%(diff)"))
    
    matrizTT[1,1]<-i18n()$t("equals(var)")
    matrizTT[1,2]<-"|"
    matrizTT[1,3]<-round(LEVENE$statistic, digits = DECIMAL_PLACES)
    matrizTT[1,4]<-round(LEVENE$p.value, digits = DECIMAL_PLACESpvalue)
    matrizTT[1,4]<-if (!is.na(as.numeric(matrizTT[1,4]))) { if (as.numeric(matrizTT[1,4]) <0.05) {paste0(matrizTT[1,4],"*")}else{matrizTT[1,4]} }else{matrizTT[1,4]}
    
    matrizTT[2,1]<-i18n()$t("diff.(var)")
    matrizTT[2,2]<-"|"
    matrizTT[2,3]<-" "
    matrizTT[2,4]<-" "
    matrizTT[1,5]<-"|"
    matrizTT[2,5]<-"|"
    matrizTT[1,6]<-round(TESTE_VAR_EQ$statistic, digits = DECIMAL_PLACES)
    matrizTT[1,7]<-round(TESTE_VAR_EQ$parameter, digits = DECIMAL_PLACES)
    matrizTT[1,8]<-round(TESTE_VAR_EQ$p.value, digits = DECIMAL_PLACESpvalue)
    matrizTT[1,8]<-if (!is.na(as.numeric(matrizTT[1,8]))) { if (as.numeric(matrizTT[1,8]) <0.05) {paste0(matrizTT[1,8],"*")}else{matrizTT[1,8]} }else{matrizTT[1,8]}
    matrizTT[2,6]<-round(TESTE_VAR_DIFF$statistic, digits = DECIMAL_PLACES)
    matrizTT[2,7]<-round(TESTE_VAR_DIFF$parameter, digits = DECIMAL_PLACES)
    matrizTT[2,8]<-round(TESTE_VAR_DIFF$p.value, digits = DECIMAL_PLACESpvalue)
    matrizTT[2,8]<-if (!is.na(as.numeric(matrizTT[2,8]))) { if (as.numeric(matrizTT[2,8]) <0.05) {paste0(matrizTT[2,8],"*")}else{matrizTT[2,8]} }else{matrizTT[2,8]}
    matrizTT[1:2,9]<-"|"
    matrizTT[1,10]<-round(mean(TESTE_VAR_EQ$conf.int), digits = DECIMAL_PLACES)
    matrizTT[1,11]<-round(TESTE_VAR_EQ$stderr, digits = DECIMAL_PLACES)
    matrizTT[1,12]<-paste0("[",round(TESTE_VAR_EQ$conf.int[1], digits = DECIMAL_PLACES),";",
                           round(TESTE_VAR_EQ$conf.int[1], digits = DECIMAL_PLACES),"]")
    matrizTT[2,10]<-round(mean(TESTE_VAR_DIFF$conf.int), digits = DECIMAL_PLACES)
    matrizTT[2,11]<-round(TESTE_VAR_DIFF$stderr, digits = DECIMAL_PLACES)
    matrizTT[2,12]<-paste0("[",round(TESTE_VAR_DIFF$conf.int[1], digits = DECIMAL_PLACES),";",
                           round(TESTE_VAR_DIFF$conf.int[2], digits = DECIMAL_PLACES),"]")
    matrizTT
    
  })
  
  
  
  
  #IN?CIO DO POST HOC KW  
  output$testes_de_comparacao_posthoc <- renderTable({
    
    base_temp1 <- data.frame(BaseEmR())[input$analise_BXP_TST]
    base_temp2 <- data.frame(BaseEmR())[input$spl_BXP1_TST]
    base_temp <- cbind(base_temp1, base_temp2)
    PT = as.matrix(round(pairwise.wilcox.test(as.numeric(base_temp[,1]), base_temp[,2], p.adjust.method="none")[3]$p.value, digits=input$DCMP_KW_TST))
    IMPRESSO <- matrix("111", length(PT[,1]), 1+length(PT[1,]))
    NOMES_LINHAS <- rownames(PT)
    IMPRESSO[,1] <- NOMES_LINHAS
    colnames(IMPRESSO) <- combine("Vs", colnames(PT))
    IMPRESSO[1:length(PT[,1]),2:(length(PT[1,])+1)] <- PT[1:length(PT[,1]),1:length(PT[1,])]
    
    for (i in 1:dim(IMPRESSO)[1]) {
      for (j in 2:dim(IMPRESSO)[2]) {
        if (i<=j){
          if (!is.na(as.numeric(IMPRESSO[i,j]))) {
            if (as.numeric(IMPRESSO[i,j]) < 0.05) { IMPRESSO[i,j] <- paste0(IMPRESSO[i,j],"*") } else {IMPRESSO[i,j] <- IMPRESSO[i,j]}
          }
        }
      }
    }
    
    IMPRESSO
    
  })
  
  #IN?CIO DO POST HOC ANOVA
  output$testes_de_comparacao_posthocANOVA <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_ANOVA_TST
    DECIMAL_PLACESpvalue <- input$DCMP_ANOVA_TSTpvalue
    testeee <- aov(get(input$analise_ANOVA_TST) ~ get(input$spl_ANOVA1_TST), data = data.frame(BaseEmR()))
    
    IMPRESSAO_RESULTADOS <- TukeyHSD(testeee)[[1]]
    IMPRESSAO_RESULTADOS[,4] <- round(IMPRESSAO_RESULTADOS[,4] , digits= DECIMAL_PLACESpvalue)
    IMPRESSAO_RESULTADOS[,1:3] <- round(IMPRESSAO_RESULTADOS[,1:3] , digits= DECIMAL_PLACES)
    
    IMPRESSAO_RESULTADOS <- cbind(rownames(IMPRESSAO_RESULTADOS),
                                  IMPRESSAO_RESULTADOS,IMPRESSAO_RESULTADOS[,4])
    colnames(IMPRESSAO_RESULTADOS)[1] <- " "
    colnames(IMPRESSAO_RESULTADOS) <-c(" ", i18n()$t("Difference"), i18n()$t("Lower limit (95%)"), i18n()$t("Upper limit (95%)"), i18n()$t("Adjusted p-value"), i18n()$t("Conclusion"))
    
    for (i in 1:dim(IMPRESSAO_RESULTADOS)[1]) {
      if (!is.na(as.numeric(IMPRESSAO_RESULTADOS[i,5]))) { if (as.numeric(IMPRESSAO_RESULTADOS[i,5]) < 0.05) {
        IMPRESSAO_RESULTADOS[i,6]<-i18n()$t("there is a statistically significant difference between the groups")} else {
          IMPRESSAO_RESULTADOS[i,6]<-i18n()$t("it was not possible to prove differences between groups") } } else {IMPRESSAO_RESULTADOS[i,6]<-""}
    }
    
    
    for (i in 1:dim(IMPRESSAO_RESULTADOS)[1]) {
      if (!is.na(as.numeric(IMPRESSAO_RESULTADOS[i,5]))) { if (as.numeric(IMPRESSAO_RESULTADOS[i,5]) < 0.05) {
        IMPRESSAO_RESULTADOS[i,5]<-paste0(IMPRESSAO_RESULTADOS[i,5],"*") } else {
          IMPRESSAO_RESULTADOS[i,5]<-IMPRESSAO_RESULTADOS[i,5] } } else {IMPRESSAO_RESULTADOS[i,5]<-IMPRESSAO_RESULTADOS[i,5]}
    }
    
    IMPRESSAO_RESULTADOS
    
  })
  
  ### INICIO correla??o
  output$testes_de_correlacaopearson <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_CORR_TST
    VARIAVELX <- input$COR_TST_X
    VARIAVELY <- input$COR_TST_Y
    BaseEmR_T <- data.frame(BaseEmR())
    HIP_ALTERNATIVA <- input$CORR_ALTERNATIVE_HIP
    
    
    
    xx <- BaseEmR_T[,VARIAVELX]
    yy <- BaseEmR_T[,VARIAVELY]
    
    xxyy <- cbind(xx,yy)
    matriz <- matrix("", 3, 2)
    colnames(matriz) <- c(i18n()$t("Parameter"), i18n()$t("Value"))
    matriz[1,1] <- i18n()$t("p-value")
    matriz[1,2] <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "pearson",
                                       alternative = HIP_ALTERNATIVA)$p.value, digits=DECIMAL_PLACES), silent=TRUE)[1]
    matriz[1,2] <- if (!is.na(as.numeric(matriz[1,2]))) { if(as.numeric(matriz[1,2])<0.05) {paste0(matriz[1,2],"*")}else{matriz[1,2]} }else{matriz[1,2]}
    
    
    matriz[2,1] <- i18n()$t("correlation")
    matriz[2,2] <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "pearson",
                                       alternative = HIP_ALTERNATIVA)$estimate, digits=DECIMAL_PLACES), silent=TRUE)[1]
    matriz[3,1] <- i18n()$t("CI95%")
    LINF <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "pearson",
                                alternative = HIP_ALTERNATIVA)[9]$conf.int[1], digits=DECIMAL_PLACES), silent = TRUE)
    LSUP <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "pearson",
                                alternative = HIP_ALTERNATIVA)[9]$conf.int[2], digits=DECIMAL_PLACES), silent = TRUE)
    matriz[3,2] <- try( paste0("[",LINF,";",LSUP,"]"), silent=TRUE)[1]
    matriz
    
  })
  
  
  output$testes_de_correlacaospearman <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_CORR_TST
    VARIAVELX <- input$COR_TST_X
    VARIAVELY <- input$COR_TST_Y
    BaseEmR_T <- data.frame(BaseEmR())
    HIP_ALTERNATIVA <- input$CORR_ALTERNATIVE_HIP
    
    xx <- BaseEmR_T[,VARIAVELX]
    yy <- BaseEmR_T[,VARIAVELY]
    
    xxyy <- cbind(xx,yy)
    matriz <- matrix("", 2, 2)
    colnames(matriz) <-  c(i18n()$t("Parameter"), i18n()$t("Value"))
    matriz[1,1] <- i18n()$t("p-value")
    matriz[1,2] <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "spearman",
                                       alternative = HIP_ALTERNATIVA)$p.value, digits=DECIMAL_PLACES), silent=TRUE)[1]
    matriz[1,2] <- if (!is.na(as.numeric(matriz[1,2]))) { if(as.numeric(matriz[1,2])<0.05) {paste0(matriz[1,2],"*")}else{matriz[1,2]} }else{matriz[1,2]}
    
    matriz[2,1] <- i18n()$t("correlation")
    matriz[2,2] <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "spearman",
                                       alternative = HIP_ALTERNATIVA)$estimate, digits=DECIMAL_PLACES), silent=TRUE)[1]
    matriz
    
  })
  
  
  output$testes_de_correlacaokendall <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_CORR_TST
    VARIAVELX <- input$COR_TST_X
    VARIAVELY <- input$COR_TST_Y
    BaseEmR_T <- data.frame(BaseEmR())
    HIP_ALTERNATIVA <- input$CORR_ALTERNATIVE_HIP
    
    xx <- BaseEmR_T[,VARIAVELX]
    yy <- BaseEmR_T[,VARIAVELY]
    
    xxyy <- cbind(xx,yy)
    matriz <- matrix("", 2, 2)
    colnames(matriz) <- c(i18n()$t("Parameter"), i18n()$t("Value"))
    matriz[1,1] <- i18n()$t("p-value")
    matriz[1,2] <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "kendall",
                                       alternative = HIP_ALTERNATIVA)$p.value, digits=DECIMAL_PLACES), silent=TRUE)[1]
    matriz[1,2] <- if (!is.na(as.numeric(matriz[1,2]))) { if(as.numeric(matriz[1,2])<0.05) {paste0(matriz[1,2],"*")}else{matriz[1,2]} }else{matriz[1,2]}
    
    matriz[2,1] <- i18n()$t("correlation")
    matriz[2,2] <- try(round(cor.test( xxyy[,1],   xxyy[,2], method = "kendall",
                                       alternative = HIP_ALTERNATIVA)$estimate, digits=DECIMAL_PLACES), silent=TRUE)[1]
    matriz
    
  })
  
  
  output$table_fisher_test <- renderTable({
    DECIMAL_PLACES <- input$DCMP_FISHER_PERCENT 
    VARIAVEL_LINHA <- input$FISHER_TST_LIN 
    VARIAVEL_COLUNA <- input$FISHER_TST_COL
    BaseEmR_T <- data.frame(BaseEmR()) 
    ALTERNATIVE <- input$FISHER_ALTERNATIVE_HIP
    
    #NESSE IN?CIO EU CRIO A MATRIZ DE TESTES, POIS O COMANDO TABLE TAVA DANDO PT
    LIN <- as.matrix(BaseEmR_T[,VARIAVEL_LINHA])
    COL <- as.matrix(BaseEmR_T[,VARIAVEL_COLUNA])
    LINCOL <- as.matrix(cbind(LIN, COL))
    colnames(LINCOL) <- c("linha","coluna")
    
    LINHAS <- sort(unique(LINCOL[,1])[!is.na(unique(LINCOL[,1]))])
    COLUNAS <- sort(unique(LINCOL[,2])[!is.na(unique(LINCOL[,2]))])
    
    matriz_teste <- matrix(0, length(LINHAS), length(COLUNAS))
    
    for (i in 1:length(LINHAS)) {
      for (j in 1:length(COLUNAS)) {
        matriz_teste[i,j] <- length(subset(data.frame(LINCOL) , linha == LINHAS[i] &  coluna == COLUNAS[j])[,1])
      }
    }
    colnames(matriz_teste) <- COLUNAS
    rownames(matriz_teste) <- LINHAS
    
    
    #FORMATA??O DA MATRIZ DE IMPRESS?O
    matriz_impressao_NUM <- matriz_teste
    TOTAL <- 0
    for (i in 1:length(rownames(matriz_impressao_NUM))) { TOTAL[i]<- sum(matriz_impressao_NUM[i,]) }
    matriz_impressao_NUM <- cbind(matriz_impressao_NUM,TOTAL)
    TOTAL <- 0
    for (i in 1:length(colnames(matriz_impressao_NUM))) { TOTAL[i]<- sum(matriz_impressao_NUM[,i]) }
    matriz_impressao_NUM <- rbind(matriz_impressao_NUM,TOTAL)
    matriz_impressao_PERC <- matrix("", dim(matriz_impressao_NUM)[1], dim(matriz_impressao_NUM)[2])
    
    
    if (input$FISHER_TST_PERCENTAGE == "(%col)") {
      for (j in 1:length(colnames(matriz_impressao_NUM))) {
        for (i in 1:length(rownames(matriz_impressao_NUM))) {
          matriz_impressao_PERC[i,j] <- paste0( round(100*matriz_impressao_NUM[i,j]/sum(matriz_impressao_NUM[1:(length(matriz_impressao_NUM[,1])-1),j]), digits= DECIMAL_PLACES), "%")
        } }
      colnames(matriz_impressao_PERC) <- paste0(colnames(matriz_impressao_NUM),"(%col)")
    } else {
      
      for (j in 1:length(colnames(matriz_impressao_NUM))) {
        for (i in 1:length(rownames(matriz_impressao_NUM))) {
          matriz_impressao_PERC[i,j] <- paste0( round(100*matriz_impressao_NUM[i,j]/sum(matriz_impressao_NUM[i,1:(length(matriz_impressao_NUM[1,])-1)]), digits= DECIMAL_PLACES), "%")
        } }
      colnames(matriz_impressao_PERC) <- paste0(colnames(matriz_impressao_NUM),"(%lin)")
    }
    
    
    separador <- as.matrix(rep("|",length(matriz_impressao_NUM[,1])))
    colnames(separador)<- "|"
    matriz_impressao_FINAL <- cbind(matriz_impressao_NUM, separador,matriz_impressao_PERC)
    matriz_impressao_FINAL <- cbind(rownames(matriz_impressao_FINAL), matriz_impressao_FINAL)
    colnames(matriz_impressao_FINAL)[1] <- "Vs."
    
    matriz_impressao_FINAL
    
  })
  
  #################################################################################################################################################################################
  output$result_fisher_test <- renderTable({
    
    DECIMAL_PLACES <- input$DCMP_FISHER_TST 
    VARIAVEL_LINHA <- input$FISHER_TST_LIN 
    VARIAVEL_COLUNA <- input$FISHER_TST_COL
    BaseEmR_T <- data.frame(BaseEmR()) 
    ALTERNATIVE <- input$FISHER_ALTERNATIVE_HIP
    
    #NESSE IN?CIO EU CRIO A MATRIZ DE TESTES, POIS O COMANDO TABLE TAVA DANDO PT
    LIN <- as.matrix(BaseEmR_T[,VARIAVEL_LINHA])
    COL <- as.matrix(BaseEmR_T[,VARIAVEL_COLUNA])
    LINCOL <- as.matrix(cbind(LIN, COL))
    colnames(LINCOL) <- c("linha","coluna")
    
    LINHAS <- sort(unique(LINCOL[,1])[!is.na(unique(LINCOL[,1]))])
    COLUNAS <- sort(unique(LINCOL[,2])[!is.na(unique(LINCOL[,2]))])
    
    matriz_teste <- matrix(0, length(LINHAS), length(COLUNAS))
    
    for (i in 1:length(LINHAS)) {
      for (j in 1:length(COLUNAS)) {
        matriz_teste[i,j] <- length(subset(data.frame(LINCOL) , linha == LINHAS[i] &  coluna == COLUNAS[j])[,1])
      }
    }
    colnames(matriz_teste) <- COLUNAS
    rownames(matriz_teste) <- LINHAS
    #COMENTEI A MATRIZ PARA N?O IMPRIMIR: matriz_teste
    #matriz_teste
    
    #FORMATA??O DA MATRIZ DE IMPRESS?O
    matriz_impressao_NUM <- matriz_teste
    TOTAL <- 0
    for (i in 1:length(rownames(matriz_impressao_NUM))) { TOTAL[i]<- sum(matriz_impressao_NUM[i,]) }
    matriz_impressao_NUM <- cbind(matriz_impressao_NUM,TOTAL)
    TOTAL <- 0
    for (i in 1:length(colnames(matriz_impressao_NUM))) { TOTAL[i]<- sum(matriz_impressao_NUM[,i]) }
    matriz_impressao_NUM <- rbind(matriz_impressao_NUM,TOTAL)
    matriz_impressao_PERC <- matrix("", dim(matriz_impressao_NUM)[1], dim(matriz_impressao_NUM)[2])
    
    if (input$FISHER_TST_PERCENTAGE == "(%col)") {
      for (j in 1:length(colnames(matriz_impressao_NUM))) {
        for (i in 1:length(rownames(matriz_impressao_NUM))) {
          matriz_impressao_PERC[i,j] <- paste0( round(100*matriz_impressao_NUM[i,j]/sum(matriz_impressao_NUM[1:(length(matriz_impressao_NUM[,1])-1),j]), digits= DECIMAL_PLACES), "%")
        } }
      colnames(matriz_impressao_PERC) <- paste0(colnames(matriz_impressao_NUM),"(%col)")
    } else {
      
      for (j in 1:length(colnames(matriz_impressao_NUM))) {
        for (i in 1:length(rownames(matriz_impressao_NUM))) {
          matriz_impressao_PERC[i,j] <- paste0( round(100*matriz_impressao_NUM[i,j]/sum(matriz_impressao_NUM[i,1:(length(matriz_impressao_NUM[1,])-1)]), digits= DECIMAL_PLACES), "%")
        } }
      colnames(matriz_impressao_PERC) <- paste0(colnames(matriz_impressao_NUM),"(%lin)")
    }
    
    separador <- as.matrix(rep("|",length(matriz_impressao_NUM[,1])))
    colnames(separador)<- "|"
    matriz_impressao_FINAL <- cbind(matriz_impressao_NUM, separador,matriz_impressao_PERC)
    matriz_impressao_FINAL <- cbind(rownames(matriz_impressao_FINAL), matriz_impressao_FINAL)
    colnames(matriz_impressao_FINAL)[1] <- "Vs"
    #COMENTEI A MATRIZ PARA N?O IMPRIMIR: matriz_impressao_FINAL
    #matriz_impressao_FINAL
    RESULTADO_DO_TESTE <- fisher.test(matriz_teste, alternative = ALTERNATIVE, simulate.p.value = TRUE, B = 20000)
    #RESULTADO_DO_TESTE
    P <- round(RESULTADO_DO_TESTE[1]$p.value, digits = DECIMAL_PLACES)
    
    E_2_POR_2 <- FALSE
    if ( (dim(matriz_teste)[1] == 2) && (dim(matriz_teste)[2] == 2) ) { E_2_POR_2 <- TRUE }
    if (E_2_POR_2) {
      OR <- round(RESULTADO_DO_TESTE[3]$estimate, digits = DECIMAL_PLACES)
      LINF_OR <- round(RESULTADO_DO_TESTE[2]$conf.int[1], digits = DECIMAL_PLACES)
      LSUP_OR <- round(RESULTADO_DO_TESTE[2]$conf.int[2], digits = DECIMAL_PLACES)
    } else {
      OR <- i18n()$t("OR is estimated only in 2x2 tables")
    }
    
    IMPRESSAO_DO_TESTE <- matrix("", 3, 3)
    IMPRESSAO_DO_TESTE[1,1] <- i18n()$t("p-value")
    IMPRESSAO_DO_TESTE[2,1] <- i18n()$t("Odds Ratio(OR)")
    IMPRESSAO_DO_TESTE[3,1] <- i18n()$t("CI95%(OR)")
    
    IMPRESSAO_DO_TESTE[1,2] <- P
    IMPRESSAO_DO_TESTE[2,2] <- OR
    IMPRESSAO_DO_TESTE[3,2] <- if (E_2_POR_2) {paste0("[",LINF_OR,";",LSUP_OR,"]")} else {OR}
    
    
    IMPRESSAO_DO_TESTE[1,3] <- if (!is.na(as.numeric(IMPRESSAO_DO_TESTE[1,2]))) { 
      if(as.numeric(IMPRESSAO_DO_TESTE[1,2]) < 0.05) {i18n()$t("there is an association between variables")}else{
        i18n()$t("it was not possible to prove any association between the variables")} }else{""}
    IMPRESSAO_DO_TESTE[2,3] <- if (E_2_POR_2) { if( ((LINF_OR-1)>0) || ((1-LSUP_OR)>0) ) {
      i18n()$t("Odds Ratio is significant")} else {
        i18n()$t("Odds Ratio was NOT significant")} } else {""}
    IMPRESSAO_DO_TESTE[3,3] <- if (E_2_POR_2) { if( ((LINF_OR-1)>0) || ((1-LSUP_OR)>0) ) {
      i18n()$t("the unit is not included in the range")} else {
        i18n()$t("the range includes the unit")} } else {""}

    IMPRESSAO_DO_TESTE[1,2] <- if (!is.na(as.numeric(IMPRESSAO_DO_TESTE[1,2]))) { 
      if(as.numeric(IMPRESSAO_DO_TESTE[1,2]) < 0.05) {paste0(IMPRESSAO_DO_TESTE[1,2],"*")}else{IMPRESSAO_DO_TESTE[1,2]} }else{IMPRESSAO_DO_TESTE[1,2]}

    
    colnames(IMPRESSAO_DO_TESTE) <- c(i18n()$t("Parameter"), i18n()$t("Value"), i18n()$t("Conclusion"))

    IMPRESSAO_DO_TESTE
    
    
  })
  
  ### INICIO normalidade Histograma
  output$testes_normalidade <- renderTable({
    teste_nor <- function(tn) {
      #TRANSFORMAMOS TN EM DATAFRAME E RESOLVEMOS O PROBLEMA DA MUDAN?A DE NOMES  
      tn = data.frame(tn)
      colnames(tn) <- colnames(BaseEmR())
      VA_NUMERICA <- match(input$analise , names(BaseEmR()))
      VA_CATEGORICA <- match(input$spl , names(BaseEmR()))
      DECIMAL_PLACES <- input$DCMP_HIST_NUM
      matriz <- matrix("",3,3)
      #matriz <- data.frame("", "Parameter", "Value")
      matriz[1,1]<-i18n()$t("sample size")
      tamanho <- 0 
      for (i in 1:length(tn[,VA_NUMERICA])) if ( !is.na(tn[,VA_NUMERICA][i]) && !is.na(tn[,VA_CATEGORICA][i]) ){ tamanho <- tamanho + 1 }
      matriz[1,2]<-tamanho
      matriz[2,1]<-i18n()$t("p-value")
      matriz[2,2]<-try(round(shapiro.test(tn[,VA_NUMERICA])$p , digits=DECIMAL_PLACES), silent=TRUE)[1]
      matriz[2,3]<-if ( !is.na(as.numeric(matriz[2,2])) ) { 
                      if (as.numeric(matriz[2,2])<0.05) { 
                        i18n()$t("the data is NOT normally distributed") } else {
                          i18n()$t("the data follows a normal distribution") } } else {""}
        
      matriz[2,2]<- if ( !is.na(as.numeric(matriz[2,2])) ) { if (as.numeric(matriz[2,2])<0.05) {paste0(matriz[2,2],"*") } else {matriz[2,2]} }
      matriz[3,1]<-i18n()$t("statistic(W)")
      matriz[3,2]<-try(round(shapiro.test(tn[,VA_NUMERICA])$statistic , digits=DECIMAL_PLACES), silent=TRUE)[1]
      matriz[3,3]<-""
      resultado <- list("Shapiro-Wilk Normality Test"= matriz)
      resultado
    }
    
    splitted_list <- split(BaseEmR(), BaseEmR()[input$spl])
    resultado_looping <- lapply(splitted_list, teste_nor)
    
    saidas_impressas <- matrix("", length(names(resultado_looping))*4, 4 )
    colnames(saidas_impressas) <- c(i18n()$t("Variable"), i18n()$t("Parameter"), i18n()$t("Value"), i18n()$t("Conclusion"))
    
    for (i in 1:length(names(resultado_looping) )  )  {
      saidas_impressas[i+ 3*(i-1),1] <- names(resultado_looping)[i]
      saidas_impressas[i+ 3*(i-1),2] <- ""
      saidas_impressas[i+ 3*(i-1),3] <- ""
      saidas_impressas[i+ 3*(i-1),4] <- ""
      saidas_impressas[i+ 3*(i-1)+1,1] <- ""
      saidas_impressas[i+ 3*(i-1)+1,2] <- as.matrix(data.frame(resultado_looping[i]))[1,1]
      saidas_impressas[i+ 3*(i-1)+1,3] <- as.matrix(data.frame(resultado_looping[i]))[1,2]
      saidas_impressas[i+ 3*(i-1)+1,4] <- ""
      saidas_impressas[i+ 3*(i-1)+2,1] <- ""
      saidas_impressas[i+ 3*(i-1)+2,2] <- as.matrix(data.frame(resultado_looping[i]))[2,1]
      saidas_impressas[i+ 3*(i-1)+2,3] <- as.matrix(data.frame(resultado_looping[i]))[2,2]
      saidas_impressas[i+ 3*(i-1)+2,4] <- as.matrix(data.frame(resultado_looping[i]))[2,3]
      saidas_impressas[i+ 3*(i-1)+3,1] <- ""
      saidas_impressas[i+ 3*(i-1)+3,2] <- as.matrix(data.frame(resultado_looping[i]))[3,1]
      saidas_impressas[i+ 3*(i-1)+3,3] <- as.matrix(data.frame(resultado_looping[i]))[3,2]
      saidas_impressas[i+ 3*(i-1)+3,4] <- ""
    }
    saidas_impressas
  })
  
  
  ### INICIO normalidade BoxPlot
  output$testes_normalidade_BXP <- renderTable({
    
    #TRANSFORMA A BASE EM DATA.FRAME
    BaseEmR_T <- data.frame(BaseEmR())
    VA_CAT_SPL_BXP0 <- input$spl_BXP0
    VA_CAT_SPL_BXP <- input$spl_BXP
    VA_NUM_ANALISE_BXP <- input$analise_BXP
    DECIMAL_PLACES <- input$DCMP_BXP_NUM
    
    #FILTRA A BASE DE DADOS E S? DEIXA OS PARES V?LIDOS
    FICAR <- matrix(TRUE, 1, length(BaseEmR_T[,1]))
    for (i in 1:dim(BaseEmR_T)[1] ) { if ( is.na(BaseEmR_T[,VA_CAT_SPL_BXP0])[i] ||  is.na(BaseEmR_T[,VA_CAT_SPL_BXP])[i] )  { FICAR[1,i] <- FALSE } }
    BaseEmR_T <- BaseEmR_T[FICAR,]
    splitted_list_BXP <- split(BaseEmR_T, list(BaseEmR_T[,VA_CAT_SPL_BXP0], BaseEmR_T[,VA_CAT_SPL_BXP]))
    
    LIMITE_LOOPING <- (length(levels(BaseEmR_T[,VA_CAT_SPL_BXP0]))*length(levels(BaseEmR_T[,VA_CAT_SPL_BXP])))
    matriz <- matrix("", LIMITE_LOOPING*4, 4)
    
    colnames(matriz) <- c(i18n()$t("Variable"), i18n()$t("Parameter"), i18n()$t("Value"), i18n()$t("Conclusion") )
    
    for  (i in 1:(LIMITE_LOOPING)) {
    matriz[i+ 3*(i-1),1]<-names(splitted_list_BXP)[i]
      matriz[1+i+ 3*(i-1),2]<-i18n()$t("sample size")
      #CONTA OS VALORES EXCLUINDO OS NA
      matriz[1+i+ 3*(i-1),3]<-length(splitted_list_BXP[[i]][,VA_NUM_ANALISE_BXP][!is.na(splitted_list_BXP[[i]][,VA_NUM_ANALISE_BXP])]) 
      matriz[1+i+ 3*(i-1),4]<-""

      #CALCULA O TESTE DE SHAPIRO-WILK
      PROBLEMA <- TRUE
      if ( ( as.numeric(matriz[1+i+ 3*(i-1),3]) > 3) &&  ( as.numeric(matriz[1+i+ 3*(i-1),3]) < 5000))  {
        RESULTADO = shapiro.test(
          splitted_list_BXP[[i]][,VA_NUM_ANALISE_BXP][!is.na(splitted_list_BXP[[i]][,VA_NUM_ANALISE_BXP])]
        )
        PROBLEMA <- FALSE
      } else { 
        RESULTADO = i18n()$t("The Shapiro-Wilk test was not performed. The sample must be between 3 and 5x1000")
      }
      matriz[2+i+ 3*(i-1),2]<-i18n()$t("p-value")
      matriz[2+i+ 3*(i-1),3]<-if(PROBLEMA == TRUE) { RESULTADO } else { round(RESULTADO[2]$p.value, digits=DECIMAL_PLACES) }
      matriz[3+i+ 3*(i-1),2]<-i18n()$t("statistic(W)")
      matriz[3+i+ 3*(i-1),3]<-if(PROBLEMA == TRUE) { RESULTADO } else { round(RESULTADO[1]$statistic, digits=DECIMAL_PLACES)  }

      matriz[2+i+ 3*(i-1),4]<- if(PROBLEMA == FALSE) {
        if ( !is.na(as.numeric(matriz[2+i+ 3*(i-1),3])) ) { 
          if (as.numeric(matriz[2+i+ 3*(i-1),3]) < 0.05) { 
            i18n()$t("the data is NOT normally distributed") } else {
              i18n()$t("the data follows a normal distribution") } } } else {""}
      matriz[2+i+ 3*(i-1),3]<-if (PROBLEMA == FALSE) { if (as.numeric(matriz[2+i+ 3*(i-1),3])<0.05) {paste0(matriz[2+i+ 3*(i-1),3],"*")} else{matriz[2+i+ 3*(i-1),3]} } else {matriz[2+i+ 3*(i-1),3]}
    }
    matriz
  })
  ### FINAL normalidade BoxPlot
  
  ### INICIO normalidade scatterplot
  output$testes_normalidade_SCTP <- renderTable({
    
    #TRANSFORMA A BASE EM DATA.FRAME
    BaseEmR_T <- data.frame(BaseEmR())
    VA_CAT_COLOR_BXP <- input$analise_SCTP3
    VA_NUM_ANALISE_SCTP1 <- input$analise_SCTP1
    VA_NUM_ANALISE_SCTP2 <- input$analise_SCTP2
    DECIMAL_PLACES <- input$DCMP_SCTP_NUM
    
    
    splitted_list_SCTP <- split(BaseEmR_T, BaseEmR_T[,VA_CAT_COLOR_BXP])
    
    head(splitted_list_SCTP[[1]])
    tn_SCTP <- data.frame(splitted_list_SCTP[[1]])
    head(tn_SCTP)
    
    teste_nor_SCTP <- function(tn_SCTP) {
      tn_SCTP <- data.frame(tn_SCTP)
      
      matriz0 <- data.frame("Variable"=c(0))
      matriz0[1:3,1]<- VA_NUM_ANALISE_SCTP1
      matriz1 <- data.frame("Parameter"=c(0),"Value"=c(0))
      matriz1[1,1]<-i18n()$t("sample size")
      matriz1[1,2]<-length(tn_SCTP[,VA_NUM_ANALISE_SCTP1][!is.na(tn_SCTP[,VA_NUM_ANALISE_SCTP1])])
      
      PROBLEMA <- TRUE
      if ( (matriz1[1,2] > 3) &&  (matriz1[1,2] < 5000) )  {
        RESULTADO = try(shapiro.test(tn_SCTP[,VA_NUM_ANALISE_SCTP1]), silent=TRUE) 
        PROBLEMA <- FALSE
      } else { RESULTADO = i18n()$t("The sample must be between 3 and 5x1000") }
      
      matriz1[2,1]<-i18n()$t("p-value")
      matriz1[2,2]<-if(PROBLEMA == TRUE) { RESULTADO } else { round(RESULTADO$p.value, digits=DECIMAL_PLACES) }
      matriz1[2,2]<-if(PROBLEMA == FALSE) { if (as.numeric(matriz1[2,2])<0.05) { paste0(matriz1[2,2],"*") } else {matriz1[2,2]} } else {matriz1[2,2]} 
      matriz1[3,1]<-i18n()$t("statistic(W)")
      matriz1[3,2]<-if(PROBLEMA == TRUE) { RESULTADO } else { round(RESULTADO$statistic, digits=DECIMAL_PLACES) }
      
      matriz2 <- data.frame("|"=c(0))
      matriz2[1:3,1]<-"|"
      
      matriz3 <- data.frame("Variable"=c(0))
      matriz3[1:3,1]<- VA_NUM_ANALISE_SCTP2
      matriz4 <- data.frame("Parameter"=c(0),"Value"=c(0))
      matriz4[1,1]<-i18n()$t("sample size")
      matriz4[1,2]<-length(tn_SCTP[,VA_NUM_ANALISE_SCTP2][!is.na(tn_SCTP[,VA_NUM_ANALISE_SCTP2])])
      
      PROBLEMA <- TRUE
      if ( (matriz4[1,2] > 3) &&  (matriz4[1,2] < 5000) )  {
        RESULTADO = try(shapiro.test(tn_SCTP[,VA_NUM_ANALISE_SCTP2]), silent=TRUE) 
        PROBLEMA <- FALSE
      } else { RESULTADO = i18n()$t("The sample must be between 3 and 5x1000") }
      
      matriz4[2,1]<-i18n()$t("p-value")
      matriz4[2,2]<-if(PROBLEMA == TRUE) { RESULTADO } else { round(RESULTADO$p.value, digits=DECIMAL_PLACES) }
      matriz4[2,2]<-if(PROBLEMA == FALSE) { if (as.numeric(matriz4[2,2])<0.05) { paste0(matriz4[2,2],"*") } else {matriz4[2,2]} } else {matriz4[2,2]} 
      matriz4[3,1]<-i18n()$t("statistic(W)")
      matriz4[3,2]<-if(PROBLEMA == TRUE) { RESULTADO } else { round(RESULTADO$statistic, digits=DECIMAL_PLACES) }
      
      #CORRIGINDO BUG DE ARREDONDAMENTO
      matriz1[1,2] <- as.character(matriz1[1,2])
      matriz4[1,2] <- as.character(matriz4[1,2])
      
      matriz <- cbind(matriz0, matriz1, matriz2, matriz3, matriz4 )
      matriz
      
    }
    
    RESULTADOS <- lapply(splitted_list_SCTP, teste_nor_SCTP)
    IMPRESSAO <- matrix("", length(names(RESULTADOS))*4, 8)
    colnames(IMPRESSAO) <-c(i18n()$t("Splity by"),paste0(i18n()$t("Variable"),"(X)"),i18n()$t("Parameter"),i18n()$t("Value"),
                            "|",
                            paste0(i18n()$t("Variable"),"(Y)"),i18n()$t("Parameter"),i18n()$t("Value"))
    for (i in 1: length(names(RESULTADOS))) {
      IMPRESSAO[1+(i-1)*4,1] <- names(RESULTADOS)[i]
      IMPRESSAO[(1+(1+(i-1)*4)):(3+(1+(i-1)*4)),2:8] <- as.matrix(RESULTADOS[[i]])
    }
    IMPRESSAO
  })
  
  ###################################################################################### INICIO SA?DAS ROC CURVE
  
  output$testes_da_ROC_Curve <- renderTable({
    BaseEmR_T <- data.frame(BaseEmR())[complete.cases(data.frame(BaseEmR())[input$ROC_DESFECHO]),c(1:length(data.frame(BaseEmR())[1,]))]
    BaseEmR_T <- BaseEmR_T[complete.cases(BaseEmR_T[input$ROC_VA]),c(1:length(BaseEmR_T[1,]))]
    var_cat_dic_ins <- input$ROC_DESFECHO
    var_num_analisada <- input$ROC_VA
    vetor_variavel_analisada <- data.frame(BaseEmR_T[,var_num_analisada])[,1]
    nivel_escolhido <- input$ROC_DESFECHO_LVL
    desfecho <- matrix(0,length(vetor_variavel_analisada),1)
    for (i in 1:length(BaseEmR_T[,var_num_analisada])) { 
      if(  nivel_escolhido == BaseEmR_T[var_cat_dic_ins][,1][i]  ) { 
        desfecho[i,1] <- 1} else { desfecho[i,1] <- 0} }
    desfecho <- data.frame(desfecho)
    names(desfecho) <- "Outcome"
    vetor_variavel_analisada <- data.frame(vetor_variavel_analisada)
    names(vetor_variavel_analisada) <- var_num_analisada
    BaseEmR_ROC <- cbind(desfecho , vetor_variavel_analisada)
    BaseEmR_ROC <- BaseEmR_ROC[complete.cases(BaseEmR_ROC[,1:length(BaseEmR_ROC[1,])]),]
    roc1 <- roc(BaseEmR_ROC[,1], BaseEmR_ROC[,2])
    rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp", "npv", 
              "ppv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv")
    saidas <- ci.coords(roc1, x="best", input = "threshold", ret=rets, best.policy = "random", boot.n = input$NUMBER_ROC_BOOTSTRAP_REP)
    #APROVEITANDO A VARI?VEL GLOBAL J? CALCULADA COM O VALOR DO AUC
    #AUC <- ci.auc(smooth(roc1, method="density"), boot.n = input$NUMBER_ROC_BOOTSTRAP_REP)
    AUC <- AUC_10000
    statistics_ROC <- matrix("", (length(rets)+1)*3,3)
    colnames(statistics_ROC) <- c(i18n()$t("Parameter"), i18n()$t("Measure"), i18n()$t("Value"))
    i=0
    i=i+1
    statistics_ROC[i,1] = "Area Under the Curve (AUC)"
    statistics_ROC[i+1,2] = i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(AUC[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(AUC[1], digits = input$DCMP_ROC_CURVE),";",round(AUC[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Cutoff [max(Sensitivity and Specificity)]"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] =  round(saidas$"threshold"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"threshold"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"threshold"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Sensitivity"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] =  round(saidas$"sensitivity"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"sensitivity"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"sensitivity"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Specificity"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] =round(saidas$"specificity"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"specificity"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"specificity"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Accuracy"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"accuracy"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"accuracy"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"accuracy"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "True negative count (TN)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"tn"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"tn"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"tn"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "True positive count (TP)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"tp"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"tp"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"tp"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "False negative count (FN)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"fn"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"fn"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"fn"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "False positive count (FP)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"fp"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"fp"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"fp"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Negative Predictive Value (NPV)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"npv"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"npv"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"npv"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Positive Predictive Value (PPV)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"ppv"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"ppv"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"ppv"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "False Negative Rate (1-Sensitivity)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"1-sensitivity"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"1-sensitivity"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"1-sensitivity"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "False Positive Rate (1-Specificity)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] =round(saidas$"1-specificity"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"1-specificity"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"1-specificity"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Complementary Accuracy (1-Accuracy)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"1-accuracy"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] =  i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"1-accuracy"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"1-accuracy"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "Complementary NPV (1-NPV)"
    statistics_ROC[i+1,2] = i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"1-npv"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[", round(saidas$"1-npv"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"1-npv"[3], digits = input$DCMP_ROC_CURVE),"]")
    i=i+3
    statistics_ROC[i,1] = "False Discovery Rate (1-PPV)"
    statistics_ROC[i+1,2] =  i18n()$t("Estimate")
    statistics_ROC[i+1,3] = round(saidas$"1-ppv"[2], digits = input$DCMP_ROC_CURVE)
    statistics_ROC[i+2,2] = i18n()$t("CI95%")
    statistics_ROC[i+2,3] = paste0("[",round(saidas$"1-ppv"[1], digits = input$DCMP_ROC_CURVE),";",round(saidas$"1-ppv"[3], digits = input$DCMP_ROC_CURVE),"]")
    
    
    statistics_ROC
    
  })
  
  
  output$summ_NUMERICAL <- renderTable({
    BaseEmR_T <- data.frame(BaseEmR())
    TotalNumerico <- 0
    VetorClasse <- 0
    BaseEmR_T <- BaseEmR_T[,2:length(BaseEmR_T[1,])]
    
    var_num <- 0
    var_cat <- 0
    
    for (i in 1:length(colnames(BaseEmR_T))) {
      if ( is.numeric(BaseEmR_T[,i]) ) {
        BaseEmR_T[,i]<- as.numeric(BaseEmR_T[,i]) 
        var_num[i] <- colnames(BaseEmR_T)[i]
        var_cat[i] <- 0
      } else {
        BaseEmR_T[,i]<- as.factor(BaseEmR_T[,i]) 
        var_num[i] <- 0                           
        var_cat[i] <- colnames(BaseEmR_T)[i]
      } 
    }
    
    BaseEmR_T_NUM <- BaseEmR_T
    BaseEmR_T_CAT <- BaseEmR_T
    
    var_num <- var_num[var_num !=0]
    var_cat <- var_cat[var_cat !=0]
    
    BaseEmR_T_NUM <- BaseEmR_T[,var_num]
    BaseEmR_T_CAT <- BaseEmR_T[,var_cat]
    
    #CRIA VARI?VEIS ALEAT?RIAS TEMPOR?RIAS PARA N?O TRAVAR O SISTEMA
    RANDOM_NUMBER_INTEGER_1_681099 <- as.numeric(round(runif(dim(BaseEmR_T_NUM)[1],0,100), digits = 0))
    RANDOM_NUMBER_INTEGER_2_681099 <- as.numeric(round(runif(dim(BaseEmR_T_NUM)[1],0,100), digits = 0))
    RANDOM_NUMBER_INTEGER_3_681099 <- as.numeric(round(runif(dim(BaseEmR_T_NUM)[1],0,100), digits = 0))
    
    #AGREGA ESSAS VARI?VEIS JUNTO AO VETOR NUM?RICO ANALISADO, MAS ESSAS VARI?VEIS SER?O DESCARTADAS NO FINAL
    BaseEmR_T_NUM <- cbind(BaseEmR_T_NUM, RANDOM_NUMBER_INTEGER_1_681099, RANDOM_NUMBER_INTEGER_2_681099, RANDOM_NUMBER_INTEGER_3_681099)
    
    
    
    #DEFINE AS BASES NUM?RICAS E CATEG?RICAS (PARA ESTRATIFICA??O)
    DivididoPorNiveis <- split(BaseEmR_T_NUM, BaseEmR_T[input$spl_SUMM1] )
    
    Descritivo_Numerico = function(BaseNumericaUtilizada) {
      RESULTADOS_GERAIS <- data.frame("Parameter"=c(0))
      RESULTADOS_GERAIS[1,1]="(n):"
      RESULTADOS_GERAIS[2,1]="mean:"
      RESULTADOS_GERAIS[3,1]="sd:"
      RESULTADOS_GERAIS[4,1]="max:"
      RESULTADOS_GERAIS[5,1]="min:"
      RESULTADOS_GERAIS[6,1]="range:"
      RESULTADOS_GERAIS[7,1]="median:"
      RESULTADOS_GERAIS[8,1]="q1:"
      RESULTADOS_GERAIS[9,1]="q3:"
      RESULTADOS_GERAIS[10,1]="IQR:"
      for (i in 1:length(names(BaseNumericaUtilizada))){
        RESULTADOS_GERAIS[1,1+i]=length(BaseNumericaUtilizada[,i][!is.na(BaseNumericaUtilizada[,i])])
        RESULTADOS_GERAIS[2,1+i]=mean(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)
        RESULTADOS_GERAIS[3,1+i]=sd(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)
        RESULTADOS_GERAIS[4,1+i]=max(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)
        RESULTADOS_GERAIS[5,1+i]=min(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)
        RESULTADOS_GERAIS[6,1+i]=range(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)[2] - range(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)[1]
        RESULTADOS_GERAIS[7,1+i]=median(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)
        RESULTADOS_GERAIS[8,1+i]=quantile(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)[2]
        RESULTADOS_GERAIS[9,1+i]=quantile(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)[4]
        RESULTADOS_GERAIS[10,1+i]=IQR(as.numeric(BaseNumericaUtilizada[,i]), na.rm=TRUE)
        names(RESULTADOS_GERAIS)[i+1] <- names(BaseNumericaUtilizada)[i]
      } 
      RESULTADOS_GERAIS
    }
    
    #DEFINE O N?VEL DE ESTRATIFICA??O
    #DivididoPorNiveis
    Estatistica_por_niveis <- lapply(DivididoPorNiveis, Descritivo_Numerico)
    Estatistica_por_niveis[["TotalGeral"]] <- Descritivo_Numerico(BaseEmR_T_NUM)
    Estatisticas_Gerais <- Estatistica_por_niveis
    
    #DEFINE MATRIZ DE RESULTADOS
    Estatisticas_Gerais_IMPRESSO <- matrix(' ', (length(names(BaseEmR_T_NUM))+10*length(names(BaseEmR_T_NUM))), (length(names(Estatisticas_Gerais)))+1)
    Estatisticas_Gerais_IMPRESSO <- data.frame(Estatisticas_Gerais_IMPRESSO)
    
    #CABE?ALHOS
    for (i in 3:(length(names(Estatisticas_Gerais))+1)) {
      #FORMATA??O DA IMPRESS?O
      names(Estatisticas_Gerais_IMPRESSO)[i] <-   paste0(names(Estatisticas_Gerais)[i-2])
      
    }
    
    Estatisticas_Gerais_IMPRESSO$"TOTAL" <- Estatisticas_Gerais_IMPRESSO[1,1]
    colnames(Estatisticas_Gerais_IMPRESSO)[length(colnames(Estatisticas_Gerais_IMPRESSO))] <- i18n()$t("Total")
    names(Estatisticas_Gerais_IMPRESSO)[1] <- i18n()$t("Variable")
    names(Estatisticas_Gerais_IMPRESSO)[2] <- i18n()$t("Measure")
    
    i = 1
    j = 1
    ii = 1
    jj = 0
    Aloca_variavel <- seq(from = 1, to = length(Estatisticas_Gerais_IMPRESSO[,1]), by = 11)
    
    Estatisticas_Gerais_IMPRESSO <- as.matrix(Estatisticas_Gerais_IMPRESSO)
    #Estatisticas_Gerais_IMPRESSO <- as.data.frame(Estatisticas_Gerais_IMPRESSO)
    while( i <= length(Estatisticas_Gerais_IMPRESSO[,1])) {
      
      if (i  %in%  Aloca_variavel) {
        Estatisticas_Gerais_IMPRESSO[i,1] <- colnames(Estatisticas_Gerais[[1]])[ii+1]
        ii <- ii+1
        i <- i+1
        jj = 1
      } else {
        Estatisticas_Gerais_IMPRESSO[i,2] <- Estatisticas_Gerais[[1]][jj,1]
        jj = jj+1
        i<-i+1
      } }
    
    #PREENCHENDO TODOS OS VALORES
    k=1
    j=3  
    while(k <= (length(names(Estatisticas_Gerais))-1)) {
      jj=2
      while(jj <= (length(Estatisticas_Gerais[[1]][1,]))) {
        ii=2
        while (ii <= 11) {
          Estatisticas_Gerais_IMPRESSO[ ii + (jj-2)*11 , j] <- round(Estatisticas_Gerais[[k]][ii-1,jj], digits=input$DCMP_DESC_NUM)
          ii <- ii + 1
        }
        jj <- jj+1
      }
      k <- k+1
      j <- j+1
    }
    
    #PREENCHENDO TODOS OS TOTAIS
    k=length(names(Estatisticas_Gerais))
    j=length(Estatisticas_Gerais_IMPRESSO[1,])
    while(k == (length(names(Estatisticas_Gerais)))) {
      jj=2
      while(jj <= (length(Estatisticas_Gerais[[1]][1,]))) {
        ii=2
        while (ii <= 11) {
          Estatisticas_Gerais_IMPRESSO[ ii + (jj-2)*11 , j] <- round(Estatisticas_Gerais[[k]][ii-1,jj], digits=input$DCMP_DESC_NUM)
          ii <- ii + 1
        }
        jj <- jj+1
      }
      k <- k+1
      j <- j+1
    }
    
    #ORDENA AS COLUNAS
    duas_primeiras_colunas <- colnames(Estatisticas_Gerais_IMPRESSO[,c(1,2)])
    ultima_coluna <- colnames(Estatisticas_Gerais_IMPRESSO)[length(Estatisticas_Gerais_IMPRESSO[1,])]
    col.order <- colnames(Estatisticas_Gerais_IMPRESSO)
    outras_colunas <- sort(col.order[3:(length(col.order)-1)])
    Estatisticas_Gerais_IMPRESSO <- Estatisticas_Gerais_IMPRESSO[,c(duas_primeiras_colunas,outras_colunas, ultima_coluna)]
    
    #REMOVENDO OS NOMES N?O ESCOLHIDOS
    REMOVE_NOMES <- matrix(0,length(Estatisticas_Gerais_IMPRESSO[,1]),1)
    for (i in 1:length(Estatisticas_Gerais_IMPRESSO[,1])) {
      if (Estatisticas_Gerais_IMPRESSO[i,1] == " ") {
        REMOVE_NOMES[i,1] <- REMOVE_NOMES[i-1,1]
      } else { REMOVE_NOMES[i,1] <- Estatisticas_Gerais_IMPRESSO[i,1] }
    }
    
    VETOR_ESCOLHIDO <- input$VAR_NUM_DESC_NAMES
    
    #FIZ ESSE IF SOMENTE PARA GARANTIR DE N?O APARECER UMA TELA DE ERRO
    if ( (length(VETOR_ESCOLHIDO) == 0 ) ||   ( length(input$VAR_NUM_DESC_STATS) == 0 )  ) {
      
      ARMAZENA_NOMES_TEMPORARIO <- colnames(Estatisticas_Gerais_IMPRESSO)
      Estatisticas_Gerais_IMPRESSO <- matrix(" ", 1, length(Estatisticas_Gerais_IMPRESSO[1,]))
      colnames(Estatisticas_Gerais_IMPRESSO) <- ARMAZENA_NOMES_TEMPORARIO
    } else { 
      
      Estatisticas_Gerais_IMPRESSO <- as.data.frame(Estatisticas_Gerais_IMPRESSO)
      Estatisticas_Gerais_IMPRESSO <- Estatisticas_Gerais_IMPRESSO[REMOVE_NOMES[,1] %in% VETOR_ESCOLHIDO ,]
      Estatisticas_Gerais_IMPRESSO <- as.matrix(Estatisticas_Gerais_IMPRESSO)
      
      #REMOVENDO ESTAT?STICAS N?O SOLICITADAS
      REMOVE_STATS <- as.vector(input$VAR_NUM_DESC_STATS)
      REFERENCIA_P_REMOVER <- matrix(FALSE, length(Estatisticas_Gerais_IMPRESSO[,2]), 1 )
      for (i in 1:length(Estatisticas_Gerais_IMPRESSO[,2])) {
        if (Estatisticas_Gerais_IMPRESSO[i,2] == " ") { REFERENCIA_P_REMOVER[i,1] = FALSE }  else {
          if ( Estatisticas_Gerais_IMPRESSO[i,2] %in%  REMOVE_STATS) { REFERENCIA_P_REMOVER[i,1] = FALSE } else {
            REFERENCIA_P_REMOVER[i,1] = TRUE
          }  }  }
      
      Estatisticas_Gerais_IMPRESSO <- Estatisticas_Gerais_IMPRESSO[ REFERENCIA_P_REMOVER[,1] == FALSE,  ]  
      
    }
    
    Estatisticas_Gerais_IMPRESSO
    
    
  })
  
  
  output$summ_CATEGORICAL <- renderTable({
    BaseEmR_T <- data.frame(BaseEmR())
    TotalNumerico <- 0
    VetorClasse <- 0
    BaseEmR_T <- BaseEmR_T[,2:length(BaseEmR_T[1,])]
    
    var_num <- 0
    var_cat <- 0
    for (i in 1:length(colnames(BaseEmR_T))) {
      if ( is.numeric(BaseEmR_T[,i]) ) {
        BaseEmR_T[,i]<- as.numeric(BaseEmR_T[,i]) 
        var_num[i] <- colnames(BaseEmR_T)[i]
        var_cat[i] <- 0
      } else {
        BaseEmR_T[,i]<- as.factor(BaseEmR_T[,i]) 
        var_num[i] <- 0                           
        var_cat[i] <- colnames(BaseEmR_T)[i]
      } 
    }
    
    var_num <- var_num[var_num !=0]
    var_cat <- var_cat[var_cat !=0]
    
    BaseEmR_T_NUM <- BaseEmR_T
    BaseEmR_T_CAT <- BaseEmR_T
    VARIAVEL_DE_DIVISAO <- BaseEmR_T_CAT[input$spl_SUMM2][,1]
    
    
    BaseEmR_T_NUM <- BaseEmR_T[,var_num]
    BaseEmR_T_CAT <- BaseEmR_T[,var_cat]
    
    
    #CRIA VARI?VEIS ALEAT?RIAS TEMPOR?RIAS PARA N?O TRAVAR O SISTEMA
    RANDOM_LETTER_CAT_1_681099 <- as.factor(sample(c("a", "b", "c", "d", "e" ), dim(BaseEmR_T_CAT)[1], replace = TRUE))
    RANDOM_LETTER_CAT_2_681099 <- as.factor(sample(c("a", "b", "c", "d", "e" ), dim(BaseEmR_T_CAT)[1], replace = TRUE))
    RANDOM_LETTER_CAT_3_681099 <- as.factor(sample(c("a", "b", "c", "d", "e" ), dim(BaseEmR_T_CAT)[1], replace = TRUE))
    
    #AGREGA ESSAS VARI?VEIS JUNTO AO VETOR NUM?RICO ANALISADO, MAS ESSAS VARI?VEIS SER?O DESCARTADAS NO FINAL
    BaseEmR_T_CAT <- cbind(BaseEmR_T_CAT, RANDOM_LETTER_CAT_1_681099, RANDOM_LETTER_CAT_2_681099, RANDOM_LETTER_CAT_3_681099)
    
    #DEFINE AS BASES CATEG?RICAS E CATEG?RICAS (PARA ESTRATIFICA??O)
    BaseCategoricaUtilizada = as.matrix.data.frame(BaseEmR_T_CAT)
    
    
    #Descritivo_categorico = function(BaseCategoricaUtilizada) {
    #CRIA UMA LISTA PARA A CONTAGEM DOS ELEMENTOS
    CONTAGEM_GERAL <- as.list(0)
    CONTAGEM_GERAL[[1]] <- NULL
    j=1
    for (j in 1:length(BaseCategoricaUtilizada[1,])) {
      CONTAGEM_GERAL[[colnames(BaseCategoricaUtilizada)[j]]] <- as.matrix.data.frame(table(BaseCategoricaUtilizada[,j], VARIAVEL_DE_DIVISAO))
    }
    #CRIA UMA LISTA PARA O PERCENTUAL COM RELA??O A COLUNA
    PERCENTUAL_COLUNA <- as.list(0)
    PERCENTUAL_COLUNA[[1]] <- NULL
    jj=1
    j=1
    i=1
    ii=1
    
    for (jj in 1:length(CONTAGEM_GERAL)) {
      PERCENTUAL_COLUNA[[names(CONTAGEM_GERAL)[jj]]] <- matrix('_', dim(CONTAGEM_GERAL[[jj]])[1], dim(CONTAGEM_GERAL[[jj]])[2])
      for (j in 1:dim(CONTAGEM_GERAL[[jj]])[2]) {
        for (i in 1:dim(CONTAGEM_GERAL[[jj]])[1]) {
          PERCENTUAL_COLUNA[[names(CONTAGEM_GERAL)[jj]]][i,j]<- paste0(round((CONTAGEM_GERAL[[jj]][i,j]/sum(CONTAGEM_GERAL[[jj]][,j]))*100, digits=input$DCMP_DESC_CAT),"%")
        } 
      }
    }
    
    #CRIA UMA LISTA PARA O PERCENTUAL COM RELA??O A LINHA
    PERCENTUAL_LINHA <- as.list(0)
    PERCENTUAL_LINHA[[1]] <- NULL
    
    
    for (jj in 1:length(CONTAGEM_GERAL)) {
      PERCENTUAL_LINHA[[names(CONTAGEM_GERAL)[jj]]] <- matrix('_', dim(CONTAGEM_GERAL[[jj]])[1], dim(CONTAGEM_GERAL[[jj]])[2])
      for (j in 1:dim(CONTAGEM_GERAL[[jj]])[2]) {
        for (i in 1:dim(CONTAGEM_GERAL[[jj]])[1]) {
          PERCENTUAL_LINHA[[names(CONTAGEM_GERAL)[jj]]][i,j]<- paste0(round((CONTAGEM_GERAL[[jj]][i,j]/sum(CONTAGEM_GERAL[[jj]][i,]))*100, digits=input$DCMP_DESC_CAT),"%")
        } 
      } 
    }
    
    
    TRANSFORMA_LISTA_TO_DF <- function(LISTA_P_CONVERTER) {
      conta_niveis <- 0
      for (i in 1:length(names(LISTA_P_CONVERTER))) {
        conta_niveis <- length(LISTA_P_CONVERTER[[i]][,1]) + conta_niveis
      }
      
      #VETOR PARA ARMAZENAR TODOS OS DADOS
      resultado <- matrix("",conta_niveis, 1+length(LISTA_P_CONVERTER[[i]][1,]))
      ires <- 1
      for (i in 1:length(names(LISTA_P_CONVERTER))) {
        for (ii in 1:length(LISTA_P_CONVERTER[[i]][,1])) {
          for (jj in 1:length(LISTA_P_CONVERTER[[i]][1,])) {
            resultado[ires,jj+1] <- LISTA_P_CONVERTER[[i]][ii,jj]
            resultado[ires,1] <- names(LISTA_P_CONVERTER)[i]
          } 
          ires <- ires+1
        }
      }
      resultado <- data.frame(resultado)
      names(resultado)[1] <- resultado[1,1]
      resultado
    }  
    
    NUMEROS <- as.matrix.data.frame(TRANSFORMA_LISTA_TO_DF(CONTAGEM_GERAL))
    PCOLUNA <- as.matrix.data.frame(TRANSFORMA_LISTA_TO_DF(PERCENTUAL_COLUNA))
    PLINHA <- as.matrix.data.frame(TRANSFORMA_LISTA_TO_DF(PERCENTUAL_LINHA))
    QUASE_IMPRESSO <- cbind(NUMEROS, PCOLUNA, PLINHA)
    
    #INSERE AS LINHAS EM BRANCO NA HORIZONTAL
    ############################################################################se der pau mexer aqui
    
    ARQUIVO_DIVIDIDO <- split(data.frame(QUASE_IMPRESSO), data.frame(QUASE_IMPRESSO)[,1])
    VET_SEQ_NM_ORI <- unique(QUASE_IMPRESSO[,1])
    NOVO_ARQUIVO_DIVIDIDO <- as.list(0)
    NOVO_ARQUIVO_DIVIDIDO[[1]] <- NULL
    
    for (i in 1:length(names(ARQUIVO_DIVIDIDO))) {
      NOVO_ARQUIVO_DIVIDIDO[[VET_SEQ_NM_ORI[i]]] <- ARQUIVO_DIVIDIDO[[VET_SEQ_NM_ORI[i]]]
      
    }
    ARQUIVO_DIVIDIDO <- NOVO_ARQUIVO_DIVIDIDO
    
    LINHA_EM_BRANCO <- matrix(" ", 1, length(ARQUIVO_DIVIDIDO[[3]][1,]))
    ARQUIVO_JUNTADO <- LINHA_EM_BRANCO
    
    for (i in 1:length(names(CONTAGEM_GERAL))) {
      ARQUIVO_JUNTADO <- rbind(ARQUIVO_JUNTADO, LINHA_EM_BRANCO, as.matrix.data.frame(ARQUIVO_DIVIDIDO[[i]]))
    }
    #REMOVO A PRIMEIRA LINHA POR CONSEQU?NCIA DO TRATAMENTO DOS DADOS MESMO
    ARQUIVO_JUNTADO <- ARQUIVO_JUNTADO[-1,]
    QUASE_IMPRESSO <- ARQUIVO_JUNTADO
    
    #REMOVE COLUNAS DA MATRIZ
    NOME_P_REMOVER <- QUASE_IMPRESSO[2,1]
    for (i in 1:(length(QUASE_IMPRESSO[1,])-3)) {
      if (i %in% match(NOME_P_REMOVER, QUASE_IMPRESSO[2,]))
        QUASE_IMPRESSO <- QUASE_IMPRESSO[,-i]
    }
    
    
    #COLOCANDO NOMES APROPRIADOS
    colnames(QUASE_IMPRESSO) <- c(paste0(unique(levels(as.factor(VARIAVEL_DE_DIVISAO)))), 
                                  paste0(unique(levels(as.factor(VARIAVEL_DE_DIVISAO))), '(%col)' ),
                                  paste0(unique(levels(as.factor(VARIAVEL_DE_DIVISAO))), '(%lin)'))
    
    #ADICIONANDO COLUNAS DAS VARI?VEIS
    Variable <- matrix(" ", length(QUASE_IMPRESSO[,1]) ,1)
    Level <- matrix(" ", length(QUASE_IMPRESSO[,1]) ,1)
    QUASE_IMPRESSO <- cbind(Variable, Level, QUASE_IMPRESSO)
    colnames(QUASE_IMPRESSO)[1] <- i18n()$t("Variable")
    colnames(QUASE_IMPRESSO)[2] <- i18n()$t("Level")
    
    
    
    #CRIA UM VETOR PARA GUIAR A INSER??O DOS NOMES DAS VARI?VEIS
    insert_space <- matrix(1,1,length(names(CONTAGEM_GERAL)))
    j<-2
    for (i in 2:length(QUASE_IMPRESSO[,3])) {
      if (QUASE_IMPRESSO[i,3] == " ") { 
        insert_space[j] <- i
        j<- j+1
      }
    }
    
    #INSERINDO NOMES NA MATRIZ DE IMPRESS?O
    for (i in 1:(length(insert_space))) {
      QUASE_IMPRESSO[insert_space[i],1] <- names(CONTAGEM_GERAL)[i]
    }
    
    
    #######################################################################
    #ANTES REDEFINIMOS COMO DATAFRAMES OS ELEMENTOS DE DENTRO DA LISTA "CONTAGEM GERAL" S? PARA PEGAR OS NOMES DOS N?VEIS
    CONTAGEM_GERAL <- as.list(0)
    CONTAGEM_GERAL[[1]] <- NULL
    j=1
    for (j in 1:length(BaseCategoricaUtilizada[1,])) {
      CONTAGEM_GERAL[[colnames(BaseCategoricaUtilizada)[j]]] <- (table(BaseCategoricaUtilizada[,j], VARIAVEL_DE_DIVISAO))
    }
    
    #INSERINDO O NOME DOS N?VEIS
    armazena_niveis <- matrix(' ',length(QUASE_IMPRESSO[,1]),1)
    posicao <- 1
    NOMES_NA_MATRIZ_FINAL <- unique(QUASE_IMPRESSO[,1])
    NOMES_NA_MATRIZ_FINAL <- NOMES_NA_MATRIZ_FINAL[NOMES_NA_MATRIZ_FINAL !=" "]
    
    for (i in 1:length(NOMES_NA_MATRIZ_FINAL)) {
      for (ii in 1:length(CONTAGEM_GERAL[[NOMES_NA_MATRIZ_FINAL[i]]][,1])) {
        posicao <- posicao+1
        armazena_niveis[posicao,1]<- row.names(CONTAGEM_GERAL[[i]])[ii]
        
      } 
      posicao <- posicao+1 
    }
    
    for (i in 1:length(QUASE_IMPRESSO[,1])) {
      QUASE_IMPRESSO[i,2] <- armazena_niveis[i,1]
    }
    
    #######################################################################
    #AGORA VOLTAMOS A TRATAR OS ELEMENTOS DE DENTRO DA LISTA CONTAGEM GERAL COMO MATRIZ
    CONTAGEM_GERAL <- as.list(0)
    CONTAGEM_GERAL[[1]] <- NULL
    j=1
    for (j in 1:length(BaseCategoricaUtilizada[1,])) {
      CONTAGEM_GERAL[[colnames(BaseCategoricaUtilizada)[j]]] <- as.matrix.data.frame(table(BaseCategoricaUtilizada[,j], VARIAVEL_DE_DIVISAO))
    }
    
    #INSERINDO UMA COLUNA DE TOTAIS
    ARMAZENA_TOTAL <- matrix(' ',length(QUASE_IMPRESSO[,1]),1)
    iii = 1
    for (i in 1:length(names(CONTAGEM_GERAL))) {
      for ( ii in 1:length(CONTAGEM_GERAL[[i]][,1])) {
        iii <- iii+1
        ARMAZENA_TOTAL[iii,1] <- sum(as.numeric(CONTAGEM_GERAL[[i]][ii,]))
      }
      iii <- iii+1
    }
    colnames(ARMAZENA_TOTAL)[1] <- i18n()$t("Total")
    
    #ORDENA AS COLUNAS
    duas_primeiras <- colnames(QUASE_IMPRESSO[,c(1,2)])
    col.order <- colnames(QUASE_IMPRESSO)
    outras_colunas <- sort(col.order[3:length(col.order)])
    QUASE_IMPRESSO <- QUASE_IMPRESSO[,c(duas_primeiras,outras_colunas)]
    Estatisticas_Categoricas_IMPRESSO <- cbind(QUASE_IMPRESSO,ARMAZENA_TOTAL)
    
    
    
    
    #REMOVENDO OS NOMES N?O ESCOLHIDOS
    REMOVE_NOMES <- as.data.frame(0)
    for (i in 1:length(Estatisticas_Categoricas_IMPRESSO[,1])) {
      if (Estatisticas_Categoricas_IMPRESSO[i,1] == " ") {
        REMOVE_NOMES[i,1] <- REMOVE_NOMES[i-1,1]
      } else { REMOVE_NOMES[i,1] <- Estatisticas_Categoricas_IMPRESSO[i,1] }
    }
    
    VETOR_ESCOLHIDO <- input$VAR_CAT_DESC_NAMES
    Estatisticas_Categoricas_IMPRESSO <- Estatisticas_Categoricas_IMPRESSO[REMOVE_NOMES[,1] %in% VETOR_ESCOLHIDO ,]
    
    
    #REMOVENDO AS COLUNAS QUE N?O H? INTERESSE
    COLUNAS_SELECIONADAS <- input$VAR_CAT_DESC_STATS
    
    if ( !('(%col)' %in% COLUNAS_SELECIONADAS)  &&   !('(%lin)' %in% COLUNAS_SELECIONADAS) ) {
      Estatisticas_Categoricas_IMPRESSO<- Estatisticas_Categoricas_IMPRESSO[,- seq(4, length(colnames(Estatisticas_Categoricas_IMPRESSO))-1, by=3 )]
      Estatisticas_Categoricas_IMPRESSO<- Estatisticas_Categoricas_IMPRESSO[,- seq(4, length(colnames(Estatisticas_Categoricas_IMPRESSO))-1, by=2 )]
    } else {
      if ( !('(%col)' %in% COLUNAS_SELECIONADAS) ) {
        
        Estatisticas_Categoricas_IMPRESSO<- Estatisticas_Categoricas_IMPRESSO[,- seq(4, length(colnames(Estatisticas_Categoricas_IMPRESSO))-1, by=3 )]
        
      } else {
        if ( !('(%lin)' %in% COLUNAS_SELECIONADAS) ) {
          
          Estatisticas_Categoricas_IMPRESSO<- Estatisticas_Categoricas_IMPRESSO[,- seq(5, length(colnames(Estatisticas_Categoricas_IMPRESSO))-1, by=3 )]
          
        } } } 
    
    Estatisticas_Categoricas_IMPRESSO
    
    
  })
  
  output$fileob <- renderPrint({
    if(is.null(input$file)){return ()}
    
    SAIDA <- try(
      str(read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                     header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE, encoding = "latin1")), TRUE 
    )
    if (!inherits(SAIDA, "try-error")) {
      str(read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                     header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE, encoding = "latin1")) 
      UTF8ENCODING <<- FALSE
    } else {
      str(read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                     header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE, encoding = "utf-8")) 
      UTF8ENCODING <<- TRUE
    }
    
    SAIDA
  })
  
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         selectInput("Select", i18n()$t("File"), choices=input$file$name)
    )
  })
  
  output$table <- renderTable({ 
    if(is.null(input$file)){return()}
    
    if (UTF8ENCODING) { read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                                   header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE,encoding = "utf-8") } else {
                                     read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                                                header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE,encoding = "latin1")
                                   }
  })
  
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel(i18n()$t("database is ok?"),
                 h4(i18n()$t("If everything is right")," ", i18n()$t("click on")," ", icon("refresh"),tags$strong(i18n()$t("ANALYZE MY DATA"))),
                 helpText(i18n()$t("If there is an error"),", ", i18n()$t("try changing the options on the side panel. (decimals or 'CSV configuration').")),
                 verbatimTextOutput("fileob")),
        tabPanel(i18n()$t("Check your data"), 
                 h4(i18n()$t("If everything is right")," ", i18n()$t("click on")," ", icon("refresh"),tags$strong(i18n()$t("ANALYZE MY DATA"))),
                 helpText(i18n()$t("If there is an error"),", ", i18n()$t("try changing the options on the side panel. (decimals or 'CSV configuration').")),
                 tableOutput("table")) )
  })
  
  BaseEmR <- eventReactive({
    input$btn_analyze
    input$SEGMENTAR_BASE
    input$RETORNAR_BASE
    1    
  },{
    if (is.null(input$file))  {
      
      BaseEmR_0 <- cbind(read.csv2(file="examples/ID_Names.csv",header = TRUE,sep=";"),
              data.frame(ggplot2::diamonds)[sample(1:53940, 1000, replace = FALSE),1:10])
      names(BaseEmR_0) <- c("ID_Name", "Carat", "Cut", "Color", "Clarity",
                          "Depth", "Table", "Price", "x_mm", "y_mm", "z_mm")
      rownames(BaseEmR_0) <- c(1:1000)

      #BaseEmR_0 <- read.table(file="http://estatistica.info/examples/DataBase/diamonds.csv",  sep=";", 
      #                        header = TRUE, dec = ",", stringsAsFactors = TRUE,encoding = "latin1")
      #BaseEmR_0 <- read.table(file="diamonds.csv",  sep=";", 
      #                        header = TRUE, dec = ",", stringsAsFactors = TRUE,encoding = "latin1")
      BaseEmR_0$OneGroup_pvalue <- rep(i18n()$t("(single group)"),dim(BaseEmR_0)[1])
      BaseEmR_0$OneGroup_pvalue <- as.factor(BaseEmR_0$OneGroup_pvalue)
      BaseEmR_0$RandomGroup_pvalue <- sample(c(i18n()$t("(a group)"), i18n()$t("(b group)")), replace = TRUE, size=dim(BaseEmR_0)[1])
      BaseEmR_0$RandomGroup_pvalue <- as.factor(BaseEmR_0$RandomGroup_pvalue)
      BaseEmR_0$RandomNumber_pvalue <- runif(dim(BaseEmR_0)[1])
      BaseEmR_0 <- BaseEmR_0 %>% select(colnames(BaseEmR_0)[1], "OneGroup_pvalue", everything())
      BaseEmR_utilizada <- BaseEmR_0
      BaseEmR_BACKUP <<- data.frame(BaseEmR_utilizada)
      
    } else { 
      
      if (UTF8ENCODING) { BaseEmR_up <- data.frame(read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                                                              header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE,encoding = "utf-8")) } else {
                                                                BaseEmR_up <- data.frame(read.table(file=input$file$datapath[input$file$name==input$Select],  sep=input$sep, 
                                                                                                    header = TRUE,  dec = input$decimals, stringsAsFactors = TRUE,encoding = "latin1"))
                                                              }
      BaseEmR_up$OneGroup_pvalue <- rep(i18n()$t("(single group)"),dim(BaseEmR_up)[1])
      BaseEmR_up$OneGroup_pvalue <- as.factor(BaseEmR_up$OneGroup_pvalue)
      BaseEmR_up$RandomGroup_pvalue <- sample(c( i18n()$t("(a group)"), i18n()$t("(b group)") ), replace = TRUE, size=dim(BaseEmR_up)[1])
      BaseEmR_up$RandomGroup_pvalue <- as.factor(BaseEmR_up$RandomGroup_pvalue)
      BaseEmR_up$RandomNumber_pvalue <- runif(dim(BaseEmR_up)[1])
      BaseEmR_up <- BaseEmR_up %>% select(names(BaseEmR_up)[1],"OneGroup_pvalue", everything())
      BaseEmR_utilizada <- BaseEmR_up
      BaseEmR_BACKUP <<- data.frame(BaseEmR_utilizada)
      
    }
    
    
    #TEMPORARIO <- c("e","f","a","b",NA,"g","g","a","e","c","b")
    #TEMPORARIO
    
    #as.character(TEMPORARIO)
    #TEMPORARIO
    
    #TEMPORARIO <- factor(TEMPORARIO, levels = sort(unique(TEMPORARIO)))
    #TEMPORARIO
    
    if ( (FOI_SEGMENTADO==TRUE) ) { 
      
      BaseEmR_Seg <- data.frame(BaseEmR_Seg)
      locais_de_reforco_de_fatores <- match(names(ULTIMO_NAMES_var_cat) , names(BaseEmR_Seg))
      for (i in 1:length(names(BaseEmR_Seg))) {
        if( names(BaseEmR_Seg)[i] %in%  locais_de_reforco_de_fatores ) {
          
         BaseEmR_Seg[,names(BaseEmR_Seg)[i]] <- as.factor(BaseEmR_Seg[,names(BaseEmR_Seg)[i]])
  
        } }
      BaseEmR_Seg
    } else { BaseEmR_BACKUP }
  }, ignoreNULL = FALSE)
  
  var_num <- eventReactive({
    input$btn_analyze
    
  },{
    var_num <- c(0, rep(length(names(BaseEmR()))))
    var_cat <- c(0, rep(length(names(BaseEmR()))))
    #DEFININDO CORRETAMENTE CADA VETOR
    BaseEmR <- data.frame(BaseEmR())
    for (i in 1:length(colnames(BaseEmR))) {
      if ( is.numeric(BaseEmR[,i]) ) {
        BaseEmR[,i]<- as.numeric(BaseEmR[,i]) 
        var_num[i] <- colnames(BaseEmR)[i]
        var_cat[i] <- 0
      } else {
        BaseEmR[,i]<- as.factor(BaseEmR[,i]) 
        var_num[i] <- 0                           
        var_cat[i] <- colnames(BaseEmR)[i]
      } 
    }
    var_num <- var_num[var_num !=0]
    var_cat <- (var_cat[var_cat !=0])
    var_cat <- var_cat[-1]
    var_cat_dic <- matrix(0,1,length(var_cat))
    for (i in 1:length(var_cat)) { if (length(unique(BaseEmR()[var_cat[i]][is.na(BaseEmR()[var_cat[i]])])) > 0 ) {
      if (length(unique(BaseEmR()[var_cat[i]])[,1]) == 3) { var_cat_dic[1,i] <- var_cat[i] } }
      if (length(unique(BaseEmR()[var_cat[i]][is.na(BaseEmR()[var_cat[i]])])) == 0 ) {
        if (length(unique(BaseEmR()[var_cat[i]])[,1]) == 2) { var_cat_dic[1,i] <- var_cat[i] } } }
    var_cat_dic <- var_cat_dic[var_cat_dic !=0]
    lista_finalmente <- as.list(t(var_cat_dic))
    names(lista_finalmente) <- var_cat_dic
    var_cat_dic = lista_finalmente
    lista_finalmente <- as.list(t(var_cat))
    names(lista_finalmente) <- var_cat
    var_cat = lista_finalmente
    lista_finalmente <- as.list(t(var_num))
    names(lista_finalmente) <- var_num
    var_num <- lista_finalmente
    ULTIMO_NAMES_var_num <<- var_num
    var_num
  }, ignoreNULL = FALSE)
  
  var_cat <- eventReactive({
    input$btn_analyze
    
  },{
    var_num <- c(0, rep(length(names(BaseEmR()))))
    var_cat <- c(0, rep(length(names(BaseEmR()))))
    #DEFININDO CORRETAMENTE CADA VETOR
    BaseEmR <- data.frame(BaseEmR())
    for (i in 1:length(colnames(BaseEmR))) {
      if ( is.numeric(BaseEmR[,i]) ) {
        BaseEmR[,i]<- as.numeric(BaseEmR[,i]) 
        var_num[i] <- colnames(BaseEmR)[i]
        var_cat[i] <- 0
      } else {
        BaseEmR[,i]<- as.factor(BaseEmR[,i]) 
        var_num[i] <- 0                           
        var_cat[i] <- colnames(BaseEmR)[i]
      } 
    }
    var_num <- var_num[var_num !=0]
    var_cat <- (var_cat[var_cat !=0])
    var_cat <- var_cat[-1]
    var_cat_dic <- matrix(0,1,length(var_cat))
    for (i in 1:length(var_cat)) { if (length(unique(BaseEmR()[var_cat[i]][is.na(BaseEmR()[var_cat[i]])])) > 0 ) {
      if (length(unique(BaseEmR()[var_cat[i]])[,1]) == 3) { var_cat_dic[1,i] <- var_cat[i] } }
      if (length(unique(BaseEmR()[var_cat[i]][is.na(BaseEmR()[var_cat[i]])])) == 0 ) {
        if (length(unique(BaseEmR()[var_cat[i]])[,1]) == 2) { var_cat_dic[1,i] <- var_cat[i] } } }
    var_cat_dic <- var_cat_dic[var_cat_dic !=0]
    lista_finalmente <- as.list(t(var_cat_dic))
    names(lista_finalmente) <- var_cat_dic
    var_cat_dic = lista_finalmente
    lista_finalmente <- as.list(t(var_cat))
    names(lista_finalmente) <- var_cat
    var_cat <- lista_finalmente
    lista_finalmente <- as.list(t(var_num))
    names(lista_finalmente) <- var_num
    var_num = lista_finalmente
    ULTIMO_NAMES_var_cat <<- var_cat
    var_cat
  }, ignoreNULL = FALSE)
  
  var_cat_dic <- eventReactive({
    input$btn_analyze
  },{
    var_num <- c(0, rep(length(names(BaseEmR()))))
    var_cat <- c(0, rep(length(names(BaseEmR()))))
    
    #DEFININDO CORRETAMENTE CADA VETOR
    BaseEmR <- data.frame(BaseEmR())
    for (i in 1:length(colnames(BaseEmR))) {
      if ( is.numeric(BaseEmR[,i]) ) {
        BaseEmR[,i]<- as.numeric(BaseEmR[,i]) 
        var_num[i] <- colnames(BaseEmR)[i]
        var_cat[i] <- 0
      } else {
        BaseEmR[,i]<- as.factor(BaseEmR[,i]) 
        var_num[i] <- 0                           
        var_cat[i] <- colnames(BaseEmR)[i]
      } 
    }
    var_num <- var_num[var_num !=0]
    var_cat <- (var_cat[var_cat !=0])
    var_cat <- var_cat[-1]
    var_cat_dic <- matrix(0,1,length(var_cat))
    for (i in 1:length(var_cat)) { if (length(unique(BaseEmR()[var_cat[i]][is.na(BaseEmR()[var_cat[i]])])) > 0 ) {
      if (length(unique(BaseEmR()[var_cat[i]])[,1]) == 3) { var_cat_dic[1,i] <- var_cat[i] } }
      if (length(unique(BaseEmR()[var_cat[i]][is.na(BaseEmR()[var_cat[i]])])) == 0 ) {
        if (length(unique(BaseEmR()[var_cat[i]])[,1]) == 2) { var_cat_dic[1,i] <- var_cat[i] } } }
    var_cat_dic <- var_cat_dic[var_cat_dic !=0]
    lista_finalmente <- as.list(t(var_cat_dic))
    names(lista_finalmente) <- var_cat_dic
    var_cat_dic <- lista_finalmente
    lista_finalmente <- as.list(t(var_cat))
    names(lista_finalmente) <- var_cat
    var_cat = lista_finalmente
    lista_finalmente <- as.list(t(var_num))
    names(lista_finalmente) <- var_num
    var_num = lista_finalmente
    var_cat_dic
  }, ignoreNULL = FALSE)
  
  
  # MENUS PRINCIPAIS #
  #####################################################  
  output$Upload_MENU <- renderText({ 
    paste0(i18n()$t("Upload"))
  })
  ## SUBMENU Upload
  #####################################################  
  output$Data_viewer_MENU <- renderText({ 
    paste0(i18n()$t("Data viewer"))
  })
  ## SUBMENU Data viewer
  output$Descriptive_statistics_SUBMENU <- renderText({ 
    paste0(i18n()$t("Descriptive statistics"))
  })
  output$View_download_data_SUBMENU <- renderText({ 
    paste0(i18n()$t("View/Download data"))
  })
  output$Data_segmentation_SUBMENU <- renderText({ 
    paste0(i18n()$t("Database segmentation"))
  })
  
  #####################################################      
  output$Custom_graphics_MENU <- renderText({ 
    paste0(i18n()$t("Custom graphics"))
  })  
  ## SUBMENU Upload
  output$Histogram_SUBMENU <- renderText({ 
    paste0(i18n()$t("Histogram"))
  })
  output$Boxplot_SUBMENU <- renderText({ 
    paste0(i18n()$t("Boxplot"))
  })
  
  output$Scatterplot_SUBMENU <- renderText({ 
    paste0(i18n()$t("Scatterplot"))
  })
  output$twofactors_x_twonumbers_SUBMENU <- renderText({ 
    paste0(i18n()$t("2 factors x 2 numbers"))
  })
  #####################################################      
  output$Tests_MENU <- renderText({ 
    paste0(i18n()$t("Tests"))
  })
  ## SUBMENU Tests
  output$KW_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("Kruskal-Wallis Test"))
  })
  output$ANOVA_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("ANOVA Test"))
  })
  output$MW_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("Mann-Whitney Test"))
  })
  output$T_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("Student's t-test"))
  })
  output$CORRELATION_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("Correlation test"))
  })
  
  output$FISHER_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("Fisher's exact test"))
  })
  output$ROC_test_SUBMENU <- renderText({ 
    paste0(i18n()$t("ROC Curve"))
  })
  #####################################################      
  output$Citation_MENU <- renderText({ 
    paste0(i18n()$t("Citation"))
  })
  output$How_cite_SUBMENU <- renderText({ 
    paste0(i18n()$t("How can I cite this web site in my publication?"))
  })
  
  #TEXTO MOSTRADO NA JANELA DA CURVA ROC
  output$TEXT_ROC_BOOTSTRAP_REP <- renderText({ 
    paste0(i18n()$t("Results and confidence intervals based on")," ", input$NUMBER_ROC_BOOTSTRAP_REP ," ",i18n()$t("bootstrap repetitions"))
  })
  
  
  
  #ATUALIZA AS OP??ES DE DESFECHO PARA CURVA ROC
  observeEvent(input$ROC_DESFECHO,{
    
    updateRadioButtons(session, 'ROC_DESFECHO_LVL', 
                       label = i18n()$t("Outcome of interest"),
                       choices=  levels(as.factor(BaseEmR()[input$ROC_DESFECHO][,1])),
                       selected = levels(as.factor(BaseEmR()[input$ROC_DESFECHO][,1]))[1] )
  })
  
  
  #VOLTA A VARI?VEL FOI_SEGMENTADO PARA FALSE, TODA VEZ QUE ? INSERIDO UM NOVO ARQUIVO PARA AN?LISE
  observeEvent(input$btn_analyze,{
    FOI_SEGMENTADO <<- FALSE
  })
  
  #POP-UP LEGAL DISCLAIMER
  observeEvent(input$LegalDisclaimer, {
    showModal(modalDialog(
      
      span(i18n()$t("The owners of this website"), ", ",
           tags$strong(span(id="logotxt", "pvalue.site")),", ",
           i18n()$t("are not responsible for"), ", ",
           i18n()$t("and expressly disclaim all liability for"), ", ",
           i18n()$t("damages of any kind arising out of use"), ", ",
           
           i18n()$t("reference to"), ", ",
           i18n()$t("or reliance on any information"), ", ",
           i18n()$t("including all results from all statistical calculators"), ", ", 
           i18n()$t("contained within this website"), ". ",
           i18n()$t("Please make sure that you understand that the information provided here"), ", ", 
           i18n()$t("including the results of statistical calculations"), ", ",
           i18n()$t("is being provided freely"), ", ",
           i18n()$t("and that no kind of agreement or contract is created between you and the owners of this site"), ", ",
           i18n()$t("the owners of the servers upon which it is housed"), ", ",
           i18n()$t("or anyone else who is in any way connected with this project"), ". ",
           
           i18n()$t("In short"), ", ",
           i18n()$t("the information within this website is presented as is"), ", ",
           i18n()$t("and you use this site"), ", ",
           i18n()$t("including its statistical calculators"), ", ",
           i18n()$t("at your own risk"), ". ",
           
           i18n()$t("While we make our best efforts to ensure the accuracy of the calculations performed here"), ", ",
           i18n()$t("we cannot rule out the possibility of error"), ", ",
           i18n()$t("and we will not be held liable in the unfortunate circumstance that such an error arises"), ". ",
           
           i18n()$t("By using this website"), ", ",
           i18n()$t("you accept the terms and conditions of this disclaimer"),"."),
      title = i18n()$t("Important message - Legal Disclaimer"),
      easyClose = FALSE, footer = modalButton(i18n()$t("I agree."))) )
  })
  
})

shinyApp(ui, server)