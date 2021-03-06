library(shiny)

### EMPEZAR ####

shinyUI(pageWithSidebar(
  
  #### Título de la Aplicación ####
  
    headerPanel(title="Ema APP", windowTitle="Ema: Estaciones de Monitoreo Ambiental"), 
  
  #### Sidebar ####
  sidebarPanel(
    
    conditionalPanel(
      condition="input.tabs!='Convertir conductividad'",
      HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Ema/master/Figuras/logo_ema_freplata.png"/></div>')),
    
    ### TABLA DE DATOS ####
    
    conditionalPanel(
      condition="input.tabs=='Tabla'", 
      tags$hr(),
      
      strong('Tipo de archivo'),br(),
      radioButtons('tipo_archivo', "", c("Texto"='.csv', "Microsoft Excel"='.xls')),
        
      conditionalPanel(
        condition="input.tipo_archivo=='.csv'", ### CSV
        fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values','text/plain')),
        
        wellPanel(
        strong('Opciones'),br(),br(),  
        checkboxInput('header', 'Cabecera', TRUE),
        selectInput('sep', 'Separador de campos',c('Coma'=',','Punto y coma'=';','Tabulador'='\t'),'Coma',FALSE),
        selectInput('quote', 'Delimitador de texto', c("Ninguno"='','Comillas dobles'='"','Comillas simples'="'"),'Ninguna',FALSE),
        selectInput('dec', 'Separador decimal', c(Punto=".", Coma=","),'Punto',FALSE),
        numericInput('skip',"Leer desde línea:",value="0")
        )
        ),
      
      conditionalPanel(
        condition="input.tipo_archivo=='.xls'", ### XLS
        fileInput('file2', '', accept=c('application/vnd.ms-excel')),
        
        wellPanel(
          strong('Opciones'),br(),br(),          
        checkboxInput('header2', 'Cabecera', TRUE),
          br(),
        textInput('sheet_name', 'Nombre de hoja', c(""))
        )
        ),
      
      tags$hr(),
      strong('Formato de Fecha y Hora'),br(),  
      selectInput('formato_fecha',"",
                  multiple=FALSE,selected="ej. 06/28/2009 22:50:60",
                  choices=c("ej. 06/28/2009 22:50:60"="%m/%d/%Y %H:%M:%S",
                            "ej. 28/06/2009 22:50:60"="%d/%m/%Y %H:%M:%S",
                            "ej. 28/06/09 22:50:60"="%d/%m/%y %H:%M:%S",
                            "ej. 28-06-09 22:50:60"="%d-%m-%y %H:%M:%S"))
      ),         
    
    ### PLOTS ####
    
    conditionalPanel(
      condition="input.tabs=='Plots'", 
      tags$hr(),
      
      wellPanel(
        strong('Gráfico'),br(),        
      radioButtons('tipo_plot', '', c("Series de tiempo"='plot', "Relaciones entre variables"='xvsy', "Histograma"='hist')),
        br(),
        strong('Fuente de datos'),br(),
        selectInput('plot_editados',label="",c('Original'='datos_originales','Editados'='datos_editados')),br(),
        strong('Escala temporal'),br(), 
        selectInput('escala_temporal',"", c("Original"='original',"Promedios diarios"='days',"Promedios semanales"='weeks', "Promedios mensuales"='months', "Promedios anuales"='years'))
        ),
      
     conditionalPanel(
        condition="input.tipo_plot=='plot'",
        
        wellPanel(
          strong('Elegir variables'),br(),        
          uiOutput("choose_columns_plot")
        ),
        
        wellPanel(
        strong('Personalizar'),br(),br(),
        radioButtons('eje_y','Eje Y:', choices=c('Maximizado' = 'allmaximized','Fijo' = 'allfixed')),
        sliderInput("fill","Relleno:", value=0, min=0, max=100, step=10))
      ),
      
      conditionalPanel(
        condition="input.tipo_plot=='xvsy'",
        
        wellPanel(
          strong('Elegir dos variables'),br(),          
        uiOutput("choose_columns_xvsy")
        ),
        checkboxInput('log','Logaritmo',FALSE)),
      
     conditionalPanel(
       condition="input.tipo_plot=='hist'",
       
       wellPanel(
         strong('Elegir variable'),br(),          
         uiOutput("choose_columns_hist")
       )
       )      
      ),
    
    ### EDITAR ####
    
    conditionalPanel(
      condition="input.tabs=='Editar'", 
      tags$hr(),
      
      wellPanel(
      
      strong('Elegir variable'),br(),          
      uiOutput("choose_columns2")
      ),
      
      wellPanel(
        strong('Intervalo'),br(),          
        selectInput('int',"",list("25"=25,"50"=50,"100"=100,"250"=250,"500"=500,"750"=750,"1000"=1000,"1500"=1500,"2000"=2000),selected="100"),br(),
      #actionButton("atras","Atrás"),
      #actionButton("adelante","Siguiente"),
      HTML("<button id=\"atras\" type=\"button\" class=\"btn action-button btn-inverse btn-mini\">Atrás</button>"),
      HTML("<button id=\"siguiente\" type=\"button\" class=\"btn action-button btn-inverse btn-mini\">Siguiente</button>")
      #br(),br(),
      #HTML("<button id=\"actualizar\" type=\"button\" class=\"btn action-button btn-primary\">Actualizar</button>")
      ),
  
      #tags$head(tags$style(type='text/css', "#selected{ display: none; }")), # saca entrada de texto
      uiOutput("selectedOut"),
      uiOutput("selectedOut2"),
      tags$head(tags$style(type='text/css', "#selected{ display: none; }")),
      tags$head(tags$style(type='text/css', "#selected_b{ display: none; }")),
      #actionButton('actualizar','Actualizar',class='btn-primary'),
      #tags$button(id='actualizar', type='button', class= 'btn btn-primary'),
      #HTML("<a id=\"descarga_edit\" class=\"btn shiny-download-link btn-success\" href=\"\" target=\"_blank\">Descargar datos editados</a>")
      wellPanel(
      downloadButton("descarga_edit","Descargar datos editados",class='btn-success')
      )
      ),
    ### RESUMEN ####
    
    conditionalPanel(
      condition="input.tabs=='Resumen'",
      tags$hr(),
    wellPanel(
      strong('Resumen'),br(),
      selectInput('resumen_editados',label="",c('Datos originales'='datos_originales','Datos editados'='datos_editados'))
      )
      ),
    
    ### CONVERTIR CONDUCTIVIDAD ####
    
      conditionalPanel(
        condition="input.tabs=='Convertir conductividad'",
        HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Ema/master/Figuras/logos_salinidad.png"/></div>'),
        tags$hr(),
        helpText("Nota: calcula la salinidad a partir de la conductividad, la temperatura y la presión.", # Nota
               "Se utiliza el algoritmo de la UNESCO descrito por Fofonoff and Millard (1983).",
               "Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of fundamental",
               "properties of seawater. Unesco Technical Papers in Marine Science, 44, 53."),
        tags$hr(),
        
        wellPanel(
        strong('Fuente'),br(),
        selectInput('sal_editados',label="",c('Datos originales'='datos_originales','Datos editados'='datos_editados'))
        ),
        
        ### CONDUCTIVIDAD
        
        wellPanel(
          strong('Conductividad'),br(),       
          
        selectInput('cond_cte',"Constante",choices=c("Si","No"),"No"),
        
        conditionalPanel(
          condition="input.cond_cte=='Si'",
          textInput('cond_val',"Valor:",0)),
        
        conditionalPanel(
          condition="input.cond_cte=='No'",
          uiOutput("col_cond")),
        
        selectInput('uni_cond',"Unidades:",c("mS/cm","S/m"))
        ),
        
                
        ### TEMPERATURA
        
        wellPanel(
          strong('Temperatura'),br(),       
          
        selectInput('temp_cte',"Constante",choices=c("Si","No"),"No"),
        
        conditionalPanel(
          condition="input.temp_cte=='Si'",
          textInput('temp_val',"Valor [ºC]:",0)),
        
        conditionalPanel(
          condition="input.temp_cte=='No'",
          uiOutput("col_temp"))
        ),
      
        ### PROFUNDIDAD
        wellPanel(
          strong('Profundidad'),br(),       
        selectInput('prof_cte',"Constante",choices=c("Si","No"),"No"),
        
        conditionalPanel(
          condition="input.prof_cte=='Si'",
          textInput('prof_val',"Valor [m]:",0)),
        
        conditionalPanel(
          condition="input.prof_cte=='No'",
          uiOutput("col_prof"))
        ),
                
        ### DESCARGAR SALINIDAD
        
        wellPanel(
          strong('Descargar Salinidad'),br(),       
          radioButtons('desc_sal',"",choices=c("Solo salinidad","Salinidad junto con datos cargados"),"Solo salinidad"),
        downloadButton("descarga","Descargar datos",class='btn-success')
        ))
    ),
      
  #### Mainpanel ####
  
  mainPanel(tabsetPanel(id="tabs",
                        tabPanel("Tabla", dataTableOutput("table")), # Tabla
                        tabPanel("Plots",htmlOutput("plot"),# Annotated Time Line
                                 htmlOutput("xvsy"), # X vs Y
                                 plotOutput("hist")),# Histogram 
                        tabPanel("Editar", htmlOutput("edit")), # Sccater Chart
                        tabPanel("Resumen", htmlOutput("summary")), # Resumen de datos editados
                        tabPanel("Convertir conductividad", dataTableOutput("table2")),# Tabla
                        
                        ### ACERCA DE ESTA APLICACIÓN ####
                        
                        tabPanel("Acerca de esta APP",
                                 h3(p(strong('Descripción'))),
                                 p(style="text-align:justify",'Esta aplicación web de R con Shiny se encuentra en desarrollo.'),
                                 p(style="text-align:justify",'La aplicación web EMA ("Estaciones de Monitoreo Ambiental") está diseñada para permitirle al usuario visualizar, analizar, y editar de manera interactiva datos oceanográficos. Está siendo desarrollada en el marco del Proyecto FREPLATA URU/09/G31 dentro del', em('"Programa de Monitoreo y Evaluación y Sistema de Información Integrado y establecido para la toma de decisiones y la Gestión del Río de la Plata y su Frente Marítimo".'),'El objetivo es generar una herramienta que permita a los usuarios procesar datos oceanográficos provenientes de la Boya oceanográfica, Torre Oyarvide o Pilote Norden.'),
                                 p(style="text-align:justify",'La mayor parte del software empleado para desarrollar esta aplicación es libre, eso quiere decir que garantiza al usuario la libertad de poder usarlo, estudiarlo, compartirlo (copiarlo), y modificarlo. El software R es un proyecto de software libre que es colaborativo y tiene muchos contribuyentes.'),
                                 tags$hr(),
                                 h3(p(strong('Guía de usuario'))),
                                 HTML('<div style="clear: left;"><img src="https://github.com/guzmanlopez/Ema/raw/master/Figuras/PDF.png?raw=true" alt="" style="width: 5%; height: 5%; float: left; margin-right:5px" /></div>'),
                                 br(),
                                 a('Ema web app', href="https://github.com/guzmanlopez/Ema/blob/master/Manual/Gu%C3%ADa%20de%20usuario%20Ema%20web%20app.pdf?raw=true", target="_blank"),
                                 tags$hr(),
                                 h3(p(strong('Código fuente'))),
                                 HTML('<div style="clear: left;"><img src="https://github.com/guzmanlopez/Ema/blob/master/Figuras/github-10-512.png?raw=true" alt="" style="width: 5%; height: 5%; float: left; margin-right:5px" /></div>'),
                                 br(),
                                 a('Repositorio GitHub', href="https://github.com/guzmanlopez/Ema.git", target="_blank"),
                                 tags$hr(),
                                 h3(p(strong('Referencias'))),
                                 p(style="text-align:justify",strong('R Core Team (2013).'),'R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL',a("http://www.R-project.org/",href="http://www.R-project.org/",target="_blank")),
                                 p(style="text-align:justify",strong('RStudio and Inc. (2013).'),'shiny: Web Application Framework for R. R package version 0.8.0.',a("http://CRAN.R-project.org/package=shiny", href="http://CRAN.R-project.org/package=shiny",target="_blank")),
                                 p(style="text-align:justify",strong('Dan Kelley (2013).'),'oce: Analysis of Oceanographic data. R package version 0.9-12.',a("http://CRAN.R-project.org/package=oce",href="http://CRAN.R-project.org/package=oce",target="_blank")),
                                 p(style="text-align:justify",strong('Markus Gesmann & Diego de Castillo.'),'Using the Google Visualisation API with R. The R Journal, 3(2):40-44, December 2011.'),
                                 p(style="text-align:justify",strong('Jeffrey A. Ryan & Joshua M. Ulrich (2013).'),'xts: eXtensible Time Series. R package version 0.9-7.',a("http://r-forge.r-project.org/projects/xts/",href="http://r-forge.r-project.org/projects/xts/",target="_blank")),
                                 p(style="text-align:justify",strong('Karoly Antal. (2012).'),'gnumeric: Read data from files readable by gnumeric. R package version 0.7-2.',a("http://CRAN.R-project.org/package=gnumeric",href="http://CRAN.R-project.org/package=gnumeric",target="_blank")),
                                 tags$hr(),
                                 HTML('<div style="clear: left;"><img src="https://github.com/guzmanlopez/Ema/blob/master/Figuras/foto_perfil.jpg?raw=true" alt="" style="float: left; margin-right:5px" /></div>'),
                                 strong('Autor'),
                                 p(
                                   a('Guzmán López', href="https://www.linkedin.com/pub/guzm%C3%A1n-l%C3%B3pez/59/230/812", target="_blank"),' - (Correo:',
                                   a('guzilop@gmail.com', target="_blank"),' | ', 'Telegram:', a('@Guzman', href="https://telegram.me/Guzman", target="_blank"),')',
                                   br(),
                                   'Biólogo | Asistente para el manejo de información oceanográfica',br(),a('Proyecto FREPLATA - URU/09/G31',href="http://www.freplata.org/", target="_blank")),
                                 br()) # Acerca de este programa
                        ))
))