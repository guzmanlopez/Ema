library(shiny)
library(oce)
library(gnumeric)
library(xts)
suppressPackageStartupMessages(library(googleVis))

### Vectores de celdas y valores para eliminar

celdas_flags_tabla <- list(NA)
valores_flags_tabla <- list(NA)

columnas <- c("Presion", "Temp", "Cond", "Turb", "OD", "Bateria", "Fluo")
                 
for(i in 1:length(columnas))
  celdas_flags_tabla[[i]] <- as.factor(NA)
names(celdas_flags_tabla) <- columnas

for(i in 1:length(columnas))
  valores_flags_tabla[[i]] <- as.factor(NA)
names(valores_flags_tabla) <- columnas


shinyServer(function(input, output) {
  
  ### Logos
  output$logo <- renderImage({
    
    filename <- "https://dl.dropboxusercontent.com/u/49775366/Ema/logo_ema_freplata.png"
    
    list(src = filename,
         contentType = 'image/png')
  }, deleteFile = FALSE)
  output$logo2 <- renderImage({
    
    filename <- "https://dl.dropboxusercontent.com/u/49775366/Ema/logos_salinidad.png"
    
    list(src = filename,
         contentType = 'image/png')
  }, deleteFile = FALSE)
  
  ### Entradas de datos
  datasetInput <- reactive({
    if (is.null(input$file1) & is.null(input$file2)) return(NULL) else
      if (input$tipo_archivo=='.csv')
        mydata <- read.table(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote, skip=input$skip, dec=input$dec) else # CSV
          if (input$tipo_archivo=='.xls')
            mydata <- read.gnumeric.sheet(file=input$file2$datapath,head=input$header2,sheet.name=input$sheet_name,drop.empty.rows="all",drop.empty.columns="none") # XLS
    return(mydata)
    })
  datasetInputGvis <- reactive({
    
    if(input$plot_editados=='datos_originales') {
    
    mydata <- datasetInput()
    data <- cbind(Fecha=strptime(x=mydata[,1],format=input$formato_fecha),mydata[,c(-1)]) ### Convertir fechas
    
    if(input$escala_temporal=='original') {
      
      return(data)  
    }
    
    if(input$escala_temporal!='original') {
      
      datos_escala <- function(datos, escala){
        
        # Pasar a objeto XTS
        datos_xts <- xts(x=datos[,-1], datos[,1])
        columnas <- colnames(datos)[-1]
        promedios <- period.apply(x=datos_xts, endpoints(datos_xts, escala, 1), FUN=mean)
        
        index <- index(promedios)
        promedios <- as.data.frame(promedios, row.names=FALSE)
        promedios <- data.frame("Fecha"=index, promedios)
        
        return(promedios)
      }
      
      data <- datos_escala(datos=data, escala=input$escala_temporal)
      
      return(data)
            
    }
    
    } else
      if(input$plot_editados=='datos_editados') {
        
        Fecha <- datasetInput()[,1]
        Fecha <- strptime(x=Fecha, format=input$formato_fecha)
        
        data <- cbind(Fecha=Fecha,as.data.frame(datasetInputGvis_edit()[c((length(columnas)+1):(2*length(columnas)))]))
        
        if(input$escala_temporal=='original') {
          
          return(data)  
        }
        
        if(input$escala_temporal!='original') {
          
          datos_escala <- function(datos, escala){
            
            # Pasar a objeto XTS
            datos_xts <- xts(x=datos[,-1], datos[,1])
            columnas <- colnames(datos)[-1]
            promedios <- period.apply(x=datos_xts, endpoints(datos_xts, escala, 1), FUN=mean)
            
            index <- index(promedios)
            promedios <- as.data.frame(promedios, row.names=FALSE)
            promedios <- data.frame("Fecha"=index, promedios)
            
            return(promedios)
          }
          
          data <- datos_escala(datos=data, escala=input$escala_temporal)
          
          return(data)
          
        }
        
      }
      
  })
  datasetInputSalinidad <- reactive({
    
    if(input$sal_editados=="datos_originales") {
                
                mydata <- datasetInput()
                
                # Profundidad:
                if(input$prof_cte=="Si") prof <- rep(as.numeric(input$prof_val),nrow(mydata)) else # Constante
                  prof <- mydata[,noquote(input$col_prof)] # No constante
                
                pres <- 0.9806652*prof
                
                # Temperatura:
                if(input$temp_cte=="Si") temp <- rep(as.numeric(input$temp_val),nrow(mydata)) else # Constante
                  temp <- mydata[,noquote(input$col_temp)] # No constante
                
                # Conductividad:
                if(input$cond_cte=="Si") cond <- rep(as.numeric(input$cond_val),nrow(mydata)) else # Constante
                  cond <- mydata[,noquote(input$col_cond)] # No constante
                
                salinidad <- round(swSCTp(conductivity=cond, temperature=temp, pressure=pres, conductivityUnit=input$uni_cond),digits=2)        
                salinidad <- data.frame(Salinidad_UPS=salinidad)
                
                return(salinidad)
                
              }
    
    if(input$sal_editados=="datos_editados") {
                  
                  Fecha <- datasetInput()[,1]
                  Fecha <- strptime(x=Fecha, format=input$formato_fecha)
                  
                  mydata <- cbind(Fecha=Fecha,as.data.frame(datasetInputGvis_edit()[c((length(columnas)+1):(2*length(columnas)))]))
                  
                  # Profundidad:
                  if(input$prof_cte=="Si") prof <- rep(as.numeric(input$prof_val),nrow(mydata)) else # Constante
                    prof <- mydata[,noquote(input$col_prof)] # No constante
                  
                  pres <- 0.9806652*prof
                  
                  # Temperatura:
                  if(input$temp_cte=="Si") temp <- rep(as.numeric(input$temp_val),nrow(mydata)) else # Constante
                    temp <- mydata[,noquote(input$col_temp)] # No constante
                  
                  # Conductividad:
                  if(input$cond_cte=="Si") cond <- rep(as.numeric(input$cond_val),nrow(mydata)) else # Constante
                    cond <- mydata[,noquote(input$col_cond)] # No constante
                  
                  salinidad <- round(swSCTp(conductivity=cond, temperature=temp, pressure=pres, conductivityUnit=input$uni_cond),digits=2)        
                  salinidad <- data.frame(Salinidad_UPS=salinidad)
                  
                  return(salinidad)                  
                }
  
    return(salinidad)
    
    })
  datasetInputGvis_edit <- reactive({
   
      columnas <- colnames(datasetInput())[-1]
      
      celdas <- celdas_flags_input_tabla()
      valores <- valores_flags_input_tabla()
                    
      datos_lista <- list(NA)
      for(i in 1:(3*length(columnas)+1))
        datos_lista[[i]] <- as.factor(NA)
      names(datos_lista) <- c(columnas, paste(columnas,"_p", sep=""), paste(columnas,"_e", sep=""), "Celda")
      
      # Ingresar valores originales a lista
      for(i in 1:length(columnas))
        datos_lista[[i]] <- datasetInput()[,i+1]
      
      # Ingresar NAs a lista de errores
      for(i in (2*(length(columnas))+1):(3*length(columnas)))
        datos_lista[[i]] <- as.numeric(rep(NA, nrow(datasetInput())))
      
      # Ingresar NAs a lista procesados
      for(i in (length(columnas)+1):(2*length(columnas)))
        datos_lista[[i]] <- as.numeric(rep(NA, nrow(datasetInput())))
      
      # Lista de índice de celdas (secuencia)
      datos_lista$Celda <- seq(1:nrow(datasetInput()))

      # Ingresar errores a listas 
      for(i in (2*(length(columnas))+1):(3*length(columnas)))
        datos_lista[[i]][as.numeric(celdas[[i-(2*length(columnas))]])[-1]] <- as.numeric(valores[[i-(2*length(columnas))]])[-1]
      
      # Ingresar datos procesados (sin errores)
      for(i in (length(columnas)+1):(2*length(columnas)))
        datos_lista[[i]][setdiff(x=datos_lista$Celda, y=as.numeric(celdas[[i-length(columnas)]])[-1])] <- datos_lista[[i-length(columnas)]][setdiff(x=datos_lista$Celda, y=as.numeric(celdas[[i-length(columnas)]])[-1])]
                  
      return(datos_lista)
      })
    
  ### Elegir columnas 
  # Plot
  output$choose_columns_plot <- renderUI({
    
    if(is.null(datasetInput())) return("")
    
    if(input$plot_editados=='datos_originales') {
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    
    # Create the checkboxes and select them all by default
    cols <- checkboxGroupInput("var", "",choices=colnames[-1], selected=colnames[2])
    
    return(cols)
    
    }
    
    if(input$plot_editados=='datos_editados') {
      
      # Get the data set with the appropriate name
      colnames <- paste(names(datasetInput()),"_p", sep="")
      
      # Create the checkboxes and select them all by default
      cols <- checkboxGroupInput("var", "",choices=colnames[-1], selected=colnames[2])
      
      return(cols)
      
    }
    
  })
  # X vs Y 
  output$choose_columns_xvsy <- renderUI({
    
    if(is.null(datasetInput())) return("")
    
    if(input$plot_editados=='datos_originales') {
      
      # Get the data set with the appropriate name
      colnames <- names(datasetInput())
      
      # Create the checkboxes and select them all by default
      cols <- checkboxGroupInput("var_xvsy", "",choices=colnames[-1], selected=c(colnames[2],colnames[3]))
      
      return(cols)
      
    }
    
    if(input$plot_editados=='datos_editados') {
      
      # Get the data set with the appropriate name
      colnames <- paste(names(datasetInput()),"_p", sep="")
      
      # Create the checkboxes and select them all by default
      cols <- checkboxGroupInput("var_xvsy", "",choices=colnames[-1], selected=c(colnames[2],colnames[3]))
      
      return(cols)
      
    }
    
  })
  # Hist
  output$choose_columns_hist <- renderUI({
    
    if(is.null(datasetInput())) return("")
    
    if(input$plot_editados=='datos_originales') {
      
      # Get the data set with the appropriate name
      colnames <- names(datasetInput())
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("var_hist", "", choices=colnames[-1], selected=colnames[2])
      
      return(cols)
      
    }
    if(input$plot_editados=='datos_editados') {
      
      # Get the data set with the appropriate name
      colnames <- paste(names(datasetInput()),"_p", sep="")
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("var_hist", "", choices=colnames[-1], selected=colnames[2])
      
      return(cols)
      
    }
    
  })
  # Editar
  output$choose_columns2 <- renderUI({
    if(is.null(datasetInputGvis()))
      return("")
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    
    # Create the checkboxes and select them all by default
    selectInput("var_dos", "", choices=colnames[-1], selected=colnames[2])
  })
  # Conductividad
  output$col_cond <- renderUI({
    if(is.null(datasetInputGvis())) return("")
    
    if(input$sal_editados=='datos_originales') {
      
      # Get the data set with the appropriate name
      colnames <- names(datasetInput())
      busqueda <- grep(pattern="cond", x=colnames, ignore.case=TRUE, value=FALSE)
      
      if(length(busqueda)==0) select <- colnames[1]
      if(length(busqueda)==1) select <- colnames[busqueda]
      if(length(busqueda) > 1) select <- colnames[busqueda[1]]
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("col_cond", "Elegir variable", choices=colnames[-1], selected=select)
            
      return(cols)
    }
    if(input$sal_editados=='datos_editados') {
      
      # Get the data set with the appropriate name
      colnames <- paste(names(datasetInput()), "_p", sep="")
      busqueda <- grep(pattern="cond", x=colnames, ignore.case=TRUE, value=FALSE)
      
      if(length(busqueda)==0) select <- colnames[1]
      if(length(busqueda)==1) select <- colnames[busqueda]
      if(length(busqueda) > 1) select <- colnames[busqueda[1]]
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("col_cond", "Elegir variable", choices=colnames[-1], selected=select)
      
      return(cols)
      
    }
  })
  # Temperatura
  output$col_temp <- renderUI({
    if(is.null(datasetInputGvis())) return("")
    
    if(input$sal_editados=='datos_originales') {
      
      # Get the data set with the appropriate name
      colnames <- names(datasetInput())
      busqueda <- grep(pattern="temp", x=colnames, ignore.case=TRUE, value=FALSE)
      
      if(length(busqueda)==0) select <- colnames[1]
      if(length(busqueda)==1) select <- colnames[busqueda]
      if(length(busqueda) > 1) select <- colnames[busqueda[1]]
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("col_temp", "Elegir variable", choices=colnames[-1], selected=select)
      
      return(cols)
    }
    if(input$sal_editados=='datos_editados') {
      
      # Get the data set with the appropriate name
      colnames <- paste(names(datasetInput()), "_p", sep="")
      busqueda <- grep(pattern="temp", x=colnames, ignore.case=TRUE, value=FALSE)
      
      if(length(busqueda)==0) select <- colnames[1]
      if(length(busqueda)==1) select <- colnames[busqueda]
      if(length(busqueda) > 1) select <- colnames[busqueda[1]]
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("col_temp", "Elegir variable", choices=colnames[-1], selected=select)
      
      return(cols)
      
    }
  })
  # Profundidad
  output$col_prof <- renderUI({
    if(is.null(datasetInputGvis())) return("")
    
    if(input$sal_editados=='datos_originales') {
      
      # Get the data set with the appropriate name
      colnames <- names(datasetInput())
      busqueda <- grep(pattern="pres", x=colnames, ignore.case=TRUE, value=FALSE)
      
      if(length(busqueda)==0) select <- colnames[1]
      if(length(busqueda)==1) select <- colnames[busqueda]
      if(length(busqueda) > 1) select <- colnames[busqueda[1]]
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("col_prof", "Elegir variable", choices=colnames[-1], selected=select)
      
      return(cols)
    }
    if(input$sal_editados=='datos_editados') {
      
      # Get the data set with the appropriate name
      colnames <- paste(names(datasetInput()), "_p", sep="")
      busqueda <- grep(pattern="pres", x=colnames, ignore.case=TRUE, value=FALSE)
      
      if(length(busqueda)==0) select <- colnames[1]
      if(length(busqueda)==1) select <- colnames[busqueda]
      if(length(busqueda) > 1) select <- colnames[busqueda[1]]
      
      # Create the checkboxes and select them all by default
      cols <- selectInput("col_prof", "Elegir variable", choices=colnames[-1], selected=select)
      
      return(cols)
      
    }
    
  })
  
  ### Plot
  output$plot <- renderGvis({
      
    if(input$tipo_plot=='plot') {
      
      if(input$plot_editados=='datos_editados') {
        
        #Fecha <- datasetInputGvis()[,1]
        #df_plot <- cbind(Fecha=Fecha,as.data.frame(datasetInputGvis_edit()[c((length(columnas)+1):(2*length(columnas)))]))
        
        df_plot <- datasetInputGvis()
                
        df_plot_reshape <- reshape(data=df_plot, varying=input$var, v.names="Valores", timevar="Variable", times=input$var, direction="long", idvar="Valores_ID", drop=setdiff(names(df_plot),c("Fecha",input$var)))
        
        return(gvisAnnotatedTimeLine(data=df_plot_reshape, datevar="Fecha", numvar="Valores", idvar="Variable", options=list(gvis.language="es", dateFormat='HH:mm - dd/MM/yyyy', width=750, height=450, scaleType=input$eje_y, scaleColumns='[0,1,2]', fill=input$fill)))
        
      }
      
      if(input$plot_editados=='datos_originales') {
        
        df_plot <- datasetInputGvis()
        
        df_plot_reshape <- reshape(data=df_plot, varying=input$var, v.names="Valores", timevar="Variable", times=input$var, direction="long", idvar="Valores_ID", drop=setdiff(names(df_plot),c("Fecha",input$var)))
        
        return(gvisAnnotatedTimeLine(data=df_plot_reshape, datevar="Fecha", numvar="Valores", idvar="Variable", options=list(gvis.language="es", dateFormat='HH:mm - dd/MM/yyyy', width=750, height=450, scaleType=input$eje_y, scaleColumns='[0,1,2]', fill=input$fill)))
        
      }      
      
    }
        
    })
  
  ### X vs Y
  output$xvsy <- renderGvis({
    
    if(input$tipo_plot=='xvsy') {
      
      if(input$log == FALSE) df_xvsy <- datasetInputGvis()[,c(input$var_xvsy)]
      if(input$log == TRUE) df_xvsy <- log(datasetInputGvis()[,c(input$var_xvsy)]+1) 
      
      #intercepto <- lm(df_xvsy[,1]~df_xvsy[,2])$coefficients[1]
      #pendiente <- lm(df_xvsy[,1]~df_xvsy[,2])$coefficients[2]
      #modelo_lineal <- curve(expr=pendiente*x+intercepto,from=min(df_xvsy[,1],na.rm=TRUE),to=max(df_xvsy[,1],na.rm=TRUE),n=length(df_xvsy[,1]))$y
      #df_xvsy <- cbind(df_xvsy,modelo_lineal)
      # , 1:{color:'red', visibleInLegend:false, curveType:'function', lineWidth:1, pointSize:0}
      
      gvis_xvsy <- gvisScatterChart(data=df_xvsy, chartid="scatterplot", options=list(legend="none", width=900, height=600, series="{0:{color:'black', visibleInLegend:false, curveType:'none', lineWidth:0, pointSize:2}}", vAxis=paste("{title:","'",input$var_xvsy[1],"'","}",sep=""),  hAxis=paste("{title:","'",input$var_xvsy[2],"'","}",sep="")))    
      
      return(gvis_xvsy)
      
    }
  })
                      
  ### Hist  
  output$hist <- renderPlot({
    
    if(input$tipo_plot=='hist'){
      
      if(input$plot_editados=='datos_originales') {
      
    df <- datasetInputGvis()
    var <- as.numeric(df[,input$var_hist])
    
    if (length(which(is.na(var))) == 0) var <- var else
      var <- var[-which(is.na(var))]
    
    histograma <- hist(x=var)
    plot(histograma, col="black", border="white", ylab="Frecuencia", xlab="", main=paste(input$var_hist,sep=" "))
      }
      if(input$plot_editados=='datos_editados') {
        
        df <- datasetInputGvis()
        var <- as.numeric(df[,input$var_hist])
        
        if (length(which(is.na(var))) == 0) var <- var else
          var <- var[-which(is.na(var))]
        
        histograma <- hist(x=var)
        plot(histograma, col="black", border="white", ylab="Frecuencia", xlab="", main=paste(input$var_hist,sep=" "))
      }
      
      
      
    }
    
    })
  
  ### Reactive Values: TABLA
  celdas_flags_input_tabla <- reactive(function(){
    
    if(is.na(input$selected_b)) return(NULL)
    celdas_flags_tabla[[input$var_dos]] <<- c(celdas_flags_tabla[[input$var_dos]], input$selected_b)
    return(celdas_flags_tabla)
    })
  valores_flags_input_tabla <- reactive(function(){
    
    if(is.na(input$selected)) return(NULL)   
    valores_flags_tabla[[input$var_dos]] <<- c(valores_flags_tabla[[input$var_dos]], input$selected)
    return(valores_flags_tabla)
    
  })
  
  ### Edit ScatterChart
  output$edit <- renderGvis({
          
    jscode <- "var sel = chart.getSelection();
                 var row = sel[0].row;
                 var text = data.getValue(row, 1);
                 $('input#selected').val(text);
                 $('input#selected').trigger('change');
                 $('input#selected_b').val(row+1);
                 $('input#selected_b').trigger('change');"
    
    int <- as.numeric(input$int)
    atras <- as.numeric(input$atras)
    siguiente <- as.numeric(input$siguiente)
    
    min_m <- int*(siguiente - atras)
    max_m <- int*(siguiente - atras) + int
    
    if(min_m <= 0) {
      min_m <- 1
      max_m <- int
    }
      
    df <- as.data.frame(cbind(X=unlist(datasetInputGvis_edit()["Celda"]), 
                              Datos_originales=datasetInputGvis_edit()[[input$var_dos]],
                              Errores=datasetInputGvis_edit()[[paste(input$var_dos,"_e",sep="")]],
                              Datos_corregidos=datasetInputGvis_edit()[[paste(input$var_dos,"_p",sep="")]]))

    
    gvis_sc <- gvisScatterChart(data=df,chartid="scatterplot",options=list(width=790,height=450,theme="maximized",gvis.listener.jscode=jscode,title=input$var_dos,series="{0:{color:'black', visibleInLegend:true, curveType:'function', lineWidth:0.25, pointSize:1},1:{color:'red', visibleInLegend:true, curveType:'none', lineWidth:0, pointSize:2},2:{color:'blue', visibleInLegend:true, curveType:'function', lineWidth:1, pointSize:1}}",hAxis=paste("{viewWindowMode:'explicit', viewWindow:{max:",max_m,",min:",min_m,"}}",sep="")))    
    
    return(gvis_sc)
    
    })
  
  output$selectedOut <- renderUI({
    numericInput("selected", "", value="")
  })
  output$selectedOut2 <- renderUI({
    numericInput("selected_b", "", value="")
  })
  
  outputOptions(output, "selectedOut", suspendWhenHidden=TRUE)
  outputOptions(output, "selectedOut2", suspendWhenHidden=TRUE)  
  
  ### Resumen
  output$summary <- renderGvis({
    
    if(input$resumen_editados=='datos_originales') {
    
    datos <- datasetInput()
    tabla_resumen <- function(datos){
      
      columnas <- colnames(datos)[-1]
      
      mat <- matrix(nrow=length(columnas), ncol=6, dimnames=list(c(columnas),c("Promedio", "ds", "Mediana", "Máximo", "Mínimo", "n")))
      
      # Promedios
      promedios <- numeric()
      for(i in 1:length(columnas))
        promedios[i] <- mean(datos[,columnas[i]], na.rm=TRUE)
      
      # Ds
      ds <- numeric()
      for(i in 1:length(columnas))
        ds[i] <- sd(datos[,columnas[i]], na.rm=TRUE)
      
      # Mediana
      med <- numeric()
      for(i in 1:length(columnas))
        med[i] <- median(datos[,columnas[i]], na.rm=TRUE)
      
      # Máximo
      max <- numeric()
      for(i in 1:length(columnas))
        max[i] <- max(datos[,columnas[i]], na.rm=TRUE)
      
      # Mínimo
      min <- numeric()
      for(i in 1:length(columnas))
        min[i] <- min(datos[,columnas[i]], na.rm=TRUE)
      
      # n
      n <- numeric()
      for(i in 1:length(columnas))
        n[i] <- length(datos[,columnas[i]])
      
      # Cargar datos a matriz
      
      mat[1:nrow(mat),1] <- promedios
      mat[1:nrow(mat),2] <- ds
      mat[1:nrow(mat),3] <- med
      mat[1:nrow(mat),4] <- max
      mat[1:nrow(mat),5] <- min
      mat[1:nrow(mat),6] <- n
      
      return(round(mat, digits=2))
      
    }
    
    mat <- data.frame("Variable"=colnames(datos)[-1], as.data.frame(tabla_resumen(datos=datos),row.names=FALSE))
    
    resumen_datos <- gvisTable(data=mat)
    return(resumen_datos)
    
    }
    else
      if(input$resumen_editados=='datos_editados') {
      
      Fecha <- datasetInput()[,1]
      Fecha <- strptime(x=Fecha, format=input$formato_fecha)
      
      datos <- cbind(Fecha=Fecha,as.data.frame(datasetInputGvis_edit()[c((length(columnas)+1):(2*length(columnas)))]))
      
      tabla_resumen <- function(datos){
        
        columnas <- colnames(datos)[-1]
        
        mat <- matrix(nrow=length(columnas), ncol=6, dimnames=list(c(columnas),c("Promedio", "ds", "Mediana", "Máximo", "Mínimo", "n")))
        
        # Promedios
        promedios <- numeric()
        for(i in 1:length(columnas))
          promedios[i] <- mean(datos[,columnas[i]], na.rm=TRUE)
        
        # Ds
        ds <- numeric()
        for(i in 1:length(columnas))
          ds[i] <- sd(datos[,columnas[i]], na.rm=TRUE)
        
        # Mediana
        med <- numeric()
        for(i in 1:length(columnas))
          med[i] <- median(datos[,columnas[i]], na.rm=TRUE)
        
        # Máximo
        max <- numeric()
        for(i in 1:length(columnas))
          max[i] <- max(datos[,columnas[i]], na.rm=TRUE)
        
        # Mínimo
        min <- numeric()
        for(i in 1:length(columnas))
          min[i] <- min(datos[,columnas[i]], na.rm=TRUE)
        
        # n
        n <- numeric()
        for(i in 1:length(columnas))
          n[i] <- length(datos[,columnas[i]])
        
        # Cargar datos a matriz
        
        mat[1:nrow(mat),1] <- promedios
        mat[1:nrow(mat),2] <- ds
        mat[1:nrow(mat),3] <- med
        mat[1:nrow(mat),4] <- max
        mat[1:nrow(mat),5] <- min
        mat[1:nrow(mat),6] <- n
        
        return(round(mat, digits=2))
        
      }
      
      mat <- data.frame("Variable"=colnames(datos)[-1], as.data.frame(tabla_resumen(datos=datos),row.names=FALSE))
      
      resumen_datos <- gvisTable(data=mat)
      return(resumen_datos)
      
    }

  })
  
  ### Tabla
  output$table <- renderDataTable({
    if (is.null(input$file1) & is.null(input$file2)) return(NULL)
    else return(datasetInput())
  }, options= list(aLengthMenu= c(10, 25, 50, 100), iDisplayLength= 10, bSortClasses = TRUE))
  
  ### Conversión de Conductividad
  output$table2 <- renderDataTable({
    
    return(datasetInputSalinidad())
    
    }, options = list(aLengthMenu= c(10, 25, 50, 100), iDisplayLength= 10, bSortClasses= TRUE))

  ### Descarga de datos editados
  output$descarga_edit <- downloadHandler(
    filename= function() {
      paste('datos_procesados','.csv', sep='')
      },
    content= function(con) {
      Fecha <- datasetInput()[,1]
      Fecha <- strptime(x=Fecha, format=input$formato_fecha)
      datos_editados <- cbind(Fecha=Fecha,as.data.frame(datasetInputGvis_edit()[c((length(columnas)+1):(2*length(columnas)))]))
      write.table(x=cbind(datasetInput(),datos_editados), file=con, sep=",", row.names=FALSE, quote=FALSE)},
    contentType="text/csv")
  
  ### Descarga de salinidad
  output$descarga <- downloadHandler(
    filename= function() {
      paste('datos','.csv', sep='')
      },
    content= function(con) {
      if(input$desc_sal=="Solo salinidad") {
        write.table(x=datasetInputSalinidad(), file=con, sep=",", row.names=FALSE, col.names="Salinidad_UPS",quote=FALSE)}
      if(input$desc_sal=="Salinidad junto con datos cargados" & input$sal_editados=='datos_originales') {
            write.table(x=cbind(datasetInput(),datasetInputSalinidad()), file=con, sep=",", row.names=FALSE, col.names=c(colnames(datasetInputGvis()),"Sal_UPS"),quote=FALSE) }
      if(input$desc_sal=="Salinidad junto con datos cargados" & input$sal_editados=='datos_editados') {
      Fecha <- datasetInput()[,1]
      Fecha <- strptime(x=Fecha, format=input$formato_fecha)
      datos_editados <- cbind(Fecha=Fecha,as.data.frame(datasetInputGvis_edit()[c((length(columnas)+1):(2*length(columnas)))]))
        write.table(x=cbind(datasetInput(),datos_editados,datasetInputSalinidad()), file=con, sep=",", row.names=FALSE, col.names=c(colnames(datasetInput()),colnames(datos_editados),"Sal_UPS_proc"),quote=FALSE)
      }
      },
    contentType="text/csv")
  })
