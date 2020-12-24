#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 geom_line labs geom_col geom_text ggsave aes
#' @noRd

app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  options(future.supportsMulticore.unstable = "quiet")
  options(stringsAsFactors=FALSE)
  options(shiny.reactlog=TRUE)
  shinyOptions(cache = memoryCache(max_size = 300e6))
  future::plan("multiprocess")
  onStop(function(state) future::plan("sequential"))
  
  i = 0
  paramFile_header <- c("name", "rt_left", "rt_right", "Mass0", "LabelAtoms")
  paramFile_num_header <- c("rt_left", "rt_right", "Mass0", "LabelAtoms")
  # onStop(parallel::stopCluster(cl))
  
  # output[["mass0"]] <- NULL
  # outputOptions(output, "mass0", suspendWhenHidden = FALSE)
  
  
  directory <- eventReactive(input$folderButton, {
    # browser()
    # req(input$fileButton)
    req(paramFile())
    
    tryCatch(
      
      # if (isNamespaceLoaded("rstudioapi")) {
      #   
      #   rstudioapi::selectDirectory()
      # } else {
      #   
      easycsv::choose_dir()
      
      # }
      ,
      warning = function(cond) {message("Please choose a valid directory"); return(NULL) }
      # error = function(cond) {message(); return(NULL)}
    )
    
  })
  
  # observeEvent(input$saveButton, {
  #   xlsx::write.xlsx(ParameterFile.tbl(), file = paramFile())
  #    showNotification("Saved !")
  # })
  
  
  rawData <- reactive({
    # browser()
    req(directory())
    
    # browser()
    # req(input$fileButton != 0)
    #if (!is.null(directory())) {
    progressr::withProgressShiny(message = 'Extracting data ...', value = 0, {
      
      netCDFs <- list.files(paste0(directory(),"/Netcdfs"), full.names = TRUE, pattern = ".CDF$", ignore.case = T)
      
      p <- progressr::progressor(along = netCDFs)
      # browser()
      tmp <- future.apply::future_lapply(seq_along(netCDFs), function(i) {
        p()
        
        data <- MSnbase::readMSData(gtools::mixedsort(netCDFs)[i], mode = "onDisk", msLevel. = 1)
        df <- methods::as(data,  "data.frame") %>% data.table::setDT()
        df$phenoData <- data@phenoData %>% rownames()
        df
      })
      
      plotIndex <<- findInterval(length(tmp)/2, seq_len(length(tmp)), all.inside = T)
    })
    tmp
    # } else { NULL }
    
  })
  
  
  output$selectedFragment <- renderUI({
    # req(input$tabs)
    req(rawData(), paramFileTable())
    # req(input$tabs == "SIM" | input$tabs == "MSpectrum" | input$tabs == "TIC" | input$tabs == "Parameters")
    
    # if (isolate(input$tabs) == "TIC") {
    # 
    selectInput('selectedFragment', 'Fragments', isolate(c("TIC", values[["DF"]][[1]])), multiple= F, selectize=TRUE)
    # 
    # } else {
    
    # selectInput('selectedFragment', 'Fragments', isolate(values[["DF"]][[1]]), multiple= F, selectize=TRUE)
    
    # }
  })
  
  # output$selectedFragment_tic <- renderUI({
  #   # req(input$tabs)
  #   req(rawData(), paramFileTable(), isolate(input$tabs) == "TIC")
  #   # req(input$tabs == "SIM" | input$tabs == "MSpectrum" | input$tabs == "TIC" | input$tabs == "Parameters")
  # 
  #   selectInput('selectedFragment', 'Fragments', c("TIC", isolate(values[["DF"]][[1]])), multiple= F, selectize=TRUE)
  # 
  # })
  
  output$plot_hover_coord <- renderPrint({
    
    # browser()
    req(!is.null(names(input)))
    
    list_plots_hover <- grep("plot_hover", names(input), value = T)
    plot_hover_data <- lapply(list_plots_hover, function(x) {
      
      input[[x]]
      
    })
    
    plot_hover_data <- unlist(plot_hover_data)
    
    validate(
      if (!is.null(plot_hover_data)) {
        NULL
      } else {
        ""
      }
    )
    
    plot_hover_data <- plot_hover_data[1:2] %>% as.numeric() %>%  round(digits = 1)
    names(plot_hover_data) <- c("x","y")
    plot_hover_data
  })
  
  # output$saveActivePlotsButton <- renderUI({
  #   
  #   req(rawData(), input$tabs != "Parameters")
  #   
  #   actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$saveTotableButton <- renderUI({
  #   
  #   req(rawData(), input$tabs == "SIM" | input$tabs == "Parameters")
  #   
  #   actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$undoButton <- renderUI({
  #   
  #   req(rawData(), input$tabs == "Parameters")
  #   
  #   actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px")
  #   
  # })
  # 
  # output$runButton <- renderUI({
  #   
  #   req(rawData(), input$tabs == "Parameters")
  #   
  #   # For jscode2 to work. Binding enterkey to runButton
  #   tagList(
  #     tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
  #     ),
  #     
  #   )
  # })
  
  observeEvent({
    req(rawData(), paramFileTable())
    input$selectedFragment
    # input$tabs
    #  paramFileTable
  }, {
    
    # browser()
    # req(rawData(), paramFileTable())
    
    output$rtime <- renderUI({
      
      # req(input$tabs == "MSpectrum")
      # browser()
      
      numericInput(inputId = "rtime",
                   label = "Retention time",
                   #min = min(rawData1()@featureData@data$retentionTime),
                   #max = max(rawData1()@featureData@data$retentionTime),
                   #value = rows(paramFileTable(),paramFileTable()[[1]] ==input$selectedFragment)[[2]], #max(rawData1()@featureData@data$retentionTime)/2,
                   value = (paramFileTable()[name == input$selectedFragment, 2] + paramFileTable()[name == input$selectedFragment, 3])/2, # to be used with  input$selectedFragment2: as.numeric(gsub(".*@",replacement = "",  input$selectedFragment2)),
                   step = 0.1)
    })
    
    output$rtimeL <- renderUI({
      
      # req(input$tabs != "MSpectrum")
      
      
      if (input$selectedFragment == "TIC" && input$tabs == "TIC") {
        
        numericInput(inputId = "rtimeL",
                     label = "Retention time (left)",
                     #min = min(rawData1()@featureData@data$retentionTime),
                     #max = max(rawData1()@featureData@data$retentionTime),
                     value = tryCatch(round(min(rawData()[[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2-10,
                     step = 0.1)
        
      } else {
        
        numericInput(inputId = "rtimeL",
                     label = "Retention time (left)",
                     #min = min(rawData1()@featureData@data$retentionTime),
                     #max = max(rawData1()@featureData@data$retentionTime),
                     value = paramFileTable()[name == input$selectedFragment, 2], # max(rawData1()@featureData@data$retentionTime)/2-10,
                     step = 0.1)
        
      }
    })
    
    output$rtimeR <- renderUI({
      
      # req(input$tabs != "MSpectrum")
      
      
      if (input$selectedFragment == "TIC" && input$tabs == "TIC") {
        
        
        numericInput(inputId = "rtimeR",
                     label = "Retention time (right)",
                     #min = min(rawData1()@featureData@data$retentionTime),
                     #max = max(rawData1()@featureData@data$retentionTime),
                     value = tryCatch(round(max(rawData()[[1]]$rt)/60), error = function(cond) {message(); return(NULL)}), # max(rawData1()@featureData@data$retentionTime)/2+10,
                     step = 0.1)
        
      } else {
        
        numericInput(inputId = "rtimeR",
                     label = "Retention time (right)",
                     #min = min(rawData1()@featureData@data$retentionTime),
                     #max = max(rawData1()@featureData@data$retentionTime),
                     value = paramFileTable()[name == input$selectedFragment, 3], # max(rawData1()@featureData@data$retentionTime)/2+10,
                     step = 0.1)
        
      }
    })
    
    output$mass0 <- renderUI({
      
      # req(input$tabs == "SIM" | input$tabs == "Parameters")
      
      
      numericInput(inputId = "mass0",
                   label = "mass0 (M0)",
                   #min = 40,
                   #max = 600,
                   value = paramFileTable()[name == input$selectedFragment, 4],
                   step = 1
      )
    })
    
    output$N_atom <- renderUI({
      
      # req(input$tabs == "SIM" | input$tabs == "Parameters")
      
      
      numericInput(inputId = "N_atom",
                   label = "Number of isotopomers",
                   #min = 1,
                   #max = 10,
                   value = paramFileTable()[name == input$selectedFragment, 5],
                   step = 1
      )
    })
    
    
    output$mzd <- renderUI({
      
      # req(input$tabs == "SIM" | input$tabs == "Parameters")
      
      
      numericInput(inputId = "mzd",
                   label = "Mass difference",
                   #min = 0,
                   #max = 1,
                   value = {if ( input$selectedFragment == "TIC") {NULL} else {0.3}},
                   step = 0.1
      )
    })
    
    output$labelThreshold <- renderUI({
      
      # req(input$tabs == "MSpectrum")
      
      
      numericInput(inputId = "labelThreshold",
                   label = "Threshold intensity for labels",
                   #min = 0,
                   #max = 1,
                   value = {if ( input$selectedFragment == "TIC") {NULL} else {10}},
                   step = 5
      )
    })
    
    # cat(file=stderr(),"at inputs:", input$mass0)
    
    
  },ignoreInit = T, priority = 2)
  
  
  observeEvent({
    req(rawData(), paramFileTable())
    input$selectedFragment
    # input$selectedFragment_tic
    input$tabs
    #  paramFileTable
  }, {
    
    # browser()
    # req(rawData(), paramFileTable())
    
    
    switch (input$tabs,
            'TIC' = {
              
              shinyjs::showElement("rtimeL")
              shinyjs::showElement("rtimeR")
              shinyjs::hideElement("rtime")
              shinyjs::hideElement("labelThreshold")
              shinyjs::hideElement("mzd")
              shinyjs::hideElement("N_atom")
              shinyjs::hideElement("mass0")
              
              output$saveActivePlotsButton <- renderUI({
                
                req(input$tabs != "Parameters")
                
                actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
                
              })
              
              output$runButton <- renderUI({
                
                req(input$tabs != "Parameters")
                
                # For jscode2 to work. Binding enterkey to runButton
                tagList(
                  tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
                  ),
                  
                )
              })
              
            } ,
            'MSpectrum' = {
              
              shinyjs::hideElement("rtimeL")
              shinyjs::hideElement("rtimeR")
              shinyjs::showElement("rtime")
              shinyjs::showElement("labelThreshold")
              shinyjs::hideElement("mzd")
              shinyjs::hideElement("N_atom")
              shinyjs::hideElement("mass0")
              
              output$saveActivePlotsButton <- renderUI({
                
                req(rawData(), input$tabs != "Parameters")
                
                actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
                
              })
              
              output$runButton <- renderUI({
                
                req(rawData(), input$tabs != "Parameters")
                
                # For jscode2 to work. Binding enterkey to runButton
                tagList(
                  tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
                  ),
                  
                )
              })
              
              
            },
            
            'SIM' = {
              
              shinyjs::showElement("rtimeL")
              shinyjs::showElement("rtimeR")
              shinyjs::hideElement("rtime")
              shinyjs::hideElement("labelThreshold")
              shinyjs::showElement("mzd")
              shinyjs::showElement("N_atom")
              shinyjs::showElement("mass0")
              
              # req(input$tabs == 'SIM')
              
              output$saveActivePlotsButton <- renderUI({
                
                req(rawData(), input$tabs != "Parameters")
                
                actionButton("saveActivePlotsButton", "Save active plots", width = "100%", style="margin-bottom:8px")
                
              })
              
              output$saveTotableButton <- renderUI({
                
                req(rawData(), input$tabs == "SIM" | input$tabs == "Parameters")
                
                actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px")
                
              })
              
              output$runButton <- renderUI({
                
                req(rawData(), input$tabs != "Parameters",cancelOutput = T)
                
                # For jscode2 to work. Binding enterkey to runButton
                tagList(
                  tagAppendAttributes(actionButton("run", "run", width = "100%", style="margin-bottom:8px"), `data-proxy-click` = "run"
                  ),
                  
                )
              })
              
              
            },
            
            'Parameters' = {
              
              shinyjs::showElement("rtimeL")
              shinyjs::showElement("rtimeR")
              shinyjs::hideElement("rtime")
              shinyjs::hideElement("labelThreshold")
              shinyjs::showElement("mzd")
              shinyjs::showElement("N_atom")
              shinyjs::showElement("mass0")
              
              # req(input$tabs == "Parameters")
              
              output$saveTotableButton <- renderUI({
                
                req(rawData(), input$tabs == "SIM" | input$tabs == "Parameters")
                
                actionButton("saveTotable", "Save to table", width = "100%", style="margin-bottom:8px")
                
              })
              
              output$undoButton <- renderUI({
                
                req(rawData(), input$tabs == "Parameters")
                
                actionButton("undoButton","undo", width = "100%", style="margin-bottom:8px")
                
              })
            }
    )
    
    
    
    # cat(file=stderr(),"at inputs:", input$mass0)
    
    
  }, priority = 1,ignoreInit = T,label = "inputs")
  
  # Solution: use updateNumericInput to maybe flush directly to the client. 
  # # OK: ajouteau debut de chaque observevent pour creer des plots (sim, tic, mspec) les valeurs, ex:
  #   
  #   N_atom <- paramFileTable()[name == input$selectedFragment, 5] etc pour decoreller UI et SEVER. 
  # Ensuite sur les tabs SIM etc utilise updatecheckbox poour choisir selected = input$selected fragment, comme ca l utilisateur n arrive jamais sur le "TIC"
  
  # observeEvent({
  #   input$rtimeL
  #   input$rtimeR
  # }, {
  #   
  #   # observe({
  #   req(input$tabs == "TIC", input$rtimeL, input$rtimeR)# , input$selectedFragment, input$rtimeL, input$rtimeR)
  #   # browser()
  #   #  if (isTRUE(input$tabs == "TIC")) {  #  replaced by req(input$tabs == "TIC")
  #   
  #   TICplots <<- lapply(seq_along(rawData()), function(i) {
  #     
  #     #browser()
  #     
  #     DT <- rawData()[[i]][, .(tic = sum(i)), by = .(rt, phenoData)]
  #     dataIndexL <- MALDIquant::match.closest(input$rtimeL*60,DT$rt)
  #     dataIndexR <- MALDIquant::match.closest(input$rtimeR*60,DT$rt)
  #     DT <- DT[dataIndexL:dataIndexR]
  #     title = paste0("TIC_",DT$phenoData[[1]])
  #     
  #     tic_p  <- canvas_plots + ggplot2::geom_line(data = DT, aes(rt/60,tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
  #     
  #     
  #     output[[paste0("tic",i, "_f", input$folderButton)]] <- renderPlot({
  #       
  #       tic_p
  #       
  #     })
  #     return(tic_p)
  #     
  #   })
  #   
  #   output$myTicsA <- renderUI({
  #     # browser()
  #     # req(TICplots())
  #     # req(rawData())
  #     myTicsA =  lapply(seq_len(plotIndex), function(i) {
  #       
  #       plotOutput(paste0("tic",i, "_f", input$folderButton),hover = "plot_hover_tic1") # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
  #     })
  #     
  #     myTicsA
  #   })
  #   
  #   output$myTicsB <- renderUI({
  #     #browser()
  #     #   req(TICplots())
  #     req(length(rawData()) > 1)
  #     # create tabPanel with datatable in it
  #     myTicsB = lapply((plotIndex+1):length(rawData()), function(i) {
  #       
  #       plotOutput(paste0("tic",i, "_f", input$folderButton),hover = "plot_hover_tic2") # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
  #       #plotOutput(paste0("MSpectrum",i))
  #       
  #     })
  #     
  #     myTicsB # do.call(mainPanel, myTics)
  #   })
  #   #    }
  #   #})
  # },ignoreInit = T,once = T)
  
  # create tic
  observeEvent({
    req(input$run,cancelOutput = T)
    # input$run2
    req(input$tabs == "TIC")
    # input$selectedFragment
  }, {
    
    # browser()
    
    # observe({
    #req(input$tabs == "TIC", input$rtimeL, input$rtimeR)# , input$selectedFragment, input$rtimeL, input$rtimeR)
    # browser()
    #  if (isTRUE(input$tabs == "TIC")) {  #  replaced by req(input$tabs == "TIC")
    
    TICplots <<- lapply(seq_along(rawData()), function(i) {
      
      #browser()
      # OLD
      
      DT <- rawData()[[i]][, .(tic = sum(i)), by = .(rt, phenoData)]
      dataIndexL <- MALDIquant::match.closest(input$rtimeL*60,DT$rt)
      dataIndexR <- MALDIquant::match.closest(input$rtimeR*60,DT$rt)
      DT <- DT[dataIndexL:dataIndexR]
      
      if (input$selectedFragment == "TIC") {
        title = paste0("TIC_",DT$phenoData[[1]])
      } else { 
        title = paste0("TIC_", input$selectedFragment,"_",DT$phenoData[[1]]) 
      }
      
      tic_p  <- canvas_plots + ggplot2::geom_line(data = DT, aes(rt/60,tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
      
      
      output[[paste0("tic",i, "_f", input$folderButton)]] <- renderPlot({
        
        tic_p
        
      })
      return(tic_p)
    })
    
    output$myTicsA <- renderUI({
      #browser()
      #req(TICplots())
      # req(rawData())
      myTicsA =  lapply(seq_len(plotIndex), function(i) {
        
        plotOutput(paste0("tic",i, "_f", input$folderButton), hover = "plot_hover_tic1") # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
      })
      
      myTicsA
    })
    
    output$myTicsB <- renderUI({
      #browser()
      #   req(TICplots())
      req(length(rawData()) > 1)
      # create tabPanel with datatable in it
      myTicsB = lapply((plotIndex+1):length(rawData()), function(i) {
        
        plotOutput(paste0("tic",i, "_f", input$folderButton), hover = "plot_hover_tic2") # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
      myTicsB # do.call(mainPanel, myTics)
    })
    #    }
    #})
  },label = "tic")
  
  
  
  #scale = list(alternating = 1)
  
  # create mass spectrum
  
  #  MSpecplots <- reactive({
  
  observeEvent({
    req(input$run,cancelOutput = T)
    #input$run2
    req(input$tabs == "MSpectrum")
    # input$selectedFragment
  }, {
    
    
    req(input$tabs == "MSpectrum", input$rtime) # if I add input$selectedFragment != "TIC" here in req() isntead of the if loop it doesnt work properly (triggers plotting twice)
    
    
    #if (input$selectedFragment != "TIC") {
    #browser()
    
    MSpecplots <<- lapply(seq_along(rawData()), function(i) {
      
      
      # NEW VERSION
      rtime <- rawData()[[i]][, unique(rt)]
      specIndex <- MALDIquant::match.closest(input$rtime*60,rtime)
      # intensities <- DT2[rt == rtime[specIndex], i]
      # masses <- DT2[rt == rtime[specIndex], mz]
      DT <- rawData()[[i]][rt == rtime[specIndex], .(i = i/max(i)*100, mz,phenoData)]
      title = paste("MS_",input$selectedFragment,"_", DT$phenoData[[1]],"~", input$rtime, "min")
      
      # intensities <- subset.data.frame(rawData,rt == rtime[specIndex])$i
      # masses <-subset.data.frame(rawData()[[i]],rt == rtime[specIndex])$mz
      # df <- data.table::data.table("mz" = masses, "intensity" = intensities) %>% transform(intensity = intensity/max(intensity)*100)
      # df[,2] <- df[,2]/max(df[,2])*100
      # data.labels <- subset(df, intensity > input$labelThreshold)
      
      # FOR TESTING
      # rtime <- unique(rawData4[[1]]$rt)
      # specIndex <- MALDIquant::match.closest(18.2*60,rtime) #input$rtime
      # intensities <- subset.data.frame(rawData4[[1]],rt == rtime[specIndex])$i
      # masses <- subset.data.frame(rawData4[[1]],rt == rtime[specIndex])$mz
      # df <- data.table::data.table("mz" = masses, "intensity" = intensities) %>% transform(intensity = intensity/max(intensity)*100)
      # check: isTRUE(length(masses) == length(intensities))
      
      #browser()
      # position_nudge(x=3,y=2)
      data.labels <- DT[i > input$labelThreshold]
      # , position = "auto"
      mspec_p <- canvas_plots + ggplot2::geom_col(data = DT, aes(mz,i)) + ggplot2::geom_text(data= data.labels, aes(x= mz, y= i, label = round(i)), nudge_y = 2) + ggplot2::labs(title = title, x = "m/z", y = "Relative intensity")
      
      output[[paste0("MSpec",i, "_f", input$folderButton)]] <- renderPlot({
        
        mspec_p
      })
      
      return(mspec_p)
      
    })
    
    output$MSpecA <- renderUI({
      # req(MSpecplots())
      #req(rawData())
      # create tabPanel with datatable in it
      MSpecA =  lapply(seq_len(plotIndex), function(i) {
        
        plotOutput(paste0("MSpec",i, "_f", input$folderButton),hover = paste0("plot_hover_MSpec",i, "_f", input$folderButton)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
      MSpecA # do.call(mainPanel, myTics)
    })
    
    output$MSpecB <- renderUI({
      # req(MSpecplots())
      req(length(rawData()) > 1)
      # create tabPanel with datatable in it
      MSpecB = lapply((plotIndex+1):length(rawData()), function(i) {
        
        plotOutput(paste0("MSpec",i, "_f", input$folderButton),hover = paste0("plot_hover_MSpec",i, "_f", input$folderButton)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
      MSpecB # do.call(mainPanel, myTics)
    })
    
    #}
  },ignoreInit = T,label = "mspec")
  
  
  # create SIM
  #SIMplots <- reactive({
  
  observeEvent({
    req(input$run,cancelOutput = T)
    req(input$tabs == "SIM")
    # input$selectedFragment
  }, {
    
    
    browser()
    # req(input$tabs == "SIM", input$mass0, input$N_atom)
    
    SIMplots <<- lapply(seq_along(rawData()), function(i) {
      
      
      
      rtMatrix <- matrix(nrow = input$N_atom, ncol = 2)
      mzMatrix <- matrix(nrow = input$N_atom, ncol = 2)
      
      mzMatrix[,1] <- input$mass0:(input$mass0+(input$N_atom-1)) - input$mzd
      mzMatrix[,2] <- input$mass0:(input$mass0+(input$N_atom-1)) + input$mzd
      
      rtMatrix[,1] <- rep(input$rtimeL*60 , input$N_atom) # - 0.5*60,  RT low,# +1 to account for the M0 isotopomer
      rtMatrix[,2] <- rep(input$rtimeR*60, input$N_atom) # + 0.5*60
      
      # browser()
      
      myData <- lapply(1:nrow(rtMatrix), rawData=rawData()[[i]], function(i, rawData, rtL = rtL,rtR = rtL) {
        
        # browser()
        rtL <- MALDIquant::match.closest(input$rtimeL*60, rawData$rt)
        rtR <- MALDIquant::match.closest(input$rtimeR*60, rawData$rt)
        mzL <- mzMatrix[i, 1]
        mzR <- mzMatrix[i, 2]
        
        DT <- rawData[data.table::between(rt, rt[[rtL]], rt[[rtR]])][data.table::between(mz, mzL, mzR)]
        
      })
      
      rtL2 <- MALDIquant::match.closest(input$rtimeL*60, rawData()[[i]]$rt)
      rtR2 <- MALDIquant::match.closest(input$rtimeR*60, rawData()[[i]]$rt)
      
      #browser()
      # TO DO
      # beofre it was myData  <- do.call(rbind, myData). Check that bind rows work
      myData  <- dplyr::bind_rows(myData)
      
      if (nrow(myData) > 0) {
        myData
      } else {
        myData <- data.table::data.table(phenoData = rawData()[[i]]$phenoData[[1]])
        myData
      }
      
      title = paste0("SIM_",input$selectedFragment,"_",myData$phenoData[[1]])
      
      if (length(myData) != 1) {
        sim_p  <- canvas_plots + ggplot2::geom_line(data = myData, mapping = aes(x = rt/60, y = i, colour = factor(round(mz)))) + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity", col= "") 
      } else { 
        sim_p  <-  canvas_plots + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity")
      }
      
      output[[paste0("SIM",i, "_f", input$folderButton)]] <- renderPlot({ 
        sim_p
      })
      
      return(sim_p)
      
    })
    
    output$SIMA <- renderUI({
      #req(rawData())
      
      # create tabPanel with datatable in it
      SIMA <-  lapply(seq_len(plotIndex), function(i) {
        # req(SIMplots())
        plotOutput(paste0("SIM",i, "_f", input$folderButton),hover = paste0("plot_hover_SIM",i, "_f", input$folderButton)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
      SIMA # do.call(mainPanel, myTics)
    })
    
    output$SIMB <- renderUI({
      #req(SIMplots())
      req(length(rawData()) > 1)
      # create tabPanel with datatable in it
      SIMB <- lapply((plotIndex+1):length(rawData()), function(i) {
        
        plotOutput(paste0("SIM",i, "_f", input$folderButton),hover = paste0("plot_hover_SIM",i, "_f", input$folderButton)) #%>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        #plotOutput(paste0("MSpectrum",i))
        
      })
      
      SIMB # do.call(mainPanel, myTics)
    })
    
  },priority = 0,label = "sim")
  
  
  
  ################# THIS VERSION ADDS MISSING VALUES IN DATA.
  
  #   extractData <- reactive({
  # 
  # #browser()
  #     req(input$tabs == "SIM")
  #     req(input$N_atom)
  #     # req(input$tabs == "SIM", input$selectedFragment, input$rtimeR,input$rtimeL,input$N_atom,input$mass0, input$mzd)
  # 
  #      # req(rawData())
  # 
  #        if (!is.null(input$mass0) & !is.null(input$N_atom)) {
  #     # extractData.df <- vector("list",length = rawData())
  #     extractData <- lapply(rawData(), function(rawData) {
  # 
  #       
  #       
  # # rtMatrix <- matrix(nrow = 6, ncol = 2) #input$N_atom
  # # mzMatrix <- matrix(nrow = 6, ncol = 2) # input$N_atom, input$mass0
  # # rtMatrix[,1] <- rep(600, 6) # RT low,# +1 to account for the M0 isotopomer
  # # rtMatrix[,2] <- rep(700, 6)
  # # mzMatrix[,1] <- 459:(459+5) - 0.3
  # # mzMatrix[,2] <- 459:(459+5) + 0.3
  # 
  # rtMatrix <- matrix(nrow = input$N_atom, ncol = 2)
  # mzMatrix <- matrix(nrow = input$N_atom, ncol = 2)
  # 
  # mzMatrix[,1] <- input$mass0:(input$mass0+(input$N_atom-1)) - input$mzd
  # mzMatrix[,2] <- input$mass0:(input$mass0+(input$N_atom-1)) + input$mzd
  # 
  # rtMatrix[,1] <- rep(input$rtimeL*60 , input$N_atom) # - 0.5*60,  RT low,# +1 to account for the M0 isotopomer
  # rtMatrix[,2] <- rep(input$rtimeR*60, input$N_atom) # + 0.5*60
  # 
  # # browser()
  # 
  # myData <- lapply(1:nrow(rtMatrix), rawData=rawData, function(i, rawData, rtL = rtL,rtR = rtL) {
  # 
  # # browser()
  # rtL <- MALDIquant::match.closest(input$rtimeL*60, rawData$rt)
  # rtR <- MALDIquant::match.closest(input$rtimeR*60, rawData$rt)
  # mzL <- mzMatrix[i, 1]
  # mzR <- mzMatrix[i, 2]
  # 
  # DT <- rawData[rt %between% c(rt[[rtL]], rt[[rtR]])][mz %between% c(mzL, mzR)]
  # 
  # # # filter retention time
  # # rtFilter <- rawData %>% filter(between(rt,rt[[myRTL]] ,rt[[myRTR]]))
  # # 
  # # # filter mz
  # # mzFilter <- rtFilter %>% filter(between(mz, mzL , mzR))
  # 
  # 
  # })
  # 
  # rtL2 <- MALDIquant::match.closest(input$rtimeL*60, rawData$rt)
  # rtR2 <- MALDIquant::match.closest(input$rtimeR*60, rawData$rt)
  # 
  #   # browser()
  # myData  <- do.call(rbind, myData)
  # 
  # if (nrow(myData) > 0) {
  #   myData
  # } else {
  #   myData <- data.table(phenoData = rawData$phenoData[[1]])
  #   myData
  # }
  # 
  # #   myData <- data.table(rt= rawData[rt == rt[[rtL2]] | rt == rt[[rtR2]], unique(rt)], i = c(0,0), phenoData = rawData$phenoData[[1]])
  # 
  # # myData <- lapply(1:nrow(rtMatrix), function(i) {
  # # 
  # # myRTL <- MALDIquant::match.closest((12.67-0.07)*60, p[[1]]$rt)
  # # myRTR <- MALDIquant::match.closest((12.67+0.07)*60, p[[1]]$rt)
  # # mzL <- mzMatrix[i, 1]
  # # mzR <- mzMatrix[i, 2]
  # # 
  # # # filter retention time
  # # rtFilter <-p[[1]] %>% filter(between(rt,rt[[myRTL]] ,rt[[myRTR]]))
  # # 
  # # # filter mz
  # # mzFilter <- rtFilter %>% filter(between(mz, mzL , mzR))
  # # 
  # # 
  # # })
  # 
  # 
  # # browser()
  # 
  # # diff <- lapply(myData, function(i) i$rt) %>% unlist %>% unique
  # # phenoData <- lapply(myData, function(i) i$phenoData) %>% unlist %>% unique
  # # 
  # # 
  # # if (length(diff) != 0) {
  # # 
  # # addMissingValues <- mapply(i=myData,k = 1:length(myData), FUN = function(i,k) {
  # # 
  # #   if (nrow(i) == 0 ) {
  # # 
  # #     i = data.frame(file= 1, rt = diff[[1]], mz = round(mean(mzMatrix[k,])),i = 0,  phenoData = phenoData)
  # # 
  # #   } else {
  # #     i
  # #   }
  # # 
  # # },SIMPLIFY = F)
  # # 
  # # addMissingValues2 <- lapply(addMissingValues, function(k) {
  # # 
  # #   #browser()
  # #   for (i in diff) {
  # # 
  # #     if (i %in% k$rt) {
  # # 
  # #   } else {
  # # 
  # #     k <- bind_rows(k, data.frame(file= 1, rt= i, mz=k$mz[[1]], i = 0, phenoData = phenoData))
  # #   # it was bind_rows before instead of full_join!
  # #   }
  # #   }
  # # 
  # #   return(k)
  # # 
  # # })
  # # 
  # # do.call(rbind.data.frame, addMissingValues2) %>% arrange(rt)
  # # } else {
  # #    if (nrow(do.call(rbind.data.frame, myData)) == 0) {
  # #      
  # #   tmp  <- mapply(i=myData,k = 1:length(myData), function(i,k) {
  # #        i =  data.frame(file= 1, rt = (input$rtimeL + input$rtimeR)/2, mz = round(mean(mzMatrix[k,])), i = 0,  phenoData = "phenoData") 
  # #        }, SIMPLIFY = F)
  # #   do.call(rbind.data.frame, tmp) # same rt for all
  # #      
  # #    } else {
  # #   do.call(rbind.data.frame, myData) %>% arrange(rt)
  # #    }
  # # }
  # 
  # 
  # 
  # })
  #     extractData
  # 
  # } #if
  #     }) # reactive
  
  
  
  
  
  
  # output$debug <- renderText({
  #     c("You have selected", "folder:",input$folderButton, "file:",input$folderButton, "rtimeL:",input$rtimeL, "rtimeR:",input$rtimeL, input$selectedFragment, input$tabs,input$runParameters,input$mzd2, input$rtime) #input$mass0,input$rtimeL, input$N_atom,input$rtimeR,input$mzd
  #     
  # })
  
  #################################     ################################# New method
  
  # rtMatrix <- matrix(nrow = 8, ncol = 2) #input$N_atom
  # mzMatrix <- matrix(nrow = 8, ncol = 2) # input$N_atom, input$mass0
  # rtMatrix[,1] <- rep(500, 8) # RT low,# +1 to account for the M0 isotopomer
  # rtMatrix[,2] <- rep(700, 8)
  # mzMatrix[,1] <- 72:(72+7) - 0.3
  # mzMatrix[,2] <- 72:(72+7) + 0.3
  # 
  # fileNames(rawDataL)
  # sps <- rawDataL %>% filterRt(rt = c(500, 700)) # %>% filterMz(mz = mzMatrix[2,])
  # sps <- split(sps, fromFile(rawDataL))
  # 
  # myChrList <- lapply(1:length(fileNames(rawDataL)),function(i) {
  #   sps %>% filterMz(mz = mzMatrix[i,])
  #   
  # })
  # 
  # 
  # xcms::chromatogram(rawData()[[i]], rtMatrix, mzMatrix)
  # # %>% filterMz(mz = c(458.7, 466.3))
  # # rts <- split(filterRt(rawDataL, rt = c(10, 1500)), fromFile(filterRt(rawDataL, rt = c(10, 1500))))
  # p <- plot(sps[[6]]) 
  # length(sps)
  # sapply(sps, fromFile)
  # 
  # sps2 <- sps %>% filterMz(mzMatrix[1,])
  # 
  # 
  # 
  # 
  # rts <- split(sps, fromFile(filterRt(rawDataL, rt = c(600, 700))))
  # 
  # test <- ldply(rts, function(i) {
  #   
  #   ldply(1:length(i), function(x) rbind(rtime(x[[1]])))
  # })
  # 
  # test2 <- ldply(rts, function(i) {
  #   ldply(intensity(i[[1]]), rbind)
  # })
  # test3 <- ldply(rts, function(i) {
  #   ldply(mz(i[[1]]), rbind)
  # })
  # 
  # sapply(sps, fromFile)
  # 
  # sps2 <-  split(sps, fromFile(rawDataL))
  # 
  #     rts <- split(filterRt(c(180, 181)), rawDataL)
  #     rest2 <- 
  #     
  #     extractData.df <- lapply(seq_len(length(rawData)), function(i) {
  #     
  #     #MSsubset <- lapply(seq_len(length(rawData)), function(i) {
  #   
  #  MSsubset <- xcms::filterRt(rawData[[i]],rtMatrix[1,])
  #   # xcms::filterMz(MSsubset[[i]],mzMatrix[1,])
  #   
  # #})
  #     
  #  #seq_len(length(N_atom))
  # SIMsubset <- lapply(seq_len(length(rawData)), function(i) {
  #   
  #   xcms::filterMz(MSsubset,mzMatrix[i,])
  #   # xcms::filterMz(MSsubset[[i]],mzMatrix[i,])
  #   
  # })
  # 
  # SIMdatalist <- lapply(seq_len(length(rawData)), function(i) {
  #   
  # intensity <- xcms::intensity(SIMsubset[[i]]) %>% sapply(FUN = num0_remove)
  # rtime <- xcms::rtime(SIMsubset[[i]])
  # 
  # extractData.df <- data.frame("intensity" = intensity, "rtime" = rtime, "mass0" = "459",check.names = F)
  #   #input$mass0
  # })
  # 
  # 
  # 
  # })
  #     
  #         num0_remove <- function(x)({
  #       
  #       if (length(x) == 0) {
  #         x <- 0
  #       } else return(x)
  #       
  #     })
  
  #     
  #     testthat::is_equivalent_to(xcms::intensity(SIMsubset[[i]])[1],numeric(0))
  
  ###################################################### end new method
  
  
  
  paramFile <- eventReactive(input$fileButton, {
    # stopifnot(is.null(input$fileButton))
    
    tryCatch(file.choose(),
             error = function(cond) {message("file choice cancelled"); return(NULL)})
    
  })
  
  
  
  values <- reactiveValues()
  values[["DF"]] <- NULL
  values[["lastValues"]] <- NULL
  # values2 <- reactiveValues(DF = NULL)
  
  # Data table
  observeEvent({input$fileButton}, {
    
    # browser()
    
    if (!is.null(paramFile())) {
      
      values[["DF"]] <- openxlsx::read.xlsx(paramFile(),sheet = 1, cols = 1:5) %>% data.table::setDT() #,.name_repair="minimal" 
      
      # expected_header <- c("name", "RT", "lOffset", "rOffset", "Mass0", "LabelAtoms")
      
      
      validate(
        
        if (identical(paramFile_header, colnames(values[["DF"]])[1:5])) {
          
          NULL
          
        } else {
          
          #     output$table <- DT::renderDT({
          #   #browser()
          # 
          #       DT::datatable(isolate(values[["DF"]]), options = list(
          #         pageLength = 50,
          #         lengthMenu = c(10, 25, 50, 100, 1000)
          #       ), 
          #       editable = 'cell',selection = 'none',)
          # })
          
          "Error: the parameterFile is not properly formatted."
        }
      )
      
      output$table <- DT::renderDT({
        #browser()
        
        DT::datatable(isolate(values[["DF"]]), options = list(
          pageLength = 50,
          lengthMenu = c(10, 25, 50, 100, 1000)
        ), 
        editable = 'cell',selection = 'none')
        
      })
      
    }
  },label = "datatable")
  
  
  # # RHANDSONTABLE
  # 
  # observeEvent({input$fileButton | input$saveTotable}, {
  #   
  #   # browser()
  #   
  #   if (!is.null(paramFile())) {
  #     
  #     values[["DF"]] <- openxlsx::read.xlsx(paramFile(),sheet = 1) #,.name_repair="minimal"
  #     
  #     # expected_header <- c("name", "RT", "lOffset", "rOffset", "Mass0", "LabelAtoms")
  #     expected_header <- c("name", "rt_left", "rt_right", "Mass0", "LabelAtoms")
  #     
  #     validate(
  #       
  #       if (isTRUE(all.equal(expected_header, colnames(values[["DF"]])[1:5]))) {
  #         NULL
  #         
  #       } else {
  #         
  #         output$hot <- rhandsontable::renderRHandsontable({
  #           # DF  <- values[["DF"]]
  #           rhandsontable::rhandsontable(data.frame(NULL), colHeaders = NULL)
  #         })
  #         
  #         "Error: the parameterFile is not properly formatted."
  #       }
  #     )
  #     
  #     output$hot <- rhandsontable::renderRHandsontable({
  #       DF  <- isolate(values[["DF"]])
  #       rhandsontable::rhandsontable(DF, columnSorting = TRUE)
  #     })
  #     
  #   }
  # })
  
  # validate(
  #     need(isTRUE(all.equal(expected_header, colnames(values[["DF"]])[1:5])), message = "need 1")
  # )
  
  # validate(
  #     if (isTRUE(all.equal(expected_header, colnames(values[["DF"]])[1:5]))) {
  #         NULL
  #     } else {
  #         
  #         values[["DF"]] <- NULL
  #         "Error: the parameterFile is not properly formatted."
  #     }
  # )
  
  
  # selection_val  <- reactiveValues()
  # selection_rowI  <- reactiveValues()
  # selection_colI  <- reactiveValues()
  # 
  # observeEvent({input$hot_select},{
  #   browser()
  # 
  #  selection_colI <- input$hot_select$select$r
  #  selection_rowI <- input$hot_select$select$c
  #  selection <- input$hot_select$data[[selection_rowI]][[selection_colI]]
  #  
  # })
  
  #     observeEvent(input$undoButton,{
  #     browser()
  #  
  #     output$hot <- renderRHandsontable({
  #         DF  <- isolate(values2[["DF"]])
  #           rhandsontable(DF,columnSorting = TRUE, selectCallback =F)
  # })
  #    
  #   })
  
  observeEvent(input$saveTotable, {
    
    # browser()
    DF <- isolate(values[["DF"]]) 
    
    # If you use data table then the syntax is: DF[c(DF[,1] == input$selectedFragment), 2] <- input$rtimeL
    DF[c(DF[,1] == input$selectedFragment), 2] <- input$rtimeL
    DF[c(DF[,1] == input$selectedFragment), 3] <- input$rtimeR
    #DF[DF[1] == input$selectedFragment, 4] <- input$mass0
    #DF[DF[1] == input$selectedFragment, 5] <- input$N_atom
    
    openxlsx::write.xlsx(DF, file = paramFile(), row.names = F, keepNA = F)
    
    output$table <- DT::renderDT({
      #browser()
      
      DT::datatable(DF, options = list(
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100, 1000)
      ), 
      editable = 'cell',selection = 'none')
      
    })
    
    showNotification("Saved !")
    
  },label = "saveTotable")
  
  
  observeEvent(input$table_cell_edit, {
    cell <- input[["table_cell_edit"]]
    
    values[["DF"]][cell$row, cell$col] <- cell$value
    
  }, label = "table_cell_edit")
  
  
  # observeEvent(input$hot, {
  #   
  #   output$hot <- rhandsontable::renderRHandsontable({
  #     # browser()
  #     
  #     
  #     DF = NULL
  #     if (!is.null(input$hot)) {
  #       
  #       if (is.even(i)) {
  #         # browser()
  #         values[["lastValues"]] <- isolate(values[["DF"]])
  #       }
  #       
  #       DF = rhandsontable::hot_to_r(input$hot)
  #       values[["DF"]] = DF
  #       
  #     } else if (!is.null(values[["DF"]])) {
  #       DF = values[["DF"]]
  #     }
  #     
  #     if (!is.null(DF)) {
  #       i <<- i + 1
  #       # cat(i)
  #       rhandsontable::rhandsontable(DF)
  #     }
  #   })
  # } ,ignoreInit = T)
  
  
  
  
  # observeEvent(input$undoButton, {
  #   
  #   # browser()
  #   
  #   lastValues <- isolate(values[["lastValues"]])
  #   
  #   if(!is.null(lastValues) && nrow(dplyr::setdiff(lastValues, isolate(values[["DF"]]))) != 0) {
  #     
  #     #browser()
  #     i <<- i + 1
  #     # cat(i)
  #     
  #     output$hot <-renderRHandsontable({
  #       
  #       rhandsontable::rhandsontable(lastValues)
  #       
  #     })
  #   }
  #   
  #   
  #   # DF <<- isolate(values$DF)
  #   
  # })
  
  observeEvent(input$saveButton, {
    
    finalDF <- isolate(values[["DF"]])
    finalDF[, (paramFile_num_header) := lapply(.SD, as.numeric), .SDcols = paramFile_num_header]
    #finalDF <- finalDF[-1] # remove the first column (automatic index when importing data)
    #na.replace(finalDF,"")
    #replace(finalDF,"#N/A")
    
    # na.replace(parameterFile," ")
    
    # xlsx::write.xlsx(finalDF, file = paramFile(),row.names = F,showNA = F) #"/home/mathieu/Documents/Data/MSinco/MSTFA over time/Parameters/ParameterFile.xlsx"
    openxlsx::write.xlsx(finalDF, file = paramFile(), row.names = FALSE, keepNA = F)
    showNotification("Saved !")
  },label = "saveButton")
  
  observeEvent({input$runButton2}, {
    
    # browser()
    
    finalDF <- isolate(values[["DF"]])
    finalDF[, (paramFile_num_header) := lapply(.SD, as.numeric), .SDcols = paramFile_num_header]
    openxlsx::write.xlsx(finalDF, file = paramFile(),row.names = F,keepNA = F) 
    showNotification("Saved prior integration ...")
    
    if (isTRUE("savePlots" %in% input$runParameters)) {
      saveAllPlots = TRUE } else { saveAllPlots = FALSE }
    
    if (isTRUE("baselineCorrection" %in% input$runParameters)) {
      baselineCorrection = TRUE } else { baselineCorrection = FALSE }
    
    
    if (isTRUE("isotopeCorrection" %in% input$runParameters)) {
      isotopeCorrection = TRUE } else {isotopeCorrection <- FALSE }
    
    if (isTRUE("correctTracerImpurity" %in% input$runParameters)) {
      correctTracerImpurity = TRUE } else {correctTracerImpurity = FALSE }
    
    if (is.null(input$mzd2)) {
      mzd <- 0.3 } else {mzd <- isolate(input$mzd2)}
    
    
    dir2 <- isolate(directory())
    rawData <- isolate(rawData())
    
    # browser()
    
    
    # callModule(intCorServer,"intCor", rawData, dir2,saveAllPlots, baselineCorrection,isotopeCorrection, mzd, correctTracerImpurity)
    intCor(rawData, dir2,saveAllPlots, baselineCorrection,isotopeCorrection, mzd, correctTracerImpurity)
    
  },label = "runButton2")
  
  #refreshInputsButton, input$fileButton
  # fragments <- eventReactive({
  #   input$refreshInputsButton
  #   }, {
  # 
  #   finalDF2 <- isolate(values[["DF"]]) # readxl::read_excel(paramFile())
  #   return(finalDF2[1])
  # 
  # })
  
  paramFileTable <- reactive({
    
    # req(values[["DF"]])
    #finalDF3 <- isolate(values[["DF"]]) 
    
    values[["DF"]] %>% data.table::setDT()
    
    
  })
  
  observeEvent(input$saveActivePlotsButton, {
    #stopifnot(!is.null(input$checkboxGroupInput))
    if (!is.null(input$saveActivePlotsButton)) {
      
      # if directory exists() ?????
      activePlotsDir <- paste0(directory(),"/Active Plots")
      
      if (!dir.exists(activePlotsDir)) {
        dir.create(activePlotsDir,mode = "0777")
      }
      
      withProgress(message = 'Saving ...', value = 0, {
        
        if (input$tabs == "TIC" && !is.null(TICplots)) {
          # browser()
          sapply(seq_along(rawData()),function(i) {
            
            incProgress(1/(length(rawData())*length(input$savePlots)))
            #   system.time({
            # png(paste0("/home/mathieu/Documents/MSinco demo/MSinco demo/Active Plots/", TICplots[[i]]$labels$title,".png"))
            # print(TICplots[[i]])
            # dev.off()
            #   })
            # system.time({
            ggplot2::ggsave(paste0(TICplots[[i]]$labels$title,".png"), plot = TICplots[[i]], path = activePlotsDir, device = "png")
            # })
          })
        }
        
        
        if (input$tabs == "SIM" && !is.null(SIMplots)) {
          
          sapply(seq_along(rawData()),function(i) {
            
            incProgress(1/(length(rawData())*length(input$savePlots)))
            # png(paste0(activePlotsDir, "/SIM_", i,".png"))
            # print(SIMplots()[[i]])
            # dev.off()
            
            ggplot2::ggsave(paste0(SIMplots[[i]]$labels$title,".png"), plot = SIMplots[[i]], path = activePlotsDir, device = "png")
          })
        }
        
        if (input$tabs == "MSpectrum" && !is.null(MSpecplots)) {
          
          sapply(seq_along(rawData()),function(i) {
            
            incProgress(1/(length(rawData())*length(input$savePlots)))
            # png(paste0(activePlotsDir, "/MSpec_", i,".png"))
            # print(MSpecplots()[[i]])
            # dev.off()
            
            ggplot2::ggsave(paste0(MSpecplots[[i]]$labels$title,".png"), plot = MSpecplots[[i]], path = activePlotsDir, device = "png")
            
          })
        }
      })
      
    }
    
  },ignoreInit = T, label = "saveActivePlotsButton")
  
  
  #   observeEvent(input$savePlotsButton, {
  #     #stopifnot(!is.null(input$checkboxGroupInput))
  #  if (!is.null(input$savePlotsButton)) {
  #    
  #     dir.create(paste0(directory(),"/Active Plots"),mode = "0777")
  #     activePlotsDir <- paste0(directory(),"/Active Plots")
  # 
  #    
  #       withProgress(message = 'Saving ...', value = 0, {
  #       
  #     if (isTRUE("TIC" %in% input$savePlots & !is.null(TICplots()))) {
  #       
  #       sapply(seq_along(rawData()),function(i) {
  #         
  #         incProgress(1/(length(rawData())*length(input$savePlots)))
  #       png(paste0(activePlotsDir, "/TIC_", i,".png"))
  #       print(TICplots()[[i]])
  #       dev.off()
  #         # ggsave(paste0("TIC_",i), plot = TICplots()[[i]], path = getwd(), device = "png")
  #       })
  #     }
  # 
  # 
  #       if (isTRUE("SIM" %in% input$savePlots)) {
  # 
  #       sapply(seq_along(rawData()),function(i) {
  #         
  #       incProgress(1/(length(rawData())*length(input$savePlots)))
  #       png(paste0(activePlotsDir, "/SIM_", i,".png"))
  #       print(SIMplots()[[i]])
  #       dev.off()
  #       #ggsave(paste0("SIM_",i), plot = SIMplots()[[i]], path = getwd(), device = "png")
  #       })
  #     }
  # 
  #       if (isTRUE("MSpectrum" %in% input$savePlots)) {
  #         
  #       sapply(seq_along(rawData()),function(i) {
  #         
  #       incProgress(1/(length(rawData())*length(input$savePlots)))
  #       png(paste0(activePlotsDir, "/MSpec_", i,".png"))
  #       print(MSpecplots()[[i]])
  #       dev.off()
  #       #ggsave(paste0("MSpectrum_",i), plot = MSpecplots()[[i]], path = getwd(), device = "png")
  # 
  #       })
  #       }
  #   })
  #    
  # }
  #     
  #  })
}
