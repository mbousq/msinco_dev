#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny xlsx lattice
#' @importFrom ggplot2 geom_line labs geom_col geom_text ggsave aes
#' @importFrom magrittr "%>%"
#' @noRd

app_server <- function(input, output, session) {
  options(future.supportsMulticore.unstable = "quiet")
  options(stringsAsFactors = FALSE)
  options(digits = 4)
  onStop(function(state) future::plan("sequential"))
  onStop(function(state) .rs.restartR())
  session$onSessionEnded(stopApp)
  n_undo <- 0
  paramFile_header <- c("fragment", "rt_left", "rt_right", "mass_0", "lab_atoms")
  paramFile_num_header <- c("rt_left", "rt_right", "mass_0", "lab_atoms")
  workers <- data.table::getDTthreads()


  values <- reactiveValues()
  values[["DF"]] <- NULL
  values[["lastValues"]] <- NULL
  values[["directory"]] <- NULL
  values[["paramFile"]] <- NULL
  values[["runNo"]] <- 0
  values[["rawData"]] <- NULL
  values[["lastFragment"]] <- NULL
  values[["plots_tic"]] <- NULL
  values[["plots_sim"]] <- NULL
  values[["plots_ms"]] <- NULL
  values[["file"]] <- NULL
  values[["rawData_tic"]] <- NULL
  values[["plotIndex"]] <- NULL
  values[["rawData_msnExp"]] <- NULL

  observeEvent(input$quitButton, {
    stopApp()
  })

  observeEvent(
    {
      input$ok
    },
    {
      shiny::removeModal()
      updateNavbarPage(session, "navbar", selected = NULL)


      tryCatch(
        {
          tmp <- easycsv::choose_dir()

          pathDir <- paste0(tmp, "/Experiment_", format(Sys.time(), "%d-%m-%y_%H%M%S"))
          dir.create(pathDir, mode = "0777")

          if (input$includeSampleData == TRUE) {
            file.copy(system.file("extdata", "Netcdfs", package = "MSinco"), pathDir, recursive = T)
          }

          if (input$includeParameters == TRUE) {
            file.copy(system.file("extdata", "Parameters", package = "MSinco"), pathDir, recursive = T)
          }



          return(pathDir)
        },
        warning = function(cond) {
          message("Please choose a valid directory")
          return(NULL)
        }
        # error = function(cond) {message(); return(NULL)}
      )
    }
  )



  observeEvent(
    {
      input$navbar
    },
    {


      # Import experiment directory PATH
      if (input$navbar == "Import experiment") {
        values[["runNo"]] <- values[["runNo"]] + 1

        tryCatch(
          {

            # if (isNamespaceLoaded("rstudioapi")) {
            #
            #   rstudioapi::selectDirectory()
            # } else {
            #

            values[["directory"]] <- easycsv::choose_dir()
          },
          warning = function(cond) {
            message("Please choose a valid directory")
            return(NULL)
          }
        )

        if (is.null(values[["directory"]]) || !file.exists(paste0(values[["directory"]], "/Parameters/ParameterFile.xlsx"))) {
          warning("Invalid directory or the 'ParameterFile.xlsx' was not found")
          updateTabsetPanel(session, inputId = "navbar", selected = "Visualization")
          req(FALSE)
        }

        # Import parameters


        values[["paramFile"]] <- paste0(values[["directory"]], "/Parameters/ParameterFile.xlsx")

        if (!is.null(values[["paramFile"]])) {
          tmp <- openxlsx::read.xlsx(values[["paramFile"]], sheet = 1, cols = 1:5) %>% data.table::setDT() # ,.name_repair="minimal"

          validate(
            if (identical(paramFile_header, colnames(tmp)[1:5])) {
              NULL
            } else {
              warning("The parameterFile is not properly formatted.")
              showNotification("Error: the parameterFile is not properly formatted.")
            }
          )

          values[["DF"]] <- tmp
        }

        # Import data

        if ("parallel" %in% input$settings) {
          future::plan("multiprocess", workers = workers)
        }

        # cat(format(Sys.time(), "%X"))
        progressr::withProgressShiny(message = "Extracting data ...", value = 0, {
          netCDFs <- list.files(paste0(values[["directory"]], "/Netcdfs"), full.names = TRUE, pattern = "(*.CDF$)|(.mzXML$)|(mzData$)|(mzML$)", ignore.case = T) %>% gtools::mixedsort()

          p <- progressr::progressor(steps = length(netCDFs) * 3)


          msnExp <- future.apply::future_lapply(netCDFs, function(netCDFs) {
            p()
            MSnbase::readMSData(netCDFs, mode = "onDisk", verbose = F)
          })


          names(msnExp) <- lapply(msnExp, function(msnExp) row.names(msnExp@phenoData)) # )rownames(MSnbase::phenoData(msnExp[[1]])) #filesName

          rawData <- future.apply::future_lapply(msnExp, FUN = function(msnExp) {
            p()
            msnExp %>%
              methods::as("data.frame") %>%
              data.table::setDT() %>%
              `attr<-`("fileName", row.names(msnExp@phenoData))
          })



          # cat(format(Sys.time(), "%X"))

          rawData_tic <- lapply(rawData, function(rawData) {
            p()
            rawData[, .(tic = sum(i)), by = .(rt)] %>% `attr<-`("fileName", attr(rawData, "fileName")) # %>% `attr<-`("file", rownames(MSnbase::phenoData(msnExp[[1]])))
          })


          values[["plotIndex"]] <- findInterval(length(msnExp) / 2, seq_len(length(msnExp)), all.inside = T)
          values[["rawData"]] <- rawData
          values[["rawData_tic"]] <- rawData_tic
          values[["rawData_msnExp"]] <- msnExp


          future::plan("sequential")
          updateTabsetPanel(session, inputId = "navbar", selected = "Visualization")
        })
      } else {
        if (input$navbar == "Create experiment") {
          showModal(
            modalDialog(
              h5("Create new directory"),
              checkboxInput("includeSampleData", "include sample data ?", value = FALSE),
              checkboxInput("includeParameters", "include Parameters examples ?", value = TRUE),
              footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
              )
            )
          )
          updateTabsetPanel(session, inputId = "navbar", selected = "Visualization")
        }
      }
    },
    ignoreInit = T
  )



  output$plot_hover_coord <- renderPrint({
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

    plot_hover_data <- plot_hover_data[1:2] %>%
      as.numeric() %>%
      round(digits = 2)
    names(plot_hover_data) <- c("x", "y")
    plot_hover_data
  })


  output$saveActivePlotsButton <- renderUI({
    req(values[["rawData"]])

    actionButton("exportActivePlotsButton", "Export active plots", width = "100%", style = "margin-bottom:8px")
  })


  output$selectedFragment <- renderUI({
    req(values[["rawData"]])
    selectInput("selectedFragment", "Fragments", isolate(c("TIC", values[["DF"]][[1]])), multiple = F, selectize = TRUE)
  })

  observeEvent(
    {
      input$selectedFragment
      values[["DF"]]
      values[["rawData"]]
    },
    {
      req(values[["rawData"]], values[["DF"]], input$selectedFragment)

      output$selectedFiles <- renderUI({
        selectInput("selectedFiles", "Files", names(values[["rawData"]]), multiple = TRUE, selectize = T, selected = names(values[["rawData"]]))
      })

      output$rtime <- renderUI({
        req(input$tabs == "MSpectrum")


        numericInput(
          inputId = "rtime",
          label = "Retention time",
          value = (values[["DF"]][fragment == input$selectedFragment, 2] + values[["DF"]][fragment == input$selectedFragment, 3]) / 2, # to be used with input$selectedFragment2: as.numeric(gsub(".*@",replacement = "", input$selectedFragment2)),
          step = 0.1
        )
      })

      output$labelThreshold <- renderUI({
        req(input$tabs == "MSpectrum")


        numericInput(
          inputId = "labelThreshold",
          label = "Threshold intensity for labels",
          value = {
            if (input$selectedFragment == "TIC") {
              NULL
            } else {
              10
            }
          },
          step = 5
        )
      })

      output$rtimeL <- renderUI({
        req(input$tabs == "TIC" | input$tabs == "SIM")


        if (input$selectedFragment == "TIC" && input$tabs == "TIC") {
          numericInput(
            inputId = "rtimeL",
            label = "Retention time (left)",
            value = tryCatch(round(min(values[["rawData"]][[1]]$rt) / 60), error = function(cond) {
              message()
              return(NULL)
            }), # max(rawData1()@featureData@data$retentionTime)/2-10,
            step = 0.1
          )
        } else {
          numericInput(
            inputId = "rtimeL",
            label = "Retention time (left)",
            value = values[["DF"]][fragment == input$selectedFragment, 2],
            step = 0.1
          )
        }
      })

      output$rtimeR <- renderUI({
        req(input$tabs == "TIC" | input$tabs == "SIM")


        if (input$selectedFragment == "TIC" && input$tabs == "TIC") {
          numericInput(
            inputId = "rtimeR",
            label = "Retention time (right)",

            value = tryCatch(round(max(values[["rawData"]][[1]]$rt) / 60), error = function(cond) {
              message()
              return(NULL)
            }), # max(rawData1()@featureData@data$retentionTime)/2+10,
            step = 0.1
          )
        } else {
          numericInput(
            inputId = "rtimeR",
            label = "Retention time (right)",

            value = values[["DF"]][fragment == input$selectedFragment, 3],
            step = 0.1
          )
        }
      })

      output$mass0 <- renderUI({
        req(input$tabs == "SIM")


        numericInput(
          inputId = "mass0",
          label = "mass0 (M0)",
          value = values[["DF"]][fragment == input$selectedFragment, 4],
          step = 1
        )
      })

      output$N_atom <- renderUI({
        req(input$tabs == "SIM")


        numericInput(
          inputId = "N_atom",
          label = "Number of isotopomers",
          value = values[["DF"]][fragment == input$selectedFragment, 5],
          step = 1
        )
      })


      output$mzd <- renderUI({
        req(input$tabs == "SIM")


        numericInput(
          inputId = "mzd",
          label = "Mass difference",
          value = {
            if (input$selectedFragment == "TIC") {
              NULL
            } else {
              0.3
            }
          },
          step = 0.1
        )
      })
    },
    ignoreInit = T
  )



  observeEvent(
    {
      input$run1
    },
    {
      req(input$rtimeL, input$rtimeR)


      plots_tic <- lapply(values[["rawData_tic"]][input$selectedFiles], function(rawData_tic) {
        dataIndexL <- MALDIquant::match.closest(input$rtimeL * 60, rawData_tic$rt)
        dataIndexR <- MALDIquant::match.closest(input$rtimeR * 60, rawData_tic$rt)
        DT <- rawData_tic[dataIndexL:dataIndexR]

        if (input$selectedFragment == "TIC") {
          title <- paste0("TIC_", attr(rawData_tic, "fileName"))
        } else {
          title <- paste0("TIC_", input$selectedFragment, "_", attr(rawData_tic, "fileName"))
        }


        if (input$graphics == "1") {
          
          lattice::xyplot(tic ~ (rt / 60), data = DT, type = "l", xlab = "Retention time (min)", ylab = "Intensity", main = title, col = "black", scales = list(tck = c(1, 0)))
          
        } else {
          canvas_plots + ggplot2::geom_line(data = DT, aes(rt / 60, tic)) + ggplot2::labs(title = title, x = "retention time (min)", y = "intensity")
        }
      })

      values[["plots_tic"]] <- plots_tic


      if ("cache" %in% input$settings) {
        lapply(seq_along(plots_tic), FUN = function(i) {
          output[[paste0("tic", i, "_f", values[["runNo"]])]] <- renderCachedPlot(
            {
              plots_tic[[i]]
            },
            cacheKeyExpr = list(input$rtimeL, input$rtimeR)
          )
        })
      } else {
        lapply(seq_along(plots_tic), FUN = function(i) {
          output[[paste0("tic", i, "_f", values[["runNo"]], input$run1)]] <- renderPlot({
            plots_tic[[i]]
          })
        })
      }
      splitIndex <- findInterval(length(plots_tic) / 2, seq_len(length(plots_tic)), all.inside = T)

      output$plots_ticA <- renderUI({
        lapply(seq_len(splitIndex), function(i) {
          plotOutput(paste0("tic", i, "_f", values[["runNo"]], input$run1), hover = paste0("plot_hover_tic", i, "_f", values[["runNo"]], input$run1)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        })
      })

      output$plots_ticB <- renderUI({
        req(length(plots_tic) > 1)
        lapply((splitIndex + 1):length(plots_tic), function(i) {
          plotOutput(paste0("tic", i, "_f", values[["runNo"]], input$run1), hover = paste0("plot_hover_tic", i, "_f", values[["runNo"]], input$run1)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        })
      })
    },
    label = "ticOut"
  )



  observeEvent(
    {
      input$run2
    },
    {
      req(input$rtime)
      plots_ms <- lapply(values[["rawData"]][input$selectedFiles], function(rawData) {
        rtime <- rawData[, unique(rt)]
        specIndex <- MALDIquant::match.closest(input$rtime * 60, rtime)
        DT <- rawData[rt == rtime[specIndex], .(i = i / max(i) * 100, mz)]
        title <- paste("MS_", input$selectedFragment, "_", attr(rawData, "fileName"), "~", input$rtime, "min")
        data.labels <- DT[i > input$labelThreshold]

        if (input$graphics == "1") {
          lattice::xyplot(i ~ mz,
            data = DT, type = "h", col = "black", xlab = "m/z", ylab = "Relative intensity", main = title, scales = list(tck = c(1, 0), x = list(at = pretty(DT$mz))),
            panel = function(...) {
              lattice::panel.text(data.labels$mz, data.labels$i,
                round(data.labels$mz),
                adj = c(1, 0), pos = 3
              )
              lattice::panel.xyplot(...)
            }
          )
        } else {
          canvas_plots + ggplot2::geom_col(data = DT, aes(mz, i)) + ggplot2::geom_text(data = data.labels, aes(x = mz, y = i, label = round(i)), nudge_y = 2) + ggplot2::labs(title = title, x = "m/z", y = "Relative intensity")
        }
      })


      values[["plots_ms"]] <- plots_ms


      if ("cache" %in% input$settings) {
        lapply(seq_along(plots_ms), FUN = function(i) {
          output[[paste0("mspec", i, "_f", values[["runNo"]]), input$run2]] <- renderCachedPlot(
            {
              plots_ms[[i]]
            },
            cacheKeyExpr = list(input$rtime, input$labelThreshold)
          )
        })
      } else {
        lapply(seq_along(plots_ms), FUN = function(i) {
          output[[paste0("mspec", i, "_f", values[["runNo"]], input$run2)]] <- renderPlot({
            plots_ms[[i]]
          })
        })
      }

      splitIndex <- findInterval(length(plots_ms) / 2, seq_len(length(plots_ms)), all.inside = T)
      output$plots_msA <- renderUI({
        lapply(seq_len(splitIndex), function(i) {
          plotOutput(paste0("mspec", i, "_f", values[["runNo"]], input$run2), hover = paste0("plot_hover_mspec", i, "_f", values[["runNo"]], input$run2)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        })
      })

      output$plots_msB <- renderUI({
        req(length(values[["rawData"]]) > 1)
        lapply((splitIndex + 1):length(plots_ms), function(i) {
          plotOutput(paste0("mspec", i, "_f", values[["runNo"]], input$run2), hover = paste0("plot_hover_mspec", i, "_f", values[["runNo"]], input$run2)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        })
      })
    },
    label = "msOut"
  )



  observeEvent(
    {
      input$run3
    },
    {
      req(input$mass0, input$N_atom, input$rtimeL, input$rtimeR)


      rtMatrix <- matrix(c(rep(input$rtimeL * 60, input$N_atom), rep(input$rtimeR * 60, input$N_atom)), nrow = input$N_atom, ncol = 2)
      mzMatrix <- matrix(c(seq(input$mass0 - input$mzd, input$mass0 + (input$N_atom - 1) - input$mzd, by = 1), seq(input$mass0 + input$mzd, input$mass0 + (input$N_atom - 1) + input$mzd, by = 1)), nrow = input$N_atom, ncol = 2)

      plots_sim <- lapply(values[["rawData"]][input$selectedFiles], function(rawData) {
        rtL <- MALDIquant::match.closest(input$rtimeL * 60, rawData$rt)
        rtR <- MALDIquant::match.closest(input$rtimeR * 60, rawData$rt)

        myData <- lapply(1:nrow(rtMatrix), function(j) {
          DT <- rawData[data.table::between(rt, rt[[rtL]], rt[[rtR]])][data.table::between(mz, mzMatrix[j, 1], mzMatrix[j, 2])]


          rtWindow <- data.frame(rt = DT$rt)
          leftScan <- na.omit(DT)[1]
          rightScan <- tail(na.omit(DT), 1) # Instead of using only one value, we could do an AVERAGE intensity of multiple scan in each side of the integration window.
          modelData <- rbind(leftScan, rightScan)

          if (nrow(DT) > 0) {
            suppressWarnings({
              model <- lm(i ~ rt, data = modelData)
              prediction <- predict(model, newdata = rtWindow, type = "response")
            })

            dplyr::mutate(DT, prediction = prediction)
          }
        })



        myData <- data.table::rbindlist(myData)
        # prediction <- data.table::rbindlist(prediction)

        if (nrow(myData) > 0) {
          myData
        } else {
          myData <- data.table::data.table(phenoData = attr(rawData, "fileName"))
        }

        title <- paste0("SIM_", input$selectedFragment, "_", attr(rawData, "fileName"))

        if (input$graphics == "1") {
          if (length(myData) != 1) {
            mzGroups <- round(myData$mz)
            lattice::xyplot(i ~ (rt / 60), data = myData, groups = mzGroups, type = "l", xlab = "Retention time (min)", ylab = "Intensity", main = title, scales = list(tck = c(1, 0)), auto.key = list(points = FALSE, lines = TRUE, title = "", corner = c(1, 1), x = 1, y = 1, size = 2)) +
              latticeExtra::as.layer(lattice::xyplot(prediction ~ rt / 60, data = myData, groups = mzGroups, type = "l", lty = 2, lwd = 0.5))
          } else {
            lattice::xyplot(0 ~ 0, type = "l", xlab = "Retention time (min)", ylab = "Intensity", main = title, scales = list(tck = c(1, 0)), auto.key = list(points = FALSE, lines = TRUE, title = "", corner = c(1, 1), x = 1, y = 1, size = 2))
          }
        } else {
          if (length(myData) != 1) {
            canvas_plots + ggplot2::geom_line(data = myData, mapping = aes(x = rt / 60, y = i, colour = factor(round(mz)))) + ggplot2::geom_line(data = myData, mapping = aes(x = rt / 60, y = prediction, colour = factor(round(mz))), linetype = "dashed", size = 0.5) + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity", col = "")
          } else {
            canvas_plots + ggplot2::labs(title = title, x = "Retention time (min)", y = "Intensity")
          }
        }
      })
      values[["plots_sim"]] <- plots_sim


      if ("cache" %in% input$settings) {
        lapply(seq_along(plots_sim), FUN = function(i) {
          output[[paste0("sim", i, "_f", values[["runNo"]])]] <- renderCachedPlot(
            {
              plots_sim[[i]]
            },
            cacheKeyExpr = list(input$mzd, input$N_atom, input$mass0, input$rtimeL, input$rtimeR)
          )
        })
      } else {
        lapply(seq_along(plots_sim), FUN = function(i) {
          output[[paste0("sim", i, "_f", values[["runNo"]], input$run3)]] <- renderPlot({
            plots_sim[[i]]
          })
        })
      }

      splitIndex <- findInterval(length(plots_sim) / 2, seq_len(length(plots_sim)), all.inside = T)

      output$plots_simA <- renderUI({
        plots_simA <- lapply(seq_len(values[["plotIndex"]]), function(i) {
          plotOutput(paste0("sim", i, "_f", values[["runNo"]], input$run3), hover = paste0("plot_hover_sim", i, "_f", values[["runNo"]], input$run3)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        })
      })

      output$plots_simB <- renderUI({
        req(length(plots_sim) > 1)
        plots_simB <- lapply((splitIndex + 1):length(plots_sim), function(i) {
          plotOutput(paste0("sim", i, "_f", values[["runNo"]], input$run3), hover = paste0("plot_hover_sim", i, "_f", values[["runNo"]], input$run3)) # %>% shinycssloaders::withSpinner(color="#000000", size = 0.2)
        })
      })
    },
    label = "simOut"
  )


  output$hot <- rhandsontable::renderRHandsontable({
    req(values[["DF"]])
    input$saveTotable

    DF <- values[["DF"]]
    rhandsontable::rhandsontable(DF, stretchH = "all", columnSorting = TRUE) %>% rhandsontable::hot_cols(valign = "htCenter")
  })

  observeEvent(input$hot,
    {
      output$hot <- rhandsontable::renderRHandsontable({
        # browser()


        DF <- NULL
        if (!is.null(input$hot)) {
          if (is.even(n_undo)) {
            values[["lastValues"]] <- isolate(values[["DF"]])
          }

          DF <- rhandsontable::hot_to_r(input$hot)
          values[["DF"]] <- DF
        } else if (!is.null(values[["DF"]])) {
          DF <- values[["DF"]]
        }

        if (!is.null(DF)) {
          n_undo <<- n_undo + 1
          rhandsontable::rhandsontable(DF, stretchH = "all", columnSorting = TRUE) %>% rhandsontable::hot_cols(valign = "htCenter")
        }
      })
    },
    ignoreInit = T
  )


  observeEvent(input$undoButton, {
    lastValues <- isolate(values[["lastValues"]])

    if (!is.null(lastValues) && nrow(dplyr::setdiff(lastValues, isolate(values[["DF"]]))) != 0) {
      output$hot <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(lastValues, stretchH = "all", columnSorting = TRUE) %>% rhandsontable::hot_cols(valign = "htCenter")
      })

      n_undo <<- n_undo + 1
    }
  })



  observeEvent(input$saveTotable,
    {
      DF <- isolate(values[["DF"]])
      DF[c(DF[, 1] == input$selectedFragment), 2] <- input$rtimeL
      DF[c(DF[, 1] == input$selectedFragment), 3] <- input$rtimeR
      DF[c(DF[, 1] == input$selectedFragment), 4] <- input$mass0
      DF[c(DF[, 1] == input$selectedFragment), 5] <- input$N_atom

      values[["DF"]] <- DF

      showNotification("Saved")
    },
    label = "saveTotable"
  )


  observeEvent(input$saveButton,
    {
      finalDF <- isolate(values[["DF"]])
      finalDF[, (paramFile_num_header) := lapply(.SD, as.numeric), .SDcols = paramFile_num_header]
      openxlsx::write.xlsx(finalDF, file = values[["paramFile"]], row.names = FALSE, keepNA = F)
      showNotification("Saved")
    },
    label = "saveButton"
  )

  observeEvent(
    {
      input$runButton2
    },
    {
      finalDF <- isolate(values[["DF"]])
      finalDF[, (paramFile_num_header) := lapply(.SD, as.numeric), .SDcols = paramFile_num_header]
      openxlsx::write.xlsx(finalDF, file = values[["paramFile"]], row.names = F, keepNA = F)
      showNotification("Saved prior integration ...")

      if (isTRUE("savePlots" %in% input$runParameters)) {
        saveAllPlots <- TRUE
      } else {
        saveAllPlots <- FALSE
      }

      if (isTRUE("baselineCorrection" %in% input$runParameters)) {
        baselineCorrection <- TRUE
      } else {
        baselineCorrection <- FALSE
      }

      if (isTRUE("isotopeCorrection" %in% input$runParameters)) {
        isotopeCorrection <- TRUE
      } else {
        isotopeCorrection <- FALSE
      }

      if (isTRUE("correctTracerImpurity" %in% input$runParameters)) {
        correctTracerImpurity <- TRUE
      } else {
        correctTracerImpurity <- FALSE
      }

      if (is.null(input$mzd2)) {
        mzd <- 0.3
      } else {
        mzd <- isolate(input$mzd2)
      }


      intCor(values[["rawData_msnExp"]], values[["directory"]], saveAllPlots, baselineCorrection, isotopeCorrection, mzd, correctTracerImpurity)
    },
    label = "runButton2"
  )


  observeEvent(input$exportActivePlotsButton,
    {
      activePlotsDir <- paste0(values[["directory"]], "/Active Plots/")

      if (!dir.exists(activePlotsDir)) {
        dir.create(activePlotsDir, mode = "0777")
      }

      withProgress(message = "Saving ...", value = 0, {
        switch(input$tabs,
          "TIC" = {
            if (!is.null(values[["plots_tic"]])) {
              incProgress(1 / (length(values[["plots_tic"]])))

              switch(input$graphics,
                "1" = {
                  lapply(values[["plots_tic"]], function(plots_tic) {
                    png(paste0(activePlotsDir, plots_tic$main, "_", input$exportActivePlotsButton, ".png"))
                    print(plots_tic)
                    dev.off()
                  })
                },
                "2" = {
                  lapply(values[["plots_tic"]], function(plots_tic) {
                    ggplot2::ggsave(paste0(plots_tic$labels$title, "_", input$exportActivePlotsButton, ".png"), plot = plots_tic, path = activePlotsDir, device = "png")
                  })
                }
              )
            }
          },
          "SIM" = {
            if (!is.null(values[["plots_sim"]])) {
              incProgress(1 / (length(values[["plots_sim"]])))

              switch(input$graphics,
                "1" = {
                  lapply(values[["plots_sim"]], function(plots_sim) {
                    png(paste0(activePlotsDir, plots_sim$main, "_", input$exportActivePlotsButton, ".png"))
                    print(plots_sim)
                    dev.off()
                  })
                },
                "2" = {
                  lapply(values[["plots_sim"]], function(plots_sim) {
                    ggplot2::ggsave(paste0(plots_sim$labels$title, "_", input$exportActivePlotsButton, ".png"), plot = plots_sim, path = activePlotsDir, device = "png")
                  })
                }
              )
            }
          },

          "MSpectrum" = {
            if (!is.null(values[["plots_ms"]])) {
              incProgress(1 / (length(values[["plots_ms"]])))

              switch(input$graphics,
                "1" = {
                  lapply(values[["plots_ms"]], function(plots_ms) {
                    png(paste0(activePlotsDir, plots_ms$main, "_", input$exportActivePlotsButton, ".png"))
                    print(plots_ms)
                    dev.off()
                  })
                },
                "2" = {
                  lapply(values[["plots_ms"]], function(plots_ms) {
                    ggplot2::ggsave(paste0(plots_ms$labels$title, "_", input$exportActivePlotsButton, ".png"), plot = plots_ms, path = activePlotsDir, device = "png")
                  })
                }
              )
            }
          }
        )
      })
    },
    label = "saveActivePlotsButton"
  )
}
