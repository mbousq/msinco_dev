#'
intCor <- function(rawData, pathExperimentFolder, saveAllPlots, baselineCorrection, isotopeCorrection, mzd, correctTracerImpurity= FALSE,correctAlsoMonoisotopic = FALSE) {

  # library(readxl);library(xcms);library(reshape2);library(xcms);library(ggplot2);library(gtools);library(knitr);
  # library(tidyverse);library(plyr);library(IsoCorrectoR);library(matrixStats);library(ecipex);library(xlsx);
  # library(xlsxjars)

  # observe({ 

  withProgress(message = 'Runing ...', value = 0, {
    
      folder <- paste0(pathExperimentFolder, "/", format(Sys.time(), "%d-%m-%y_%H%M%S"))
      dir.create(folder, mode = "0777")
      dir.create(paste0(folder, "/PLOTS", sep = ""), mode = "0777")
      file.copy(paste0(pathExperimentFolder, "/Parameters/ParameterFile.xlsx"), folder)
      file.copy(paste0(pathExperimentFolder, "/Parameters/MoleculeFile.xlsx"), folder)

    
    #  
    # Set paths
    parameterFile <- openxlsx::read.xlsx(paste0(pathExperimentFolder, "/Parameters", "/ParameterFile.xlsx"))
    moleculeFile <- openxlsx::read.xlsx(paste0(pathExperimentFolder, "/Parameters", "/MoleculeFile.xlsx"))
    results <- paste0(folder, "/")
    
    # Save some parameter info
    write(knitr::kable(data.frame(Sys.time(), mzd, baselineCorrection, isotopeCorrection, correctTracerImpurity), align = "c", row.names = FALSE), file = paste0(folder, "/conf.txt"))
    
    netCDFs <- list.files(paste0(pathExperimentFolder,"/Netcdfs"), full.names = TRUE, pattern = ".CDF$", ignore.case = T)
    integrals.array2 <- paste0(rep("integrals.df", length(rawData)), seq_along(rawData))
    
    #
    rtMatrix <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 2)
    mzMatrix <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 2)
    namesMatrix <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 1)
    namesMatrix2 <- matrix(nrow = sum(parameterFile[[5]], length(parameterFile[[5]])), ncol = 1)
    namesMatrix3 <- c()
    

    k <- 0
    for (a in seq_along(parameterFile[[1]])) {
      
      temp <- stringr::str_split(parameterFile[[1]][a], pattern = "(-)|(_)| |(\\.)", simplify = T)
      if (length(temp) <= 2) {
        namesMatrix3 <- c(namesMatrix3, paste(temp[1], as.numeric(temp[2]) + seq_len(parameterFile[[5]][a] + 1) - 1))
      } else {
        namesMatrix3 <- c(namesMatrix3, paste(temp[1], paste0(as.numeric(temp[2]) + seq_len(parameterFile[[5]][a] + 1) - 1, "-", temp[3])))
      }
    }
    
    
    namesMatrix2[, 1] <- namesMatrix3

    k <- 0
    for (a in seq_along(parameterFile[[1]])) {
      for (i in 1:(parameterFile[[5]][a] + 1)) { # +1 to account for the M0 isotopomer
        mzMatrix[k + i, 1] <- (parameterFile[[4]][a] + i - 1 - mzd)
        mzMatrix[k + i, 2] <- (parameterFile[[4]][a] + i - 1 + mzd)
      }
      k <- k + i
    }
    
    rtMatrix[, 1] <- rep(parameterFile[[2]], (parameterFile[[5]] + 1)) * 60 # RT low,# +1 to account for the M0 isotopomer
    rtMatrix[, 2] <- rep(parameterFile[[3]], (parameterFile[[5]] + 1)) * 60 # RT high,# +1 to account for the M0 isotopomer
    namesMatrix[, 1] <- rep(parameterFile[[1]], parameterFile[[5]] + 1)

    
    metaboliteData <- function(x) {
      if (all(is.na(myChr[x]@intensity)) == FALSE &
          all(is.na(myChr[x]@rtime)) == FALSE) {
        data.frame(intensity = subset(myChr[x]@intensity, is.na(myChr[x]@intensity) == FALSE), rtime = subset(myChr[x]@rtime, is.na(myChr[x]@intensity) == FALSE), mass0 = rep(round((myChr[x]@filterMz[1] + myChr[x]@filterMz[2]) / 2), length(subset(myChr[x]@rtime, is.na(myChr[x]@intensity) == FALSE))))
      } else {
        if (all(is.na(myChr[x]@intensity)) == TRUE &
            all(is.na(myChr[x]@rtime)) == TRUE) {
          data.frame(
            intensity = rep(0, 1),
            rtime = rawData[[j]]@featureData@data$retentionTime[1],
            mass0 = rep(round((myChr[x]@filterMz[1] + myChr[x]@filterMz[2]) / 2), 1)
          )
        } else {
          data.frame(
            intensity = rep(0, length(myChr[x]@rtime)),
            rtime = myChr[x]@rtime,
            mass0 = rep(round((myChr[x]@filterMz[1] + myChr[x]@filterMz[2]) / 2), length(myChr[x]@rtime))
          )
        }
      }
    }

    for (j in seq_along(rawData)) {
      incProgress(1/(nrow(unique(namesMatrix))+ length(rawData)))
      myChr <- MSnbase::chromatogram(rawData[[j]], rtMatrix, mzMatrix)
      extractData.ls <- lapply(seq_len(nrow(myChr)), FUN = metaboliteData) # list of data.frames
      extractData.ls2 <- extractData.ls
      names(extractData.ls) <- namesMatrix
      names(extractData.ls2) <- namesMatrix2
      
      extractData.df <- do.call(rbind, extractData.ls) %>% dplyr::mutate(fragment = gsub(pattern = "\\..*",replacement = "", x = row.names(.)), .before = 1)

      
      predictions.ls2 <- list()
      correctedIntensities4 <- list()
      
      if (baselineCorrection == TRUE) {
        
        for (i in seq_along(namesMatrix)) {
          
          if (all(is.na(extractData.ls2[[i]]$intensity[1])) == FALSE) {
            rtWindow <- data.frame(rtime = extractData.ls2[[i]]$rtime)
            leftScan <- subset(extractData.ls2[[i]], is.na(intensity) == FALSE)[1, ]
            rightScan <- subset(extractData.ls2[[i]], is.na(intensity) == FALSE)[nrow(subset(extractData.ls2[[i]], is.na(intensity) == FALSE)), ] # Instead of using only one value, we could do an AVERAGE intensity of multiple scan in each side of the integration window.
            modelData <- rbind(leftScan, rightScan)
            
            suppressWarnings({
            model <- lm(intensity ~ rtime, data = modelData)
            })
            
            prediction <- predict(model, newdata = rtWindow, type = "response")
            
            prediction2 <- data.frame(intensity = prediction, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0)
            predictions.ls <- list(prediction2)
            names(predictions.ls) <- namesMatrix[i] 
            predictions.ls2 <- append(predictions.ls2, predictions.ls)
            
            correctedIntensities <- data.frame(intensity = extractData.ls2[[i]]$intensity - prediction, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0)
            correctedIntensities$intensity[correctedIntensities$intensity < 0] <- 0 # removes negative values
            correctedIntensities3 <- list(correctedIntensities)
            names(correctedIntensities3) <- namesMatrix2[i]
            correctedIntensities4 <- append(correctedIntensities4, correctedIntensities3)
          } else {
            prediction <- 0 
            predictions.ls <- list(data.frame(intensity = 0, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0))
            names(predictions.ls) <- namesMatrix[i] 
            predictions.ls2 <- append(predictions.ls2, predictions.ls)
            warning("No intensities were found at the the RT low parameter (NA). Baseline correction could not be applied. Please change the RT low parameter")
            
            correctedIntensities <- data.frame(intensity = extractData.ls2[[i]]$intensity - prediction, rtime = extractData.ls2[[i]]$rtime, mass0 = extractData.ls2[[i]]$mass0)
            correctedIntensities3 <- list(correctedIntensities)
            names(correctedIntensities3) <- namesMatrix2[i]
            correctedIntensities4 <- append(correctedIntensities4, correctedIntensities3)
          }
        }
        predictions.df <- do.call(rbind, predictions.ls2) %>% dplyr::mutate(fragment = gsub(pattern = "\\..*",replacement = "", x = row.names(.)), .before = 1)
        
        # Replaced matrixstat:sum2 by base sum()
        integrals.ls <- sapply(correctedIntensities4, function(x) sum(x$intensity, na.rm = TRUE)) 
        integrals.df <- data.frame(namesMatrix2, integrals.ls, namesMatrix)
        colnames(integrals.df) <- c("isotopomer", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "Subset")
        assign(integrals.array2[j], integrals.df)
      } else {
        integrals.ls <- sapply(extractData.ls2, function(x) sum(x$intensity, na.rm = TRUE)) 
        integrals.df <- data.frame(namesMatrix2, integrals.ls, namesMatrix)
        colnames(integrals.df) <- c("isotopomer", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "Subset")
        assign(integrals.array2[j], integrals.df)
      }
      
      if (saveAllPlots == TRUE & baselineCorrection == TRUE) {
        for (i in unique(namesMatrix)) {
          incProgress(1/(3*nrow(unique(namesMatrix))))
          
          if (dir.exists(paste0(folder, "/PLOTS/", i)) == FALSE) {
            dir.create(paste0(folder, "/PLOTS/", i), mode = "0777")
          }

          png(paste0(folder, "/PLOTS/", i, "/File_", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "_", i, ".png"))

          print(
            lattice::xyplot(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$intensity ~ subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$rtime/60, groups= subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$mass0,type="l", xlab="Retention time (min)", ylab= "Intensity", main= gsub(".CDF", "", row.names(rawData[[j]]@phenoData)),scales = list(tck=c(1,0)), auto.key=list(points = FALSE, lines = TRUE, title=paste0(sub(" .*", "", unique(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$.id))),cex.title=1.2, corner = c(1, 1), x = 1, y = 1, size = 1.5)) + 
              
              latticeExtra::as.layer(lattice::xyplot(subset.data.frame(predictions.df, subset = predictions.df[, 1] == i)$intensity~ subset.data.frame(predictions.df, subset = predictions.df[, 1] == i)$rtime/60, groups= subset.data.frame(predictions.df, subset = predictions.df[, 1] == i)$mass0,type="l",lty=2, lwd = 0.5)) 
          )
          dev.off()

        }
      } else {
        if (saveAllPlots == TRUE & baselineCorrection == FALSE) { 
          for (i in unique(namesMatrix)) {
            if (dir.exists(paste0(folder, "/PLOTS/", i)) == FALSE) {
              dir.create(paste0(folder, "/PLOTS/", i), mode = "0777")
            }
            
            ggplot() + geom_line(data = subset.data.frame(extractData.df, subset = extractData.df[, 1] == i), mapping = aes(x = rtime / 60, y = intensity, colour = factor(mass0))) + labs(title = gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), x = "Time (min)", y = "Intensity", colour = paste0(sub(" .*", "", unique(subset.data.frame(extractData.df, subset = extractData.df[, 1] == i)$.id)), " m/z")) +
              theme(
                axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
                axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
                plot.title = element_text(size = 20, color = "Black")
              ) + ggsave(paste0(folder, "/PLOTS/", i, "/File_", gsub(".CDF", "", row.names(rawData[[j]]@phenoData)), "_", i, ".png"))
          }
        }
      }
    }
    
    mytable <- matrix()
    for (i in unique(namesMatrix)) {
      
      incProgress(1/(nrow(unique(namesMatrix)) + length(rawData)))
      
      mytable <- lapply(seq_along(rawData), function(x) {
        
        base::get(integrals.array2[x]) %>% dplyr::filter(Subset == i) %>% .[2] %>% dplyr::mutate("Measurements/Samples" = paste0(i, "_", 0:(nrow(.) - 1))) %>% .[2:1]
        
      })
      
      mytable <- multi_full_join(mytable)
      

      xlsx::write.xlsx(mytable, paste0(results, i, ".xlsx"),row.names = F)
      
      mytable2 <- mytable[-1]
      
      if (isotopeCorrection == TRUE) {
        # Perform isotopic correction using isocorrectorR.
        IsoCorrectoR::IsoCorrection(MeasurementFile = paste0(results, i, ".xlsx"), ElementFile = paste(pathExperimentFolder, "/Parameters/ElementFile.xlsx", sep = ""), MoleculeFile = paste0(pathExperimentFolder, "/Parameters/MoleculeFile.xlsx"), DirOut = paste0(results), FileOutFormat = "xls", FileOut = i, CorrectAlsoMonoisotopic = correctAlsoMonoisotopic,CorrectTracerImpurity =  correctTracerImpurity)
        
        file.remove(paste0(results, i, ".xlsx"))
        dirList <- list.dirs(results, recursive = FALSE, full.names = TRUE)
        file.rename(dirList[1], paste0(results, i)) # format(Sys.time(),"%H%M%S")
      }

      if (isotopeCorrection == TRUE) {
        # Create a RawDataFractions sheet in the isocorrectorR file
        m <- sapply(mytable2, function(x) {
          as.numeric(x) / sum(x)
        })
        rownames(m) <- rownames(mytable2)
        
        wb2 <- xlsx::loadWorkbook(list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        try(RawDataFractions <- xlsx::createSheet(wb2, sheetName = "RawDataFractions"), silent = TRUE)
        RawDataFractions <- xlsx::createSheet(wb2, sheetName = "RawDataFractions")
        xlsx::addDataFrame(data.frame(m, check.names = FALSE), RawDataFractions)
        xlsx::saveWorkbook(wb2, list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        
        # get the theoritical MIDs
        formula <- gsub("LabC[0-9]{1,3}", "", subset(moleculeFile, moleculeFile[1] == i, select = 2))
        theoriticalMID <- ecipex::ecipex(formula)
        theoriticalMID2 <- sapply(theoriticalMID, function(x) {
          x[3] <- round(x[1])
          myfill <- matrix(nrow = nrow(unique(x[3])), ncol = 3)
          k <- 1
          for (l in unlist(unique(x[3]))) {
            s <- subset(x, mass.1 == l)
            mySum <- sum(s$abundance)
            myMass <- weighted.mean(s$mass, s$abundance)
            myfill[k, 1] <- myMass
            myfill[k, 2] <- mySum
            k <- k + 1
          }
          myfill[, 3] <- myfill[, 2] / max(myfill[, 2]) * 100
          x <- list(data.frame("mass" = myfill[, 1], "abundance% theory" = myfill[, 2], "mol% theory" = myfill[, 3], check.names = FALSE))
        })
        
        isocorFractions <- readxl::read_excel(list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE), sheet = "CorrectedFractions")
        isocorFractions <- isocorFractions[, -1]

        if (length(rawData) > 1) {
          
          ## raw data ratios
          meanRawData <- matrix(rowMeans(m, na.rm = TRUE))
          sdsRawData <- matrix(matrixStats::rowSds(as.matrix(m), na.rm = TRUE))
          
          ## raw data mol%
          m2 <- sapply(data.frame(m), function(x) {
            as.numeric(x) / x[1] * 100
          })
          
          meanMolRawData <- matrix(rowMeans(m2, na.rm = TRUE))
          sdsMolRawData <- matrix(matrixStats::rowSds(m2, na.rm = TRUE))
          
          # isocor data ratios
          meanIsocorData <- matrix(rowMeans(sapply(isocorFractions, function(x) {
            as.numeric(x)
          }), na.rm = TRUE))
          sdsIsocorData <- matrix(matrixStats::rowSds(sapply(isocorFractions, function(x) {
            as.numeric(x)
          }), na.rm = TRUE))
          
          # isocor data mol%
          m3 <- sapply(isocorFractions, function(x) {
            as.numeric(x) / as.numeric(x)[1] * 100
          })
          
          meanMolIsocorData <- matrix(rowMeans(m3, na.rm = TRUE))
          sdsMolIsocorData <- matrix(matrixStats::rowSds(m3, na.rm = TRUE))
          
          # Diff mol% raw data and theoritical mol%
          difference <- meanMolRawData - theoriticalMID2[[1]]$`mol% theory`[1:length(meanMolRawData)]
          group <- cbind.fill("mean abundance data" = meanRawData, "mean mol% data" = meanMolRawData, "stdev mol% data" = sdsMolRawData, "difference" = difference, "mean abundance isocor" = meanIsocorData, fill = NA)
          colnames(group) <- c("mean abundance data", "mean mol% data", "stdev mol% data", "difference", "mean abundance isocor")
        } else {
          
          ## raw data ratios
          singleRawData <- RawData[1] / sum(RawData[1])
          
          ## raw data mol%
          singleMolRawData <- singleRawData / singleRawData[[1]][1] * 100
          
          # isocor data ratios
          singleIsocorData <- isocorFractions[1]
          
          # isocor data mol%
          singleMolIsocorData <- singleIsocorData / singleIsocorData[[1]][1] * 100
          
          # Diff mol% raw data and theoritical mol%
          difference <- singleMolRawData - theoriticalMID2[[1]]$`mol% theory`[1:nrow(singleMolRawData)]
          group <-  cbind.fill("abundance data" = singleRawData, "mol% data" = singleMolRawData, "difference" = difference, "abundance isocor" = singleIsocorData, fill = NA)
          colnames(group) <- c("abundance data", "mol% data", "difference", "abundance isocor")
        }
        
        ## Write new sheet
        wb3 <- loadWorkbook(list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        
        # cell styles
        cs1 <- CellStyle(wb3) + Font(wb3, isBold = TRUE, boldweight = 700) + Border(color = "black", position = c("TOP", "BOTTOM")) + Alignment(h = "ALIGN_CENTER", v = "VERTICAL_CENTER") + DataFormat("0.00")
        cs2 <- CellStyle(wb3) + Font(wb3, isBold = TRUE, boldweight = 700) + Border(color = "black", position = "BOTTOM") + Alignment(h = "ALIGN_LEFT", v = "VERTICAL_CENTER") + DataFormat("0.00")
        cs3 <- CellStyle(wb3) + Font(wb3) + Alignment(h = "ALIGN_CENTER", v = "VERTICAL_CENTER") + DataFormat("0.00")
        cs4 <- CellStyle(wb3) + Font(wb3) + Alignment(h = "ALIGN_LEFT", v = "VERTICAL_CENTER") + DataFormat("0.00")
        
        try(sheet <- createSheet(wb3, sheetName = "Checks"), silent = TRUE)
        Checks <- createSheet(wb3, sheetName = "Checks")
        
        addDataFrame(data.frame(c("Formula", "Exact mass"), c(names(theoriticalMID2), round(theoriticalMID2[[1]][[1]][1], 3))), Checks, startRow = 2, row.names = FALSE, col.names = FALSE)
        addDataFrame(theoriticalMID2[[1]], Checks, startRow = 4, row.names = FALSE, colnamesStyle = cs1)
        addDataFrame(as.data.frame(group), Checks, startRow = 4, startColumn = 4, row.names = FALSE, colnamesStyle = cs1)
        addDataFrame(i, Checks, startRow = 1, startColumn = 1, row.names = FALSE, col.names = FALSE)
        myRows <- getRows(Checks)
        myCells <- createCell(myRows[1], colIndex = 2:10)
        sapply(getCells(myRows[1]), function(x) {
          setCellStyle(x, cellStyle = cs2)
        })
        sapply(getCells(myRows[2:3]), function(x) {
          setCellStyle(x, cellStyle = cs4)
        })
        sapply(getCells(myRows[5:length(myRows)]), function(x) {
          setCellStyle(x, cellStyle = cs3)
        })
        
        xlsx::saveWorkbook(wb3, list.files(paste0(results, i), pattern = paste0("IsoCorrectoR_", i, ".xls$"), recursive = TRUE, full.names = TRUE))
        
        # Write some basic parameters used for the analysis
        # Concatenate data from all sheets into one single sheet
        paths <- list.files(folder, pattern = "(.*IsoCorrectoR_)(.*xls$)", recursive = TRUE, full.names = TRUE)
        info <- file.info(paths)
        sortedPaths <- rownames(info[with(info, order(as.POSIXct(mtime))), ])
        
        mergedRawData <- plyr::ldply(seq_along(sortedPaths), function(i) {readxl::read_excel(sortedPaths[i], "RawData")})
        mergedCorrectedRawData <- plyr::ldply(seq_along(sortedPaths), function(i) {readxl::read_excel(sortedPaths[i], "Corrected")}) %>% do.call(rbind.data.frame,.)
        mergedRawDataFractions <- plyr::ldply(seq_along(sortedPaths), function(i) {readxl::read_excel(sortedPaths[i], "RawDataFractions")}) %>% do.call(rbind.data.frame,.)
        mergedCorrectedFractions <- plyr::ldply(seq_along(sortedPaths), function(i) {readxl::read_excel(sortedPaths[i], "CorrectedFractions")}) %>% do.call(rbind.data.frame,.)
        
        wb4 <- createWorkbook(type = "xlsx")
        mySheets <- c("RawData", "Corrected", "RawDataFractions", "CorrectedFractions")
        sapply(mySheets, FUN = function(x) createSheet(wb4, x))
        addDataFrame(mergedRawData, sheet = getSheets(wb4)$"RawData", row.names = FALSE)
        addDataFrame(mergedCorrectedRawData, sheet = getSheets(wb4)$"Corrected", row.names = FALSE)
        addDataFrame(mergedRawDataFractions, sheet = getSheets(wb4)$"RawDataFractions", row.names = FALSE)
        addDataFrame(mergedCorrectedFractions, sheet = getSheets(wb4)$"CorrectedFractions", row.names = FALSE)
        saveWorkbook(wb4, file = paste0(folder, "/mergedData.xlsx"))
      }
    }
    
  })
  #})
}
