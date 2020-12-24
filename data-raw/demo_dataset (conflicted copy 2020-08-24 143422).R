## code to prepare `demo_dataset` dataset goes here
future::plan("multiprocess")
future::plan("sequential")

usethis::use_data(demo_dataset, overwrite = TRUE)

demo_dataset <- reactive({
  # browser()
  future::plan("multiprocess")
  # browser()
  # req(input$fileButton != 0)
  #if (!is.null(directory())) {

    netCDFs <- list.files("/home/mathieu/R/MSinco/data-raw",full.names = TRUE, pattern = ".CDF$", ignore.case = T)
    cat(netCDFs)
    # browser()
    tmp <- future.apply::future_lapply(seq_along(netCDFs), function(i) {
      
      data <- MSnbase::readMSData(gtools::mixedsort(netCDFs)[i], mode = "onDisk", msLevel. = 1)
      df <- methods::as(data,  "data.frame") %>% data.table::setDT()
      df$phenoData <- data@phenoData %>% rownames()
      df
    })
    
    plotIndex <<- findInterval(length(tmp)/2, seq_len(length(tmp)), all.inside = T)

  
  # future::plan("sequential")
  tmp
  # } else { NULL }
  
})
msinco_sample_data <- tmp
moleculeFile_example <- openxlsx::read.xlsx("/home/mathieu/Documents/MSinco\ demo/MSinco\ demo/Parameters/MoleculeFile_dataset.xlsx")
parameterFile_example <- openxlsx::read.xlsx("/home/mathieu/Documents/MSinco\ demo/MSinco\ demo/Parameters/ParameterFile_dataset.xlsx")
elementFile_example <- openxlsx::read.xlsx("/home/mathieu/Documents/MSinco\ demo/MSinco\ demo/Parameters/ElementFile_dataset.xlsx")


# # faster way of extracting the data
# plot(test@backend$var$total_intensity$dim[[1]]$vals)
# 
# system.time({
# 
#   files <- mzR::openMSfile(netCDFs[[1]])
#   files@backend$var$total_intensity$dim[[1]]$vals
#   pl <- mzR::spectra(files) # pl <- mzR::peaks(files) # peaks and spectra re equivalent i think
#   #pl2 <- rtime(onMSdiskExp object or whatever) # so still need to do readMSdata(netcdf file)
#     test2  <- mapply(function(x,y) {
# 
#       cbind(rt = y, x)
# 
#     },x= pl, y=pl2) %>% do.call(rbind,.) %>% as.data.frame() %>% data.table::setDT()
# 
# })
# # 
# system.time({
# 
#   spectras <- MSnbase::spectrapply(object = a, FUN = function(x) {
# 
#     data.frame(rt = x@rt, mz = x@mz, i = x@intensity, tic = x@tic)
# 
#   })  %>% do.call(rbind,.) %>% data.table::setDT()
# 
# })
# 
# 
# system.time({
#   
#   spectras <- MSnbase::spectra(a)
#     
#   test2  <- mapply(function(x,y) {
#     
#     cbind(rt = y, x)
#     
#   },x= spectras, y=pl2) %>% do.call(rbind,.) %>% as.data.frame() %>% data.table::setDT()
#   
# 
# })

