future::plan("multiprocess")


## code to prepare `demo_dataset` dataset goes here
## En gros ce fichier creer des donnes qui seront place dans /data. Ces donnes sont inclus dans le packet et accesible a l utilisateur.
## Sinon place les donnes dans isnt/

netCDFs <- list.files("/home/mathieu/R/MSinco/data-raw",full.names = TRUE, pattern = ".CDF$", ignore.case = T)
cat(netCDFs)

demo_dataset <- future.apply::future_lapply(seq_along(netCDFs), function(i) {
    
    data <- MSnbase::readMSData(gtools::mixedsort(netCDFs)[i], mode = "onDisk", msLevel. = 1)
    df <- methods::as(data,  "data.frame") %>% data.table::setDT()
    df$phenoData <- data@phenoData %>% rownames()
    df
  })

future::plan("sequential")

moleculeFile <- openxlsx::read.xlsx("/home/mathieu/Documents/MSinco\ demo/MSinco\ demo/Parameters/MoleculeFile_dataset.xlsx")
parameterFile <- openxlsx::read.xlsx("/home/mathieu/Documents/MSinco\ demo/MSinco\ demo/Parameters/ParameterFile_dataset.xlsx")
elementFile <- openxlsx::read.xlsx("/home/mathieu/Documents/MSinco\ demo/MSinco\ demo/Parameters/ElementFile_dataset.xlsx")


usethis::use_data(demo_dataset, overwrite = TRUE)
usethis::use_data(moleculeFile, overwrite = TRUE)
usethis::use_data(parameterFile, overwrite = TRUE)
usethis::use_data(elementFile, overwrite = TRUE)

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

