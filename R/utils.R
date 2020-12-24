keybinding_return <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#run").click();
}});'


canvas_plots  <- ggplot2::ggplot() + ggplot2::theme(
  axis.text.x = ggplot2::element_text(size = 11,colour = "grey0"), axis.title.x = ggplot2::element_text(size = 12,colour = "grey0"),
  axis.text.y = ggplot2::element_text(size = 11, colour = "grey0"), axis.title.y = ggplot2::element_text(size = 12, colour = "grey0",angle = 90),
  plot.title = ggplot2::element_text(size = 14, color = "grey0"),
  panel.background = ggplot2::element_rect(fill = "white"),
  panel.border = ggplot2::element_rect(fill = NA,colour = "grey0"),
  legend.background = ggplot2::element_blank(),
  legend.box.background = ggplot2::element_blank(),
  legend.position = c(0.95, 0.95),
  legend.justification = c("right", "top"),
  legend.key= ggplot2::element_blank(),
  complete = T
)

multi_full_join <- function(list) Reduce(function(x,y) dplyr::full_join(x,y , by ="Measurements/Samples"), list)


choose.files <- function () 
{
  os = easycsv::Identify.OS()
  if (tolower(os) == "windows") {
    file <- utils::choose.file()
  }
  if (tolower(os) == "linux") {
    file <- system("zenity --file-selection --multiple > /tmp/R_folder_file", 
                   intern = TRUE) %>%  strsplit("\\|") %>% unlist()
  }
  if (tolower(os) == "macosx") {

    file <- system("osascript -e 'tell app \"RStudio\" to choose file with prompt \"Choose file:\" multiple selections allowed'", 
           intern = TRUE) %>% strsplit(",") %>% unlist() %>% sub("^[^:]*:",":", .) %>% gsub(":","/", .)
  }
  return(file)
}

detach_all <- function() {
  basic.pkg <- c("package:stats", "package:graphics", "package:grDevices", 
                 "package:utils", "package:datasets", "package:methods", "package:base")
  
  pkg.list <- base::search()[ifelse(unlist(gregexpr("package:", search())) == 1 ,TRUE, FALSE)]
  
  pkg.list <- dplyr::setdiff(pkg.list, basic.pkg)
  
  base::lapply(pkg.list, detach, character.only = TRUE)
}

is.even <- function(x) {(x %% 2) == 0}


## from package xcms
match_closest <- function(x, y, maxDiff = min(mean(diff(x)), mean(diff(y)))) {
  vapply(x, function(a) {
    diffs <- abs(y - a)
    idx <- dplyr::intersect(which(diffs <= maxDiff), which.min(diffs))
    if (length(idx))
      idx
    else NA_integer_
  }, integer(1))
}

##
## from package xcms
rbind_fill <- function(x, y) {
  cnx <- colnames(x)
  cny <- colnames(y)
  cn <- dplyr::union(cnx, cny)
  mis_col <- dplyr::setdiff(cn, colnames(x))
  for (mc in mis_col) {
    if (is.factor(y[, mc]))
      x <- cbind(x, tmp = as.factor(NA))
    else
      x <- cbind(x, tmp = as(NA, class(y[, mc])))
  }
  colnames(x) <- c(cnx, mis_col)
  mis_col <- dplyr::setdiff(cn, colnames(y))
  for (mc in mis_col) {
    if (is.factor(x[, mc]))
      y <- cbind(y, tmp = as.factor(NA))
    else
      y <- cbind(y, tmp = as(NA, class(x[, mc])))
  }
  colnames(y) <- c(cny, mis_col)
  rbind(x, y[, colnames(x)])
}

##
## from package rowr

cbind.fill<-function(...,fill=NULL)
{
  inputs<-list(...)
  inputs<-lapply(inputs,vert)
  maxlength<-max(unlist(lapply(inputs,len)))
  bufferedInputs<-lapply(inputs,buffer,length.out=maxlength,fill,preserveClass=FALSE)
  return(Reduce(cbind.data.frame,bufferedInputs))
}

len <- function(data)
{
  result<-ifelse(is.null(nrow(data)),length(data),nrow(data))
  return(result)
}

vert<-function(object)
{
  if(is.list(object))
    object<-cbind(object)
  object<-data.frame(object)
  
  return(object)
}

as2<-function(object,class)
{
  object<-as.matrix(object)
  if(class=='factor')
    return(as.factor(as.character(object)))
  if(class=='data.frame')
    return(as.data.frame(object))
  else
    return(as(object,class))
}

buffer<-function(x,length.out=len(x),fill=NULL,preserveClass=TRUE)
{
  xclass<-class(x)
  input<-lapply(vert(x),unlist)
  results<-as.data.frame(lapply(input,rep,length.out=length.out))
  if(length.out>len(x) && !is.null(fill))
  {
    results<-t(results)
    results[(length(unlist(x))+1):length(unlist(results))]<-fill
    results<-t(results)
  }
  if(preserveClass)
    results<-as2(results,xclass)
  return(results)   
}

##



