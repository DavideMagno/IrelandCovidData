library(magrittr)

FilterData <- function(Data, type) {
  if (grepl("National", type)) {
    data <- Data$covid.ireland
    fixed.columns <- c("Date")
  } else {
    data <- Data$covid.regions
    fixed.columns <- c("Date", "County")
  }
  ireland <- Data$ireland.regions
  ireland$name <- ireland@data$NAME_1
  
  return(list(data = data,
              fixed.columns = fixed.columns,
              ireland = ireland))
}

Extract <- function(Data, filter_by = "", select_method = "Cumulative", 
                    type = "County", select_field = "Total", 
                    start_date = as.Date("2020-02-24"), end_date = NULL) {
  
  raw.data <- FilterData(Data, type)
  
  data <- purrr::pluck(raw.data, "data") 
  
  if (grepl("County", type)) {
    data  %<>% 
      dplyr::filter(grepl(paste(filter_by, collapse="|"), .[[type]]))
  } 
  
  data %<>% 
    dplyr::filter(Date >= start_date) %>% 
    dplyr::select(purrr::pluck(raw.data, "fixed.columns"),
                  paste(select_method, select_field, sep = "_")) 
  
  colnames(data) <- gsub(paste0(select_method,"_"), "", colnames(data), 
                         fixed=TRUE)
  
  if (is.null(start_date)) {
    data %<>% 
      dplyr::filter(Date >= as.Date("2020-02-24"))
  }
  
  if (!is.null(end_date)) {
    data %<>% 
      dplyr::filter(Date <= end_date)
  }
  return(data)
}

PrepareDataForExtraction <- function(regions, date.range, last.date,
                                     data.field) {
  
  if (is.null(regions)) {
    filter_by <- ""
    type <- "National"
    if (is.null(data.field)) {
      data.field <- "Total"
    } else {
      data.field <- data.field
    }
  } else {
    filter_by <- regions
    type <- "County"
    data.field <- "Total"
  }
  
  start_date <- ifelse(is.null(date.range[1]), 
                       as.Date("2020-03-23"), date.range[1])
  end_date <- ifelse(is.null(date.range[2]), 
                     last.date, date.range[2])
  
  return(list(filter_by = filter_by, type = type, start_date = start_date,
              end_date = end_date, data.field = data.field))
}