CalculateIncrement <- function(data, fields) {
  
  inc.data <- data %>% 
    dplyr::mutate(Date = as.Date(Date)) %>% 
    dplyr::arrange(Date) %>% 
    dplyr::group_by_at(dplyr::vars(fields)) %>% 
    dplyr::mutate_at(dplyr::vars(-Date, -fields), 
                     function(x) x - dplyr::lag(x, default = 0)) %>% 
    dplyr::ungroup()
  
  return(inc.data)
}

WrangleCovidData <- function(data, all.ireland) {
  fixed.columns <- setdiff(colnames(data), c("Date", "Value"))
  
  data.inc <- CalculateIncrement(data, fixed.columns) %>%
    dplyr::mutate(Type = "Increment")
  
  data %<>% 
    dplyr::arrange(Date) %>% 
    dplyr::mutate(Type = "Cumulative") %>% 
    dplyr::bind_rows(data.inc)
  
  if (all.ireland) {
    types <- data %>% 
      dplyr::select_at(fixed.columns) %>% 
      unique %>% 
      unlist %>% 
      unname
    
    data %<>% 
      tidyr::pivot_wider(id_cols = c("Date"),
                         names_from = c("Type", fixed.columns),
                         values_from = "Value")
  } else {
    types <- "Total"
    
    data %<>% 
      dplyr::rename("Total" = "Value") %>% 
      tidyr::pivot_longer(-c("Date", fixed.columns, "Type"),
                          names_to = "Field",
                          values_to = "Value") %>% 
      tidyr::pivot_wider(id_cols = c("Date", fixed.columns),
                         names_from = c("Type", "Field"),
                         values_from = "Value")
  }
  
  return(list(data = data, types = types))
}

CalculateChange <- function(data, field) {
  ratio.header <- paste("Ratio", field, sep = "_")
  denominator <- rlang::sym(paste("Cumulative", field, sep = "_"))
  numerator <- rlang::sym(paste("Increment", field, sep = "_"))
  
  data %<>% 
    dplyr::transmute(!!ratio.header := 1/(!!denominator/!!numerator - 1))
  return(data)
}

CalculateReturns <- function(data, types) {
  final.data <- cbind(data, purrr::map_dfc(types, ~CalculateChange(data, .x))) %>% 
    tibble::as_tibble(.)
  
  return(final.data)
}

GetRawData <- function() {
  address <- "https://raw.githubusercontent.com/DavideMagno/IrelandCovidData/master/"
  
  file.prefix <- paste0(address, "Covid19_Data_")
  
  files <- c(paste0(file.prefix, c("Total_Ireland.csv", "By_County.csv")))
  
  covid <- purrr::map(files, ~readr::read_csv(url(.x))) %>% 
    purrr::map2(c(TRUE, FALSE), WrangleCovidData) %>% 
    purrr::map(~CalculateReturns(.x$data, .x$types))
  
  ireland.regions <- readRDS(here::here("Dashboard/data/gadm36_IRL_1_sp.rds"))
  
  ireland.regions@data$NAME_1 %<>%
    stringr::str_replace("Laoighis", "Laois") 
  
  return(list(covid.ireland = covid[[1]],
              covid.regions = covid[[2]],
              ireland.regions = ireland.regions))
}