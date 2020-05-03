library(magrittr)


# Extraction & Wrangle Functions -------------------------------------------

GetLinksFromHTML <- function(link) {
  
  link %>% 
    xml2::read_html(.) %>% 
    rvest::html_nodes("a") %>%
    rvest::html_attr(name = "href") %>% 
    {.[grepl("pdf", .)]}
  
}

ReadPDFs <- function(link) {
  
  date <- stringr::str_extract(link, "[\\d]+.[\\d]+.[\\d]+") 
  if (!stringr::str_detect(date, "\\.")) {
    date <- paste0(substring(date, first = 1, last = 4),".",
                   substring(date, first = 5, last = 6),".",
                   substring(date, first = 7, last = 8), collapse = "") %>% 
      as.Date(format = "%Y.%m.%d")
  } else {
    date %<>% 
      as.Date(format = "%d.%m.%Y") 
  }
  
  date <- date - lubridate::days(2)
    
  
  pdf <- paste0("https://www.hpsc.ie", link) %>% 
    stringr::str_replace_all(" ", "%20") %>% 
    pdftools::pdf_text(.) %>% 
    readr::read_lines(.) 
  
  return(list(date = date, pdf = pdf))
  
}

ReadPDFsTabulizer <- function(link) {
  date <- stringr::str_extract(link, "[\\d]+.[\\d]+.[\\d]+")
  
  tables <- paste0("https://www.hpsc.ie", link) %>% 
    stringr::str_replace_all(" ", "%20") %>% 
    tabulizer::extract_tables(method = "stream")
  
  return(list(date = date, tables = tables))
  
}

ExtractRegionalData <- function(document) {
  
  start.line <- stringr::str_which(document$pdf, "Carlow")
  
  document$pdf %>%
    {.[start.line:(start.line + 25)]} %>% 
    stringr::str_squish() %>%
    stringr::str_replace_all("\\.|,|\\*","") %>% 
    strsplit(split = " ") %>% 
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename("County" = V1, "Value" = V2, "%Change" = V3) %>% 
    dplyr::mutate_at(dplyr::vars(Value), as.numeric) %>% 
    dplyr::mutate(Date = document$date) %>% 
    dplyr::select(Date, County, Value)
  
}

ExtractTransmissionData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "Transmission classification")
  
  if(length(start.line) == 0) {
    return(tibble::tibble(Date = NA, Key = NA, Value = NA))
  } else {
    table.list <- document$pdf %>%
      {.[(start.line + 1):(start.line + 8)]} %>% 
      stringr::str_squish() %>% 
      stringr::str_replace_all("\\.|,|\\*","")
    
    data.column <- stringr::str_split(table.list, "[\\D]+") %>% 
      purrr::map(purrr::pluck(2)) %>% 
      plyr::ldply(.) %>% 
      tibble::as_tibble(.) %>% 
      dplyr::rename( "Value" = V1) %>% 
      dplyr::mutate_at(dplyr::vars(Value), as.double) %>% 
      dplyr::select(Value)
    
    table <- stringr::str_split(table.list, "[\\d]+") %>% 
      purrr::map(purrr::pluck(1)) %>% 
      purrr::map(stringr::str_trim) %>% 
      plyr::ldply(.) %>% 
      dplyr::rename("Key" = V1) %>% 
      dplyr::bind_cols(data.column) %>% 
      dplyr::mutate(Date = document$date) %>% 
      dplyr::select(Date, Key, Value) %>% 
      dplyr::filter(!grepl("Total", Key)) %>% 
      dplyr::filter(!grepl("Appendix", Key))
  }
  
  return(table)
}

ExtractWorkersData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "No foreign travel")
  
  if(length(start.line) == 0) {
    return(tibble::tibble(Date = NA, `HSE area` = NA, Key = NA, Value = NA))
  } else {
    table.list <- document$pdf %>%
      {.[(start.line + 1):(start.line + 8)]} %>% 
      stringr::str_squish() %>% 
      stringr::str_replace_all("\\.|,|\\*","") %>% 
      stringr::str_replace_all("HB","") %>%
      stringr::str_replace_all("ERHA","HSE East") %>%
      stringr::str_replace_all("MW","HSE Mid-West") %>%
      stringr::str_replace_all("\\b[M]\\b","HSE Midlands") %>%
      stringr::str_replace_all("\\b[S]\\b","HSE South") %>%
      stringr::str_replace_all("\\b[W]\\b","HSE West") %>%
      stringr::str_replace_all("\\b[E]\\b","HSE East") %>%
      stringr::str_replace_all("NE","HSE North-East") %>%
      stringr::str_replace_all("NW","HSE North-West") %>%
      stringr::str_replace_all("SW","HSE South-West") %>%
      stringr::str_replace_all("\\bSE\\b","HSE South-East") %>%
      stringr::str_replace_all("\\bNortheast \\b","North-East") %>%
      stringr::str_replace_all("\\bNorthwest \\b","North-West") %>%
      stringr::str_replace_all("\\bMidwest\\b","Mid-West") %>% 
      stringr::str_replace_all("\\bSoutheast\\b","South-East")
    
    if(document$date == "2020-03-26") {
      table.list %<>% 
        purrr::map_chr(~paste("HSE", .x, collapse = " "))
    }
    
    if(document$date == "2020-03-27") {
      table.list[5] %<>% 
        stringr::str_split(" ") %>% 
        unlist %>% 
        {paste(paste(.[1:4], collapse = " "), "0", .[5])}
    }
    
    numeric.part <- table.list %>% 
      stringr::str_split("[\\D]+") %>% 
      plyr::ldply(.) %>% 
      tibble::as_tibble(.) %>% 
      dplyr::select("Foreign travel" = V2, "Local/Community transmission" = V3,
                    "Not specified" = V4) %>% 
      dplyr::mutate_all(as.double)
    
    table <- stringr::str_split(table.list, "[\\d]+") %>% 
      purrr::map(purrr::pluck(1)) %>% 
      purrr::map(stringr::str_trim) %>% 
      plyr::ldply(.) %>% 
      dplyr::rename("HSE area" = V1) %>% 
      dplyr::bind_cols(numeric.part) %>% 
      tidyr::pivot_longer(-`HSE area`, names_to = "Key", values_to = "Value") %>% 
      dplyr::mutate(Date = document$date) %>% 
      dplyr::select(Date, `HSE area`, Key, Value) 
  }
  
  return(table)
}

ExtractAgeData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "\\(n\\)") %>% 
    {.[length(.)]}
  
  if(length(start.line) == 0) {
    return(tibble::tibble(Date = NA, `Age Group` = NA, Key = NA, Value = NA))
  } else {
    table.list <- document$pdf %>%
      {.[(start.line + 1):(start.line + 9)]} %>% 
      stringr::str_replace_all(",","") %>% 
      stringr::str_replace_all("5 - 14","5-14") %>% 
      stringr::str_replace_all("\\bUnk\\b","Unknown") %>% 
      stringr::str_squish()
    
    test <- stringr::str_which(table.list, "\\d\\.\\d\\s\\d\\.\\d")
    
    if(length(test) != 0) {
      old <- stringr::str_extract(table.list[test], "\\d\\.\\d\\s\\d\\.\\d")
      new <- old %>% 
        stringr::str_split(" ") %>% 
        unlist %>% 
        {paste(.[1], "0", .[2])}
      table.list[test] <- stringr::str_replace(table.list[test], old, new)
    }
    if (document$date == "2020-04-23") {
      table.list[9] %<>% 
        stringr::str_split(" ") %>% 
        unlist %>% 
        {paste(c(.,"0","0","0","0"))} %>% 
        paste(collapse = " ")
    }
    
    table <- table.list %>% 
      strsplit(split = " ") %>%
      plyr::ldply(.) %>% 
      tibble::as_tibble(.) %>% 
      dplyr::rename("Age Group" = V1, "Total" = V2, "Hospitalised" = V3,
                    "In ICU" = V5, "Died" = V7) %>% 
      dplyr::select(-V4, -V6, -V8) %>%
      dplyr::mutate_at(dplyr::vars(-`Age Group`), as.double) %>% 
      tidyr::pivot_longer(-`Age Group`, names_to = "Key", values_to = "Value") %>% 
      dplyr::mutate(Date = document$date) %>% 
      dplyr::select(Date, dplyr::everything())
  }
  
  return(table)
}

ExtractCharacteristicData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "Sex")
  
  table.list <- document$pdf %>%
    {.[(start.line + 1):(start.line + 23)]}  %>% 
    stringr::str_replace_all(",","") %>%  
    stringr::str_squish() %>% 
    {.[-c(4, 15)]}
  
  first.part <- table.list %>% 
    {.[1:13]} %>%
    strsplit(split = " ") %>%
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename("Key" = V1, "Value" = V2) %>% 
    dplyr::select(Key, Value) %>% 
    dplyr::mutate_at(dplyr::vars(Value), as.numeric)
  
  first.part[ 3, "Key"] <- "Unknown Sex"
  first.part[13, "Key"] <- "Unknown Age"
  
  second.part <- table.list %>% 
    {.[14:21]} %>% 
    stringr::str_replace_all("HSE MW","HSE Mid-West") %>%
    stringr::str_replace_all("\\b[M]\\b","Midlands") %>%
    stringr::str_replace_all("\\b[S]\\b","South") %>%
    stringr::str_replace_all("\\b[W]\\b","West") %>%
    stringr::str_replace_all("\\b[E]\\b","East") %>%
    stringr::str_replace_all("HSE NE","HSE North-East") %>%
    stringr::str_replace_all("HSE NW","HSE North-West") %>%
    stringr::str_replace_all("HSE SW","HSE South-West") %>%
    stringr::str_replace_all("HSE SE","HSE South-East")
  
  if (document$date == "2020-04-26") {
    second.part[8] <- "HSE West 939 4.8"
  }
  
  second.part.numeric <- second.part %>% 
    stringr::str_split("[\\D]+") %>% 
    purrr::map(purrr::pluck(2)) %>% 
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename( "Value" = V1) %>% 
    dplyr::mutate_at(dplyr::vars(Value), as.double) %>% 
    dplyr::select(Value)
  
  table <- second.part %>% 
    stringr::str_split("[\\d]+") %>% 
    purrr::map(purrr::pluck(1)) %>% 
    stringr::str_trim(.) %>% 
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename("Key" = V1) %>% 
    dplyr::bind_cols(second.part.numeric) %>% 
    dplyr::bind_rows(first.part) %>% 
    dplyr::mutate(Date = document$date) %>% 
    dplyr::select(Date, Key, Value) %>% 
    dplyr::mutate(`Key 1` = c(rep("HSE area", 8),
                              rep("Sex", 3),
                              rep("Age Group", 10)),
                  `Key 3` = NA) %>% 
    dplyr::filter(!grepl("Age Group", .$'Key 1')) %>% 
    dplyr::select(Date, `Key 1`, `Key 2` = Key, `Key 3`, Value)
  
}

ExtractTotalData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "Total number of confirmed cases")
  
  if (length(stringr::str_which(document$pdf, "CFR")) == 0) {
    end.line <- 9
    lines.tbr <- 2
  } else {
    end.line <- 11
    lines.tbr <- c(2, 3, 7)
  }
  
  table.list <- document$pdf %>%
    {.[start.line:(start.line + end.line)]} %>% 
    stringr::str_squish() %>% 
    stringr::str_replace_all("\\.|,|\\*","") %>% 
    stringr::str_replace("Total number of confirmed cases", "Total") %>% 
    stringr::str_replace("Total number hospitalised", "Hospitalised") %>%
    stringr::str_replace("Total number admitted to ICU", "In ICU") %>%
    stringr::str_replace("Total number of outbreaks/clusters", "Clusters") %>%
    stringr::str_replace("Total number cases associated with clusters", 
                         "In Clusters") %>%
    stringr::str_replace("Total number of deaths", "Dead") %>% 
    stringr::str_replace("Total number of imported cases", "Imported") %>%
    stringr::str_replace("Number of cases in Healthcare workers", 
                         "Healthcare") %>%
    stringr::str_replace("Number of cases in HCW", "Healthcare") %>%
    stringr::str_replace("Median age \\(years\\)", "Median Age")
  
  table.list <- table.list[-lines.tbr]
  
  data.column <- stringr::str_split(table.list, "[\\D]+") %>% 
    purrr::map(purrr::pluck(2)) %>% 
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename( "Values" = V1) %>% 
    dplyr::mutate_at(dplyr::vars(Values), as.double) %>% 
    dplyr::select(Values)
  
  table <- stringr::str_split(table.list, "[\\d]+") %>% 
    purrr::map(purrr::pluck(1)) %>% 
    purrr::map(stringr::str_trim) %>% 
    plyr::ldply(.) %>% 
    dplyr::rename("Key" = V1) %>% 
    dplyr::bind_cols(data.column) %>% 
    dplyr::mutate(Date = document$date) %>% 
    dplyr::select(Date, Key, Values) %>% 
    tidyr::pivot_wider(names_from = "Key", values_from = "Values") %>% 
    dplyr::select(Date, Total, Hospitalised, `In ICU`, Dead, Clusters, 
                  `In Clusters`, Imported, Healthcare, `Median Age`) %>% 
    tidyr::pivot_longer(-Date, names_to = "Key", values_to = "Value")
  
}


# Routine -----------------------------------------------------------------

link <- paste0("https://www.hpsc.ie/a-z/respiratory/coronavirus/",
               "novelcoronavirus/casesinireland/")

PDFs <- purrr::map(c(link,
                     paste0(link, "epidemiologyofcovid-19inireland/"),
                     paste0(link,"epidemiologyofcovid-19inireland/march2020/"),
                     paste0(link,"epidemiologyofcovid-19inireland/april2020/")),
                   GetLinksFromHTML) %>%
  unlist %>%
  purrr::map(ReadPDFs)

county <- purrr::map_dfr(PDFs, ExtractRegionalData) %>%
  dplyr::distinct(.keep_all = TRUE)

total <- purrr::map_dfr(PDFs, ExtractTotalData) %>%
  dplyr::distinct(.keep_all = TRUE)

characteristics <- purrr::map_dfr(PDFs, ExtractCharacteristicData) %>%
  dplyr::distinct(.keep_all = TRUE) 

transmission <- purrr::map_dfr(PDFs, ExtractTransmissionData) %>%
  na.omit %>%
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(`Key 1` = "Transmission Type",
                `Key 3` = NA) %>% 
  dplyr::select(Date, `Key 1`, `Key 2` = Key, `Key 3`, Value)

age <- purrr::map_dfr(PDFs, ExtractAgeData) %>%
  na.omit %>%
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(`Key 1` = "Age Group") %>% 
  dplyr::select(Date, `Key 1`, `Key 2` = `Age Group`, `Key 3` = Key, Value)

workers <- purrr::map_dfr(PDFs, ExtractWorkersData) %>%
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate('Key 1' = "Healthcare Workers Cases") %>% 
  dplyr::select(Date, `Key 1`, `Key 2` = `HSE area`, `Key 3` = Key, Value)

granular.data <- characteristics %>% 
  dplyr::bind_rows(transmission) %>% 
  dplyr::bind_rows(age) %>% 
  dplyr::bind_rows(workers)


# Save Files --------------------------------------------------------------

write.csv(county, here::here("Covid19_Data_By_County.csv"), row.names = FALSE)
write.csv(total,  here::here("Covid19_Data_Total_Ireland.csv"), row.names = FALSE)
write.csv(granular.data,  
          here::here("Covid19_Data_Positive_Characteristics.csv"),
          row.names = FALSE)