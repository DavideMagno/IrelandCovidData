library(magrittr)


# Extraction & Wrangle Functions -------------------------------------------

GetLinksFromHTML <- function(link) {
  
  link %>% 
    xml2::read_html(.) %>% 
    rvest::html_nodes("a") %>%
    rvest::html_attr(name = "href") %>% 
    {.[grepl("pdf", .)]}
  
}

GetDates <- function(link) {
  date <- stringr::str_extract(link, "[\\d]+.[\\d]+.[\\d]+") 
  if (!stringr::str_detect(date, "\\.")) {
    if (substring(date, 1, 4) == 2020) {
      date <- paste0(substring(date, first = 1, last = 4),".",
                     substring(date, first = 5, last = 6),".",
                     substring(date, first = 7, last = 8), collapse = "")
    } else {
      date <- paste0(substring(date, first = 5, last = 8),".",
                     substring(date, first = 3, last = 4),".",
                     substring(date, first = 1, last = 2), collapse = "")
    }
     date %<>% 
      as.Date(format = "%Y.%m.%d")
  } else {
    date %<>% 
      as.Date(format = "%d.%m.%Y") 
  }
  
  date - lubridate::days(2)
}

ReadPDFs <- function(link) {
  
  date <- GetDates(link)
  
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
    {.[start.line[1]:(start.line[1] + 25)]} %>% 
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

  if(document$date >= "2020-05-24") {
    start.line <- start.line[1]
    end.line <- 4
  } else {
    end.line <- 8
  }
  
  if(length(start.line) == 0) {
    return(tibble::tibble(Date = NA, Key = NA, Value = NA))
  } else {
    table.list <- document$pdf %>%
      {.[(start.line + 1):(start.line + end.line)]} %>% 
      stringr::str_squish() %>% 
      stringr::str_replace_all("\\.|,|\\*","") %>% 
      stringr::str_remove_all("incl possible community transmission") %>% 
      stringr::str_remove_all("incl possible local transmission") %>% 
      stringr::str_remove_all("incl possible travel abroad") %>% 
      stringr::str_remove_all("Possible") 
    
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
      dplyr::mutate(Key = stringr::str_to_title(Key)) %>% 
      dplyr::group_by(Key) %>% 
      dplyr::summarise_at("Value", sum) %>% 
      dplyr::mutate(Date = document$date) %>% 
      dplyr::select(Date, Key, Value) %>% 
      dplyr::filter(!grepl("Total", Key)) %>% 
      dplyr::filter(!grepl("Appendix", Key)) %>% 
      dplyr::ungroup()
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
  
  if (document$date <= "2020-05-23") {
    message(document$date)
    if (document$date >= "2020-05-07") {
      end.line <- 11
    } else {
      end.line <- 9
    }
    start.line <- stringr::str_which(document$pdf, "\\(n\\)") %>% 
      {.[length(.)]}
    
    if(length(start.line) == 0) {
      return(tibble::tibble(Date = NA, `Age Group` = NA, Key = NA, Value = NA))
    } else {
      table.list <- document$pdf %>%
        {.[(start.line + 1):(start.line + end.line)]} %>% 
        stringr::str_replace_all(",","") %>% 
        stringr::str_replace_all("5 - 14","5-14") %>% 
        stringr::str_replace_all("\\bUnk\\b","Unknown") %>% 
        stringr::str_squish()
      
      test <- stringr::str_which(table.list, "\\d\\.\\d\\s\\d\\.\\d")
      
      if(length(test) != 0) {
        if (document$date <= "2020-05-13" & 
            document$date > "2020-05-08") {
          table.list <- stringr::str_replace_all(table.list,"\\b0.0\\b", "0 0.0")
        } else {
          old <- stringr::str_extract(table.list[test], "\\d\\.\\d\\s\\d\\.\\d")
          new <- old %>%
            stringr::str_split(" ") %>%
            purrr::map_chr(~paste(.x[1], "0", .x[2]))
          for (i in seq_along(test)) {
            table.list[test[i]] <- stringr::str_replace(table.list[test[i]], old[i], new[i])
          }
        }
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
  } else {
    table <- document$pdf[stringr::str_detect(document$pdf, "\\(n\\)")] %>% 
      {.[-1]} %>% 
      stringr::str_remove_all("\\(n\\)") %>% 
      stringr::str_replace_all(",","") %>% 
      stringr::str_replace_all("cases","Total") %>% 
      stringr::str_replace_all("Deaths","Died") %>% 
      stringr::str_replace_all("hospitalised","Hospitalised") %>% 
      stringr::str_squish() %>% 
      stringr::str_split(" ") %>% 
      plyr::ldply(.) %>% 
      tibble::as_tibble(.) %>% 
      dplyr::rename("Key" = V1, "<5" = V2, "5-14" = V3, "15-24" = V4, 
                    "25-34" = V5, "35-44" = V6, "45-54" = V7, "55-64" = V8, 
                    "65-74" = V9, "75-84" = V10, "85+" = V11, "Unknown" = V12) %>% 
      dplyr::mutate_at(dplyr::vars(-"Key"), as.double) %>% 
      tidyr::pivot_longer(-Key, names_to = "Age Group", values_to = "Value") %>% 
      dplyr::mutate(Key = dplyr::case_when(
        grepl("\\bICU\\b", Key) ~ "In ICU", 
        TRUE ~ Key)) %>% 
      dplyr::mutate(Date = document$date) %>% 
      dplyr::select(Date, 'Age Group', Key, Value)
  }
  
  return(table)
}

ExtractCharacteristicData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "Sex")

  if(document$date >= "2020-05-24") {
    end.line <- 17
    lines.tbr <- 4:6
    first.part.end <- 14
    size.agegroup <- 11
  } else {
    end.line <- 23
    first.part.end <- 13
    size.agegroup <- 10
    if(document$date >= "2020-05-07") {
      end.line <- end.line + 2
      first.part.end <- first.part.end + 2
      size.agegroup <- size.agegroup + 2
      lines.tbr <- c(4, 17)
    }  else {
      lines.tbr <- c(4, 15)
    }
  }
  
  table.list <- document$pdf %>%
    {.[(start.line + 1):(start.line + end.line)]}  %>% 
    stringr::str_replace_all(",","") %>%  
    stringr::str_squish() %>% 
    {.[-lines.tbr]} %>% 
    stringr::str_remove_all("yrs ")
  
  first.part <- table.list %>% 
    {.[1:first.part.end]} %>%
    strsplit(split = " ") %>%
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename("Key" = V1, "Value" = V2) %>% 
    dplyr::select(Key, Value) %>% 
    dplyr::mutate_at(dplyr::vars(Value), as.numeric)
  
  first.part[ 3, "Key"] <- "Unknown Sex"
  first.part[first.part.end, "Key"] <- "Unknown Age"
  
  if (document$date < "2020-05-24") {
  second.part <- table.list %>% 
    {.[(first.part.end + 1):(length(table.list))]} %>% 
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
                              rep("Age Group", size.agegroup)),
                  `Key 3` = NA) %>% 
    dplyr::filter(!grepl("Age Group", .$'Key 1')) %>% 
    dplyr::select(Date, `Key 1`, `Key 2` = Key, `Key 3`, Value)
  } else {
    table <- first.part %>% 
      dplyr::mutate(Date = document$date) %>% 
      dplyr::select(Date, Key, Value) %>% 
      dplyr::mutate(`Key 1` = c(rep("Sex", 3),
                                rep("Age Group", size.agegroup)),
                    `Key 3` = NA) %>% 
      dplyr::filter(!grepl("Age Group", .$'Key 1')) %>% 
      dplyr::select(Date, `Key 1`, `Key 2` = Key, `Key 3`, Value)
  }
}

ExtractTotalData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "Total number of confirmed cases")
  if (length(stringr::str_which(document$pdf, "CFR")) == 0) {
    if (length(stringr::str_which(document$pdf, "death rate")) == 0) {
      end.line <- 9
    } else {
      end.line <- 12
    }
  } else {
    if (length(stringr::str_which(document$pdf, "M:F")) == 0) {
      if (document$date <= "2020-03-20") {
        end.line <- 10
        lines.tbr <- c(2, 5, 10)
      } else {
        end.line <- 11
      }
    } else {
      end.line <- 18
    }
  }
  table.list <- document$pdf %>%
    {.[start.line:(start.line + end.line)]} %>% 
    stringr::str_squish() %>% 
    stringr::str_replace_all("\\.|,|\\*","") %>% 
    stringr::str_replace("Total number of confirmed cases", "Total") %>% 
    stringr::str_replace("Total number hospitalised", "Hospitalised") %>%
    stringr::str_replace("Total number of cases hospitalised", "Hospitalised") %>%
    stringr::str_replace("Total number admitted to ICU", "In ICU") %>%
    stringr::str_replace("Total number of cases admitted to ICU", "In ICU") %>%
    stringr::str_replace("Total number of outbreaks/clusters", "Clusters") %>%
    stringr::str_replace("Total number cases associated with clusters", 
                         "In Clusters") %>%
    stringr::str_replace("Total number of cases associated with clusters", 
                         "In Clusters") %>%
    stringr::str_replace("Total number of deaths", "Dead") %>% 
    stringr::str_replace("Total number of deaths among confirmed cases", "Dead") %>% 
    stringr::str_replace("Dead among confirmed cases", "Dead") %>% 
    stringr::str_replace("Total number of imported cases", "Imported") %>%
    stringr::str_replace("Number of cases in Healthcare workers", 
                         "Healthcare") %>%
    stringr::str_replace("Number of cases in healthcare workers", 
                         "Healthcare") %>%
    stringr::str_replace("Number of cases in HCW", "Healthcare") %>%
    stringr::str_replace("Median age \\(years\\)", "Median Age")
  
  col.get <- stringr::str_detect(table.list, paste0("Total|Hospitalised|In ICU|",
                                                    "Dead|Clusters|Imported|",
                                                    "Healthcare|Median"))
  
  table.list <- table.list[col.get]
  
  if (document$date <= "2020-03-20") {
    table.list <- c(table.list, "In ICU 17")
  }
  
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

setwd("/home/davide_magno/IrishCovid19")

last.date.github <- "https://raw.githubusercontent.com/DavideMagno/IrelandCovidData/master/" %>% 
  paste0("Covid19_Data_") %>% 
  paste0("Total_Ireland.csv") %>% 
  {readr::read_csv(url(.))} %>% 
  {dplyr::first(.$Date)}

last.date.website <- paste0("https://www.hpsc.ie/a-z/respiratory/coronavirus/",
                            "novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/") %>% 
  GetLinksFromHTML %>% 
  {.[1]} %>% 
  GetDates

# if (last.date.github == last.date.website) {
#   message(paste(Sys.time(),"No need to update data"))
# } else {
  message(paste(Sys.time(),"Updating Data"))
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
  
  write.csv(county, "Covid19_Data_By_County.csv", 
            row.names = FALSE)
  write.csv(total, "Covid19_Data_Total_Ireland.csv", 
            row.names = FALSE)
  write.csv(granular.data,  
            "Covid19_Data_Positive_Characteristics.csv",
            row.names = FALSE)
  message(paste(Sys.time(),"Data Updated"))
  
  system("git add Covid19_Data_By_County.csv")
  system("git add Covid19_Data_Total_Ireland.csv")
  system("git add Covid19_Data_Positive_Characteristics.csv")
  system(paste("git commit -m 'Data as at ", Sys.Date(),"'"))
  system("git push origin master")
# }

