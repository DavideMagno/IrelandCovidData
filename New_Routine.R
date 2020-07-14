# Functions ---------------------------------------------------------------
library(magrittr)

GetLinksFromHTML <- function(link) {
  link %>% 
    xml2::read_html(.) %>% 
    rvest::html_nodes("a") %>%
    rvest::html_attr(name = "href") %>% 
    {.[grepl("pdf", .)]}
}

ReadPDFs <- function(link) {
  pdf <- paste0("https://www.hpsc.ie", link) %>% 
    stringr::str_replace_all(" ", "%20") %>% 
    pdftools::pdf_text(.) %>% 
    readr::read_lines(.) 
  
  date <- pdf %>% 
    {.[stringr::str_which(., "HPSC")[1]]} %>% 
    stringr::str_extract("[\\d]+.[\\d]+.[\\d]+") %>% 
    as.Date(format = "%d/%m/%y") %>% 
    {. - lubridate::days(2)}
  
  return(list(date = date, pdf = pdf))
}

ManageStringList <- function(table.list, date){
  data.column <- stringr::str_split(table.list, "[\\D]+") %>% 
    purrr::map(purrr::pluck(2)) %>% 
    plyr::ldply(.) %>% 
    tibble::as_tibble(.) %>% 
    dplyr::rename("Value" = V1) %>% 
    dplyr::mutate_at(dplyr::vars(Value), as.double) %>% 
    dplyr::select(Value)
  
  table <- stringr::str_split(table.list, "[\\d]+") %>% 
    purrr::map(purrr::pluck(1)) %>% 
    purrr::map(stringr::str_trim) %>% 
    plyr::ldply(.) %>% 
    dplyr::rename("Key" = V1) %>% 
    dplyr::bind_cols(data.column) %>% 
    dplyr::mutate(Date = date) %>% 
    dplyr::select(Date, Key, Value)
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

ExtractAgeData <- function(document, n.doc) {
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
    dplyr::rename("Key 3" = V1, "<5" = V2, "5-14" = V3, "15-24" = V4, 
                  "25-34" = V5, "35-44" = V6, "45-54" = V7, "55-64" = V8, 
                  "65-74" = V9, "75-84" = V10, "85+" = V11, "Unknown" = V12) %>% 
    dplyr::mutate_at(dplyr::vars(-"Key 3"), as.double) %>% 
    tidyr::pivot_longer(-`Key 3`, names_to = "Key 2", values_to = "Value") %>% 
    dplyr::mutate(`Key 3` = dplyr::case_when(
      grepl("\\bICU\\b", `Key 3`) ~ "In ICU", 
      TRUE ~ `Key 3`)) %>% 
    dplyr::mutate(Date = document$date) %>% 
    dplyr::mutate(`Key 1` = "Age Group") %>% 
    dplyr::select(Date, `Key 1`, `Key 2`, `Key 3`, Value)
  
  return(table)
}

ExtractTotalData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "associated")
  
  table.list <- document$pdf %>%
    {.[start.line:(start.line + 1)]} %>% 
    stringr::str_squish() %>% 
    stringr::str_replace_all("\\.|,|\\*","") %>% 
    stringr::str_replace("Total number of cases associated with clusters", 
                         "In Clusters") %>%
    stringr::str_replace("Total number of imported cases", "Imported")
  
  col.get <- stringr::str_detect(table.list, "Clusters|Imported") 
  
  table <- table.list[col.get] %>% 
    ManageStringList(document$date)
  
  return(table)
}

ExtractTransmissionData <- function(document) {
  start.line <- stringr::str_which(document$pdf, "Transmission classification")[1]
  
  table <- document$pdf %>%
    {.[(start.line + 1):(start.line + 4)]} %>% 
    stringr::str_squish() %>% 
    stringr::str_replace_all("\\.|,|\\*","") %>% 
    stringr::str_remove_all("incl possible community transmission") %>% 
    stringr::str_remove_all("incl possible local transmission") %>% 
    stringr::str_remove_all("incl possible travel abroad") %>% 
    stringr::str_remove_all("Possible") %>% 
    ManageStringList(document$date) %>% 
    dplyr::mutate(`Key 1` = "Transmission Type",
                  `Key 3` = NA) %>% 
    dplyr::select(Date, `Key 1`, `Key 2` = Key, `Key 3`, Value)
  
  return(table)
}


# Raw Data urls -----------------------------------------------------------

url.county <- paste0("http://opendata-geohive.hub.arcgis.com/datasets/d",
                     "9be85b30d7748b5b7c09450b8aede63_0.csv?outSR={%22l",
                     "atestWkid%22:3857,%22wkid%22:102100}")

url.ireland <- paste0("http://opendata-geohive.hub.arcgis.com/datasets/d8eb52d5",
                      "6273413b84b0187a4e9117be_0.csv?outSR={%22latestWkid%22:",
                      "3857,%22wkid%22:102100}")

url.report.total <- paste0("https://raw.githubusercontent.com/DavideMagno/Ire",
                           "landCovidData/master/Covid19_Data_Total_Ireland.csv")

url.report.characteristics <- paste0("https://raw.githubusercontent.com/DavideM",
                                     "agno/IrelandCovidData/master/Covid19_Data",
                                     "_Positive_Characteristics.csv")

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
  ReadPDFs

# Data from PDF Report -------------------------------------------------------------

report.total <- ExtractTotalData(last.date.website) 

report.char <- ExtractAgeData(last.date.website) %>% 
  dplyr::bind_rows(ExtractTransmissionData(last.date.website)) 

report.county <- ExtractRegionalData(last.date.website)


# Extract Previous Information ---------------------------------------------

github.total <- readr::read_csv(url(url.report.total)) %>% 
  dplyr::filter(stringr::str_detect(Key, "I\\w+")) %>% 
  dplyr::filter(!stringr::str_detect(Key, "ICU")) %>% 
  dplyr::arrange(Date)

github.characteristics <- readr::read_csv(url(url.report.characteristics),
                                          col_types = readr::cols(
                                            `Key 3` = readr::col_character()
                                          )) %>% 
  dplyr::filter(!stringr::str_detect(`Key 1`, "Sex")) %>% 
  dplyr::filter(!is.na(Date)) %>% 
  dplyr::arrange(Date)

ireland.data <- readr::read_csv(url(url.ireland)) %>% 
  dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  dplyr::mutate(Date = anytime::anydate(Date) - lubridate::days(2))

county <- readr::read_csv(url(url.county)) %>%
  dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::mutate(Date = anytime::anydate(TimeStamp) - lubridate::days(2)) %>%
  dplyr::select(Date, County = CountyName, Value = ConfirmedCovidCases)

# Manca "Imported" and "In Clusters"
total <- ireland.data  %>% 
  dplyr::select(Date, Total = TotalConfirmedCovidCases,
                Hospitalised = HospitalisedCovidCases,
                `In ICU` = RequiringICUCovidCases,
                Dead = TotalCovidDeaths,
                Clusters = ClustersNotified,
                Healthcare = HealthcareWorkersCovidCases,
                `Median Age` = Median_Age) %>% 
  tidyr::pivot_longer(-Date, names_to = "Key", values_to = "Value") %>% 
  dplyr::bind_rows(github.total) %>% 
  dplyr::bind_rows(report.total) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::arrange(Date)

granular.data <- ireland.data  %>%
  dplyr::select(Date, Male, Female, `Unknown Sex` = Unknown) %>%
  tidyr::pivot_longer(-Date, names_to = "Key 2", values_to = "Value") %>%
  dplyr::mutate(`Key 1` = "Sex",
                `Key 3` = NA) %>%
  dplyr::select(Date, `Key 1`, `Key 2`, `Key 3`, Value) %>%
  dplyr::bind_rows(github.characteristics) %>%
  dplyr::bind_rows(report.char) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::arrange(Date)

# Save Files --------------------------------------------------------------

if (lubridate::month(dplyr::last(county$Date)) == 
    lubridate::month(dplyr::last(total$Date))) {
  write.csv(county, "Covid19_Data_By_County.csv",
            row.names = FALSE)
  system("git add Covid19_Data_By_County.csv")
}

if (dplyr::last(total$Date) > dplyr::last(github.total$Date)) {
  write.csv(total, "Covid19_Data_Total_Ireland.csv",
            row.names = FALSE)
  system("git add Covid19_Data_Total_Ireland.csv")
}

if (dplyr::last(total$Date) > dplyr::last(github.total$Date)) {
  write.csv(granular.data,
            "Covid19_Data_Positive_Characteristics.csv",
            row.names = FALSE)
  system("git add Covid19_Data_Positive_Characteristics.csv")
}
message(paste(Sys.time(),"Data Updated"))  
system(paste("git commit -m 'Data as at ", Sys.Date(),"'"))
system("git push origin master")
