# Time Series of the Covid19 epidemic data in Ireland ![alt text](https://github.com/DavideMagno/IrelandCovidData/blob/master/IMG_0080.jpeg "Covid")
In this repo I make available the time series of data as communicated by the [Health Protection Surveillance Centre](https://www.hpsc.ie/a-z/respiratory/coro|virus/novelcoro|virus/casesinireland/) every day.

Three data-sets are provided starting from the 25th of February:

*File name*                                    | *Data included*
:---------------------------------------------:|:--------------------------------------------------:
**Covid19_Data_Total_Ireland.csv**             | Positive cases in Ireland distirbguished by different categories. See table below for additional info. 
**Covid19_Data_By_County.csv**                 | Total number of positive cases by date and county
**Covid19_Data_Positive_Characteristics.csv**  | Characteristics of positive cases. See table below for additional info

The table below details the columns for the file **Covid19_Data_Total_Ireland.csv**

|*Column name*    | *Description*
|:---------------:|:-----------------------:
|**Date**         | Date of reporting
|**Hospitalised** | Total number of cases admitted in hospitals with symptoms (excluding |people in ICU)
|**In ICU**       | Total number of cases hospitalised and under intensive care units
|**Dead**         | Total number of cases who have died
|**Clusters**     | Total number of identified outbreak/clusters
|**In Clusters**  | Total number of cases relative to identified outbreak/clusters
|**Imported**     | Total number of cases whose contagion happened outside Ireland
|**Healthcare**   | Total number of cases who is a healthcare professio|l
|**Total**        | Total number of cases

The file **Covid19_Data_Positive_Characteristics.csv** is organised with a number of different 

|*Key 1*        | *Key 2*        | *Key 3*   
|:-------------:|:--------------:|:---------:
|HSE Area          | HSE East       | |
|                  | HSE Midlands   | |
|                  | HSE Mid-West   | |
|                  | HSE North-East | |
|                  | HSE North-West | |
|                  | HSE South-East | |
|                  | HSE South      | |
|                  | HSE West       | |
|Sex               | Male           | |
|                  | Female         | |
|                  | Unknown Sex    | |
|Transmission Type | Community transmission | |
|                  | Local transmission     | |
|                  | Possible community transmission | |
|                  | Possible local transmission | |
|                  | Possible travel abroad | |
|                  | Unknown | |
|Age Group         | <5   | Total
|                  | 5-14 | Died
|                  | 15-24 | Hospitalised
|                  | 25-34 | In ICU
|                  | 35-44 |  |
|                  | 45-54 |  |
|                  | 55-64 |  |
|                  | 65+   |  |
|                  | Unknown   |  |
|Healthcare Workers Cases          | HSE East       |Foreign travel |
|                  | HSE Midlands   |Local/Community transmission |
|                  | HSE Mid-West   |Not specified |
|                  | HSE North-East | |
|                  | HSE North-West | |
|                  | HSE South-East | |
|                  | HSE South      | |
|                  | HSE West       | |

