### Read in CSV file ----
#install.packages('readr')
library(readr)
?read_csv
hospitalData = read_csv("Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv")
View(hospitalData) #only in RStudio

### Read in Excel file ----
#install.packages('readxl')
library(readxl)
?read_excel
excelData = read_excel("excel version.xlsx",sheet=1,col_names = TRUE)

### dplyr tutorial ----
#install.packages('dplyr')
library(dplyr)

## Subsetting Observations

# filter
nrow(hospitalData)
filteredData = hospitalData %>%
  filter(`Provider State` == "AK")
nrow(filteredData)

hospitalData %>% 
  filter(`Provider State` %in% c("TN","VA","AL","SC","KY") & 
           (`Total Discharges` > 25 | `Total Discharges` <= 10) & 
           grepl("HOSPITAL",`Provider Name`))

# new data for distinct
set.seed(2016) # by fixing the random seed, results will be stable
df = data.frame(
  x = sample(10,500,rep=TRUE), # sample 500 times from 1-10 with replacement
  y = sample(10,500,rep=TRUE),
  z = sample(10,500,rep=TRUE)
)
nrow(df) # how many rows in df
nrow(distinct(df)) # how many distinct rows
nrow(distinct(df,x,y)) # how many distinct combinations of x and y
head(distinct(df,x,y)) # what does the result of this look like?
head(distinct(df,x,y,.keep_all=TRUE)) # what if we want to keep all columns?
df %>% filter(x == 2 & y == 3) # what is kept for the extra columns?

# sampling data

hospitalData %>% sample_n(10,FALSE)

hospitalData %>% sample_frac(0.0001,FALSE)

# specific rows

hospitalData %>% slice(1:5)
hospitalData %>% slice(c(1,5,163065))

## Subsetting variables
names(hospitalData)
hospitalData %>% 
  select(definition = `DRG Definition`,
         starts_with("Aver")) %>% 
  names()
hospitalData %>% 
  select(-contains("Total")) %>% 
  names()

## Grouping data
hospitalData %>% 
  distinct(`DRG Definition`,`Provider State`) %>% 
  nrow()
hospitalData %>% 
  group_by(`DRG Definition`,`Provider State`) %>% 
  sample_n(5) %>% 
  nrow()
hospitalData %>% 
  group_by(`DRG Definition`,`Provider State`) %>% 
  sample_n(5,replace=T) %>% 
  nrow()
hospitalData %>% 
  group_by(`DRG Definition`,`Provider State`) %>% 
  head()

## Summarizing data

hospitalData %>% 
  group_by(`DRG Definition`,`Provider State`) %>% 
  summarise(nRows = n(),
            averageDischarges = mean(`Total Discharges`)) %>% 
  head()

hospitalData %>% 
  group_by(`DRG Definition`,`Provider State`) %>% 
  summarise(nRows = n(),
            averageDischarges = mean(`Total Discharges`)) %>% 
  summarise(nStates = n(),
            maxDischarges = max(averageDischarges)) %>% 
  head()

## Making new variables
hospitalData %>% 
  mutate(averageCovered = as.numeric(gsub("\\$","",`Average Covered Charges`))) %>% 
  mutate(totalCovered = averageCovered * `Total Discharges`) %>% 
  select(contains("Covered")) %>% 
  head()

hospitalData %>% 
  mutate(averageDischarges = mean(`Total Discharges`)) %>% 
  group_by(`DRG Definition`) %>% 
  mutate(groupAverageDischarges = mean(`Total Discharges`)) %>% 
  ungroup() %>% 
  distinct(`DRG Definition`,averageDischarges,groupAverageDischarges) %>% 
  head()

### tidyr tutorial ----
#install.packages('tidyr')
library(tidyr)

## Pivot data

hospitalData %>% 
  filter(grepl("W MCC|W/O MCC",`DRG Definition`)) %>% 
  mutate(MCC = ifelse(grepl("W MCC",`DRG Definition`),"With MCC","Without MCC")) %>% 
  mutate(DRG = gsub("^[0-9]+ \\- ","",`DRG Definition`)) %>% 
  mutate(DRG = gsub(" W/?O? MCC","",DRG)) %>% 
  mutate(averageTotal = as.numeric(gsub("\\$","",`Average Total Payments`))) %>% 
  group_by(DRG,MCC) %>% 
  summarise(averageTotalPayments = mean(averageTotal)) %>% 
  spread(MCC,averageTotalPayments,fill=NA) %>% 
  filter(!is.na(`With MCC`) & !is.na(`Without MCC`)) %>% 
  mutate(`% Cost of MCC` = paste0(round(((`With MCC`/`Without MCC`)-1)*100,0),"%")) %>% 
  arrange(desc(round(((`With MCC`/`Without MCC`)-1)*100,0)))

## Unpivot data

pivotedData = read_csv("Total Payments by State, DRG.csv")
View(pivotedData)

pivotedData %>% 
  gather("State","TotalPayment",-1)


