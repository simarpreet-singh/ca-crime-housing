## ----message=FALSE-------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(stargazer)

## ------------------------------------------------------------------------
crime_85_17_raw <- read.csv('./data/crime/crime_clearances_85-17.csv', stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
# filter data to 2000 to 2016
crime_00_16_raw <- crime_85_17_raw %>% filter(Year %in% c(2000:2016))

# check the years in filtered data
levels(as.factor(crime_00_16_raw$Year))

## ------------------------------------------------------------------------
# General Structure of the data set.
str(crime_00_16_raw)

## ------------------------------------------------------------------------
#View All Data
#View(crime_00_16_raw)

# ... I think there may be no NAs in this dataset (wow). Unless they are written as 0s.


## ----results='asis'------------------------------------------------------
#get the columns that have NAs
na_col <- c()
for (i in 1:69) {
  if (length(c(levels(as.factor(is.na(crime_00_16_raw[,i]))))) == 2) {
    na_col <- c(na_col, i)
  }
}

# get a boolean vector of which record is NA for the columsn found above.
na_vals <- c()

for (i in na_col) {
  var_name <- paste("na_vals", as.character(eval(parse(text = "i"))), sep="_")
  assign(var_name, c(na_vals, is.na(crime_00_16_raw[,i])))
}

row_bools <- list(na_vals_22, na_vals_23, na_vals_24, na_vals_25, na_vals_26)

#display all records with NA data and the column that contains it.

k = 1
for (i in na_col) {
  col_name <- as.name(colnames(crime_00_16_raw)[i])
  # disp(col_name)
  # print(crime_00_16_raw[row_bools[[k]],])
  data <- crime_00_16_raw[row_bools[[k]],i]
  # cat(paste(as.character(eval(parse(text = "col_name"))), "Data Class: ", as.character(class(crime_00_16_raw[row_bools[[k]],i])), sep="\n"))
  cat(
    paste(
      as.character(eval(parse(text = "col_name"))),
      "Data Class: ",
      as.character(class(crime_00_16_raw[row_bools[[k]],i])),
      sep="\n"
    )
  )
  # print(data %>%
  #   kable() %>%
  #   kable_styling(bootstrap_options = "striped") %>%
  #   scroll_box(width = "100%", height = "20%") %>%
  #   footnote(general = as.character(eval(parse(text = "col_name"))))
  # )
  k = k + 1
}


## ------------------------------------------------------------------------
df <- head(crime_00_16_raw, 17)
df %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(width = "100%", height = "500px")

## ------------------------------------------------------------------------
crimes_reported <- crime_00_16_raw %>% select(Year, County, NCICCode, Violent_sum:LTtotal_sum)
clearances_reported <- crime_00_16_raw %>% select(Year, County, NCICCode, ViolentClr_sum:LTtotalClr_sum)
arson_crime_clearences_reported <- crime_00_16_raw %>% select(Year, County, NCICCode, TotalStructural_sum:GrandTotClr_sum)

## ----results='asis'------------------------------------------------------
stargazer(crimes_reported, type="html", align=TRUE, title="Summary Statistics of Crimes (2000 to 2016, CA)", median = TRUE)

stargazer(clearances_reported, type="html", align=TRUE, title="Summary Statistics of Crime Clearances (2000 to 2016, CA)", median = TRUE)

stargazer(arson_crime_clearences_reported, type="html", align=TRUE, title="Summary Statistics of Arson Crimes and Clearances (2000 to 2016, CA)", median = TRUE)

## ------------------------------------------------------------------------
unique(crime_00_16_raw$County)
length(unique(crime_00_16_raw$County))

## ------------------------------------------------------------------------
table(crime_00_16_raw$County)

## ------------------------------------------------------------------------
colSums(sapply(crime_00_16_raw, is.na)) 

## ------------------------------------------------------------------------
la_county <- crime_00_16_raw %>% filter(County == "Los Angeles County")

## ----results='asis'------------------------------------------------------
stargazer(la_county, type="html", align=TRUE, title="Summary Statistics of LA County Crimes & Arson (2000 to 2016)", median = TRUE)

## ------------------------------------------------------------------------
crime_00_16_raw %>% filter(Violent_sum > mean(Violent_sum)) 

## ------------------------------------------------------------------------
crime_00_16_raw %>% filter(Violent_sum > mean(Violent_sum)) %>% distinct(County)

## ------------------------------------------------------------------------
crime_00_16_raw %>% group_by(Year) %>% summarize(Count = n())

## ------------------------------------------------------------------------
crime_00_16_raw %>% group_by(Year) %>% summarize(Sum = sum(Violent_sum))

## ------------------------------------------------------------------------
not_cleared_violent <- crime_00_16_raw %>% mutate(Not_Cleared_Violent = Violent_sum - ViolentClr_sum) %>% select(Not_Cleared_Violent, County, Year)


## ----results='asis'------------------------------------------------------

stargazer(not_cleared_violent, type = "html", align=TRUE, median=TRUE, title="Violent Crimes Not Cleared Summary") 

## ------------------------------------------------------------------------
not_cleared_violent_all <- crime_00_16_raw %>% mutate(Not_Cleared_Violent = Violent_sum - ViolentClr_sum) %>% group_by(County, Year) %>% summarise(Count = sum(Not_Cleared_Violent))

not_cleared_violent_all %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(width = "100%", height = "500px")

## ------------------------------------------------------------------------
crime_00_16_raw %>% filter(Violent_sum == max(Violent_sum)) %>% select(Year)

## ------------------------------------------------------------------------
crime_00_16_raw %>% filter(Property_sum == max(Property_sum)) %>% select(Year)

## ------------------------------------------------------------------------
crime_00_16_raw %>% filter(Property_sum == max(Property_sum)) %>% select(County)

