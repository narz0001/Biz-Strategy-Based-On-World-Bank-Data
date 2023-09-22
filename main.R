#Initializing the required packages and clearing cache
library(factoextra)
library(cluster)
library(stringr)
library(tidyr)
rm(list=ls())

#Function to clean World-Bank-Data, removing necessary headers and footers and formatting Year column
#Credit: Prof. CHIN Chee Kai - Nanyang Business School
cleanWBData = function(df) {
  ### Remove last (or first) rows with only
  ### trailing copyright, author, etc.
  df2 = df[df$Country.Name != "", ]
  ### Detect patterns of column names
  ### that match "xxxxYR2019xxxx"
  ### Gotta do it iteratively.
  hdrnames = colnames(df2)
  nh = length(hdrnames)
  for (i in 1:nh) {
    nm = hdrnames[i]
    pattern = ".+(YR[12][0-9]{3}).*"
    if (str_detect(nm, pattern) == FALSE)  next
    ### Found "...X2019..YR2019.."
    yrname = str_extract(nm, "YR[12][0-9][0-9][0-9]")
    ### Now yrname is like 'YR2019'
    ### Rename df2's actual column name to yrname
    colnames(df2)[i] = yrname
    ### Change all string values into float
    ### Some NA's will be introduced.
    df2[, yrname] = as.numeric(df2[, yrname])
  }
  return (df2)
}

#Function for grouping data by unique country names. Useful for comparing parameters across countries.
#Credit: Prof. CHIN Chee Kai, Nanyang Business School
groupByCountries = function(df) {
  ### Get unique groupByCountriescountries
  countries = unique(df$Country.Name)
  indicators = unique(df$Series.Code)
  allcols = colnames(df)
  removecols = c("Country.Name", "Country.Code","Series.Code","Series.Name")
  allcols = allcols[! allcols %in% removecols]
  ### Want to create a new df such
  ### that rows are unique countries
  ### and columns are indicators
  ### (which are currently repeated
  ### across many rows)
  print(allcols)
  dfout = data.frame(Country.Name=countries)
  for (ind in indicators) {
    newcolnames = paste(ind, allcols, sep=".")
    dfvec = df[df$Series.Code==ind, c("Country.Name", allcols)]
    names(dfvec) = c("Country.Name", newcolnames)
    print(dfvec)
    df2 = merge(dfout, dfvec,
                by="Country.Name",
                all=TRUE)
    dfout = df2
  }
  return (dfout)
}

#Function to subset dataframes based on x series code
subset_df = function(df, x){
  return(df[df$Series.Code==x,])
}

#Cleans the dataframe on the following parameters: (Year Data starts from 5th column)
#1. If the 5th column has the value NA, search the rest of the columns serially (starting from the 6th column to the last column) and put the first non-NA value found in the 5th column.
#2. 6th column onwards if the current column has the value NA, take the value of the previous column.
#3. If the values in the 5th to the last columns are all NA, drop the row.
clean_dataframe <- function(df) {
  # Iterate over rows
  for (i in 1:nrow(df)) {
    # If the 5th column is NA
    if (is.na(df[i, 5])) {
      # Search for the first non-NA value in columns 6 and onwards
      first_non_na <- NA
      for (j in 6:ncol(df)) {
        if (!is.na(df[i, j])) {
          first_non_na <- df[i, j]
          break
        }
      }
      # Assign the first non-NA value to the 5th column
      df[i, 5] <- first_non_na
    }

    # Fill NA values in columns 6 and onwards with the previous column's value
    for (j in 6:ncol(df)) {
      if (is.na(df[i, j])) {
        df[i, j] <- df[i, j - 1]
      }
    }
  }

  return(df)
}

#Function to remove rows that have NA values across 2013-2023
remove_rows_with_all_data_na <- function(df) {
  df[complete.cases(df[, 5:14]), ]
}

#Main Section
setwd("/Users/geolangsatnarzary/Study - NTU_NBS/6002 - ML and AI/PyCharm Projects/Biz-Strategy-Based-On-World-Bank-Data/")
filename <- "WB-Data.csv"
df00 <- read.csv(filename)

df <- cleanWBData(df00)

#Flattening the data by using groupByCountries function
df <- groupByCountries(df)

