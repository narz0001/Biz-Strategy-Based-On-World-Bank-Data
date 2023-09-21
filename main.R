#Initializing the required packages and clearing cache
library(cluster)
library(stringr)
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

