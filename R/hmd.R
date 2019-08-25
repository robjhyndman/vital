# Read file from hmd
# Handles Births, deaths, population, exposure, death rates, life tables and life expectancy
# Does not handle lexis triangles or cohort data

read_hmd <- function(file) {
  # Type of data from first line of file
  firstline <- scan(file, nlines=1, what="character", sep=",", strip.white=TRUE)
  if(any(grepl("Births",firstline)))
    type <- "Births"
  else if(any(grepl("Deaths",firstline)))
    type <- "Deaths"
  else if(any(grepl("Population",firstline)))
    type <- "Population"
  else if(any(grepl("Exposure",firstline)))
    type <- "Exposure"
  else if(any(grepl("Death rates",firstline)))
    type <- "Mortality"
  else if(any(grepl("Life tables",firstline)))
    type <- "Life_tables"
  else if(any(grepl("Life expectancy",firstline)))
    type <- "Life_expectancy"
  else
    stop("Unknown file type")

  # Read data
  df <- HMDHFDplus::readHMD(file)

  # If population data, drop duplicates
  if(type=="Population") {
    df <- df[,!grepl("2", colnames(df))]
    colnames(df) <- gsub("[[:digit:]]+", "", colnames(df))
  }

  # pivot long if contains sexes
  if("Female" %in% colnames(df))
    df <- gather(df, Male, Female, Total, key=Sex, value={{type}})

  # Turn it into a tsibble
  key_var <- na.omit(colnames(df)[match(c("Age","Sex"), colnames(df))])
  df <- as_tsibble(df, index=Year, key=!!key_var)

  # Reorder columns
  vars <- na.omit(colnames(df)[match(c("Year", "Age", "OpenInterval", "Sex"), colnames(df))])
  df <- select(df, !!vars, everything())

  return(df)
}

read_hmd("~/Downloads/Aus_Deaths_5x10.txt")
read_hmd("~/Downloads/Aus_Mx_1x1.txt")
read_hmd("~/Downloads/bltper_1x1.txt")
read_hmd("~/Downloads/Deaths_5x10.txt")
read_hmd("~/Downloads/E0per.txt")
read_hmd("~/Downloads/Exposures_1x10.txt")
read_hmd("~/Downloads/Population.txt")




