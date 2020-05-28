##############################################################################################################
###                                                                                                        ###
### Untersuchungen zur Prognostizierbarkeit von Inflation innerhalb eines Statistical-Learning-Frameworks  ###
###                                                                                                        ###
##############################################################################################################

# Das nachfolgende Dokument enthält den Code zur Reproduktion der Ergebnisse der genannten Bachelorarbeit. 

# Abschnitt 1: Datenimport

# Um die in der Arbeit verwendeten Pakete verwenden zu können, müssen diese ggf. installiert werden.
# Hierzu biete die nachfolgenden Zeilen entkommentieren und den darin enthaltenen Code ausführen.

# install.packages(c("dplyr", "readr", "readxl", "lubridate", "naniar", "quantmod", "tibble", "stringr", "purrr",
#                 "timetk", "tsibble", "Metrics", "glmnet", "randomForest", "neuralnet", "gbm",
#                "foreach", "doParallel", "ggplot2", "MCS"))

# Laden der relevanten Pakete

library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(naniar)
library(quantmod)
library(tibble)
library(stringr)
library(purrr)
library(timetk)
library(tsibble)
library(Metrics)

# Pakete für Statistical Learning Modelle
library(glmnet)
library(randomForest)
library(neuralnet)
library(gbm)

# Pakete für die parallele Verarbeitung der Daten
library(foreach)
library(doParallel)

# Paket für graphische Darstellungen
library(ggplot2)

# Paket für die Konstruktion der Model Confidence Sets
library(MCS)

# Für die erstmalige Ausführung des Skripts wird die Verwendung des folgenden Seeds empfohlen.

set.seed(28101997)

# Für den Import der jeweiligen Dateien muss der Dateipfad entsprechend angepasst werden. 

# Import der Inflationsdaten (PCE-Inflation)

pce <- read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/PCE.xlsx", col_names = TRUE)

pce <- pce[53:nrow(pce), ]

pce <- pce[, c(1, grep("PCON96Q2", colnames(pce)):ncol(pce))]

pce <- replace_with_na_all(pce, condition = ~.x == "#N/A")

pce$DATE <- parse_date_time(pce$DATE, orders = "Y:q!*")
pce$DATE <- as_date(pce$DATE)

DATE <- pce$DATE

pce[, 2:ncol(pce)] <- lapply(2:ncol(pce), function(x) as.numeric(pce[[x]]))

# Annualisierung der Quartalsinflation

annualize_inflation <- function(pce) {
  
  DATES <- tibble(DATE = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "quarter"))
  
  eventually <- tibble(x = rep(0, nrow(pce)))
  k <- 1
  
  for (c in 2:ncol(pce)) {
    
    container <- c(0)
    ind <- 2
    
    for (i in 2:nrow(pce)) {
      
      data <- pce[c]
      
      annum <- (data[i, ]/data[i - 1, ])^4 - 1
      
      container[ind] <- annum
      ind <- ind + 1
      
    } 
    
    eventually[[k]] <- container
    k <- k + 1
    
  }
  
  eventually_names <- names(pce[, 2:98])
  
  colnames(eventually) <- eventually_names
  
  eventually <- bind_cols(DATES, eventually)
  
  eventually
  
}

INFL_h <- annualize_inflation(pce)

INFL_h[is.na(INFL_h)] <- 0

INFL_h[, 2:ncol(INFL_h)] <- lapply(2:ncol(INFL_h), function(x) as.numeric(INFL_h[[x]]))

INFL_h[1, 2:ncol(INFL_h)] <- INFL_h[2, 2:ncol(INFL_h)]

get_h_ahead_diffed_inflation <- function(infl, diff_order) {
  
  infl[, 2:ncol(infl)] <- lapply(2:ncol(infl), function(x) difference(infl[[x]], lag = diff_order))
  
  infl <- as_tibble(infl)
  
  infl[1:diff_order, 2:98] <- 0
  
  infl
}

INFL_h[1:2, 2:ncol(INFL_h)] <- INFL_h[3, 2:ncol(INFL_h)]

# DataFrame für den Gesamtdatensatz

INFL_COMP <- INFL_h
INFL_COMP[145:nrow(INFL_COMP), 2:ncol(INFL_COMP)] <- replace_with_na_all(INFL_COMP[145:nrow(INFL_COMP), 2:ncol(INFL_COMP)], condition = ~.x == 0)

# Ersatz fehlender Daten durch Daten des vorherigen Vintages

rep_ind <- grep("99Q4", names(INFL_COMP))

INFL_COMP[1:137, rep_ind] <- INFL_COMP[1:137, (rep_ind - 1)]

# Differenz der Inflation

for (av in 1:8) {
  
  obj <- get_h_ahead_diffed_inflation(INFL_h, av)
  
  obj[145:nrow(obj), 2:ncol(obj)] <- replace_with_na_all(obj[145:nrow(obj), 2:ncol(obj)], condition = ~.x == 0)
  
  
  assign(paste0("INFL_DIFFED_h", av), obj) 
  av <- av + 1
}

# Funktion zur Bereinigung der Vintage-Diagonalen

purge_vintage <- function(x, h) {
  
  for (col in (2:(length(x) - 1))) {
    
    x[(length(x[[col]]) - sum(is.na(x[[col]])) - h + 1):(length(x[[col]]) - sum(is.na(x[[col]]))), col] <- NA
    
    if (col == 98) {
      break
    } 
  }
  
  x
  
}

# Bereinigung sämtlicher Horizonte

infl_dfs <- mget(ls(pattern = "INFL_DIFFED_h"))

infl_list <- list()
h <- 1

for (j in 1:length(infl_dfs)) {
  
  res <- purge_vintage(infl_dfs[[j]], h = j)
  infl_list[[h]] <- res
  h <- h + 1
  
}

# Funktion zur Extraktion der Diagonalwerte (Algorithmus 8/Hilfsfunktion 6)

get_inflation <- function(x, h) {
  
  train_end <- ncol(x) - h
  
  true <- c() # Hierbei wird jeweils der First-Release-Wert extrahiert
  i <- 1
  
  for (col in ((2 + h):(train_end + h))) {
    true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
    i <- i + 1
  }
  
  true
  
} 

# Import weiterer Datensätze 
# Aufgrund der speziellen Struktur mancher DataFrames mussten einzelne Importe vorgenommen werden, anstatt
# eines gesammelten Imports.

colnames_unemp <- names(read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/UNEMPR.xlsx", col_names = TRUE))

unemp <- read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/UNEMPR.xlsx", col_names = FALSE)

unemp <- unemp[2:nrow(unemp), ]

colnames(unemp) <- colnames_unemp

ind <- match("RUC96Q1", names(unemp)) + 1

unemp <- unemp[157:nrow(unemp), c(1, ind:length(unemp))]

unemp <- replace_with_na_all(unemp, condition = ~.x == "#N/A")

unemp$DATE <- parse_date_time(unemp$DATE, orders = "Y:m!*")
unemp$DATE <- as_date(unemp$DATE)

unemp[, 2:ncol(unemp)] <- lapply(2:ncol(unemp), function(x) as.numeric(unemp[[x]]))

for (col in (2:length(unemp))) {
  unemp[(length(unemp[[col]]) - sum(is.na(unemp[col]))), col] <- NA
}

unemp <- unemp[-722, ]
unemp[is.na(unemp)] <- 0

unemp <- tk_xts(unemp)

unemp$DATE <- as.yearmon(unemp$DATE)

quarter_end <- endpoints(unemp, on = "quarters")

quarter_by_month <- period.apply(unemp, INDEX = quarter_end, FUN = mean)

unemp_q_core <- coredata(quarter_by_month)

unemp_dates <- tibble(DATE = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "quarter"))

unemp_df <- cbind(unemp_dates, unemp_q_core)
unemp_df$DATE <- as.yearqtr(unemp_df$DATE)

unemp_df <- unemp_df[-241,]

unemp_df$RUC20Q2 <- unemp_df$RUC20Q1

ruc_comp <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/RUC.csv")

new_ruc <- mean(ruc_comp$UNRATE[1:3])

new_data <- rep(0, 98)
new_data[1] <- as.yearqtr("2020-01-01")

unemp_df <- rbind(unemp_df, new_data)

unemp_df[241, 98] <- new_ruc

unemp_df[, 2:98] <- replace_with_na_all(unemp_df[, 2:98], condition = ~.x == 0)
unemp_df[, 2:98] <- unemp_df[, 2:98]/100

UNEMP_PC <- as_tibble(unemp_df) # Only for Phillips Curve Model
UNEMP_PC_TRAIN <- UNEMP_PC[, c(1:72)]
UNEMP_PC_TEST <- UNEMP_PC[, c(1,73:98)]

# Für die Statistical Learning Modelle wird die prozentuale Veränderung der Arbeitslosigkeitsrate als Variable geschaffen.

get_h_ahead_feature <- function(feat, h) {
  
  feat[, 2:ncol(feat)] <- lapply(2:ncol(feat), function(x) Delt(feat[[x]], k = h))
  
  feat <- as.matrix(feat)
  feat <- as_tibble(feat)
  feat[, 2:ncol(feat)] <- lapply(2:ncol(feat), function(x) as.numeric(feat[[x]]))
  
  feat
}


UNEMP <- get_h_ahead_feature(unemp_df, h = 1)

unemp_df[, 2:ncol(unemp_df)] <- lapply(2:ncol(unemp_df), function(x) difference(unemp_df[[x]], lag = 1))

UNEMP[1, 2:98] <- UNEMP[2, 2:98]

assign(paste0("UNEMP_TRAIN", "_h", 1), UNEMP)

# Import der Echtzeit-Daten (Quartalsfrequenz)

base_path <- "/Users/nfsturm/Documents/GrandFinale/data/Variables/"

filenames_quarterly <- c("DIVIDENDS", "FEDGOVCONSUM",
                         "NOMPERSAVE", "NOMPERSINC", "PERSINT", "PERSTAX", "PINTPAID", "REALIMPORT", 
                         "RESINVEST", "STATELOCCONSUM", "TRANSFERS", "WAGESAL", "REALGDP", "REALEXPORT", 
                         "RCSERV", "RCXNONDURAB", "RCXDURAB", "SOCSECCONTRIB", "RENTINC", "FOREIGNTRANS", "PROPI",
                         "INVEST", "WAGESUPPL", "PINVENTORIES", "PXIMP", "RATESAVE")

filenames_money <- c("M1MONEY", "M2MONEY")

make_path <- function(base_path, filename) {
  
  path <- paste0(base_path, filename, ".xlsx", collapse = "")
  path
  
}

files <- list()
k <- 1

for (filename in filenames_quarterly) {
  
  file <- make_path(base_path, filename)
  files[[k]] <- file
  k <- k + 1
  
}

for (i in 1:length(files)) {
  
  file <- read_excel(files[[i]])
  
  file <- as_tibble(file)
  
  start_ind <- grep("96Q2", names(file))
  
  file <- file[, c(1, start_ind:length(file))]
  
  file <- replace_with_na_all(file, condition = ~.x == "#N/A")
  
  file$DATE <- parse_date_time(file$DATE, orders = "Y:q")
  
  file <- file[file$DATE >= "1960-01-01", ]
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) as.numeric(file[[x]]))
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) Delt(file[[x]], k = 1))
  
  file <- as.matrix(file)
  file <- as_tibble(file)
  file[1, 2:length(file)] <- file[2, 2:length(file)]
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) as.numeric(file[[x]]))
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) difference(file[[x]], lag = 1))
  
  file[1, 2:98] <- 0
  
  reg_na <- rev(0:96)
  reg_data <- 145:241
  
  for (col in 2:length(file)) {
    
    sum_na <- sum(is.na(file[col]))
    
    if(sum_na != reg_na[col - 1]) {
      
      column <- file[col]
      
      data <- column[1:reg_data[col - 1], ]
      
      na_rows <- which(is.na(data))
      
      data[na_rows, ] <- file[na_rows, col - 1]
      
      file[na_rows, col] <- data[na_rows,]
      
    }
    
  }
  
  assign(paste0(filenames_quarterly[i], "_TRAIN", "_h", 1), file)
}


# Spezielle Bereinigung für PINVENTORIES

start_ind <- grep("99Q4", names(PINVENTORIES_TRAIN_h1))
end_ind <- grep("03Q4", names(PINVENTORIES_TRAIN_h1))

for (j in start_ind:end_ind) {
  
  PINVENTORIES_TRAIN_h1[130:131, j] <- PINVENTORIES_TRAIN_h1[130:131, 15]
  
}

# Import der Daten für die Geldmengen.
# Diese Datengruppe ist in quartalsweisen Vintages aufgestellt, enthält allerdings monatliche Beobachtungen. 

m2sup <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/M2SUP.csv")
m1sup <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/M1SUP.csv")

means <- c(mean(m1sup$M1SL[2:4]), mean(m2sup$M2SL[2:4]))

m_files <- list()
k <- 1

for (filename in filenames_money) {
  
  file <- make_path(base_path, filename)
  m_files[[k]] <- file
  k <- k + 1
  
}


for(i in 1:length(m_files)) {
  
  df <- read_excel(m_files[[i]])
  
  file <- as_tibble(df)
  
  file <- file[-nrow(file),]
  
  start_ind <- grep("96Q2", names(file))
  
  file <- file[, c(1, start_ind:length(file))]
  
  file <- replace_with_na_all(file, condition = ~.x == "#N/A")
  
  file$DATE <- parse_date_time(file$DATE, orders = "Y:m")
  
  file <- file[file$DATE >= "1960-01-01" & file$DATE < "2020-01-01", ]
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) as.numeric(file[[x]]))
  
  file <- tk_xts(file)
  
  file$DATE <- as.yearmon(file$DATE)
  
  quarter_end <- endpoints(file, on = "quarters")
  
  quarter_by_month <- period.apply(file, INDEX = quarter_end, FUN = mean)
  
  file_q_core <- coredata(quarter_by_month)
  
  file_dates <- tibble(DATE = seq(as.Date("1960-01-01"), as.Date("2019-10-01"), by = "quarter"))
  
  file <- cbind(file_dates, file_q_core)
  
  file$LAST <- file[length(file)]
  
  file <- rbind(file, rep(NA, times = 96))
  
  file[nrow(file), 1] <- as_date("2020-01-01")
  file[nrow(file), length(file)] <- means[i]
  
  file$LAST <- as.matrix(file$LAST)
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) Delt(file[[x]], k = 1))
  
  file <- as.matrix(file)
  file <- as_tibble(file)
  file[1, 2:length(file)] <- file[2, 2:length(file)]
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) as.numeric(file[[x]]))
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) difference(file[[x]], lag = 1))
  
  file[1, 2:97] <- file[2, 2:97]
  
  reg_na <- rev(0:96)
  reg_data <- 145:241
  
  for (col in 2:length(file)) {
    
    sum_na <- sum(is.na(file[col]))
    
    if(sum_na != reg_na[col - 1]) {
      
      column <- file[col]
      
      data <- column[1:reg_data[col - 1], ]
      
      na_rows <- which(is.na(data))
      
      data[na_rows, ] <- file[na_rows, col - 1]
      
      file[na_rows, col] <- data[na_rows,]
      
    }
    
  }
  
  assign(paste0(filenames_money[i], "_TRAIN", "_h", 1), file)
  
}

# Import von Daten auf Monatsfrequenz

filenames_monthly <- c("HSTARTSM", "CAPUTM", "INDUSTX", "HRSGDS")

HSTARTSM <- read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/HSTARTSM.xlsx")

CAPUTM <- read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/CAPUTM.xlsx")

INDUSTX <- read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/INDUSTX.xlsx")

HRSGDS <- read_excel("/Users/nfsturm/Documents/GrandFinale/data/Variables/HRSGDS.xlsx")

cols <- c(ncol(HSTARTSM) - 2, ncol(CAPUTM) - 2, ncol(INDUSTX) - 2, ncol(HRSGDS) - 2)

# Spezielle Behandlung von HRSGDS

start_ind <- grep("92M11", names(HRSGDS))
end_ind <- grep("97M10", names(HRSGDS))

for (j in start_ind:end_ind) {
  
  HRSGDS[1:204, j] <- HRSGDS[1:204, (start_ind - 1)]
  
}

# Spezielle Behandlung für Capacity Utilization Rate

start_ind <- grep("97M2", names(CAPUTM))
end_ind <- grep("98M10", names(CAPUTM))

for (j in start_ind:end_ind) {
  
  CAPUTM[1:252, j] <- CAPUTM[1:252, 211]
  
}

# Spezielle Behandlung für INDUSTX

start_ind1 <- grep("90M4", names(INDUSTX))
end_ind1 <- grep("98M10", names(INDUSTX))

for (j in start_ind1:end_ind1) {
  
  INDUSTX[1:576, j] <- INDUSTX[1:576, 330]
  
}

start_ind2 <- grep("97M2", names(INDUSTX))
end_ind2 <- grep("98M10", names(INDUSTX))

for (j in start_ind2:end_ind2) {
  
  INDUSTX[577:600, j] <- INDUSTX[577:600, 412]
  
}

start_ind3 <- grep("97M12", names(INDUSTX))
end_ind3 <- grep("98M10", names(INDUSTX))

for (j in start_ind3:end_ind3) {
  
  INDUSTX[601:612, j] <- INDUSTX[601:612, 422]
  
}

# Setzen der Daten auf monatliche Frequenz

j <- 1
for (df in list(HSTARTSM, CAPUTM, INDUSTX, HRSGDS)) {
  
  file <- as_tibble(df)
  
  start_ind <- grep("96M2", names(file))[1]
  
  file <- file[, c(1, start_ind:length(file))]
  
  file <- replace_with_na_all(file, condition = ~.x == "#N/A")
  
  file$DATE <- parse_date_time(file$DATE, orders = "Y:m")
  
  file <- file[file$DATE >= "1960-01-01", ]
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) as.numeric(file[[x]]))
  
  # Aufteilung der Daten in einen oberen und unteren Teil
  
  # Oberer Teil
  
  upper <- file[file$DATE < "1996-01-01", ]
  
  upper_cols <- seq(from = 4, to = length(upper), by = 3)
  
  upper <- upper[c(1, upper_cols)]
  
  upper <- tk_xts(upper)
  
  upper$DATE <- as.yearmon(upper$DATE)
  
  quarter_end <- endpoints(upper, on = "quarters")
  
  quarter_by_month <- period.apply(upper, INDEX = quarter_end, FUN = mean)
  
  file_q_core <- coredata(quarter_by_month)
  
  file_dates <- tibble(DATE = seq(as.Date("1960-01-01"), as.Date("1995-10-01"), by = "quarter"))
  
  upper <- cbind(file_dates, file_q_core)
  
  # Unterer Teil
  
  lower <- file[file$DATE >= "1996-01-01", ]
  
  lower_cols <- seq(from = 4, to = length(lower), by = 3)
  
  lower <- lower[, c(1, lower_cols)]
  
  lower <- tk_xts(lower)
  
  lower$DATE <- as.yearmon(lower$DATE)
  
  quarter_end <- endpoints(lower, on = "quarters")
  
  quarter_by_month <- period.apply(lower, INDEX = quarter_end, FUN = mean)
  
  file_q_core <- coredata(quarter_by_month)
  
  file_dates <- tibble(DATE = seq(as.Date("1996-01-01"), as.Date("2020-01-01"), by = "quarter"))
  
  lower <- cbind(file_dates, file_q_core)
  
  # Kombination der Teile
  
  file <- rbind(upper, lower)
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) Delt(file[[x]], k = 1))
  
  file <- as.matrix(file)
  file <- as_tibble(file)
  file[1, 2:length(file)] <- 0
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) as.numeric(file[[x]]))
  
  file[, 2:ncol(file)] <- lapply(2:ncol(file), function(x) difference(file[[x]], lag = 1))
  
  file[1, 2:length(file)] <- 0
  
  out_name <- rep(filenames_monthly[j], 97)
  YY <- rep(1996:2020, each = 4)
  Quarter <- "Q"
  Quarter_suffix <- rep(1:4, 24)
  
  colnames_out <- paste0(out_name, YY, Quarter, Quarter_suffix)[2:98]
  
  colnames(file) <- c("DATE", colnames_out)
  
  file <- as_tibble(file)
  
  assign(paste0(filenames_monthly[j], "_TRAIN", "_h", 1), file)
  j <- j + 1
  
}

# Schreiben von Vintages mit Echtzeitdaten (Algorithmus 4/Hilfsfunktion 2)

write_vintage <- function(df, df_name) {
  
  df <- tk_xts(df)
  
  quarter_end <- endpoints(df, on = "quarters")
  
  quarter_by_month <- period.apply(df, INDEX = quarter_end, FUN = mean)
  
  quarter_by_month <- quarter_by_month[1:241, ]
  
  file_q_core <- coredata(quarter_by_month)
  
  file_dates <- tibble(DATE = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "quarter"))
  
  file <- cbind(file_dates, file_q_core)
  
  vintage <- tibble(x = rep(0, nrow(file))) # Initialisierung eines leeren DataFrame
  
  k <- 1
  
  for (i in 1:97) {
    
    data <- file[1:(145 + i - 1), 2]
    
    padded <- c(data, rep(NA, (nrow(file) - length(data))))
    
    vintage[k] <- padded
    k <- k + 1
    
  }
  
  DATES <- seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "quarter")
  
  vintage_df <- cbind(DATES, vintage)
  
  df_name <- rep(df_name, 97)
  yy <- rep(1996:2020, each = 4)
  quarter <- "Q"
  quarter_suffix <- rep(1:4, round(97/4))
  
  column_names <- paste0(df_name, yy, quarter, quarter_suffix)[2:98]
  
  colnames(vintage_df) <- c("DATE", column_names)
  
  vintage_df
}

# Import von Echtzeit-Daten in Nicht-Vintage-Format

FEDFUNDS <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/FEDFUNDS.csv")
SP500 <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/SP500M.csv")
OILWTI <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/OILWTI.csv")

SP500 <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/SP500M.csv") %>%
  select(Date, `Adj Close`)

GS1 <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/GS1.csv")

TB3M <- read_csv("/Users/nfsturm/Documents/GrandFinale/data/Variables/TB3MS.csv")

SPREAD1_3 <- bind_cols(GS1, TB3M) %>% 
  select(DATE, GS1, TB3MS) %>%
  mutate(TB3MS = 100*((365*TB3MS)/100)/(360-(91*TB3MS/100))) %>%
  mutate(SPREAD = GS1 - TB3MS) %>%
  select(DATE, SPREAD)

SPREAD1_3 <- tk_xts(SPREAD1_3)

quarter_end <- endpoints(SPREAD1_3, on = "quarters")

quarter_by_month <- period.apply(SPREAD1_3, INDEX = quarter_end, FUN = mean)

spread_q_core <- coredata(quarter_by_month)

spread_dates <- tibble(DATE = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "quarter"))

SPREAD1_3 <- cbind(spread_dates, spread_q_core)

SPREAD1_3_DF <- write_vintage(SPREAD1_3, "SPREAD")
SPREAD1_3_TRAIN <- SPREAD1_3_DF[, c(1:72)]
SPREAD1_3_TEST <- SPREAD1_3_DF[, c(1, 73:98)]

n <- 1
csv_names <- c("FEDFUNDS", "SP500", "OILWTI", "SPREAD1_3")

for (df in list(FEDFUNDS, SP500, OILWTI, SPREAD1_3)) {
  
  vintage <- write_vintage(df, csv_names[n])
  
  vintage[1, 2:length(vintage)] <- vintage[2, 2:length(vintage)]
  
  vintage[, 2:ncol(vintage)] <- lapply(2:ncol(vintage), function(x) Delt(vintage[[x]], k = 1))
  
  vintage <- as.matrix(vintage)
  vintage <- as_tibble(vintage)
  vintage[1, 2:length(vintage)] <- vintage[2, 2:length(vintage)]
  
  vintage[, 2:ncol(vintage)] <- lapply(2:ncol(vintage), function(x) as.numeric(vintage[[x]]))
  
  vintage[, 2:ncol(vintage)] <- lapply(2:ncol(vintage), function(x) difference(vintage[[x]], lag = 1))
  
  vintage[1, 2:98] <- 0
  
  assign(paste0(csv_names[n], "_TRAIN", "_h", 1), vintage)
  n <- n + 1
}

# Überprüfung der Diagonal-Elemente (Algorithmus 5/Hilfsfunktion 3)

diagonal_check <- function(x) {
  
  check_df <- x[145:nrow(x), 2:ncol(x)]
  
  tr <- diag(as.matrix(check_df))
  
  sum_na_tr <- sum(is.na(tr))
  
  if (sum_na_tr == 0) {
    
    return(print("No NA values detected. Trace appears to be clear."))
    
  } else {
    
    return(print("Warning! NA values detected."))
    
  }
  
}

anomalies <- c()
d <- 1

for (df in mget(ls(pattern = "_TRAIN_h1"))) {
  
  res <- diagonal_check(df)
  anomalies[d] <- res
  d <- d + 1
  
}

repair_anomaly <- function(df) {
  
  check_df <- df[(nrow(df[2]) - sum(is.na(df[2]))):nrow(df[2]), 2:ncol(df)] 
  
  upper <- df[1:(nrow(df[2]) - sum(is.na(df[2])) - 1) , 2:ncol(df)]
  
  tr <- diag(as.matrix(check_df))
  
  ind <- which(is.na(tr))
  
  for (col in ind) {
    
    check_df[(nrow(check_df[col]) - sum(is.na(check_df[col])) + 1), col] <- check_df[(nrow(check_df[col]) - sum(is.na(check_df[col]))), col]
    
  }
  
  repaired <- rbind(upper, check_df)
  
  DATE <- df$DATE
  
  out <- cbind(DATE, repaired)
  
  return(out)
  
}

features <- mget(ls(pattern = "_TRAIN_h1"))

repair_ind <- grep("Warning", anomalies)

features_repair <- features[repair_ind]

repaired <- list() 
l <- 1

for (df in 1:length(features_repair)) {
  
  repaired[[l]] <- repair_anomaly(features_repair[[df]])
  l <- l + 1
}

names(repaired) <- names(features_repair)

good_ind <- grep("No NA", anomalies)

features_shaped <- features[good_ind]

features_complete <- c(repaired, features_shaped)

# Erstellung der Variablenlisten für das Training-/Test-Set

train_end <- round(((ncol(INFL_COMP) - 1) * 3/4) - 1)
total_end <- ncol(INFL_COMP)

feature_list <- features_complete

feature_list_appl <- feature_list
feature_list_appl$INFL_TRAIN_h1 <- infl_list[[1]]

names(feature_list_appl) <- gsub("_TRAIN_h1", "", names(feature_list_appl))

# Variablen für das Training-Set

feature_list_train <- list()

for (j in 1:length(feature_list_appl)) {
  
  feature_list_train[[j]] <- feature_list_appl[[j]][, c(1:train_end)]
  
}

names_train_features <- names(feature_list_appl)

names(feature_list_train) <- gsub("_TRAIN_h1", "", names_train_features)

# Variablen für das Test-Set

names_test_features <- names(feature_list_appl)

feature_list_test <- list()

for (j in 1:length(feature_list_appl)) {
  
  feature_list_test[[j]] <- feature_list_appl[[j]][, c(1, train_end:total_end)]
  
}

names(feature_list_test) <- gsub("_TRAIN_h1", "", names_test_features)

infl_list_test <- list()

for (j in 1:length(infl_list)) {
  
  infl_list_test[[j]] <- infl_list[[j]][, c(1, train_end:total_end)]
  
}

names(infl_list_test) <- paste0("INFL_TEST_h", 1:8)


infl_list_train <- list()

for (j in 1:length(infl_list)) {
  
  infl_list_train[[j]] <- infl_list[[j]][, 1:train_end]
  
}

names(infl_list_train) <- paste0("INFL_TRAIN_h", 1:8)

# Finale Überprüfung von fehlenden Werten (NA) oder Unendlichkeiten (inf)

test_list <- list()
z <- 1
for (j in 1:length(feature_list_train)) {
  
  file <- feature_list_train[[j]]
  
  sums <- c()
  l <- 1
  for (k in 1:length(file)) {
    
    out <- sum(is.na(file[[k]]))
    sums[l] <- out
    l <- l + 1
  }
  
  test_list[[z]] <- sums
  z <- z + 1
  
}

collector <- list()
l <- 1
for (i in 1:length(test_list)) {
  
  out <- Reduce("+", test_list[[i]])
  
  collector[[l]] <- out
  l <- l + 1
}

# Abschnitt 2: Variablenauswahl mithilfe eines Random Forest

# Definition der Extraktions-Funktion

# Die Extraktionsfunktion berechnet die Variablenwichtigkeit (quantifziert durch den »Mean Decrease in Accuracy«).
# Die Lags der Einzelvariablen wurden als Gesamtschau zusammengeführt. D.h. die Variablenwichtigkeit
# einer einzelnen Variable entspricht der summierten Wichtigkeit der jeweiligen Lags.

extract_rf_importance <- function(infl_list, feature_list) {
  
  # Seed für die interne Reproduzierbarkeit der Ergebnisse
  set.seed(28101997)
  
  # Parallele Verarbeitung der Daten
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  rf_measure <- function(x, lag_nr, h, features) {
    
    final <- ncol(x)
    
    append_lags <- function(data, df, lag_nr) { # Algorithmus 8/Hilfsfunktion 6
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    mtry <- 1:((length(features) + 1)/3)
    
    rf_list <- list()
    v <- 1
    
    for (val in mtry) {
      
      var_imp <- c()
      j <- 1
      
      for (col in 2:(final - h)) {
        
        # Innere Schleife für das wiederholte Schätzen über die Vintages
        data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
        
        data <- na.omit(data)
        names(data) <- "Y"
        
        for (f in 1:length(features)) {
          
          data <- append_lags(data, features[[f]], lag_nr)
          
        }
        
        data <- as.matrix(data) 
        data <- as_tibble(data)
        data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
        
        train_data <- data
        
        variables <- names(data[, 2:ncol(data)])
        
        var_strings <- paste0(variables, collapse = " + ")
        spec <- paste0("Y", " ~ ", var_strings)
        
        mod <- randomForest(formula(spec), data = train_data, mtry = val, importance = TRUE)
        
        obj <- importance(mod)
        
        var_imp[[j]] <- obj[, 1]
        j <- j + 1
        
      }
      
      rf_list[[v]] <- var_imp
      v <- v + 1
    }
    
    rf_list
  } 
  
  # Die Horizont-Länge entspricht zugleich der Länge der Variablenliste
  
  horizons <- length(infl_list)
  
  importance_estimator <- function(x, lags, horizon, features) { # Diese Funktion entspricht Algorithmus 2.
    
    so_much_time <- rf_measure(x, lag_nr = lags, h = horizon, features = features) # Als maximale Lag-Länge wurde 6 festgelegt. 
    
    lag_nr <- lags
    
    var_num <- length(so_much_time[[1]][[1]])
    
    var_seq <- seq(from = 1, to = var_num, by = lag_nr)
    
    mtries <- list()
    g <- 1
    
    for (i in 1:length(so_much_time)) {
      
      var_imp <- list()
      k <- 1
      
      for (el in 1:length(so_much_time[[i]])) {
        
        out <- c()
        c <- 1
        
        for (var in var_seq) {
          
          mtry <- so_much_time[[i]]
          out[c] <- sum(mtry[[el]][var:(var + lag_nr - 1)])
          c <- c + 1
        }
        
        var_imp[[k]] <- out
        k <- k + 1
        
      }
      
      mtries[[g]] <- Reduce("+", var_imp)
      g <- g + 1
    }
    
    res <- Reduce("+", mtries)
    
    return(res)
  }
  
  # Schaffung eines speziellen Containers für die Ergebnisse
  
  varImps <- function(resultsImp = NULL) {
    
    me <- list(
      resultsImp = resultsImp)
    
    class(me) <- append(class(me), "varImps")
    return(me)
    
  }
  
  finalList <- foreach(h = 1:horizons, .packages = c("tibble", "randomForest", "dplyr")) %dopar% {
    
    out <- importance_estimator(x = infl_list[[h]], lags = 6, horizon = h, features = feature_list)
    importances <- varImps()
    importances$resultsImp <- out
    return(importances)
  }
  
  # Beendigung des parallelen Clusters
  
  stopCluster(cl)
  
  prep <- unlist(finalList, recursive = FALSE)
  
  colnames <- paste("Horizon", 1:horizons)
  
  feature_names <- c(names(feature_list))
  
  names <- gsub("_TRAIN_h1", "", feature_names)
  
  names(prep) <- colnames
  
  prep <- as_tibble(prep)
  
  final <- bind_cols(Features = names, prep)
  
  final$Overall <- rowSums(final[, 2:ncol(final)])
  
  return(final)
  
}

# Aus den errechneten Variablenwichtigkeiten wird nun eine Rangaufstellung geschaffen, aus der diejenigen
# Variablen extrahiert werden, die keinen negativen MDA-Wert auf den Prognosehorizonten aufweisen. 

train_features_imp <- extract_rf_importance(infl_list_train, feature_list_train)

feats_r2 <- !(apply(train_features_imp[, 2:length(train_features_imp)], 1, function(row) any(row < 0)))

feature_list_train2 <- feature_list_train[feats_r2]

res_2 <- extract_rf_importance(infl_list_train, feature_list_train2)

feats_r3 <- !(apply(res_2[, 2:10], 1, function(row) any(row < 0)))

feature_list_train3 <- feature_list_train2[feats_r3]

res_3 <- extract_rf_importance(infl_list_train, feature_list_train3)

res_3 %>%
  select(Features, Overall) %>%
  arrange(desc(Overall)) %>%
  pull(Features) -> final_features

features_imp_train <- feature_list_train[final_features]
features_imp_test <- feature_list_test[final_features]

# Abschnitt 3: Modellfunktionen

# Die Modellfunktionen verfügen über zwei Modi. Einer ist der Trainings-Modus, in welchem die jeweils optimalen Modellparameter
# für ein bestimmtes Feld bestimmt werden. Der zweite Mode ist der Test-Modus, in dem bereits bestimmte Parameter eingesetzt werden. 
# Konkret wird dies für alle Modelle mit Ausnahme des AO-Modells mithilfe der Paramter "cv.override" und "params.override" erreicht. 

# Ökonometrische Funktionen

# Atkeson-Ohanian (AO)

estimate_ao <- function(x, h) {
  
  if (h == 1) {
    
    ao <- c()
    k <- 1
    
    for (col in 2:(length(x)-1)) {
      past_4obs <- pull(x[(length(x[[col]]) - sum(is.na(x[col])) - 3):(length(x[[col]]) - sum(is.na(x[col]))), col])
      avg <- mean(past_4obs)
      ao[k] <- avg
      k <- k + 1
    }
    
    ao
    
  }
  
  else {
    
    ao <- c()
    z <- 1
    
    for (col in 2:(length(x) - h)) {
      past_4obs <- c(pull(x[(length(x[[col]]) - sum(is.na(x[col])) - 3):(length(x[[col]]) - sum(is.na(x[col]))), col]))
      
      pred_vec <- c(past_4obs, mean(past_4obs)) 
      
      collector <- c()
      k <- 1 
      for (step in 2:h) {
        
        new_pred <- rollmean(pred_vec, 4)[step]
        
        collector[k] <- new_pred
        k <- k + 1
        pred_vec <- c(pred_vec, new_pred)
        
      }
      
      ao[z] <- tail(collector, 1)
      z <- z + 1
    }
    
    ao
  }
  
}

# Autoregressionen (AR)

estimate_ar_rt <- function(x, infl_delta1, max_lag_nr, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Seed für interne Reproduzierbarkeit
  set.seed(28101997)
  
  # Parallele Bearbeitung
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores() - 1 
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Registration der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c()
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    stopCluster(cl)
    
    lag_nr <- params.override
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    forecasts_ar <- c()
    j <- 1
    
    for (col in 2:train_end) {
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data)[1] <- "Y"
      
      data <- append_lags(data, infl_delta1, lag_nr)
      
      variables <- names(data[, 2:ncol(data)])
      
      mod <- lm(Y ~ ., data = data)
      
      newdata <- pull(infl_delta1[(length(infl_delta1[[col]]) - sum(is.na(infl_delta1[col])) - lag_nr + 1):(length(infl_delta1[[col]]) - sum(is.na(infl_delta1[col]))), col])
      
      newdata <- rev(newdata)
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(newdata)) {
        
        new_data[variables[i]] <- newdata[i]
        
      }
      
      new_data[1] <- NULL
      
      pred <- predict(mod, new_data)
      
      forecasts_ar[j] <- pred
      j <- j + 1
    }
    
    return(preds = forecasts_ar)
    
  } else {
    
    multistepAR <- function(x, infl_delta1, lag_nr, h) {
      
      append_lags <- function(data, df, lag_nr) { # Algorithmus 3/Hilfsfunktion 1
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      forecasts_ar <- c()
      j <- 1
      
      for (col in 2:train_end) {
        data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
        
        data <- na.omit(data)
        names(data)[1] <- "Y"
        
        data <- append_lags(data, infl_delta1, lag_nr)
        
        variables <- names(data[, 2:ncol(data)])
        
        mod <- lm(Y ~ ., data = data)
        
        newdata <- pull(infl_delta1[(length(infl_delta1[[col]]) - sum(is.na(infl_delta1[col])) - lag_nr + 1):(length(infl_delta1[[col]]) - sum(is.na(infl_delta1[col]))), col])
        
        newdata <- rev(newdata)
        
        new_data <- tibble(x = rep(0, 1))
        
        for (i in 1:length(newdata)) {
          
          new_data[variables[i]] <- newdata[i]
          
        }
        
        new_data[1] <- NULL
        
        pred <- predict(mod, new_data)
        
        forecasts_ar[j] <- pred
        j <- j + 1
      }
      
      forecasts_ar
    }
    
    predsTune <- function(resultsPreds = NULL) {
      
      me <- list(
        resultsPreds = resultsPreds
      )
      
      class(me) <- append(class(me), "predsTune")
      return(me)
      
    }
    
    finalList <- foreach(lag_nr_tune = 1:max_lag_nr, .packages = c("tibble", "dplyr", "purrr")) %dopar% {
      
      tempItem <- multistepAR(x, infl_delta1, lag_nr = lag_nr_tune, h = h)
      
      results <- predsTune()
      results$resultsPreds <- tempItem
      return(results)
      
    }
    
    stopCluster(cl)
    
    preds <- list()
    k <- 1
    
    for (j in 1:length(finalList)) {
      
      preds[[k]] <- finalList[[j]][[1]]
      k <- k + 1
      
    }
    
    comp <- list()
    t <- 1
    for (j in 1:length(preds)) {
      
      out <- Metrics::rmse(true_inflation, preds[[j]])
      comp[[t]] <- out
      t <- t + 1
    }
    
    best_model_preds <- preds[[which.min(comp)]]
    best_lag <- which.min(comp)
    
    return(list(predictions = best_model_preds, opt.lag = best_lag))
    
  }
  
}

# ADL-u (Phillips-Curve)

estimate_phillips_curve <- function(x, infl_delta1, unemp, lag_nr_infl, lag_nr_u, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    lag_val_infl <- params.override[["lags_infl"]]
    lag_val_u <- params.override[["lags_u"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    adl_u_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data) <- "Y"
      
      data <- append_lags(data, infl_delta1, lag_val_infl)
      
      data <- append_lags(data, unemp, lag_val_u)
      
      variables <- names(data[, 2:ncol(data)])
      
      form_base <- paste0(variables, collapse = " + ")
      
      seq_form <- paste0("Y ~", form_base, collapse = " + ")
      
      mod <- lm(formula(seq_form), data = data)
      
      lags <- c(lag_val_infl, lag_val_u)
      
      new_obs <- c()
      i <- 1 
      
      for (df in list(infl_delta1, unemp)) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lags[i] + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
        i <- i + 1
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) {
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      pred <- predict(mod, new_data)
      
      adl_u_preds[j] <- pred
      j <- j + 1
    }
    
    return(preds = adl_u_preds)
    
  } else {
    
    adl_u_estimator <- function(x, infl_delta1, unemp, lag_nr_infl, lag_nr_u, h) {
      
      adl_u_preds <- c()
      j <- 1
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      for (col in 2:train_end) {
        
        data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
        
        data <- na.omit(data)
        names(data) <- "Y"
        
        data <- append_lags(data, infl_delta1, lag_nr_infl)
        
        data <- append_lags(data, unemp, lag_nr_u)
        
        variables <- names(data[, 2:ncol(data)])
        
        form_base <- paste0(variables, collapse = " + ")
        
        seq_form <- paste0("Y ~", form_base, collapse = " + ")
        
        mod <- lm(formula(seq_form), data = data)
        
        lags <- c(lag_nr_infl, lag_nr_u)
        
        new_obs <- c()
        i <- 1 
        
        for (df in list(infl_delta1, unemp)) {
          
          new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lags[i] + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
          
          if (is_tibble(new_data) == TRUE) {
            
            new_data <- pull(map_df(.x = new_data, .f = rev))
            
            new_obs <- c(new_obs, new_data)
            
          } else {
            
            new_data <- rev(new_data)
            
            new_obs <- c(new_obs, new_data)
            
          }
          
          i <- i + 1
        }
        
        new_data <- tibble(x = rep(0, 1))
        
        for (i in 1:length(new_obs)) {
          
          new_data[variables[i]] <- new_obs[i]
          
        }
        
        new_data[1] <- NULL
        
        pred <- predict(mod, new_data)
        
        adl_u_preds[j] <- pred
        j <- j + 1
      }
      
      adl_u_preds
    }
    
    # Schätzung der optimalen Lag-Länge mithilfe eines Wertegitters
    
    unemp_grid <- expand.grid(lag_nr_infl = 1:lag_nr_infl, lag_nr_u = 1:lag_nr_u)
    
    unemp_grid[, 1:2] <- lapply(1:2, function(x) as.numeric(unemp_grid[[x]]))
    
    results <- list()
    k <- 1
    
    for (i in 1:nrow(unemp_grid)) {
      
      res <- adl_u_estimator(x, infl_delta1, unemp, lag_nr_infl = unemp_grid[i, 1], lag_nr_u = unemp_grid[i, 2], h = h)
      results[[k]] <- res
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(results)) {
      
      out <- Metrics::rmse(true_inflation, results[[j]])
      comp[[h]] <- out
      h <- h + 1
      
    }
    
    best_model_preds <- results[which.min(comp)]
    best_lag_u <- unemp_grid[which.min(comp), ][2]
    best_lag_u <- best_lag_u[[1]]
    
    best_lag_infl <- unemp_grid[which.min(comp), ][1]
    best_lag_infl <- best_lag_infl[[1]]
    
    return(list(predictions = best_model_preds, opt.lag.infl = best_lag_infl, opt.lag.u = best_lag_u))
    
  }
  
}

# ADL-spread 

estimate_adl_spread <- function(x, infl_delta1, spread, lag_nr_infl, lag_nr_spread, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    lag_val_infl <- params.override[["lags_infl"]]
    lag_val_spread <- params.override[["lags_spread"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    adl_spread_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data) <- "Y"
      
      data <- append_lags(data, infl_delta1, lag_val_infl)
      
      data <- append_lags(data, spread, lag_val_spread)
      
      variables <- names(data[, 2:ncol(data)])
      
      form_base <- paste0(variables, collapse = " + ")
      
      seq_form <- paste0("Y ~", form_base, collapse = " + ")
      
      mod <- lm(formula(seq_form), data = data)
      
      lags <- c(lag_val_infl, lag_val_spread)
      
      new_obs <- c()
      i <- 1 
      
      for (df in list(infl_delta1, spread)) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lags[i] + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
        i <- i + 1
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) {
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      pred <- predict(mod, new_data)
      
      adl_spread_preds[j] <- pred
      j <- j + 1
    }
    
    return(preds = adl_spread_preds)
    
  } else {
    
    adl_spread_estimator <- function(x, infl_delta1, spread, lag_nr_infl, lag_nr_spread, h) {
      
      adl_spread_preds <- c()
      j <- 1
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      for (col in 2:train_end) {
        
        data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
        
        data <- na.omit(data)
        names(data) <- "Y"
        
        data <- append_lags(data, infl_delta1, lag_nr_infl)
        
        data <- append_lags(data, spread, lag_nr_spread)
        
        variables <- names(data[, 2:ncol(data)])
        
        form_base <- paste0(variables, collapse = " + ")
        
        seq_form <- paste0("Y ~", form_base, collapse = " + ")
        
        mod <- lm(formula(seq_form), data = data)
        
        lags <- c(lag_nr_infl, lag_nr_spread)
        
        new_obs <- c()
        i <- 1 
        
        for (df in list(infl_delta1, spread)) {
          
          new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lags[i] + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
          
          if (is_tibble(new_data) == TRUE) {
            
            new_data <- pull(map_df(.x = new_data, .f = rev))
            
            new_obs <- c(new_obs, new_data)
            
          } else {
            
            new_data <- rev(new_data)
            
            new_obs <- c(new_obs, new_data)
            
          }
          
          i <- i + 1
        }
        
        new_data <- tibble(x = rep(0, 1))
        
        for (i in 1:length(new_obs)) {
          
          new_data[variables[i]] <- new_obs[i]
          
        }
        
        new_data[1] <- NULL
        
        pred <- predict(mod, new_data)
        
        adl_spread_preds[j] <- pred
        j <- j + 1
      }
      
      adl_spread_preds
    }
    
    # Schätzung der optimalen Lag-Länge mithilfe eines Wertegitters
    
    spread_grid <- expand.grid(lag_nr_infl = 1:lag_nr_infl, lag_nr_spread = 1:lag_nr_spread)
    
    spread_grid[, 1:2] <- lapply(1:2, function(x) as.numeric(spread_grid[[x]]))
    
    results <- list()
    k <- 1
    
    for (i in 1:nrow(spread_grid)) {
      
      res <- adl_spread_estimator(x, infl_delta1, spread, lag_nr_infl = spread_grid[i, 1], lag_nr_spread = spread_grid[i, 2], h = h)
      results[[k]] <- res
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(results)) {
      
      out <- Metrics::rmse(true_inflation, results[[j]])
      comp[[h]] <- out
      h <- h + 1
      
    }
    
    best_model_preds <- results[which.min(comp)]
    best_lag_spread <- spread_grid[which.min(comp), ][2]
    best_lag_spread <- best_lag_spread[[1]]
    
    best_lag_infl <- spread_grid[which.min(comp), ][1]
    best_lag_infl <- best_lag_infl[[1]]
    
    return(list(predictions = best_model_preds, opt.lag.infl = best_lag_infl, opt.lag.spread = best_lag_spread))
    
  }
  
}

# Statistical Learning Modelle 

# Die Schätzung mithilfe von Real-Time-Daten ermöglicht eine abgewandelte Form der Kreuzvalidierung.
# Hierzu werden für eine initial festgelegte Lag-Länge jeweils die besten Hyperparameter für diese
# Lag-Länge bestimmt. Der zweite Schritt besteht dann in der Bestimmung der jeweils besten Lag-Länge
# gegeben die Hyperparameterkombinationen. 

# LASSO

estimate_lasso_rt <- function(x, max_lag_nr, features, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Seed für interne Reproduzierbarkeit
  set.seed(28101997)
  
  # Parallele Verarbeitung der Modelle
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores() - 1 
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    stopCluster(cl)
    
    grid_val <- params.override[["cv_val"]]
    lag_val <- params.override[["lag_val"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    lasso_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      # Schleife für die innere Schätzung der besten Hyperparameter 
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      names(data) <- "Y"
      
      for (f in 1:length(features)) {
        
        data <- append_lags(data, features[[f]], lag_val)
        
      }
      
      data <- as.matrix(data) 
      data <- as_tibble(data)
      data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
      
      train_data <- data
      
      variables <- names(train_data[, 2:ncol(train_data)])
      
      x_train <- as.matrix(train_data[variables])
      y_train <- as.matrix(train_data["Y"])
      
      mod <- glmnet(x_train, y_train, family = "gaussian", alpha = 0, lambda = grid_val)
      
      new_obs <- c()
      
      for (df in features) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_val + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) { 
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      new_data <- as.matrix(new_data)
      
      pred <- c(predict(mod, new_data))
      
      lasso_preds[j] <- pred
      j <- j + 1
      
    }  
    
    return(preds = lasso_preds)
    
  } else {
    
    lambda <- c(seq(0.0005, 0.1, by = 0.0025))
    
    lasso_estimator <- function(x, lag_nr, h, features) {
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      lasso_list <- list()
      v <- 1
      
      for (val in lambda) {
        
        lasso_preds <- c()
        j <- 1
        
        for (col in 2:train_end) {
          
          # Schleife für die innere Schätzung der besten Hyperparameter 
          data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
          
          data <- na.omit(data)
          names(data) <- "Y"
          
          for (f in 1:length(features)) {
            
            data <- append_lags(data, features[[f]], lag_nr)
            
          }
          
          data <- as.matrix(data) 
          data <- as_tibble(data)
          data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
          
          train_data <- data
          
          variables <- names(train_data[, 2:ncol(train_data)])
          
          x_train <- as.matrix(train_data[variables])
          y_train <- as.matrix(train_data["Y"])
          
          mod <- glmnet(x_train, y_train, family = "gaussian", alpha = 1, lambda = val)
          
          new_obs <- c()
          
          for (df in features) {
            
            new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_nr + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
            
            if (is_tibble(new_data) == TRUE) {
              
              new_data <- pull(map_df(.x = new_data, .f = rev))
              
              new_obs <- c(new_obs, new_data)
              
            } else {
              
              new_data <- rev(new_data)
              
              new_obs <- c(new_obs, new_data)
              
            }
            
          }
          
          new_data <- tibble(x = rep(0, 1))
          
          for (i in 1:length(new_obs)) { 
            
            new_data[variables[i]] <- new_obs[i]
            
          }
          
          new_data[1] <- NULL
          
          new_data <- as.matrix(new_data)
          
          pred <- c(predict(mod, new_data))
          
          lasso_preds[j] <- pred
          j <- j + 1
          
        }
        
        lasso_list[[v]] <- lasso_preds
        v <- v + 1
        
      }
      
      comp <- list()
      l <- 1
      for (j in 1:length(lasso_list)) {
        
        out <- Metrics::rmse(true_inflation, lasso_list[[j]])
        comp[[l]] <- out
        l <- l + 1
      }
      
      best_model_preds <- lasso_list[[which.min(comp)]]
      best_tune = lambda[which.min(comp)]
      
      
      return(list(predictions = best_model_preds, opt.tune = best_tune))
    }
    
    predsTune <- function(resultsPreds = NULL, resultsTune = NULL) {
      
      me <- list(
        resultsPreds = resultsPreds,
        resultsTune= resultsTune
      )
      
      class(me) <- append(class(me), "predsTune")
      return(me)
      
    }
    
    finalList <- foreach(lag_nr = 1:max_lag_nr, .packages = c("tibble", "glmnet", "dplyr", "purrr")) %dopar% {
      
      tempItem <- lasso_estimator(x, lag_nr, h = h, features = features)
      predictions <- tempItem$predictions
      tune <- tempItem$opt.tune
      
      results <- predsTune()
      results$resultsPreds <- predictions
      results$resultsTune <- tune
      return(results)
      
    }
    
    # Beendigung des Clusters zur parallelen Verarbeitung
    stopCluster(cl)
    
    tunes <- list()
    preds <- list()
    k <- 1
    
    for (j in 1:length(finalList)) {
      
      preds[[k]] <- finalList[[j]][[1]]
      tunes[[k]] <- finalList[[j]][[2]]
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(preds)) {
      
      out <- Metrics::rmse(true_inflation, preds[[h]])
      comp[[h]] <- out
      h <- h + 1
    }
    
    best_model_preds <- preds[[which.min(comp)]]
    best_lag <- which.min(comp)
    best_tune <- tunes[[which.min(comp)]][[1]]
    
    return(list(predictions = best_model_preds, opt.lag = best_lag, opt.tune = best_tune))
    
  }
  
}

# Ridge 

estimate_ridge_rt <- function(x, max_lag_nr, features, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Seed für interne Reproduzierbarkeit
  set.seed(28101997)
  
  # Parallele Verarbeitung der Modelle
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores() - 1 
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    stopCluster(cl)
    
    grid_val <- params.override[["cv_val"]]
    lag_val <- params.override[["lag_val"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    ridge_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      # Schleife für die innere Schätzung der besten Hyperparameter 
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data) <- "Y"
      
      for (f in 1:length(features)) {
        
        data <- append_lags(data, features[[f]], lag_val)
        
      }
      
      data <- as.matrix(data) 
      data <- as_tibble(data)
      data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
      
      train_data <- data
      
      variables <- names(train_data[, 2:ncol(train_data)])
      
      x_train <- as.matrix(train_data[variables])
      y_train <- as.matrix(train_data["Y"])
      
      mod <- glmnet(x_train, y_train, family = "gaussian", alpha = 0, lambda = grid_val)
      
      new_obs <- c()
      
      for (df in features) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_val + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) { 
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      new_data <- as.matrix(new_data)
      
      pred <- c(predict(mod, new_data))
      
      ridge_preds[j] <- pred
      j <- j + 1
      
    }  
    
    return(preds = ridge_preds)
    
  } else {
    
    lambda <- c(seq(0.0005, 0.1, by = 0.0025))
    
    ridge_estimator <- function(x, lag_nr, h, features) {
      
      
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      ridge_list <- list()
      v <- 1
      
      for (val in lambda) {
        
        ridge_preds <- c()
        j <- 1
        
        for (col in 2:train_end) {
          
          
          # Schleife für die innere Schätzung der besten Hyperparameter 
          data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
          
          data <- na.omit(data)
          names(data) <- "Y"
          
          for (f in 1:length(features)) {
            
            data <- append_lags(data, features[[f]], lag_nr)
            
          }
          
          data <- as.matrix(data) 
          data <- as_tibble(data)
          data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
          
          train_data <- data
          
          variables <- names(train_data[, 2:ncol(train_data)])
          
          x_train <- as.matrix(train_data[variables])
          y_train <- as.matrix(train_data["Y"])
          
          mod <- glmnet(x_train, y_train, family = "gaussian", alpha = 0, lambda = val)
          
          new_obs <- c()
          
          for (df in features) {
            
            new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_nr + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
            
            if (is_tibble(new_data) == TRUE) {
              
              new_data <- pull(map_df(.x = new_data, .f = rev))
              
              new_obs <- c(new_obs, new_data)
              
            } else {
              
              new_data <- rev(new_data)
              
              new_obs <- c(new_obs, new_data)
              
            }
            
          }
          
          new_data <- tibble(x = rep(0, 1))
          
          for (i in 1:length(new_obs)) { 
            
            new_data[variables[i]] <- new_obs[i]
            
          }
          
          new_data[1] <- NULL
          
          new_data <- as.matrix(new_data)
          
          pred <- c(predict(mod, new_data))
          
          ridge_preds[j] <- pred
          j <- j + 1
          
        }
        
        ridge_list[[v]] <- ridge_preds
        v <- v + 1
        
      }
      
      comp <- list()
      l <- 1
      for (j in 1:length(ridge_list)) {
        
        out <- Metrics::rmse(true_inflation, ridge_list[[j]])
        comp[[l]] <- out
        l <- l + 1
      }
      
      best_model_preds <- ridge_list[[which.min(comp)]]
      best_tune = lambda[which.min(comp)]
      
      
      return(list(predictions = best_model_preds, opt.tune = best_tune))
    }
    
    
    
    predsTune <- function(resultsPreds = NULL, resultsTune = NULL) {
      
      me <- list(
        resultsPreds = resultsPreds,
        resultsTune= resultsTune
      )
      
      # Class Name
      class(me) <- append(class(me), "predsTune")
      return(me)
      
    }
    
    finalList <- foreach(lag_nr = 1:max_lag_nr, .packages = c("tibble", "glmnet", "dplyr", "purrr")) %dopar% {
      
      tempItem <- ridge_estimator(x, lag_nr, h = h, features = features)
      predictions <- tempItem$predictions
      tune <- tempItem$opt.tune
      
      results <- predsTune()
      results$resultsPreds <- predictions
      results$resultsTune <- tune
      return(results)
      
    }
    
    # Beendigung des Clusters zur parallelen Verarbeitung
    stopCluster(cl)
    
    tunes <- list()
    preds <- list()
    k <- 1
    
    for (j in 1:length(finalList)) {
      
      preds[[k]] <- finalList[[j]][[1]]
      tunes[[k]] <- finalList[[j]][[2]]
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(preds)) {
      
      out <- Metrics::rmse(true_inflation, preds[[h]])
      comp[[h]] <- out
      h <- h + 1
    }
    
    best_model_preds <- preds[[which.min(comp)]]
    best_lag <- which.min(comp)
    best_tune <- tunes[[which.min(comp)]][[1]]
    
    return(list(predictions = best_model_preds, opt.lag = best_lag, opt.tune = best_tune))
    
  }
  
}

# NNAR

estimate_nnar_rt <- function(x, max_lag_nr, features, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Seed für interne Reproduzierbarkeit
  set.seed(28101997)
  
  # Parallele Verarbeitung der Modelle
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores() - 1 
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    stopCluster(cl)
    
    grid_val <- params.override[["cv_val"]]
    lag_val <- params.override[["lag_val"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    nnar_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      # Schleife für die innere Schätzung der besten Hyperparameter 
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data) <- "Y"
      
      for (f in 1:length(features)) {
        
        data <- append_lags(data, features[[f]], lag_val)
        
      }
      
      data <- as.matrix(data) 
      data <- as_tibble(data)
      data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
      
      train_data <- data
      
      variables <- names(train_data[, 2:ncol(train_data)])
      
      form_base <- paste0(variables, collapse = " + ")
      
      seq_form <- paste0("Y ~", form_base, collapse = " + ")
      
      mod <- neuralnet(formula(seq_form), data = train_data, rep = 30, hidden = grid_val, stepmax = 1e+06, threshold = 0.01)
      
      new_obs <- c()
      
      for (df in features) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_val + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) { 
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      pred <- c(predict(mod, new_data))
      
      nnar_preds[j] <- pred
      j <- j + 1
      
    }
    
    return(preds = nnar_preds)
    
  } else {
    
    nnar_grid <-  list(6, 8, 10, 12, 14, c(14, 12), c(14, 10), c(14, 8), c(14, 6), c(12, 10), c(12, 8), c(12, 6), c(10, 8), c(10, 6), 
                       c(14, 12, 10), c(14, 10, 8), c(14, 8, 6), c(12, 10, 8), c(12, 8, 6))
    
    nnar_estimator <- function(x, lag_nr, h, features) {
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      nnar_list <- list()
      v <- 1
      
      for (val in 1:length(nnar_grid)) {
        
        nnar_preds <- c()
        j <- 1
        
        for (col in 2:train_end) {
          
          
          # Schleife für die innere Schätzung der besten Hyperparameter 
          data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
          
          data <- na.omit(data)
          names(data) <- "Y"
          
          for (f in 1:length(features)) {
            
            data <- append_lags(data, features[[f]], lag_nr)
            
          }
          
          data <- as.matrix(data) 
          data <- as_tibble(data)
          data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
          
          train_data <- data
          
          variables <- names(train_data[, 2:ncol(train_data)])
          
          form_base <- paste0(variables, collapse = " + ")
          
          seq_form <- paste0("Y ~", form_base, collapse = " + ")
          
          mod <- neuralnet(formula(seq_form), data = train_data, rep = 30, hidden = nnar_grid[[val]], stepmax = 1e+06, threshold = 0.01)
          
          new_obs <- c()
          
          for (df in features) {
            
            new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_nr + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
            
            if (is_tibble(new_data) == TRUE) {
              
              new_data <- pull(map_df(.x = new_data, .f = rev))
              
              new_obs <- c(new_obs, new_data)
              
            } else {
              
              new_data <- rev(new_data)
              
              new_obs <- c(new_obs, new_data)
              
            }
            
          }
          
          new_data <- tibble(x = rep(0, 1))
          
          for (i in 1:length(new_obs)) { 
            
            new_data[variables[i]] <- new_obs[i]
            
          }
          
          new_data[1] <- NULL
          
          new_data <- as.matrix(new_data)
          
          pred <- c(predict(mod, new_data))
          
          nnar_preds[j] <- pred
          j <- j + 1
          
        }
        
        nnar_list[[v]] <- nnar_preds
        v <- v + 1
        
      }
      
      comp <- list()
      l <- 1
      for (j in 1:length(nnar_list)) {
        
        out <- Metrics::rmse(true_inflation, nnar_list[[j]])
        comp[[l]] <- out
        l <- l + 1
      }
      
      best_model_preds <- nnar_list[[which.min(comp)]]
      best_tune = nnar_grid[which.min(comp)]
      
      
      return(list(predictions = best_model_preds, opt.tune = best_tune))
    }
    
    
    
    predsTune <- function(resultsPreds = NULL, resultsTune = NULL) {
      
      me <- list(
        resultsPreds = resultsPreds,
        resultsTune= resultsTune
      )
      
      # Class Name
      class(me) <- append(class(me), "predsTune")
      return(me)
      
    }
    
    finalList <- foreach(lag_nr = 1:max_lag_nr, .packages = c("tibble", "neuralnet", "dplyr", "purrr")) %dopar% {
      
      tempItem <- nnar_estimator(x, lag_nr, h = h, features = features)
      predictions <- tempItem$predictions
      tune <- tempItem$opt.tune
      
      results <- predsTune()
      results$resultsPreds <- predictions
      results$resultsTune <- tune
      return(results)
      
    }
    
    # Beendigung des Clusters zur parallelen Verarbeitung
    stopCluster(cl)
    
    tunes <- list()
    preds <- list()
    k <- 1
    
    for (j in 1:length(finalList)) {
      
      preds[[k]] <- finalList[[j]][[1]]
      tunes[[k]] <- finalList[[j]][[2]]
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(preds)) {
      
      out <- Metrics::rmse(true_inflation, preds[[h]])
      comp[[h]] <- out
      h <- h + 1
    }
    
    best_model_preds <- preds[[which.min(comp)]]
    best_lag <- which.min(comp)
    best_tune <- tunes[[which.min(comp)]][[1]]
    
    return(list(predictions = best_model_preds, opt.lag = best_lag, opt.tune = best_tune))
  }
  
}

# NNADL

estimate_nnadl_rt <- function(x, max_lag_nr, features, h, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Seed für interne Reproduzierbarkeit
  set.seed(28101997)
  
  # Parallele Verarbeitung der Modelle
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores() - 1 
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    stopCluster(cl)
    
    grid_val <- params.override[["cv_val"]]
    lag_val <- params.override[["lag_val"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    nnadl_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      # Schleife für die innere Schätzung der besten Hyperparameter 
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data) <- "Y"
      
      for (f in 1:length(features)) {
        
        data <- append_lags(data, features[[f]], lag_val)
        
      }
      
      data <- as.matrix(data) 
      data <- as_tibble(data)
      data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
      
      train_data <- data
      
      variables <- names(train_data[, 2:ncol(train_data)])
      
      form_base <- paste0(variables, collapse = " + ")
      
      seq_form <- paste0("Y ~", form_base, collapse = " + ")
      
      mod <- neuralnet(formula(seq_form), data = train_data, rep = 30, hidden = grid_val, stepmax = 1e+06, threshold = 0.01)
      
      new_obs <- c()
      
      for (df in features) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_val + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) { 
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      pred <- c(predict(mod, new_data))
      
      nnadl_preds[j] <- pred
      j <- j + 1
      
    }
    
    return(preds = nnadl_preds)
    
  } else {
    
    nnadl_grid <- list(40, 30, 20, 10, 5, c(40, 30), c(40, 20), c(40, 10), c(40, 5),
                       c(30, 20), c(30, 10), c(30, 5), c(20, 10), c(20, 5), c(10, 5),
                       c(40, 30, 20), c(40, 30, 10), c(40, 30, 5), c(40, 20, 10), c(40, 20, 5),
                       c(30, 20, 10), c(30, 10, 5), c(20, 10, 5))
    
    nnadl_estimator <- function(x, lag_nr, h, features) {
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      nnadl_list <- list()
      v <- 1
      
      for (val in 1:length(nnadl_grid)) {
        
        nnadl_preds <- c()
        j <- 1
        
        for (col in 2:train_end) {
          
          
          # Schleife für die innere Schätzung der besten Hyperparameter 
          data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
          
          data <- na.omit(data)
          names(data) <- "Y"
          
          for (f in 1:length(features)) {
            
            data <- append_lags(data, features[[f]], lag_nr)
            
          }
          
          data <- as.matrix(data) 
          data <- as_tibble(data)
          data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
          
          train_data <- data
          
          variables <- names(data[, 2:ncol(data)])
          
          form_base <- paste0(variables, collapse = " + ")
          
          seq_form <- paste0("Y ~", form_base, collapse = " + ")
          
          mod <- neuralnet(formula(seq_form), data = train_data, rep = 30, hidden = nnadl_grid[[val]], stepmax = 1e+06, threshold = 0.01)
          
          new_obs <- c()
          
          for (df in features) {
            
            new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_nr + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
            
            if (is_tibble(new_data) == TRUE) {
              
              new_data <- pull(map_df(.x = new_data, .f = rev))
              
              new_obs <- c(new_obs, new_data)
              
            } else {
              
              new_data <- rev(new_data)
              
              new_obs <- c(new_obs, new_data)
              
            }
            
          }
          
          new_data <- tibble(x = rep(0, 1))
          
          for (i in 1:length(new_obs)) { 
            
            new_data[variables[i]] <- new_obs[i]
            
          }
          
          new_data[1] <- NULL
          
          new_data <- as.matrix(new_data)
          
          pred <- c(predict(mod, new_data))
          
          nnadl_preds[j] <- pred
          j <- j + 1
          
        }
        
        nnadl_list[[v]] <- nnadl_preds
        v <- v + 1
        
      }
      
      comp <- list()
      l <- 1
      for (j in 1:length(nnadl_list)) {
        
        out <- Metrics::rmse(true_inflation, nnadl_list[[j]])
        comp[[l]] <- out
        l <- l + 1
      }
      
      best_model_preds <- nnadl_list[[which.min(comp)]]
      best_tune = nnadl_grid[which.min(comp)]
      
      
      return(list(predictions = best_model_preds, opt.tune = best_tune))
    }
    
    predsTune <- function(resultsPreds = NULL, resultsTune = NULL) {
      
      me <- list(
        resultsPreds = resultsPreds,
        resultsTune= resultsTune
      )
      
      # Class Name
      class(me) <- append(class(me), "predsTune")
      return(me)
      
    }
    
    finalList <- foreach(lag_nr = 1:max_lag_nr, .packages = c("tibble", "neuralnet", "dplyr", "purrr")) %dopar% {
      
      tempItem <- nnadl_estimator(x, lag_nr, h = h, features = features)
      predictions <- tempItem$predictions
      tune <- tempItem$opt.tune
      
      results <- predsTune()
      results$resultsPreds <- predictions
      results$resultsTune <- tune
      return(results)
      
    }
    
    # Beendigung des Clusters zur parallelen Verarbeitung
    stopCluster(cl)
    
    tunes <- list()
    preds <- list()
    k <- 1
    
    for (j in 1:length(finalList)) {
      
      preds[[k]] <- finalList[[j]][[1]]
      tunes[[k]] <- finalList[[j]][[2]]
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(preds)) {
      
      out <- Metrics::rmse(true_inflation, preds[[h]])
      comp[[h]] <- out
      h <- h + 1
    }
    
    best_model_preds <- preds[[which.min(comp)]]
    best_lag <- which.min(comp)
    best_tune <- tunes[[which.min(comp)]][[1]]
    
    return(list(predictions = best_model_preds, opt.lag = best_lag, opt.tune = best_tune))
    
  }
  
}

# GBM

estimate_gbm_rt <- function(x, max_lag_nr, h, features, cv.override = FALSE, params.override = NULL) {
  
  # Setzen des Datenendes
  
  train_end <- ncol(x) - h
  
  # Seed für interne Reproduzierbarkeit
  set.seed(28101997)
  
  # Parallele Verarbeitung der Modelle
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores() - 1 
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Registrierung der First-Release-Inflation
  
  get_inflation <- function(x, h) {
    
    true <- c() 
    i <- 1
    
    for (col in ((2 + h):(train_end + h))) {
      true[i] <- x[[(length(x[[col]]) - sum(is.na(x[col]))), col]]
      i <- i + 1
    }
    
    true
    
  }
  
  true_inflation <- get_inflation(x, h = h)
  
  if (cv.override == TRUE) {
    
    stopCluster(cl)
    
    grid_val <- params.override[["cv_val"]]
    lag_val <- params.override[["lag_val"]]
    
    append_lags <- function(data, df, lag_nr) { 
      
      var_names <- c()
      s <- 1
      z <- 1
      
      for (i in h:(lag_nr + h - 1)) {
        
        variable_names <- names(df[col])
        lag <- tibble(lag(df[[col]], n = i))
        vars <- paste0(variable_names, "_Lag", i)
        
        var_names[s] <- vars
        s <- s + 1
        
        lag <- lag[1:nrow(data), ]
        names(lag) <- vars
        
        lag[is.na(lag)] <- 0
        
        lag <- lag[1:nrow(data), ]
        
        data[var_names[z]] <- lag
        z <- z + 1
      }
      data
    }
    
    gbm_preds <- c()
    j <- 1
    
    for (col in 2:train_end) {
      
      
      # Schleife für die innere Schätzung der besten Hyperparameter 
      data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
      
      data <- na.omit(data)
      names(data) <- "Y"
      
      for (f in 1:length(features)) {
        
        data <- append_lags(data, features[[f]], lag_val)
        
      }
      
      data <- as.matrix(data) 
      data <- as_tibble(data)
      data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
      
      train_data <- data
      
      variables <- names(train_data[, 2:ncol(train_data)])
      
      form_base <- paste0(variables, collapse = " + ")
      
      seq_form <- paste0("Y ~", form_base, collapse = " + ")
      
      mod <- gbm(formula(seq_form), data = train_data, distribution = "gaussian", n.trees = 1500, interaction.depth = 5, shrinkage = grid_val)
      
      new_obs <- c()
      
      for (df in features) {
        
        new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_val + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
        
        if (is_tibble(new_data) == TRUE) {
          
          new_data <- pull(map_df(.x = new_data, .f = rev))
          
          new_obs <- c(new_obs, new_data)
          
        } else {
          
          new_data <- rev(new_data)
          
          new_obs <- c(new_obs, new_data)
          
        }
        
      }
      
      new_data <- tibble(x = rep(0, 1))
      
      for (i in 1:length(new_obs)) { 
        
        new_data[variables[i]] <- new_obs[i]
        
      }
      
      new_data[1] <- NULL
      
      pred <- c(predict(mod, new_data, n.trees = 1500))
      
      gbm_preds[j] <- pred
      j <- j + 1
      
    }
    
    return(preds = gbm_preds)
    
  } else {
    
    gbm_grid <- seq(0.0005, 0.2, 0.01)
    
    gbm_estimator <- function(x, lag_nr, h, features) {
      
      append_lags <- function(data, df, lag_nr) { 
        
        var_names <- c()
        s <- 1
        z <- 1
        
        for (i in h:(lag_nr + h - 1)) {
          
          variable_names <- names(df[col])
          lag <- tibble(lag(df[[col]], n = i))
          vars <- paste0(variable_names, "_Lag", i)
          
          var_names[s] <- vars
          s <- s + 1
          
          lag <- lag[1:nrow(data), ]
          names(lag) <- vars
          
          lag[is.na(lag)] <- 0
          
          lag <- lag[1:nrow(data), ]
          
          data[var_names[z]] <- lag
          z <- z + 1
        }
        data
      }
      
      gbm_list <- list()
      v <- 1
      
      for (val in 1:length(gbm_grid)) {
        
        gbm_preds <- c()
        j <- 1
        
        for (col in 2:train_end) {
          
          # Schleife für die innere Schätzung der besten Hyperparameter 
          data <- x[1:(length(x[[col]]) - sum(is.na(x[col]))), col]
          
          data <- na.omit(data)
          names(data) <- "Y"
          
          for (f in 1:length(features)) {
            
            data <- append_lags(data, features[[f]], lag_nr)
            
          }
          
          data <- as.matrix(data) 
          data <- as_tibble(data)
          data[, 1:ncol(data)] <- lapply(1:ncol(data), function(x) as.numeric(data[[x]]))
          
          train_data <- data
          
          variables <- names(train_data[, 2:ncol(train_data)])
          
          form_base <- paste0(variables, collapse = " + ")
          
          seq_form <- paste0("Y ~", form_base, collapse = " + ")
          
          mod <- gbm(formula(seq_form), data = train_data, distribution = "gaussian", n.trees = 1500, interaction.depth = 5, shrinkage = gbm_grid[val])
          
          new_obs <- c()
          
          for (df in features) {
            
            new_data <- df[(length(df[[col]]) - sum(is.na(df[col])) - lag_nr + 1):(length(df[[col]]) - sum(is.na(df[col]))), col]
            
            if (is_tibble(new_data) == TRUE) {
              
              new_data <- pull(map_df(.x = new_data, .f = rev))
              
              new_obs <- c(new_obs, new_data)
              
            } else {
              
              new_data <- rev(new_data)
              
              new_obs <- c(new_obs, new_data)
              
            }
            
          }
          
          new_data <- tibble(x = rep(0, 1))
          
          for (i in 1:length(new_obs)) { 
            
            new_data[variables[i]] <- new_obs[i]
            
          }
          
          new_data[1] <- NULL
          
          pred <- c(predict(mod, new_data, n.trees = 1500))
          
          gbm_preds[j] <- pred
          j <- j + 1
          
        }
        
        gbm_list[[v]] <- gbm_preds
        v <- v + 1
        
      }
      
      comp <- list()
      l <- 1
      for (j in 1:length(gbm_list)) {
        
        out <- Metrics::rmse(true_inflation, gbm_list[[j]])
        comp[[l]] <- out
        l <- l + 1
      }
      
      best_model_preds <- gbm_list[[which.min(comp)]]
      best_tune = gbm_grid[which.min(comp)]
      
      
      return(list(predictions = best_model_preds, opt.tune = best_tune))
    }
    
    
    predsTune <- function(resultsPreds = NULL, resultsTune = NULL) {
      
      me <- list(
        resultsPreds = resultsPreds,
        resultsTune= resultsTune
      )
      
      # Class Name
      class(me) <- append(class(me), "predsTune")
      return(me)
      
    }
    
    finalList <- foreach(lag_nr = 1:max_lag_nr, .packages = c("tibble", "gbm", "dplyr", "purrr")) %dopar% {
      
      tempItem <- gbm_estimator(x, lag_nr, h = h, features = features)
      predictions <- tempItem$predictions
      tune <- tempItem$opt.tune
      
      results <- predsTune()
      results$resultsPreds <- predictions
      results$resultsTune <- tune
      return(results)
      
    }
    
    # Beendigung des Clusters zur parallelen Verarbeitung
    stopCluster(cl)
    
    tunes <- list()
    preds <- list()
    k <- 1
    
    for (j in 1:length(finalList)) {
      
      preds[[k]] <- finalList[[j]][[1]]
      tunes[[k]] <- finalList[[j]][[2]]
      k <- k + 1
      
    }
    
    comp <- list()
    h <- 1
    for (j in 1:length(preds)) {
      
      out <- Metrics::rmse(true_inflation, preds[[h]])
      comp[[h]] <- out
      h <- h + 1
    }
    
    best_model_preds <- preds[[which.min(comp)]]
    best_lag <- which.min(comp)
    best_tune <- tunes[[which.min(comp)]][[1]]
    
    return(list(predictions = best_model_preds, opt.lag = best_lag, opt.tune = best_tune))
    
  }
  
}

# Abschnitt 4: Modellvorhersagen und Analysegrundlagen

set.seed(28101997)

# AR(1) - Benchmark

results_bench <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ar_rt(infl_list_train[[j]], infl_list_train[[1]], max_lag_nr = 1, h = j, cv.override = FALSE)
  results_bench[[r]] <- res
  r <- r + 1
  
}


# AR(p)

results_ar <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ar_rt(infl_list_train[[j]], infl_list_train[[1]], max_lag_nr = 6, h = j, cv.override = FALSE)
  results_ar[[r]] <- res
  r <- r + 1
  
}

# ADL-u 

results_adl_u <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_phillips_curve(infl_list_train[[j]], infl_list_train[[1]], UNEMP_PC_TRAIN, lag_nr_infl = 6, lag_nr_u = 6, h = j, cv.override = FALSE)
  results_adl_u[[r]] <- res
  r <- r + 1
  
}  

# ADL-spread

results_adl_spread <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_adl_spread(infl_list_train[[j]], infl_list_train[[1]], SPREAD1_3_TRAIN, lag_nr_infl = 6, lag_nr_spread = 6, h = j, cv.override = FALSE)
  results_adl_spread[[r]] <- res
  r <- r + 1
  
}  


# AO

inflation_ao <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ao(INFL_COMP[, 1:72], h = j)
  inflation_ao[[r]] <- res
  r <- r + 1
  
}  

# LASSO 

results_lasso <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_lasso_rt(infl_list_train[[j]], max_lag_nr = 6, h = j, features = features_imp_train, cv.override = FALSE)
  results_lasso[[r]] <- res
  r <- r + 1
  
}  

# RIDGE

results_ridge <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ridge_rt(infl_list_train[[j]], max_lag_nr = 6, h = j, features = features_imp_train, cv.override = FALSE)
  results_ridge[[r]] <- res
  r <- r + 1
  
}  

# NNAR

results_nnar <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_nnar_rt(infl_list_train[[j]], max_lag_nr = 6, h = j, features = list(infl_list_train[[1]]), cv.override = FALSE)
  results_nnar[[r]] <- res
  r <- r + 1
  
} 

# NNADL

results_nnadl <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_nnadl_rt(infl_list_train[[j]], max_lag_nr = 6, h = j, features = features_imp_train, cv.override = FALSE)
  results_nnadl[[r]] <- res
  r <- r + 1
  
}

# Boosting

results_gbm <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_gbm_rt(infl_list_train[[j]], max_lag_nr = 6, h = j, features = features_imp_train, cv.override = FALSE)
  results_gbm[[r]] <- res
  r <- r + 1
  
}

# Extraktion der tatsächlichen Inflationsvorhersage mithilfe von invertierter Differenzierung

# Funktion zur Sammlung von Inflationsvorhersagen (Algorithmus 7/Hilfsfunktion 5)
collect_predictions <- function(base_rates, predictions, h) {
  
  collector <- c()
  s <- 1
  i <- 1
  
  collect_end <- ncol(base_rates) - h
  
  for (col in 2:collect_end) {
    
    base <- base_rates[[length(base_rates[[col]]) - sum(is.na(base_rates[col])), col]]
    inflation_pred <- base + predictions[i]
    i <- i + 1
    
    collector[s] <- inflation_pred
    s <- s + 1
  }
  
  collector
  
} 

# Sammlung der Vorhersagen des Validation-Sets

# AR(1) - Benchmark 

inflation_bench <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_bench[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_bench[[m]] <- inflation
  m <- m + 1
  
}

# AR 

inflation_ar <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_ar[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_ar[[m]] <- inflation
  m <- m + 1
  
}

# Phillips-Curve (ADL-u)

inflation_adl_u <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_adl_u[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_adl_u[[m]] <- inflation
  m <- m + 1
  
}

# ADL-spread

inflation_adl_spread <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_adl_spread[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_adl_spread[[m]] <- inflation
  m <- m + 1
  
}

# Ridge

inflation_ridge <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_ridge[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_ridge[[m]] <- inflation
  m <- m + 1
  
}

# LASSO

inflation_lasso <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_lasso[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_lasso[[m]] <- inflation
  m <- m + 1
  
}

# NNAR

inflation_nnar <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_nnar[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_nnar[[m]] <- inflation
  m <- m + 1
  
}

# NNADL

inflation_nnadl <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_nnadl[[d]][[1]]
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_nnadl[[m]] <- inflation
  m <- m + 1
  
}

# GBM (Boosting)

inflation_gbm <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_gbm[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, 1:train_end], predictions, h = d)
  inflation_gbm[[m]] <- inflation
  m <- m + 1
  
}

# Konstruktion der Loss-Serien für das Validation-Set

# Quadratischer Loss (y - y_pred)^2

quad_loss <- function(actual, predicted) {
  
  res <- (actual - predicted)^2
  return(res)
  
}

# Registrierung der First-Release-Werte des Validation-Sets

real_inflation <- list()
z <- 1

for (i in 1:8) {
  
  inflation <- get_inflation(INFL_COMP[, 1:72], h = i)
  real_inflation[[z]] <- inflation
  z <- z + 1
}

# AR

train_loss_ar <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_ar[[j]], .f = quad_loss))
  train_loss_ar[[r]] <- loss
  r <- r + 1
  
}


# Phillips-Curve (ADL-u)

train_loss_adl_u <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_adl_u[[j]], .f = quad_loss))
  train_loss_adl_u[[r]] <- loss
  r <- r + 1
  
}

# ADL-spread

train_loss_adl_spread <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_adl_spread[[j]], .f = quad_loss))
  train_loss_adl_spread[[r]] <- loss
  r <- r + 1
  
}

# AO 

train_loss_ao <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_ao[[j]], .f = quad_loss))
  train_loss_ao[[r]] <- loss
  r <- r + 1
  
}

# Ridge

train_loss_ridge <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_ridge[[j]], .f = quad_loss))
  train_loss_ridge[[r]] <- loss
  r <- r + 1
  
}

# LASSO

train_loss_lasso <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_lasso[[j]], .f = quad_loss))
  train_loss_lasso[[r]] <- loss
  r <- r + 1
  
}

# NNAR

train_loss_nnar <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_nnar[[j]], .f = quad_loss))
  train_loss_nnar[[r]] <- loss
  r <- r + 1
  
}

# NNADL

train_loss_nnadl <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_nnadl[[j]], .f = quad_loss))
  train_loss_nnadl[[r]] <- loss
  r <- r + 1
  
}

# GBM

train_loss_gbm <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation[[j]], .y = inflation_gbm[[j]], .f = quad_loss))
  train_loss_gbm[[r]] <- loss
  r <- r + 1
  
}

# Konstruktion der Loss-Matrix für jeden Horizont

model_names <- c("adl_spread", "adl_u", "ao", "ar", "gbm", "lasso", "nnadl", "nnar", "ridge")

obj <- mget(ls(pattern = "train_loss_"))

for (j in (1:(length(obj) - 1))) {
  
  temp <- list()
  z <- 1
  
  for (i in 1:length(obj)) {
    
    cbd <- obj[[i]][[j]]
    temp[[z]] <- cbd
    z <- z + 1
    
  }
  
  names(temp) <- model_names[1:length(obj)]
  
  temp <- as_tibble(temp)    
  
  temp <- as.matrix(temp)
  
  assign(paste0("LOSS_MATRIX_TRAIN", j), temp)
  
}

# RMSE für das Benchmark-Model

error_bench <- c()
k <- 1

for (j in 1:8) {
  
  entry <- Metrics::rmse(real_inflation[[j]], inflation_bench[[j]])
  entry <- round(entry * 100, digits = 3)
  error_bench[k] <- entry
  k <- k + 1
  
}

# Fehlermaße (absolut und relativ) für das Validation-Set

# ADL-spread

error_adl_spread <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_adl_spread[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_adl_spread[[k]] <- entry
  k <- k + 1
  
}

# ADL-u

error_adl_u <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_adl_u[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_adl_u[[k]] <- entry
  k <- k + 1
  
}

# AO

error_ao <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_ao[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_ao[[k]] <- entry
  k <- k + 1
  
}

# AR(p)

error_ar <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_ar[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_ar[[k]] <- entry
  k <- k + 1
  
}

# GBM

error_gbm <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_gbm[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_gbm[[k]] <- entry
  k <- k + 1
  
}

# LASSO

error_lasso <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_lasso[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_lasso[[k]] <- entry
  k <- k + 1
  
}

# NNADL

error_nnadl <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_nnadl[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_nnadl[[k]] <- entry
  k <- k + 1
  
}

# NNAR

error_nnar <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_nnar[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_nnar[[k]] <- entry
  k <- k + 1
  
}

error_ridge <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation[[j]], inflation_ridge[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_ridge[[k]] <- entry
  k <- k + 1
  
}

# Durchführung der Tests für die Model Confidence Sets 

# Prognosehorizont 1

res1 <- MCSprocedure(LOSS_MATRIX_TRAIN1, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 2

res2 <- MCSprocedure(LOSS_MATRIX_TRAIN2, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 3

res3 <- MCSprocedure(LOSS_MATRIX_TRAIN3, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 4

res4 <- MCSprocedure(LOSS_MATRIX_TRAIN4, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 5

res5 <- MCSprocedure(LOSS_MATRIX_TRAIN5, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 6

res6 <- MCSprocedure(LOSS_MATRIX_TRAIN6, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 7

res7 <- MCSprocedure(LOSS_MATRIX_TRAIN7, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 8

res8 <- MCSprocedure(LOSS_MATRIX_TRAIN8, alpha = 0.15, B = 10000, statistic = "TR")


# Anwendung der Modell-Hyperparameterkombinationen aus dem Validation-Set auf das test-Set

# AR(1) - Benchmark Test

results_bench_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ar_rt(infl_list_test[[j]], infl_list_test[[1]], max_lag_nr = 1, h = j, cv.override = FALSE)
  results_bench_test[[r]] <- res
  r <- r + 1
  
}

# AR(p)

results_ar_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ar_rt(infl_list_test[[j]], infl_list_test[[1]], max_lag_nr = 6, h = j, cv.override = TRUE, params.override = results_ar[[j]][["opt.lag"]])
  results_ar_test[[r]] <- res
  r <- r + 1
  
}

# ADL-u 

results_adl_u_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_phillips_curve(infl_list_test[[j]], infl_list_test[[1]], UNEMP_PC_TEST, lag_nr_infl = 6, lag_nr_u = 6, h = j, cv.override = TRUE, params.override = list(lags_infl = results_adl_u[[j]][["opt.lag.infl"]], lags_u = results_adl_u[[j]][["opt.lag.u"]]))
  results_adl_u_test[[r]] <- res
  r <- r + 1
  
}  

# ADL-spread

results_adl_spread_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_adl_spread(infl_list_test[[j]], infl_list_test[[1]], SPREAD1_3_TEST, lag_nr_infl = 6, lag_nr_spread = 6, h = j, cv.override = TRUE, params.override = list(lags_infl = results_adl_spread[[j]][["opt.lag.infl"]], lags_spread = results_adl_spread[[j]][["opt.lag.spread"]]))
  results_adl_spread_test[[r]] <- res
  r <- r + 1
  
}  


# AO

inflation_ao_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ao(INFL_COMP[, c(1, 72:98)], h = j)
  inflation_ao_test[[r]] <- res
  r <- r + 1
  
}  

# LASSO 

results_lasso_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_lasso_rt(infl_list_test[[j]], max_lag_nr = 6, h = j, features = features_imp_test, cv.override = TRUE, params.override = list(cv_val = results_lasso[[j]][["opt.tune"]], lag_val = results_lasso[[j]][["opt.lag"]]))
  results_lasso_test[[r]] <- res
  r <- r + 1
  
}  

# RIDGE

results_ridge_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_ridge_rt(infl_list_test[[j]], max_lag_nr = 6, h = j, features = features_imp_test, cv.override = TRUE, params.override = list(cv_val = results_ridge[[j]][["opt.tune"]], lag_val = results_ridge[[j]][["opt.lag"]]))
  results_ridge_test[[r]] <- res
  r <- r + 1
  
}  

# NNAR

results_nnar_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_nnar_rt(infl_list_test[[j]], max_lag_nr = 6, h = j, features = list(infl_list_test[[1]]), cv.override = TRUE, params.override = list(cv_val = results_nnar[[j]][["opt.tune"]], lag_val = results_nnar[[j]][["opt.lag"]]))
  results_nnar_test[[r]] <- res
  r <- r + 1
  
} 

# NNADL

results_nnadl_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_nnadl_rt(infl_list_test[[j]], max_lag_nr = 6, h = j, features = features_imp_test, cv.override = TRUE, params.override = list(cv_val = results_nnadl[[j]][["opt.tune"]], lag_val = results_nnadl[[j]][["opt.lag"]]))
  results_nnadl_test[[r]] <- res
  r <- r + 1
  
}

# Boosting

results_gbm_test <- list()
r <- 1

for (j in 1:8) {
  
  res <- estimate_gbm_rt(infl_list_test[[j]], max_lag_nr = 6, h = j, features = features_imp_test, cv.override = TRUE, params.override = list(cv_val = results_gbm[[j]][["opt.tune"]], lag_val = results_gbm[[j]][["opt.lag"]]))
  results_gbm_test[[r]] <- res
  r <- r + 1
  
}

# Sammlung der Inflationsvorhersagen für das Test-Set

# AR(1) - Benchmark Test

inflation_bench_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- unlist(results_bench_test[[d]][[1]])
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_bench_test[[m]] <- inflation
  m <- m + 1
  
}

# AR 

inflation_ar_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_ar_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_ar_test[[m]] <- inflation
  m <- m + 1
  
}

# Phillips-Curve (ADL-u)

inflation_adl_u_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_adl_u_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_adl_u_test[[m]] <- inflation
  m <- m + 1
  
}

# ADL-spread

inflation_adl_spread_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_adl_spread_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_adl_spread_test[[m]] <- inflation
  m <- m + 1
  
}

# Ridge

inflation_ridge_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_ridge_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_ridge_test[[m]] <- inflation
  m <- m + 1
  
}

# LASSO

inflation_lasso_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_lasso_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_lasso_test[[m]] <- inflation
  m <- m + 1
  
}

# NNAR

inflation_nnar_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_nnar_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_nnar_test[[m]] <- inflation
  m <- m + 1
  
}

# NNADL

inflation_nnadl_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_nnadl_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_nnadl_test[[m]] <- inflation
  m <- m + 1
  
}

# GBM (Boosting)

inflation_gbm_test <- list()
m <- 1

for (d in 1:8) {
  
  predictions <- results_gbm_test[[d]]
  inflation <- collect_predictions(INFL_COMP[, c(1, 72:98)], predictions, h = d)
  inflation_gbm_test[[m]] <- inflation
  m <- m + 1
  
}

# Construct loss series for test set 

real_inflation_test <- list()
z <- 1

for (i in 1:8) {
  
  inflation <- get_inflation(INFL_COMP[, c(1, 72:98)], h = i)
  real_inflation_test[[z]] <- inflation
  z <- z + 1
}

# AR

test_loss_ar <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_ar_test[[j]], .f = quad_loss))
  test_loss_ar[[r]] <- loss
  r <- r + 1
  
}

# Phillips-Curve (ADL-u)

test_loss_adl_u <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_adl_u_test[[j]], .f = quad_loss))
  test_loss_adl_u[[r]] <- loss
  r <- r + 1
  
}

# ADL-spread

test_loss_adl_spread <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_adl_spread_test[[j]], .f = quad_loss))
  test_loss_adl_spread[[r]] <- loss
  r <- r + 1
  
}

# AO 

test_loss_ao <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_ao_test[[j]], .f = quad_loss))
  test_loss_ao[[r]] <- loss
  r <- r + 1
  
}

# Ridge

test_loss_ridge <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_ridge_test[[j]], .f = quad_loss))
  test_loss_ridge[[r]] <- loss
  r <- r + 1
  
}

# LASSO

test_loss_lasso <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_lasso_test[[j]], .f = quad_loss))
  test_loss_lasso[[r]] <- loss
  r <- r + 1
  
}

# NNAR

test_loss_nnar <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_nnar_test[[j]], .f = quad_loss))
  test_loss_nnar[[r]] <- loss
  r <- r + 1
  
}

# NNADL

test_loss_nnadl <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_nnadl_test[[j]], .f = quad_loss))
  test_loss_nnadl[[r]] <- loss
  r <- r + 1
  
}

# GBM

test_loss_gbm <- list()
r <- 1

for (j in 1:8) {
  
  loss <- unlist(map2(.x = real_inflation_test[[j]], .y = inflation_gbm_test[[j]], .f = quad_loss))
  test_loss_gbm[[r]] <- loss
  r <- r + 1
  
}

# Konstruktion der Loss-Matrizen für die Prognosehorizonte 

model_names <- c("adl_spread", "adl_u", "ao", "ar", "gbm", "lasso", "nnadl", "nnar", "ridge")

obj <- mget(ls(pattern = "test_loss_"))

for (j in (1:(length(obj) - 1))) {
  
  temp <- list()
  z <- 1
  
  for (i in 1:length(obj)) {
    
    cbd <- obj[[i]][[j]]
    temp[[z]] <- cbd
    z <- z + 1
    
  }
  
  names(temp) <- model_names[1:length(obj)]
  
  temp <- as_tibble(temp)    
  
  temp <- as.matrix(temp)
  
  assign(paste0("LOSS_MATRIX", "_TEST", j), temp)
  
}

# Extrahierung der Fehlermaße des Test-Sets 

# RMSE für das Benchmark Model

error_bench_test <- c()
k <- 1

for (j in 1:8) {
  
  entry <- Metrics::rmse(real_inflation_test[[j]], inflation_bench_test[[j]])
  entry <- round(entry * 100, digits = 3)
  error_bench_test[k] <- entry
  k <- k + 1
  
}

# Relative Fehler

# ADL-spread

error_adl_spread_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_adl_spread_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_adl_spread_test[[k]] <- entry
  k <- k + 1
  
}

# ADL-u

error_adl_u_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_adl_u_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_adl_u_test[[k]] <- entry
  k <- k + 1
  
}

# AO

error_ao_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_ao_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_ao_test[[k]] <- entry
  k <- k + 1
  
}

# AR(p)

error_ar_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_ar_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_ar_test[[k]] <- entry
  k <- k + 1
  
}

# GBM

error_gbm_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_gbm_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_gbm_test[[k]] <- entry
  k <- k + 1
  
}

# LASSO

error_lasso_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_lasso_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_lasso_test[[k]] <- entry
  k <- k + 1
  
}

# NNADL

error_nnadl_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_nnadl_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_nnadl_test[[k]] <- entry
  k <- k + 1
  
}

# NNAR

error_nnar_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_nnar_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_nnar_test[[k]] <- entry
  k <- k + 1
  
}

error_ridge_test <- list()
k <- 1

for (j in 1:8) {
  
  entry1 <- Metrics::rmse(real_inflation_test[[j]], inflation_ridge_test[[j]])
  entry1 <- round(entry1 * 100, digits = 3)
  
  entry2 <- round(entry1/error_bench_test[[j]], digits = 3)
  
  entry <- list(entry1, entry2)
  
  error_ridge_test[[k]] <- entry
  k <- k + 1
  
}

# Durchführung der Tests für die Model Confidence Sets 

# Prognosehorizont 1

res1_test <- MCSprocedure(LOSS_MATRIX_TEST1, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 2

res2_test <- MCSprocedure(LOSS_MATRIX_TEST2, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 3

res3_test <- MCSprocedure(LOSS_MATRIX_TEST3, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 4

res4_test <- MCSprocedure(LOSS_MATRIX_TEST4, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 5

res5_test <- MCSprocedure(LOSS_MATRIX_TEST5, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 6

res6_test <- MCSprocedure(LOSS_MATRIX_TEST6, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 7

res7_test <- MCSprocedure(LOSS_MATRIX_TEST7, alpha = 0.15, B = 10000, statistic = "TR")

# Prognosehorizont 8

res8_test <- MCSprocedure(LOSS_MATRIX_TEST8, alpha = 0.15, B = 10000, statistic = "TR")

# Abschnitt 5: Graphiken

# Vergleich zwischen First-Release-Inflation und aktuellstem Vintage

colors <- c("First-Release" = "black", "Aktuellstes Vintage" = "#5e8ef4")

train_inflation <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = real_inflation[[1]])
train_recent <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = INFL_COMP[[length(INFL_COMP)]][146:215])

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "First-Release")) + geom_line(size = 0.8, linetype = "dotdash") + 
  geom_line(data = train_recent, aes(x = Date, y = Inflation, color = "Aktuellstes Vintage"), size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") +
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")


test_inflation <- tibble(Date = seq(as.Date("2013-10-01"), as.Date("2020-01-01"), by = "quarter"), Inflation = real_inflation_test[[1]])
test_recent <- tibble(Date = seq(as.Date("2013-10-01"), as.Date("2020-01-01"), by = "quarter"), Inflation = INFL_COMP[[length(INFL_COMP)]][216:nrow(INFL_COMP)])

ggplot(test_inflation, aes(x = Date, y = Inflation, color = "First-Release")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = test_recent, aes(x = Date, y = Inflation, color = "Aktuellstes Vintage"), size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4) + 
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")

# Definition der Color-Codes für Modelle

colors <- c("Inflation" = "black", "AO" = "#6b8fb1", "AR" = "#89bbff", "GBM" = "#7861d3", "RIDGE" = "#d51968", "ADL-u" = "#19a1db",
            "ADL-spread" = "#cc6666", "NNAR" = "#cbd1ee", "NNADL" = "#948ee3", "LASSO" = "#f0cb74")

# Rezessionen

begin <- c("2001-04-01", "2008-01-01")
end <- c("2001-11-01", "2009-06-01")
rec_dates_train <- tibble(begin, end)
rec_dates_train$begin <- as.Date(rec_dates_train$begin)
rec_dates_train$end <- as.Date(rec_dates_train$end)

## Loss der Serie — TRAIN (h1)

LOSSES <- list()
k <- 1
for (j in 1:ncol(LOSS_MATRIX_TRAIN1)) {
  
  dat <- LOSS_MATRIX_TRAIN1[, j]
  
  result <- cumsum(dat)
  
  LOSSES[[k]] <- result
  k <- k + 1
  
}

names(LOSSES) <- model_names

library(ggplot2)

val_ao <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["ao"]])

val_ar <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation =  LOSSES[["ar"]])

val_gbm <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["gbm"]])

val_ridge <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["ridge"]])

val_lasso <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["lasso"]])

val_nnar <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["nnar"]])

val_nnadl <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["nnadl"]])

val_adl_u <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["adl_u"]])

val_adl_spread <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["adl_spread"]])

ggplot(val_ao, aes(x = Date, y = Inflation, color = "AO")) + geom_line(size = 0.7) + 
  geom_line(data = val_ar, aes(x = Date, y = Inflation, color = "AR"), size = 0.7) + 
  geom_line(data = val_gbm, aes(x = Date, y = Inflation, color = "GBM"), size = 0.7) +
  geom_line(data = val_ridge, aes(x = Date, y = Inflation, color = "RIDGE"), size = 0.7) +
  geom_line(data = val_lasso, aes(x = Date, y = Inflation, color = "LASSO"), size = 0.7) +
  geom_line(data = val_adl_u, aes(x = Date, y = Inflation, color = "ADL-u"), size = 0.7) + 
  geom_line(data = val_adl_spread, aes(x = Date, y = Inflation, color = "ADL-spread"), size = 0.7) + 
  geom_line(data = val_nnar, aes(x = Date, y = Inflation, color = "NNAR"), size = 0.7) + 
  # geom_line(data = val_nnadl, aes(x = Date, y = Inflation, color = "NNADL"), size = 0.7) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  theme_classic() + labs(x = "Jahr", y = "Kumulierter quadratischer Loss", color = "Modellreferenz", 
                         caption = "Anmerkung: Das NNADL-Modell wurde aufgrund des deutlich höheren Losses \n aus Darstellungsgründen ausgelassen. In grau hinterlegt sind Rezessionsquartale.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.03))

## Loss der Serie — TRAIN (h8)

LOSSES <- list()
k <- 1
for (j in 1:ncol(LOSS_MATRIX_TRAIN8)) {
  
  dat <- LOSS_MATRIX_TRAIN8[, j]
  
  result <- cumsum(dat)
  
  LOSSES[[k]] <- result
  k <- k + 1
  
}

names(LOSSES) <- model_names

library(ggplot2)

val_ao <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["ao"]])

val_ar <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation =  LOSSES[["ar"]])

val_gbm <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["gbm"]])

val_ridge <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["ridge"]])

val_lasso <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["lasso"]])

val_nnar <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["nnar"]])

val_nnadl <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["nnadl"]])

val_adl_u <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["adl_u"]])

val_adl_spread <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = LOSSES[["adl_spread"]])

ggplot(val_ao, aes(x = Date, y = Inflation, color = "AO")) + geom_line(size = 0.7) + 
  geom_line(data = val_ar, aes(x = Date, y = Inflation, color = "AR"), size = 0.7) + 
  geom_line(data = val_gbm, aes(x = Date, y = Inflation, color = "GBM"), size = 0.7) +
  geom_line(data = val_ridge, aes(x = Date, y = Inflation, color = "RIDGE"), size = 0.7) +
  geom_line(data = val_lasso, aes(x = Date, y = Inflation, color = "LASSO"), size = 0.7) +
  geom_line(data = val_adl_u, aes(x = Date, y = Inflation, color = "ADL-u"), size = 0.7) + 
  geom_line(data = val_adl_spread, aes(x = Date, y = Inflation, color = "ADL-spread"), size = 0.7) + 
  geom_line(data = val_nnar, aes(x = Date, y = Inflation, color = "NNAR"), size = 0.7) + 
  #geom_line(data = val_nnadl, aes(x = Date, y = Inflation, color = "NNADL"), size = 0.7) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  theme_classic() + labs(x = "Jahr", y = "Kumulierter quadratischer Loss", color = "Modellreferenz", 
                         caption = "Anmerkung: Das NNADL-Modell wurde aufgrund des deutlich höheren Losses \n aus Darstellungsgründen ausgelassen. In grau hinterlegt sind Rezessionsquartale.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y")

# Graphischer Vergleich der prognostizierten Inflationsvorhersagen

# Training Set (h = 1)

train_inflation <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = real_inflation[[1]])

train_inflation_ao <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_ao[[1]])

train_inflation_ar <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation =  inflation_ar[[1]])

train_inflation_gbm <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_gbm[[1]])

train_inflation_ridge <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_ridge[[1]])

train_inflation_lasso <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_lasso[[1]])

train_inflation_nnar <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_nnar[[1]])

train_inflation_nnadl <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_nnadl[[1]])

train_inflation_adl_u <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_adl_u[[1]])

train_inflation_adl_spread <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_adl_spread[[1]])

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "Inflation")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = train_inflation_adl_u, aes(x = Date, y = Inflation, color = "ADL-u"), size = 0.8) + 
  geom_line(data = train_inflation_adl_spread, aes(x = Date, y = Inflation, color = "ADL-spread"), size = 0.8) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  geom_hline(yintercept = 0) + 
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_y_continuous(breaks = c(-0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05))

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "Inflation")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = train_inflation_ar, aes(x = Date, y = Inflation, color = "AR"), size = 0.9) + 
  geom_line(data = train_inflation_ao, aes(x = Date, y = Inflation, color = "AO"), size = 0.9) + 
  geom_line(data = train_inflation_nnar, aes(x = Date, y = Inflation, color = "NNAR"), size = 0.9) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  geom_hline(yintercept = 0) + 
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_y_continuous(breaks = c(-0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05))

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "Inflation")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = train_inflation_gbm, aes(x = Date, y = Inflation, color = "GBM"), size = 0.6) +
  geom_line(data = train_inflation_ridge, aes(x = Date, y = Inflation, color = "RIDGE"), size = 0.6) +
  # geom_line(data = train_inflation_nnadl, aes(x = Date, y = Inflation, color = "NNADL"), size = 0.6) + 
  geom_line(data = train_inflation_lasso, aes(x = Date, y = Inflation, color = "LASSO"), size = 0.6) + 
  geom_hline(yintercept = 0) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = c(-0.08, -0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06))

# Training Set (h = 8)

train_inflation <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = real_inflation[[8]])

train_inflation_ao <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_ao[[8]])

train_inflation_ar <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation =  inflation_ar[[8]])

train_inflation_gbm <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_gbm[[8]])

train_inflation_ridge <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_ridge[[8]])

train_inflation_lasso <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_lasso[[8]])

train_inflation_nnar <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_nnar[[8]])

train_inflation_nnadl <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_nnadl[[8]])

train_inflation_adl_u <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_adl_u[[8]])

train_inflation_adl_spread <- tibble(Date = seq(as.Date("1998-01-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = inflation_adl_spread[[8]])

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "Inflation")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = train_inflation_adl_u, aes(x = Date, y = Inflation, color = "ADL-u"), size = 0.8) + 
  geom_line(data = train_inflation_adl_spread, aes(x = Date, y = Inflation, color = "ADL-spread"), size = 0.8) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  geom_hline(yintercept = 0) + 
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_y_continuous(breaks = c(-0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05))

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "Inflation")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = train_inflation_ar, aes(x = Date, y = Inflation, color = "AR"), size = 0.9) + 
  geom_line(data = train_inflation_ao, aes(x = Date, y = Inflation, color = "AO"), size = 0.9) + 
  geom_line(data = train_inflation_nnar, aes(x = Date, y = Inflation, color = "NNAR"), size = 0.9) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  geom_hline(yintercept = 0) + 
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_y_continuous(breaks = c(-0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05))

ggplot(train_inflation, aes(x = Date, y = Inflation, color = "Inflation")) + geom_line(size = 0.6, linetype = "dotdash") + 
  geom_line(data = train_inflation_gbm, aes(x = Date, y = Inflation, color = "GBM"), size = 0.6) +
  geom_line(data = train_inflation_ridge, aes(x = Date, y = Inflation, color = "RIDGE"), size = 0.6) +
  # geom_line(data = train_inflation_nnadl, aes(x = Date, y = Inflation, color = "NNADL"), size = 0.6) + 
  geom_line(data = train_inflation_lasso, aes(x = Date, y = Inflation, color = "LASSO"), size = 0.6) + 
  geom_hline(yintercept = 0) + 
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") + 
  theme(text = element_text(family = "Crimson", size = 16)) +
  scale_color_manual(values = colors) + scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = c(-0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06))

# Volatilität des quadratischen Losses 

# Validation-Set

# Entfernen der Rezessionen

pre1 <- as_tibble(LOSS_MATRIX_TRAIN1)
pre2 <- seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter")
extract1 <- cbind(pre1, Date = pre2)

b1 <- as.numeric(rownames(subset(extract1, Date == "2001-04-01")))
e1 <- as.numeric(rownames(subset(extract1, Date == "2001-10-01")))

b2 <- as.numeric(rownames(subset(extract1, Date == "2008-01-01")))
e2 <- as.numeric(rownames(subset(extract1, Date == "2009-07-01")))

begin <- c("2001-04-01", "2008-01-01")
end <- c("2001-11-01", "2009-06-01")

row_indices <- c(1:(b1-1), (e1+1):(b2-1), (e2 +1):70)

obj <- mget(ls(pattern = "LOSS_MATRIX_TRAIN"))
h <- 0
for (i in (1:(length(obj)))) {
  
  row_indices <- c(1:(b1-1-h), (e1+1-h):(b2-1-h), (e2 +1-h):(70-h))
  
  VOL <- list()
  k <- 1
  
  for (j in 1:ncol(obj[[1]])) {
    
    dat <- obj[[i]][, j]
    
    dat <- dat[row_indices]
    
    result <- round(sd(dat) * 1000, digits = 3)
    
    VOL[[k]] <- result
    k <- k + 1
    
  }
  
  names(VOL) <- model_names
  
  temp <- as_tibble(VOL)    
  
  temp <- as.matrix(temp)
  
  assign(paste0("VOL", "_TRAIN", i), temp)
  h <- h + 1
  
}

# Test-Set

obj <- mget(ls(pattern = "LOSS_MATRIX_TEST"))

for (i in (1:(length(obj)))) {
  
  VOL <- list()
  k <- 1
  
  for (j in 1:ncol(obj[[1]])) {
    
    dat <- obj[[i]][, j]
    
    result <- round(sd(dat) * 1000, digits = 3)
    
    VOL[[k]] <- result
    k <- k + 1
    
  }
  
  
  names(VOL) <- model_names
  
  temp <- as_tibble(VOL)    
  
  temp <- as.matrix(temp)
  
  assign(paste0("VOL", "_TEST", i), temp)
  
}

# Vergleich der Inflationsprognosen zwischen den Modellen (Real-Time-Inflation gegenüber revidierter Inflation)

train_inflation <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = real_inflation[[1]])
train_recent <- tibble(Date = seq(as.Date("1996-04-01"), as.Date("2013-07-01"), by = "quarter"), Inflation = INFL_COMP[[length(INFL_COMP)]][146:215])

ggplot(train_inflation, aes(x = Date, y = Inflation)) + geom_line(size = 0.6, linetype = "dotdash", color = "black") + 
  geom_line(data = train_recent, aes(x = Date, y = Inflation), size = 0.8, color = "grey") +
  geom_rect(data = rec_dates_train, aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), alpha = 0.25, fill= "grey80", inherit.aes = FALSE) +
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz", caption = "Anmerkung: In grau hinterlegt sind Rezessionen der Jahre 2001 und 2008/2009 gemäß NBER-Richtlinien.") 

test_inflation <- tibble(Date = seq(as.Date("2013-10-01"), as.Date("2020-01-01"), by = "quarter"), Inflation = real_inflation_test[[1]])
test_recent <- tibble(Date = seq(as.Date("2013-10-01"), as.Date("2020-01-01"), by = "quarter"), Inflation = INFL_COMP[[length(INFL_COMP)]][216:nrow(INFL_COMP)])

ggplot(test_inflation, aes(x = Date, y = Inflation)) + geom_line(size = 0.6, linetype = "dotdash", color = "black") + 
  geom_line(data = test_recent, aes(x = Date, y = Inflation), size = 0.8, color = "grey") +
  theme_classic() + labs(x = "Jahr", y = "Annualisierte Quartalsinflation", color = "Modellreferenz")

# Korrelation zwischen Vorhersage und Zielwert

# Validation-Set Korrelation

train <- mget(ls(pattern = "inflation_"))
train <- train[-grep("train", names(train))]
train <- train[-grep("test", names(train))]
train <- train[-grep("bench", names(train))]

for (j in (1:(length(train) - 1))) {
  
  temp <- list()
  z <- 1
  
  for (i in 1:length(train)) {
    
    cbd <- train[[i]][[j]]
    temp[[z]] <- cbd
    z <- z + 1
    
  }
  
  names(temp) <- model_names[1:length(train)]
  
  temp$actual <- real_inflation[[j]]
  
  temp <- as_tibble(temp)    
  
  temp <- as.matrix(temp)
  
  temp <- cor(temp)
  
  assign(paste0("cor_train", j), temp)
  
}

train_cor <- mget(ls(pattern = "cor_train"))

cors <- matrix(nrow = 8, ncol = 10)
k <- 1
for (j in 1:length(train_cor)) {
  
  dat <- train_cor[[j]]
  
  row <- dat[nrow(dat), ]
  cors[k, ] <- row
  k <- k + 1
}

cors <- cors[, -10]
colnames(cors) <- model_names
cors <- t(cors)

# Test-Set Korrelation

test <- mget(ls(pattern = "inflation_"))
test <- test[-grep("train", names(test))]
test <- test[grep("test", names(test))]
test <- test[-grep("bench", names(test))]
test <- test[-grep("test_inflation", names(test))]
test <- test[-grep("real", names(test))]

for (j in (1:(length(test) - 1))) {
  
  temp <- list()
  z <- 1
  
  for (i in 1:length(test)) {
    
    cbd <- test[[i]][[j]]
    temp[[z]] <- cbd
    z <- z + 1
    
  }
  
  names(temp) <- model_names[1:length(test)]
  
  temp$actual <- real_inflation_test[[j]]
  
  temp <- as_tibble(temp)    
  
  temp <- as.matrix(temp)
  
  temp <- cor(temp)
  
  assign(paste0("cor_test", j), temp)
  
}

test_cor <- mget(ls(pattern = regex("cor_test[1-9]{+}")))

cors2 <- matrix(nrow = 8, ncol = 10)
k <- 1
for (j in 1:length(test_cor)) {
  
  dat <- test_cor[[j]]
  
  row <- dat[nrow(dat), ]
  cors2[k, ] <- row
  k <- k + 1
}

cors2 <- cors2[, -10]
colnames(cors2) <- model_names
cors2 <- t(cors2)

################### Ende des R Skripts ###################