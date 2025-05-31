# Sets working directory and configure downlaod timeout so that it has time to download
setwd("C:/Users/Eliott/Desktop/R directory")
options(timeout = 2000)

# Defines dataset URL and file paths
downloadLink <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
camelsZip <- "camels_basin_timeseries.zip"
camelsData <- "camels_data"

# Downloads and unzip the dataset if necessary
if (file.exists(camelsZip)) file.remove(camelsZip)
download.file(downloadLink, camelsZip, mode = "wb")
if (!dir.exists(camelsData) && file.exists(camelsZip)) unzip(camelsZip, exdir = camelsData)

# Lists and filters the model output files
allData <- list.files(camelsData, recursive = TRUE, full.names = TRUE)
requiredData <- grep("model_output", allData, value = TRUE, ignore.case = TRUE)

# Reads and stores the data from the required files
if (length(requiredData) == 0) stop("No model output files found in the dataset.")
outputData <- lapply(requiredData, function(file) read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE))

# Displays first few imported files to verify correct installation
cat("First files imported:\n")
print(head(names(outputData)))
