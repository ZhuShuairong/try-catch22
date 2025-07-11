# Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(naniar)

#Load the dataset
data <- read.csv("ori_house_data.csv")
View(data)
ncol(data)

#View headers pnly
head(data,0)

#Check missing values (quite alot)
sum(is.na(data))

#Changing headers into English
new_headers <- c(
  "x",
  "townshipAndCityDistrict",
  "transactionTarget",
  "landLocationAndBuildingNumber",
  "totalAreaOfLandTransferredInSquareMeters",
  "urbanLandUseZoning",
  "nonUrbanLandUseZoning",
  "nonUrbanLandUseClassification",
  "transactionDate",
  "numberOfBuildingsInTransaction",
  "transferLevel",
  "totalNumberOfFloors",
  "buildingType",
  "mainPurpose",
  "mainBuildingMaterials",
  "constructionCompletionDate",
  "totalAreaOfBuildingTransferredInSquareMeters",
  "currentBuildingLayoutRooms",
  "currentBuildingLayoutHalls",
  "currentBuildingLayoutBathrooms",
  "currentBuildingLayoutPartitions",
  "whetherThereIsAManagementOrganization",
  "totalPriceRMB",
  "unitPriceRMBSqM",
  "parkingSpaceType",
  "totalAreaOfParkingSpaceTransferredSqM",
  "totalPriceOfParkingSpaceRMB",
  "remarks",
  "number",
  "mainBuildingArea",
  "ancillaryBuildingArea",
  "balconyArea",
  "elevator",
  "transferNumber",
  "yearSeason",
  "totalAreaOfParkingSpaceTransferredSqM_2",
  "isPresale",
  "projectName",
  "buildingNumber",
  "terminationSituation"
)

colnames(data) <- new_headers
head(data,0)

create_column_summary <- function(df) {
  
  # Count unique values per column
  unique_count <- df %>%
    summarise(across(everything(), ~ n_distinct(.x, na.rm = FALSE))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "unique_values")
  
  # Count missing or empty values per column
  missing_count <- df %>%
    summarise(across(everything(), ~ sum(is.null(.x) | .x == "" | is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_or_empty")
  
  # Get data type per column
  class_info <- df %>%
    summarise(across(everything(), ~ class(.x)[1])) %>%
    pivot_longer(everything(), names_to = "column", values_to = "data_type")
  
  # Join all summaries
  column_summary <- unique_count %>%
    left_join(missing_count, by = "column") %>%
    left_join(class_info, by = "column")
  
  return(column_summary)
}

summary_df <- create_column_summary(data)
View(summary_df)

cleaned_data <- data %>%
  
  # Remove rows where townshipAndCityDistrict (column 2) is NA or empty
  filter(!is.na(townshipAndCityDistrict) & townshipAndCityDistrict != "") %>%
  
  # NEW STEP: Replace "有"/"True" → TRUE, "無"/"False" → FALSE in selected columns
  mutate(across(c(isPresale, elevator, whetherThereIsAManagementOrganization, currentBuildingLayoutPartitions),
                ~ case_when(
                  . %in% c("有", "True", "TRUE") ~ TRUE,
                  . %in% c("無", "False", "FALSE") ~ FALSE,
                  TRUE ~ NA
                ))) %>%
  
  # Calculate unitPriceRMBSqM as totalPriceRMB / totalAreaOfBuildingTransferredInSquareMeters
  mutate(
    unitPriceRMBSqM = ifelse(
      is.na(unitPriceRMBSqM) | unitPriceRMBSqM == "",
      round(totalPriceRMB / totalAreaOfBuildingTransferredInSquareMeters, 2),
      unitPriceRMBSqM
    )
  ) %>%
  
  # Columns 6–8: urbanLandUseZoning, nonUrbanLandUseZoning, nonUrbanLandUseClassification → "Not Applicable"
  mutate(across(c(urbanLandUseZoning, nonUrbanLandUseZoning, nonUrbanLandUseClassification),
                ~ ifelse(is.na(.) | . == "", "Not Applicable", .))) %>%
  
  # Columns 11–12 and 14–16: transferLevel, totalNumberOfFloors, mainPurpose, mainBuildingMaterials, constructionCompletionDate → "Unknown"
  mutate(across(c(transferLevel, totalNumberOfFloors, mainPurpose, mainBuildingMaterials, constructionCompletionDate),
                ~ ifelse(is.na(.) | . == "", "Unknown", .))) %>%
  
  # Column 25: parkingSpaceType → "None"
  mutate(parkingSpaceType = ifelse(is.na(parkingSpaceType) | parkingSpaceType == "", "None", parkingSpaceType)) %>%
  
  # Column 26: totalAreaOfParkingSpaceTransferredSqM → 0
  mutate(totalAreaOfParkingSpaceTransferredSqM = ifelse(is.na(totalAreaOfParkingSpaceTransferredSqM), 0, totalAreaOfParkingSpaceTransferredSqM)) %>%
  
  # Column 28: remarks → "None"
  mutate(remarks = ifelse(is.na(remarks) | remarks == "", "None", remarks)) %>%
  
  # Columns 30–32: mainBuildingArea, ancillaryBuildingArea, balconyArea → impute using totalAreaOfBuildingTransferredInSquareMeters
  mutate(
    mainBuildingArea = ifelse(is.na(mainBuildingArea) | mainBuildingArea == "",
                              totalAreaOfBuildingTransferredInSquareMeters * 0.7,
                              as.numeric(mainBuildingArea)),
    
    ancillaryBuildingArea = ifelse(is.na(ancillaryBuildingArea) | ancillaryBuildingArea == "",
                                   totalAreaOfBuildingTransferredInSquareMeters * 0.2,
                                   as.numeric(ancillaryBuildingArea)),
    
    balconyArea = ifelse(is.na(balconyArea) | balconyArea == "",
                         totalAreaOfBuildingTransferredInSquareMeters * 0.1,
                         as.numeric(balconyArea))
  ) %>%
  
  # Column 33: elevator → "None"
  mutate(elevator = ifelse(is.na(elevator), "None", elevator)) %>%
  
  # Column 34: transferNumber → 0
  mutate(transferNumber = ifelse(is.na(transferNumber), 0, transferNumber)) %>%
  
  # Columns 38–39: projectName, buildingNumber → "Unknown"
  mutate(across(c(projectName, buildingNumber),
                ~ ifelse(is.na(.) | . == "", "Unknown", .))) %>%
  
  # Column 40: terminationSituation → "Unterminated"
  mutate(terminationSituation = ifelse(is.na(terminationSituation) | terminationSituation == "", "Unterminated", terminationSituation))

# Optional: Summary after cleaning
summary_df <- create_column_summary(cleaned_data)
View(summary_df)

View(cleaned_data)

