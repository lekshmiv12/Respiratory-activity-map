# Respiratory-activity-map

## How to Update the Respiratory Activity Level Map on the Website

### Respiratory Activity Level Mapping

This repository contains R code and supporting datasets used to generate weekly respiratory activity level maps. The data sources include:

### Download

Download 'Respiratory activity map' folder into your working R directory - https://swdistricthealth.sharepoint.com/:f:/s/EnvironmentalandCommunityHealthServices/Ehi0Wktpk3JKt_KiJS34v78B_ZtQlngqwEpYLtBzmdKaVQ?e=xLbr7S 

### ESSENCE Data for:
- Influenza-like Illness (ILI)
- COVID-like Illness (CLI)
- Respiratory Syncytial Virus (RSV)  
  *(All based on percent of ED visits)*

### NBS Data for:
- Pertussis  
  *(Case counts)*

---

### Data Preparation Instructions

#### ESSENCE Data (ILI, CLI, RSV)
- Download weekly data from ESSENCE (based on percent of CC and DD category).
- Open the Excel file and **delete the first two rows** (descriptive metadata).
- Rename the column containing week information to **`date`**.
- Save the cleaned file in your working R project directory.

#### Pertussis Data (NBS)
- Manually download the weekly pertussis data from NBS.
- Rename the column **`Investigation Start Date`** to **`date`**.
- Save the file as **`data.xlsx`** in your working R project directory.
- Run the script **`Pertussis Data.R`** to process the file.
- The output will be saved as **`pertussislevels.xlsx`**, which is used in the mapping workflow.

---

### Data Frequency
- All datasets are structured as **weekly time series**.
