# validitas-reliabilitas-shiny
Interactive R Shiny dashboard for questionnaire item validity and reliability analysis.

# Dashboard Analisis Validitas & Reliabilitas Instrumen

## Overview

This project is an interactive dashboard built with R Shiny to support questionnaire instrument analysis. It enables users to perform item validity and internal consistency reliability assessments through a user-friendly interface.

## Features

- **Data Upload:** Import questionnaire data from CSV or Excel files.
- **Item Selection:** Select questionnaire items based on their column-name prefixes.
- **Item Mapping:** Assign indicators to their respective constructs or dimensions manually or using an uploaded mapping file.
- **Validity Analysis:** Evaluate item-total correlations using corrected item-total correlation (`r.drop`) and compare them against a critical correlation value (`r table`).
- **Reliability Analysis:** Calculate Cronbach's Alpha for each assigned construct.
- **Interactive Results:** View and filter analysis results in interactive tables.
- **Excel Export:** Download validity and reliability results in an Excel workbook.

## Tech Stack

- R
- Shiny
- psych
- readxl
- writexl
- DT
- shinyjs

## How to Run

1. Install R and RStudio.
2. Install the required packages:

   ```r
   install.packages(c(
     "shiny", "psych", "readxl",
     "writexl", "DT", "shinyjs"
   ))
   ```

3. Open `app.R` in RStudio.
4. Run the application using the **Run App** button or execute:

   ```r
   shiny::runApp()
   ```

5. Upload questionnaire data, assign items to constructs, set the significance level, and click **Analyze**.
6. Review the results and download the Excel output if needed.

## Output

The dashboard provides two main outputs:

- **Validity Results:** Item correlation, critical correlation value, and validity decision.
- **Reliability Results:** Number of items, number of respondents, Cronbach's Alpha, and reliability decision.

## Notes

Use appropriately prepared questionnaire data and verify that the selected columns contain valid numeric item responses. Analysis results should be interpreted in accordance with the research design and measurement methodology.

For demonstration purposes, use anonymized or synthetic data rather than confidential respondent information.
