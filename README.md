# PharmaPortfolio
An RShiny app to analyse and compare investment portfolios for a fictional pharmaceutical company.

# README for Portfolio Comparison App [generated with ChatGPT, and checked by me - a human!]

## Overview
This Shiny web application allows users to compare two different investment portfolios by selecting and deselecting specific projects. The app provides a detailed analysis of various key metrics including financial inputs and outputs, freed-up resources (money and FTE), as well as qualitative factors such as efficacy, safety, feasibility, and competition levels. It is particularly useful for decision-making in pharmaceutical investments, helping users visualize and evaluate potential portfolio configurations.

---

## Key Features

- **Project Selection**: Users can select and compare two sets of projects for two different portfolio options.
- **Portfolio Metrics**: The app calculates and visualizes metrics such as:
  - Probability of Launch (PoL)
  - Freed-up costs and FTEs when projects are dropped
  - Total investment input and output in MCHF (millions of Swiss Francs)
  - Average scores for efficacy, safety, feasibility, and competition
  - Financial metrics like total profit, expected profit, and global peak sales

---

## Libraries Used
The following libraries are loaded to support the functionality of the app:

- `shiny`: Core library for building interactive web applications in R.
- `shinydashboard`: Provides a layout framework to structure the app into a dashboard format.
- `dplyr`: Data manipulation package for filtering and transforming datasets.
- `reshape2`: Used for reshaping data between wide and long formats for easier plotting.
- `ggplot2`: Powerful visualization package used to create various plots.
- `plotly`: Adds interactivity to `ggplot2` plots.
- `shinyWidgets`: Enhances the UI with advanced widgets and inputs.
- `gridExtra`: Provides grid functions to arrange multiple plots.

---

## Dataset
The data for the portfolio analysis comes from a CSV file called **`caseStudy_forDashboard_Public.csv`**, which contains information about various pharmaceutical projects including columns for:

- **Indication**: The name of the pharmaceutical project.
- **PoL**: Probability of Launch for each project.
- **MCHF_total**: Total investment in millions of Swiss Francs.
- **MY_total**: Total full-time equivalent (FTE) resources.
- **Value**: Peak sales value for each project.
- **Profit**: Calculated profit for each project.
- **Expected profit**: The expected profit considering the PoL.
- **Efficacy**, **Safety**, **Operational Feasibility**, **Level of Competition**: Qualitative measures scored for each project.

---

## How the App Works

### UI (User Interface)
The app layout is created using a **fluidPage** structure and includes:
1. **Checkbox Inputs**: Users can select which projects to include in two different portfolio options (Option 1 and Option 2) using checkboxes. These selections influence all subsequent calculations and visualizations.
2. **Plots and Visualizations**: The UI displays several plots, including:
   - **PoL Plot**: A bar chart comparing the Probability of Launch (PoL) for the entire portfolio and both selected options.
   - **Freed-up Costs and FTE**: Two separate plots show the freed-up monetary and human resources if certain projects are dropped.
   - **Financial Inputs and Outputs**: Plots showing total input and output values (MCHF) for each option.
   - **Qualitative Scores**: Visualizations of the average efficacy, safety, feasibility, and competition for the selected projects.

### Server Logic
The server function handles all the reactive elements of the app:
1. **Data Filtering**: Based on the user's project selections, the app filters the dataset to calculate metrics for the selected projects in Option 1 and Option 2.
2. **Plot Generation**: Several plots are dynamically generated, updating in real-time as the user changes their selections. Plots include:
   - **Bar plots** for PoL and freed-up resources.
   - **Facet plots** to compare inputs, outputs, and qualitative scores for different portfolio options.
3. **Summary Calculations**: The app computes key metrics such as total MCHF, FTE, value, and profit for the entire portfolio and both options.

---

## How to Run the App

1. **Install Required Packages**: Ensure that you have the required R packages installed. You can install them using the following command:

   ```R
   install.packages(c("shiny", "shinydashboard", "dplyr", "reshape2", "ggplot2", "plotly", "shinyWidgets", "gridExtra"))
   ```

2. **Run the App**: After installing the packages, run the app using the following R code:

   ```R
   library(shiny)
   runApp('path_to_your_app_directory')
   ```

3. **Upload the Data**: Make sure that the `caseStudy_forDashboard_Public.csv` file is placed in the working directory or the specified path, as the app loads this data at startup.

---

## Future Enhancements
Potential improvements could include:
- Enhanced interactivity using `plotly` to enable hover information on the plots.
- Additional financial models or metrics for a more detailed analysis of portfolio performance.
- Ability to upload custom datasets.

---

This app serves as a powerful tool for comparing different portfolio configurations in pharmaceutical investments, helping users optimize resource allocation and maximize potential outcomes.

