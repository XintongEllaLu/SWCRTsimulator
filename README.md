# SWCRTsimulator: A Sample Size Calculator for Stepped Wedge CRTs

[![Shiny App](https://img.shields.io/badge/Shiny-App-blue)](https://lucyella.shinyapps.io/SWCRTsimulator/)

## Overview

**SWCRTsimulator** is an interactive web-based tool designed to facilitate the sample size determination for **Stepped Wedge Cluster Randomized Trials (SWCRTs)** with time-to-event outcomes. This simulator allows researchers to explore different design parameters, visualize trial structures, and assess statistical power through Monte Carlo simulations.

The application is built using **R Shiny** and provides a user-friendly interface for researchers and practitioners in biostatistics and clinical trials.

## Features

- **Simulates time-to-event outcomes** in stepped wedge designs
- **Allows customization** of key design parameters:
  - Number of clusters
  - Switch intervals
  - Follow-up duration
  - Event rates and treatment effects
- **Provides real-time visualization** of study design
- **Generates statistical power estimates** for different sample size scenarios
- **Flexible model assumptions**, including proportional hazards

## Getting Started

### 📌 Access the App

You can use the SWCRTsimulator directly online:

🔗 **[Launch the App](https://lucyella.shinyapps.io/SWCRTsimulator/)**  

No installation required—just open the link and start exploring.

### 💻 Run Locally

To run the simulator on your own machine, follow these steps:
Install R (if not already installed) from [CRAN](https://cran.r-project.org/).

#### ✅ Option 1: Clone the repository
```bash
git clone https://github.com/XintongEllaLu/SWCRTsimulator.git
cd SWCRTsimulator/RShiny_Apps
```
#### ✅ Option 2: Download manually  
1. Go to **[SWCRTsimulator app.R](https://github.com/XintongEllaLu/SWCRTsimulator/blob/main/Rshiny%20Apps/app.R)**  
2. Click **"Code" → "Download"**  

#### 🚀 Run the App  
After obtaining the files, install the required R packages and launch the app:

```r
# Install required R packages
install.packages(c("shiny", "ggplot2", "dplyr", "survival"))

# Run the Shiny app
library(shiny)
runApp("app.R")
