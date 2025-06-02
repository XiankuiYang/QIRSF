# QIRSF: Quick Input-Response Space-Filling Shiny App

This R package provides a Shiny application for generating and visualizing Pareto fronts of Input–Response Space-Filling (IRSF) designs using clustering-based methods. The app is designed to help users construct efficient experimental designs that balance space-filling in both the input and response spaces.

📍 **Website Version**: [https://xiankuiyangstatistics.shinyapps.io/IRSF_clustering_method/](https://xiankuiyangstatistics.shinyapps.io/IRSF_clustering_method/)

---

## 🚀 Features

- Generate Pareto fronts for IRSF designs
- Visualize contour plots, 3D surfaces, and diagnostic plots
- Download design tables interactively
- Includes built-in candidate design scenarios

---

## 📦 Installation

To install the package from GitHub:

```r
# Install the remotes package if not already installed
install.packages("remotes")

# Install the QIRSF package from GitHub
remotes::install_github("XiankuiYang/QIRSF")

# After installation, load the package and launch the app:
library(QIRSF)
run_app()

This will open the IRSF clustering method app in your default web browser.



