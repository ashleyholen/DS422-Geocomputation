# 🌿 NDVI Analysis Project Worksheet  

This worksheet will guide you through the process of designing and carrying out an NDVI-based analysis in R. Fill in the prompts for your chosen research question, location, time period, data sources, and methods.  

---

## 1. Framing the Question  

**What environmental or social issue will you explore?**  
Throughout this project, we will be looking at the forest health and rapid invasive grass spread around Kealakekua Mountain Reserve. 

---

**Why is NDVI an appropriate tool for this question?**  
NDVI will help us detect areas of regrowth of depleation over time in this area. 

---

**Who might find your results meaningful or useful?**  
These results will be meaningful for Hiki Ola/KMR crews, DLNR, local schools, community partners, and fire/fuel managers.

---

## 2. Choosing a Place and Time  

**What geographic area will you focus on?**  
Kealakekua Mountain Reserve, uka of Kona Big Island, HI. 

---

**What time frame makes sense for your question?**  
(e.g., single date, multiple years, seasonal patterns)  
We will look at wet/dry season across the beginning of the project in 2019 to present (2025). 

---

**How will you define the scope of your analysis?**  
Compare fenced restoration plots to the un-fenced areas. 

---

## 3. Finding Data  

**Where could you get satellite imagery or NDVI data?**  
NDVI data of Kona 2019-2025 from Copernicus Browser Sentinel-2 L1C. One wet season and one dry season date from each year. 

---

**What resolution and frequency are appropriate?**  
Low cloud coverage - 2 days from each year 2019-2025.

---

**Will you download data manually or use an R package? Which one?**  
Download manually off of Copernicus Browser.

---

## 4. Bringing Data into R  

**What R packages can help you work with spatial data?**  
library(here)
library(terra)
library(sf)
library(mapview)
library(dplyr)
library(ggplot2)
library(leaflet)
library(ggspatial)

---

**How will you handle projections, boundaries, or missing data?**  
Take out areas with too many clouds, set the boundary to just the reserve and nearby areas, using coordinate reference system EPSG:32605

---

**What file formats will you be working with?**  
.jp2, .tif, .shp, .geojson files

---

## 5. Calculating NDVI  

**What is the NDVI formula?**  
NDVI = (NIR - Red) / (NIR + Red)

---

**Which spectral bands are needed?**  
B4 and B8

---

**How will you apply this formula in R?**  
red <- rast(red_path)
nir <- rast(nir_path) 
ndvi <- (nir - red) / (nir + red)
plot(ndvi, main = "NDVI")

---

## 6. Exploring and Visualizing Data  

**How will you summarize NDVI values?**  
(maps, plots, tables)  
Creating ggplot visuals, leaflet map, table comparing the years

---

**Will you compare locations, before and after events, look at seasonal patterns, or study long-term trends?**  
We will look at one wet season date and one dry season date from each year to compare growth over time in the reserve versus outside.

---

**How will you make your visualizations clear and interpretable?**  
Using basic plots and tables, highlighting changes that are easy to see, focusing on a small area and color coded. 

---

## 7. Interpreting Results  

**What patterns or relationships do you expect to see?**  
We expect to see the reserve area with a higher NDVI than the non-preserved areas. Also, the wet season having a higher NDVI than the dry season. 

---

**How do they relate to your research question?**  
This will help track growth and restoration over time while comparing to nearby areas that are not preserved.  

---

**What uncertainties or data limitations should you acknowledge?**  
Cloud coverage and the fact that we are only grabbing one day per season per year. 

---

## 8. Reflecting on Impact  


**Could your results inform decisions, policies, or further research?**  
Potentially could prove the need for more reservations similar to Kealakekua. This could be data to show government officials for additional support. 

---

**What new questions emerge from your findings?**  
How can reservation programs be implemented in different locations? Does the rainfall in this area significantly effect the restorative process?

