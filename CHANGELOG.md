CHANGELOG for SEG Shiny Heatmap (version 0.3.3)
================
Martin Frigaard
2020-04-17

# Welcome to the SEG Shiny app (version 0.3.3)

***This is not the code for the working app\!***

This page outlines the code used to develop the SEG application. The
working version is on the Diabetes Technology Society website
[here](https://www.diabetestechnology.org/seg/).

The preview version (not necessarily stable) is available [here]().

For questions, issues, or feature requests, please email Martin at
<support@quesgen.com>.

## Required packages

``` r
library(dplyr) # Data wrangling, glimpse(50) and tbl_df().
library(ggplot2) # Visualise data.
library(lubridate) # Dates and time.
library(readr) # Efficient reading of CSV data.
library(stringr) # String operations.
library(tibble) # Convert row names into a column.
library(tidyr) # Prepare a tidy dataset, gather().
library(magrittr) # Pipes %>%, %T>% and equals(), extract().
library(tidyverse) # all tidyverse packages
library(mosaic) # favstats and other summary functions
library(fs) # file management functions
library(shiny) # apps
library(datapasta) # for pasting tibbles
library(styler) # cleaner code
```

Document info:

  - **Created date:** 2020-04-17

  - **R version:** R version 3.6.3 (2020-02-29)

## Issues/bugs/fixes

The app was throwing some errors on the DTS site

<img src="image/2020-04-17-seg-error-dts.png" width="1860" />

This looks like a problem in the helpers file, but will investigate and
confirm.

There is also a problem with a SEG table summary, specifically, the
`segTable()` function and it’s interactions with DT.

``` r
source("App/helpers.R")
```

    #>  
    #>  Attaching package: 'colourpicker'

    #>  The following object is masked from 'package:shiny':
    #>  
    #>      runExample

    #>  Loading required package: plotly

    #>  
    #>  Attaching package: 'plotly'

    #>  The following object is masked from 'package:mosaic':
    #>  
    #>      do

    #>  The following object is masked from 'package:ggplot2':
    #>  
    #>      last_plot

    #>  The following object is masked from 'package:stats':
    #>  
    #>      filter

    #>  The following object is masked from 'package:graphics':
    #>  
    #>      layout

    #>  Loading required package: viridis

    #>  Loading required package: viridisLite

    #>  Registered S3 method overwritten by 'seriation':
    #>    method         from 
    #>    reorder.hclust gclus

    #>  
    #>  ======================
    #>  Welcome to heatmaply version 1.1.0
    #>  
    #>  Type citation('heatmaply') for how to cite the package.
    #>  Type ?heatmaply for the main documentation.
    #>  
    #>  The github page is: https://github.com/talgalili/heatmaply/
    #>  Please submit your suggestions and bug-reports at: https://github.com/talgalili/heatmaply/issues
    #>  Or contact: <tal.galili@gmail.com>
    #>  ======================

    #>  
    #>  Attaching package: 'data.table'

    #>  The following object is masked from 'package:purrr':
    #>  
    #>      transpose

    #>  The following objects are masked from 'package:lubridate':
    #>  
    #>      hour, isoweek, mday, minute, month, quarter, second, wday, week,
    #>      yday, year

    #>  The following objects are masked from 'package:dplyr':
    #>  
    #>      between, first, last

    #>  Welcome to clipr. See ?write_clip for advisories on writing to the clipboard in R.

    #>  Parsed with column specification:
    #>  cols(
    #>    BGM = col_double(),
    #>    REF = col_double()
    #>  )

    #>  Parsed with column specification:
    #>  cols(
    #>    RiskPairID = col_double(),
    #>    REF = col_double(),
    #>    BGM = col_double(),
    #>    RiskFactor = col_double(),
    #>    abs_risk = col_double()
    #>  )

    #>  Parsed with column specification:
    #>  cols(
    #>    risk_cat = col_double(),
    #>    ABSLB = col_double(),
    #>    ABSUB = col_double()
    #>  )

    #>  Parsed with column specification:
    #>  cols(
    #>    Ref = col_double(),
    #>    UB = col_double(),
    #>    LB = col_double()
    #>  )

``` r
segTable(dat = base::paste0(github_data_root, 
                            "VanderbiltComplete.csv")) %>% 
  # get data grouped/tallied by risk_cat
        dplyr::count(risk_cat, sort = TRUE) %>%
  # join to lkpSEGRiskCat4 table
        dplyr::full_join(x = .,
                         y = lkpSEGRiskCat4, 
                         by = "risk_cat") %>%
  # create new risk_cat
            dplyr::mutate(
            risk_cat = base::as.numeric(risk_cat),
            # and percent
            Percent = base::paste0(base::round(n / nrow(VandComp) * 100,
                                               digits = 1),
                                   if_else(condition = is.na(n),
                                          true = "", false = "%"))) %>%
  # sort the n column
          dplyr::arrange(desc(n)) %>%
  # rename everything prettier names
          dplyr::select(
                `SEG Risk Level` = risk_cat, # changed to level
                `SEG Risk Category` = risk_cat_txt,
                `Number of Pairs` = n,
                Percent) %>%
 # pass to color formatting
                DT::datatable(., options = list(lengthChange = FALSE,
                                         dom = 't',
                                         rownames = FALSE )) %>%
                # select numerical reference
                DT::formatStyle('SEG Risk Level',
                      target = "row",
                      backgroundColor = DT::styleEqual(
                          levels =  # eight levels/labels
                                c(0, 1, 2, 3,
                                    4, 5, 6, 7),
                            values = c("#00EE00", "#ADFF2F", "#FFFF00",
                                       "#FFD700", "#FFA500","#EE7600",
                                       "#FF4500", "#FF0000")))
```

    #>  Parsed with column specification:
    #>  cols(
    #>    BGM = col_double(),
    #>    REF = col_double()
    #>  )

    #>  PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<div id="htmlwidget-e20b76b4f8195feaf48f" class="datatables html-widget" style="width:100%;height:auto;">

</div>

<script type="application/json" data-for="htmlwidget-e20b76b4f8195feaf48f">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8"],[0,1,2,3,4,5,6,7],["None","Slight, Lower","Slight, Higher","Moderate, Lower","Moderate, Higher","Severe, Lower","Severe, Upper","Extreme"],[9474,294,55,24,11,10,null,null],["95.8%","3%","0.6%","0.2%","0.1%","0.1%","NA","NA"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>SEG Risk Level<\/th>\n      <th>SEG Risk Category<\/th>\n      <th>Number of Pairs<\/th>\n      <th>Percent<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"lengthChange":false,"dom":"t","rownames":false,"columnDefs":[{"className":"dt-right","targets":[1,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nvar value=data[1]; $(row).css({'background-color':value == 0 ? \"#00EE00\" : value == 1 ? \"#ADFF2F\" : value == 2 ? \"#FFFF00\" : value == 3 ? \"#FFD700\" : value == 4 ? \"#FFA500\" : value == 5 ? \"#EE7600\" : value == 6 ? \"#FF4500\" : value == 7 ? \"#FF0000\" : value});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script>

<!--/html_preserve-->

This was fixed with a new column name for `SEG Risk Level`.

## Error with SEG graph

The SEG tab was throwing this error:

`transformation for secondary axes must be a function`

And this was debugged below. In order to build the plot with the
Gaussian smoothed background, a new image `seg_gaussian_layer_shiny`
needed to be built in the `helpers.R` file

``` r
# 1.0 - DEFINE heatmap inputs ============= 
# 1.0.1 read seg600.png with readPNG ----- -----
library(jpeg)
library(png)
# load the seg600.png file
download.file(url = "https://raw.githubusercontent.com/mjfrigaard/seg-shiny-data/master/Image/seg600.png", 
              destfile = "Image/seg600.png")
BackgroundSmooth <- png::readPNG("Image/seg600.png")
# 1.0.2 mmol conversion factor ---- -----
mmolConvFactor <- 18.01806
# 1.0.3 rgb2hex function ---- -----
# This is the RGB to Hex number function for R
rgb2hex <- function(r, g, b) rgb(r, g, b, maxColorValue = 255)
# 1.0.4 risk factor colors ----  -----
# These are the values for the colors in the heatmap.
abs_risk_0.0000_color <- rgb2hex(0, 165, 0)
# abs_risk_0.0000_color
abs_risk_0.4375_color <- rgb2hex(0, 255, 0)
# abs_risk_0.4375_color
abs_risk_1.0625_color <- rgb2hex(255, 255, 0)
# abs_risk_1.0625_color
abs_risk_2.7500_color <- rgb2hex(255, 0, 0)
# abs_risk_2.7500_color
abs_risk_4.0000_color <- rgb2hex(128, 0, 0)
# abs_risk_4.0000_color
riskfactor_colors <- c(
  abs_risk_0.0000_color,
  abs_risk_0.4375_color,
  abs_risk_1.0625_color,
  abs_risk_2.7500_color,
  abs_risk_4.0000_color
)
# 1.0.5 create base_data data frame ---- ----- 
base_data <- data.frame(
  x_coordinate = 0,
  y_coordinate = 0,
  color_gradient = c(0:4)
)
# These are the data objects used to define various components in the graph. 

# First create the base layer, but add the axes formats and adjust the point 
# to be almost non-existent. 
# - create base_layer plot ---- 
base_layer <- base_data %>% 
  ggplot(aes(
      x = x_coordinate,
      y = y_coordinate,
      fill = color_gradient)) +
  geom_point(size = 0.00000001,
             color = "white")
# scales_layer ------
scales_layer <- base_layer + 
  ggplot2::scale_y_continuous(
    limits = c(0, 600),
    sec.axis =
      sec_axis(~. / mmolConvFactor,
        name = "Measured blood glucose (mmol/L)"
      ),
    name = "Measured blood glucose (mg/dL)"
  ) +
  scale_x_continuous(
    limits = c(0, 600),
    sec.axis =
      sec_axis(~. / mmolConvFactor,
        name = "Reference blood glucose (mmol/L)"
      ),
    name = "Reference blood glucose (mg/dL)"
  )

# Now that the axes are set, I can add the smoothed image and create a 
# gaussian_layer
# the gaussian_layer ---- 
gaussian_layer <- scales_layer +
  ggplot2::annotation_custom(
    grid::rasterGrob(image = BackgroundSmooth, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                               xmin = 0, 
                               xmax =  600, 
                               ymin = 0, 
                               ymax =  600) 


# In the next layer, I'll add the color gradient scaling and values, and also
# the custom labels for each level. 
# - the seg_gaussian_layer_shiny ---- 
seg_gaussian_layer_shiny <- gaussian_layer + 
    ggplot2::scale_fill_gradientn( # scale_*_gradientn creats a n-color gradient
    values = scales::rescale(c(
      0, # darkgreen
      0.4375, # green
      1.0625, # yellow
      2.75, # red
      4.0 # brown
    )), 
    limits = c(0, 4),
    colors = riskfactor_colors,
    guide = guide_colorbar(
      ticks = FALSE,
      barheight = unit(100, "mm")
    ),
    breaks = c(
      0.25,
      1,
      2,
      3,
      3.75
    ),
    labels = c(
      "none",
      "slight",
      "moderate",
      "high",
      "extreme"
    ),
    name = "risk level")
```

In the final layer, I’ll add the sample data to the
`seg_gaussian_layer_shiny` plot.

``` r
seg_gaussian_layer_shiny
```

![](image/print-seg_gaussian_layer_shiny-1.png)<!-- --> Now we can test
this with the `SampMeasData` file.

``` r
SampMeasData <- VandComp %>% 
  dplyr::sample_frac(tbl = ., size = 0.10)
heatmap_plot <- seg_gaussian_layer_shiny +
  geom_point(
    data = SampMeasData, # introduce sample data frame
    aes(
      x = REF,
      y = BGM
    ),
    shape = 21,
    fill = "white",
    size = 1.1,
    stroke = 0.4,
    alpha = 0.8
  )
heatmap_plot
```

![](image/heatmap_plot-1.png)<!-- -->

Just to make sure this works, I will remove this object and build the
SEG plot like I would in the Shiny application.

``` r
# remove seg_gaussian_layer_shiny
rm(seg_gaussian_layer_shiny)
# import rds file
seg_gaussian_layer_shiny <- readr::read_rds("App/seg_gaussian_layer_shiny.rds")
```

``` r
seg_gaussian_layer_shiny
```

![](image/check-seg_gaussian_layer_shiny-1.png)<!-- -->

Now I add the sample data to the gaussian layer.

``` r
# 6.6 - introduce sample data frame -----
heat_map_2.0 <- seg_gaussian_layer_shiny + 
  geom_point(
    data = SampMeasData, 
    aes(
      x = REF,
      y = BGM
    ),
    shape = 21,
    fill = "white",
    size = 1.1,
    stroke = 0.4,
    alpha = 0.8
  )

# final plot -------
heat_map_2.0
```

![](image/add-sample-data-to-seg_gaussian_layer_shiny-1.png)<!-- -->

Remove `seg600.png`

``` r
unlink("seg600.png")
```
