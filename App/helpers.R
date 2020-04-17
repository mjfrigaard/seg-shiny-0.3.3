# =====================================================================#
# This is the helper code to create: data inputs, mmol constant, colors
# plot canvas data, and a function for cleaning/preparing the uploaded
# data files to the shiny app.
# Authored by and feedback to mjfrigaard@gmail.com
# MIT License
# Version: 0.1.3.3 ----
# =====================================================================#

options(shiny.maxRequestSize = 30 * 1024^2)
library(colourpicker)
library(shinythemes)
library(tidyverse)
library(styler)
library(heatmaply)
library(tools) # for tools
library(data.table) # for fread() function
library(datapasta)
library(clipr)
require(dplyr) # Data wrangling, glimpse(50) and tbl_df().
require(ggplot2) # Visualise data.
require(lubridate) # Dates and time.
require(readr) # Efficient reading of CSV data.
require(stringr) # String operations.
require(tibble) # Convert row names into a column.
require(tidyr) # Prepare a tidy dataset, gather().
require(magrittr) # Pipes %>%, %T>% and equals(), extract().
require(mosaic) # favstats and other summary functions
require(fs) # file management functions
require(shiny) # apps
require(datapasta) # for pasting tibbles

# 0.0.0 - DEFINE data inputs ============= ----


# 0.0.1 - define github data repo ---- ---- 
github_data_root <- "https://raw.githubusercontent.com/mjfrigaard/seg-shiny-data/master/Data/"

# 0.0.2 - VanderbiltComplete file ---- ---- 
full_sample_repo <- base::paste0(github_data_root, 
                                 "VanderbiltComplete.csv")

# 0.0.3 - the AppRiskPairData file  ---- ---- 
app_riskpair_repo <- base::paste0(github_data_root, 
                                  "AppRiskPairData.csv")
# app_riskpair_repo
# 0.0.4 - the AppLookUpRiskCat file  ---- ---- 
app_lookup_repo <- base::paste0(github_data_root, 
                                "AppLookUpRiskCat.csv")

# 0.0.5 - the APPSEGBlandAltmanRefVals file  ---- ---- 
ba_ref_repo <- base::paste0(github_data_root, 
                           "APPSEGBlandAltmanRefVals.csv")

# # 0.1.0 - IMPORT DATA ============= ----

# this is not needed for the app, but loaded for the README.Rmd
VandComp <- readr::read_csv(full_sample_repo )

# 0.1.1 - load RiskPairData ----
RiskPairData <- readr::read_csv(file = app_riskpair_repo)

# 0.1.2 - LookUpRiskCat ----
LookUpRiskCat <- readr::read_csv(file = app_lookup_repo)

# 0.1.3 - SEGBlandAltmanRefVals -----
SEGBlandAltmanRefVals <- readr::read_csv(file = ba_ref_repo)

# 0.1.4 - define risk grade look-up table lkpRiskGrade  ---- ---- ----
# for RiskGradeTable3
lkpRiskGrade <- tibble::tribble(
~`risk_grade_id`, ~`risk_grade`, ~`REF`,
1, "A", "0 - 0.5",
2, "B", "> 0.5 - 1.0",
3, "C", "> 1.0 - 2.0",
4, "D", "> 2.0 - 3.0",
5, "E", "> 3.0")
lkpRiskGrade

# 0.1.5 - define lkpSEGRiskCat4 ---- ---- ---- 
lkpSEGRiskCat4 <- tibble::tribble(
    ~risk_cat,      ~risk_cat_txt, ~ABSLB, ~ABSUB,
    0L,             "None", -0.001,    0.5,
    1L,    "Slight, Lower",    0.5,      1,
    2L,   "Slight, Higher",      1,    1.5,
    3L,  "Moderate, Lower",    1.5,      2,
    4L, "Moderate, Higher",      2,    2.5,
    5L,    "Severe, Lower",    2.5,      3,
    6L,    "Severe, Upper",      3,    3.5,
    7L,          "Extreme",      3,   1000
)

# lkpSEGRiskCat4$risk_cat <- as.numeric(lkpSEGRiskCat4$risk_cat)

# 0.1.6 - define lkpISORanges look-up table for ISORangeTable5  ---- ---- ----
lkpISORanges <- tibble::tribble(
       ~ID,                ~iso_range,
        1L,    "<= 5% or 5 mg/dL",
        2L,  "> 5 - 10% or mg/dL",
        3L, "> 10 - 15% or mg/dL",
        4L,    "> 15 - 20% mg/dL",
        5L,   "> 20% or 20 mg/dL")
# lkpISORanges

# 1.0 - DEFINE heatmap inputs ============= ----
library(jpeg)
library(png)

# 1.0.0 - create base_data data frame ---- ----- 
base_data <- data.frame(
  x_coordinate = 0,
  y_coordinate = 0,
  color_gradient = c(0:4)
)

# 1.0.1 read seg600.png with readPNG ----- -----
# load the seg600.png file
download.file(url = "https://raw.githubusercontent.com/mjfrigaard/seg-shiny-data/master/Image/seg600.png", 
              destfile = "seg600.png")
BackgroundSmooth <- png::readPNG("seg600.png")
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

# 1.0.5 - create scales_layer plot ---- 
base_layer <- base_data %>% 
  ggplot(aes(
    x = x_coordinate,
    y = y_coordinate,
    fill = color_gradient)) +
  geom_point(size = 0.00000001,
             color = "white")
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
# scales_layer
# 1.0.6 - add gaussian_layer ---- 
gaussian_layer <- scales_layer +
  ggplot2::annotation_custom(
    grid::rasterGrob(image = BackgroundSmooth, 
                     width = unit(1,"npc"), 
                     height = unit(1,"npc")), 
    xmin = 0, 
    xmax =  600, 
    ymin = 0, 
    ymax =  600) 
# gaussian_layer

# 1.0.7 - create seg_gaussian_layer_shiny ----
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

# 2.0 - DEFINE pairtypeTable function ============= ----
pairtypeTable <- function(dat) {
  # 2.1.1 - import data frame/define SampMeasData ----
  SampMeasData <- readr::read_csv(dat)
  # 2.1.2 - convert the columns to numeric  ----
  SampMeasData <- SampMeasData %>%
    dplyr::mutate(BGM = as.double(BGM),
                  REF = as.double(REF)) %>%
    # 2.2 create bgm_pair_cat ---- ----  ----
  dplyr::mutate(
    bgm_pair_cat =
      dplyr::case_when(
        BGM < REF ~ "BGM < REF",
        BGM == REF ~ "BGM = REF",
        BGM > REF ~ "BGM > REF"
      )
  ) %>%
    # 2.3 create excluded ---- ----  ----
  dplyr::mutate(
    excluded =
      dplyr::case_when(
        REF > 600 ~ "REF > 600: Excluded from SEG Analysis",
        TRUE ~ NA_character_
      )
  ) %>%
    # 2.4 create included ---- ----  ----
  dplyr::mutate(
    included =
      dplyr::case_when(
        REF <= 600 ~ "Total included in SEG Analysis",
        REF > 600 ~ "Total excluded in SEG Analysis"
      )
  )
  # 2.5 create BGMPairs ---- ---- ----
  BGMPairs <- SampMeasData %>%
    dplyr::count(bgm_pair_cat) %>%
    dplyr::rename(
      `Pair Type` = bgm_pair_cat,
      Count = n
    )
  # 2.6 create Excluded ---- ---- ----
  Excluded <- SampMeasData %>%
    dplyr::count(excluded) %>%
    dplyr::rename(
      `Pair Type` = excluded,
      Count = n
    ) %>%
    dplyr::filter(!is.na(`Pair Type`))
  # 2.7 create Included ---- ---- ----
  Included <- SampMeasData %>%
    dplyr::count(included) %>%
    dplyr::rename(
      `Pair Type` = included,
      Count = n
    ) %>%
    dplyr::filter(`Pair Type` == "Total included in SEG Analysis")
  # 2.8 create PairTypeTable ----  ----  ----
  PairTypeTable <- dplyr::bind_rows(BGMPairs, 
                                    Excluded, 
                                    Included)
  # 2.9 add the Total row  ----  ----  ----
  PairTypeTable <- PairTypeTable %>% tibble::add_row(
    `Pair Type` = "Total",
    Count = nrow(SampMeasData),
    .after = 0
  )
  return(PairTypeTable)
}

# PairTypeTest <- pairtypeTable(paste0(github_root, full_sample_repo))
# PairTypeTest

# 3.0 segTable() FUNCTION  ---- ----  ---- ----  ---- ----  ---- ----
segTable <- function(dat) {

  # 3.1 - import data frame ---- ---- -----
  SampMeasData <- suppressWarnings(readr::read_csv(file = dat))

  SampMeasData %>%
    dplyr::mutate(BGM = as.double(BGM),
                  REF = as.double(REF)) %>%

  # 3.2 create bgm_pair_cat ---- ----  ----
    dplyr::mutate(
      bgm_pair_cat =
        dplyr::case_when(
          BGM < REF ~ "BGM < REF",
          BGM == REF ~ "BGM = REF",
          BGM > REF ~ "BGM > REF"
        )
    ) %>%
    # 3.3 create ref_pair_2cat ---- ----  ----
    dplyr::mutate(
      ref_pair_2cat =
        dplyr::case_when(
          REF > 600 ~ "REF > 600: Excluded from SEG Analysis",
          REF < 21 & REF <= 600 ~ "REF <21: Included in SEG Analysis"
        )
    ) %>%
    # # 3.4 create included ---- ----  ----
    dplyr::mutate(
      included =
        dplyr::case_when(
          REF <= 600 ~ "Total included in SEG Analysis",
          REF > 600 ~ "Total excluded in SEG Analysis"
        )
    ) %>%
    # 3.5 join to RiskPairData ---- ----  ----
    dplyr::inner_join(.,
      y = RiskPairData,
      by = c("BGM", "REF")
    ) %>%
    dplyr::mutate( # 3.6 Create risk_cat variable ---- ---- ----
      risk_cat =
        base::findInterval(
          x = abs_risk, # the abs_risk absolute value
          vec = LookUpRiskCat$ABSLB, # the lower bound absolute risk
          left.open = TRUE
        ) - 1
    ) %>%
    dplyr::inner_join( # 3.7 Join to LookUpRiskCat data ---- ----  ----
      x = ., y = LookUpRiskCat, # inner join to look-up
      by = "risk_cat"
    ) %>%
    dplyr::mutate(
      risk_cat_txt = # text risk categories
      dplyr::case_when(
        abs_risk < 0.5 ~ "None",
        abs_risk >= 0.5 & abs_risk <= 1 ~ "Slight, Lower",
        abs_risk > 1 & abs_risk <= 1.5 ~ "Slight, Higher",
        abs_risk > 1.5 & abs_risk <= 2.0 ~ "Moderate, Lower",
        abs_risk > 2 & abs_risk <= 2.5 ~ "Moderate, Higher",
        abs_risk > 2.5 & abs_risk <= 3.0 ~ "Severe, Lower",
        abs_risk > 3.0 & abs_risk <= 3.5 ~ "Severe, Higher",
        abs_risk > 3.5 ~ "Extreme"
      )
    ) %>%
    dplyr::mutate(
      rel_diff = (BGM - REF) / REF, # relative diff
      abs_rel_diff = abs(rel_diff), # abs relative diff
      sq_rel_diff = rel_diff^2,
      iso_diff =
      if_else(REF >= 100, # condition 1
        100 * abs(BGM - REF) / REF, # T 1
        if_else(REF < 100, # condition 2
          abs(BGM - REF), # T 2
          NA_real_
        ), # F 2
        NA_real_
      ), # F1
      iso_range = # # 4.3.16 create iso range variable ----
      dplyr::case_when(
          # # A tibble: 5 x 2
          #      ID iso_range
          #   <int> <chr>
          # 1     1 <= 5% or 5 mg/dL
          # 2     2 > 5 - 10% or mg/dL
          # 3     3 > 10 - 15% or mg/dL
          # 4     4 > 15 - 20% mg/dL
          # 5     5 > 20% or 20 mg/dL
        iso_diff <= 5 ~ "<= 5% or 5 mg/dL",
        iso_diff > 5 & iso_diff <= 10 ~ "> 5 - 10% or mg/dL",
        iso_diff > 10 & iso_diff <= 15 ~ "> 10 - 15% or mg/dL",
        iso_diff > 15 & iso_diff <= 20 ~ "> 15 - 20% mg/dL",
        iso_diff > 20 ~ "> 20% or 20 mg/dL"),

      risk_grade = dplyr::case_when(
        abs_risk >= 0.0 & abs_risk < 0.5 ~ "A",
        abs_risk >= 0.5 & abs_risk < 1.0 ~ "B",
        abs_risk >= 1.0 & abs_risk < 2.0 ~ "C",
        abs_risk >= 2.0 & abs_risk < 3.0 ~ "D",
        abs_risk >= 3.0 ~ "E"
      ),
      risk_grade_txt = dplyr::case_when(
        abs_risk >= 0.0 & abs_risk < 0.5 ~ "0 - 0.5",
        abs_risk >= 0.5 & abs_risk < 1.0 ~ "> 0.5 - 1.0",
        abs_risk >= 1.0 & abs_risk < 2.0 ~ "> 1.0 - 2.0",
        abs_risk >= 2.0 & abs_risk < 3.0 ~ "> 2.0 - 3.0",
        abs_risk >= 3.0 ~ "> 3.0"
      )
    )
}

# test segTable
# TestSEGTableFunc <- segTable(paste0(github_root, full_sample_repo))
# TestSEGTableFunc %>% dplyr::count(included)

# 4.5.0 binomialTable ---- ----  ---- ----  ---- ----  ---- ----
binomialTable <- function(dat) {

  dataset <- segTable(dat)

  CompliantPairs <- nrow(dataset) - base::nrow(dplyr::filter(
    dataset, iso_diff > 15)) %>%
  tibble(
    `Compliant Pairs` = .)
# CompliantPairs
# Then calculate the percent
CompliantPairs <- CompliantPairs %>%
  dplyr::mutate(
    `Compliant Pairs %` =
      base::paste0(base::round(
        100 * `Compliant Pairs` / nrow(dataset),
        1
      ), "%") )
# CompliantPairs
# create probability
prb <- 0.95
p_value <- 0.05
df_size <- nrow(dataset)
qbinom_tibble <- qbinom(
    p = p_value,
    size = df_size,
    prob = prb) %>%
    tibble(`value` = .) %>%
    # clean up this variable in the tibble for display
    dplyr::rename(`Lower Bound for Acceptance` = value)
# qbinom_tibble
QbinomTable <- qbinom_tibble %>%
  dplyr::mutate(
    `Lower Bound for Acceptance %` =
      base::paste0(base::round(
        100 * `Lower Bound for Acceptance` / nrow(dataset),
        1
      ), "%")
  )
BinomialTest6 <- bind_cols(CompliantPairs, QbinomTable)
BinomialTest6 <- BinomialTest6 %>% dplyr::mutate(
  Result =
    if_else(condition = `Compliant Pairs` < `Lower Bound for Acceptance`,
      true = paste0(
        BinomialTest6$`Compliant Pairs %`[1],
        " < ",
        BinomialTest6$`Lower Bound for Acceptance %`[1],
        " - Does not meet BGM Surveillance Study Accuracy Standard"
      ),
      false = paste0(
        BinomialTest6$`Compliant Pairs %`[1],
        " > ",
        BinomialTest6$`Lower Bound for Acceptance %`[1],
        " - Meets BGM Surveillance Study Accuracy Standard"
      )
    )
)
return(BinomialTest6)
}

# test binomialTable
# TestBinomial <- binomialTable(paste0(github_root, full_sample_repo))

