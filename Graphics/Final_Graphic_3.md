Producing Graphics For Portland Sediment Toxicity Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
7/16/2020

  - [Strategy](#strategy)
      - [Special Considerations](#special-considerations)
  - [Install Libraries](#install-libraries)
  - [Assemble Data](#assemble-data)
      - [Create Site Data](#create-site-data)
      - [Folder References](#folder-references)
      - [List of Names of Parameters](#list-of-names-of-parameters)
      - [Load Core Data](#load-core-data)
      - [Assemble Data in CBEP Preferred
        Form](#assemble-data-in-cbep-preferred-form)
          - [A Check for Half Censored
            Observations](#a-check-for-half-censored-observations)
      - [Assemble Data with Maximum Likelihood Estimates of
        Non-detects](#assemble-data-with-maximum-likelihood-estimates-of-non-detects)
          - [Metals](#metals)
          - [PAHs](#pahs)
          - [PCBs](#pcbs)
          - [DDT Residues](#ddt-residues)
  - [Generate Final Data for Plotting and
    Maps](#generate-final-data-for-plotting-and-maps)
      - [Combine PAH, PCB, and DDT Data](#combine-pah-pcb-and-ddt-data)
  - [Cleanup](#cleanup)
  - [Build Screening Levels Tibble](#build-screening-levels-tibble)
      - [Generate Screening Level
        Flags](#generate-screening-level-flags)
      - [A Wide Version for Export to
        GIS](#a-wide-version-for-export-to-gis)
  - [Graphic Development](#graphic-development)
      - [Draft](#draft)
      - [Reorder Factors](#reorder-factors)
      - [Principal Graphic](#principal-graphic)
      - [Alternate Form Showing Correlated
        Levels](#alternate-form-showing-correlated-levels)
  - [Number and Proportion of Exceedences
    Table](#number-and-proportion-of-exceedences-table)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

## Strategy

Overall, we want a visually simple graphic that captures the most
important aspects of contamination in Portland Harbor. From other
analyses, we know that 1. Metals levels are consistently below screening
levels. 2. Most pesticide residues were never or almost never detected,
the only exceptions being the DDT residues. 3. Other contaminants exceed
screening levels at about half of sampling locations. 4. Levels of
contaminants are highly correlated.

Multiple contaminants with complicated names can make for intimidating
graphic. We need to identify ways to simplify the data display without
distorting the meaning. Screening levels are available for Total PAHs,
Total PCBs, and sum of DDT residues, suggesting we can focus on those
three aggregate indicators of contamination and simplify the
presentation. There is no equivalent way of simplifying display of the
metals data, but all metals are below levels of concern.

The data is complicated by the presence of many non-detects. Here, we
handle non-detects by replacing them with a maximum likelihood estimator
of the expected value of non-detects based on an (assumed) lognormal
distribution. See CBEP’s LCensMeans Package for details.

Here we consider both and ‘J’ flags to be "non-detects’ as both are
below the reporting limits. A more sophisticated analysis, with more
data might model ‘J’ and ‘U’ flags differently, but here we combine
them.

### Special Considerations

1.  Reporting limits for organic contaminants from sample CSP-8 were
    exceptionally high. For PCBs the reporting limit was sufficiently
    high to bias any approach to estimating PCB loads, so we delete this
    point from consideration
2.  DDT concentrations from Sample CSS-15 were replicates that failed a
    QA/QC check. One was a non-detect, while the other was roughly
    double the reporting limit. Here we report results based on
    averaging imputed values of the non-detect with the observed value
    from the other sample.

# Install Libraries

``` r
library(readxl)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggthemes)
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(maxLik)
```

    ## Loading required package: miscTools

    ## 
    ## Please cite the 'maxLik' package as:
    ## Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.
    ## 
    ## If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
    ## https://r-forge.r-project.org/projects/maxlik/

``` r
library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())

library(LCensMeans)
```

# Assemble Data

## Create Site Data

``` r
sitename <-  c("East End Beach", "Amethyst Lot",
               "Maine State Pier/Ocean Gateway", "Maine Wharf",
               "Portland Pier", "Chandler's Wharf",
               "Union Wharf", "Union Wharf", "Wright Wharf",
               "Holyoke Wharf", "Deake's Wharf",
               "Portland Yacht Services", "Ricker's Wharf",
               "South Port Marina", "Aspasia/Sunset Marina",
               "Port Harbor/ Breakwater Marina")
SAMPLE_ID <- c('CSP-1', 'CSP-2', 'CSP-3', 'CSP-4', 'CSP-5',
                'CSP-6', 'CSP-7', 'CSP-7D', 'CSP-8', 'CSP-9',
                'CSP-10', 'CSP-11', 'CSP-12', 'CSS-13', 'CSP-14',
                'CSS-15')

site_info <- tibble(SAMPLE_ID,sitename)
rm(SAMPLE_ID, sitename)
```

## Folder References

``` r
sibfldnm <- 'Original_Data'
niecefldnm <- 'Final_Data_Transmittal'
parent <- dirname(getwd())
niece = file.path(parent,sibfldnm, niecefldnm)

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## List of Names of Parameters

We need lists of names of parameters is specific categories to
facilitate later sums, totals, and graphics. The Parameters are grouped
in categories in the secondary tables in the source Excel data.

``` r
fn <- "draft_Combined_data_20190917.xls"

sed_names <- c('% COARSE SAND', '% FINE SAND', '% MEDIUM SAND',
                'FINES', 'GRAVEL', 'MOISTURE', 'SOLIDS, TOTAL',
                'TOTAL ORGANIC CARBON (REP1)', 'TOTAL ORGANIC CARBON (REP2)')

metal_names <- read_excel(paste(niece,fn, sep='/'),
                           sheet = "Metals", skip = 3) %>%
  select(1) %>%
  slice(1:8) %>%
  mutate(PARAMETER_NAME = substr(PARAMETER_NAME, 1, nchar(PARAMETER_NAME)-7))
```

    ## New names:
    ## * Result -> Result...3
    ## * Qual -> Qual...4
    ## * Result -> Result...5
    ## * Qual -> Qual...6
    ## * Result -> Result...7
    ## * ...

``` r
metal_names <- metal_names$PARAMETER_NAME

pah_names <- read_excel(paste(niece,fn, sep='/'),
                        sheet = "PAHs", skip = 3) %>%
  select(1) %>%
  slice(1:16)
```

    ## New names:
    ## * Result -> Result...3
    ## * Qual -> Qual...4
    ## * Result -> Result...5
    ## * Qual -> Qual...6
    ## * Result -> Result...7
    ## * ...

``` r
pah_names <- pah_names$PARAMETER_NAME

pcb_names <- read_excel(paste(niece,fn, sep='/'),
                        sheet = "PCBs", skip = 3) %>%
  select(1) %>%
  slice(1:22) #%>%
```

    ## New names:
    ## * Result -> Result...3
    ## * Qual -> Qual...4
    ## * Result -> Result...5
    ## * Qual -> Qual...6
    ## * Result -> Result...7
    ## * ...

``` r
pcb_names <- pcb_names$PARAMETER_NAME

pesticide_names <- read_excel(paste(niece,fn, sep='/'),
                               sheet = "Pesticides", skip = 3) %>%
  select(1) %>%
  slice(1:8)
```

    ## New names:
    ## * Result -> Result...3
    ## * Qual -> Qual...4
    ## * Result -> Result...5
    ## * Qual -> Qual...6
    ## * Result -> Result...7
    ## * ...

``` r
pesticide_names <- pesticide_names$PARAMETER_NAME

(parmnames = c(sed_names, metal_names, pah_names, pcb_names, pesticide_names))
```

    ##  [1] "% COARSE SAND"               "% FINE SAND"                
    ##  [3] "% MEDIUM SAND"               "FINES"                      
    ##  [5] "GRAVEL"                      "MOISTURE"                   
    ##  [7] "SOLIDS, TOTAL"               "TOTAL ORGANIC CARBON (REP1)"
    ##  [9] "TOTAL ORGANIC CARBON (REP2)" "ARSENIC"                    
    ## [11] "CADMIUM"                     "CHROMIUM"                   
    ## [13] "COPPER"                      "MERCURY"                    
    ## [15] "NICKEL"                      "LEAD"                       
    ## [17] "ZINC"                        "ACENAPHTHENE"               
    ## [19] "ACENAPHTHYLENE"              "ANTHRACENE"                 
    ## [21] "BENZ(A)ANTHRACENE"           "BENZO(A)PYRENE"             
    ## [23] "BENZO(B)FLUORANTHENE"        "BENZO(GHI)PERYLENE"         
    ## [25] "BENZO(K)FLUORANTHENE"        "CHRYSENE"                   
    ## [27] "DIBENZ(A,H)ANTHRACENE"       "FLUORANTHENE"               
    ## [29] "FLUORENE"                    "INDENO(1,2,3-CD)PYRENE"     
    ## [31] "NAPHTHALENE"                 "PHENANTHRENE"               
    ## [33] "PYRENE"                      "CL2-BZ#8"                   
    ## [35] "CL3-BZ#18"                   "CL3-BZ#28"                  
    ## [37] "CL4-BZ#44"                   "CL4-BZ#49"                  
    ## [39] "CL4-BZ#52"                   "CL4-BZ#66"                  
    ## [41] "CL5-BZ#87"                   "CL5-BZ#101"                 
    ## [43] "CL5-BZ#105"                  "CL5-BZ#118"                 
    ## [45] "CL6-BZ#128"                  "CL6-BZ#138"                 
    ## [47] "CL6-BZ#153"                  "CL7-BZ#170"                 
    ## [49] "CL7-BZ#180"                  "CL7-BZ#183"                 
    ## [51] "CL7-BZ#184"                  "CL7-BZ#187"                 
    ## [53] "CL8-BZ#195"                  "CL9-BZ#206"                 
    ## [55] "CL10-BZ#209"                 "4,4'-DDD"                   
    ## [57] "4,4'-DDE"                    "4,4'-DDT"                   
    ## [59] "ALDRIN"                      "CIS-CHLORDANE"              
    ## [61] "CIS-NONACHLOR"               "DIELDRIN"                   
    ## [63] "ENDOSULFAN I"

## Load Core Data

``` r
fn <- "draft_Combined_data_20190917.xls"

the_data <- read_excel(paste(niece,fn, sep='/'), 
    sheet = "Combined", col_types = c("skip", 
        "text", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "text", "text", "numeric", "text", 
        "numeric", "text", "numeric", "numeric", 
        "text", "text", "text", "skip", 
        "skip", "skip", "skip", "skip", 
        "numeric", "numeric", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip")) %>%
  mutate(SAMPLE_ID = factor(SAMPLE_ID, levels = c("CSP-1", "CSP-2", "CSP-3",
                                                  "CSP-4", "CSP-5", "CSP-6",
                                                  "CSP-7", "CSP-7D", "CSP-8",
                                                  "CSP-9", "CSP-10", "CSP-11",
                                                  "CSP-12", "CSS-13", "CSP-14",
                                                  "CSS-15"))) %>%
  # Remove ', TOTAL' from the end of the names of metals
  mutate(PARAMETER_NAME = 
           ifelse(substr(PARAMETER_NAME, nchar(PARAMETER_NAME)-6,
                          nchar(PARAMETER_NAME)) == ', TOTAL',
                  substr(PARAMETER_NAME, 1, nchar(PARAMETER_NAME)-7),
                  PARAMETER_NAME))
```

## Assemble Data in CBEP Preferred Form

We filter data to eliminate QA/QC samples, then combine Reporting Limits
and Observed Concentrations into data on concentrations. We then add a
flag indicating which are censored values. Finally, we group data into
groups, and use the groups.

``` r
sed_data_long <- the_data %>% 
  filter (the_data$PARAMETER_NAME %in% parmnames) %>%
  filter(is.na(`%_RECOVERY`)) %>%
  filter(SAMPLE_ID != 'QC') %>%
  mutate(CONCENTRATION = ifelse(is.na(CONCENTRATION) &
                                  LAB_QUALIFIER %in% c('U', 'J'),
                                REPORTING_LIMIT,
                                CONCENTRATION)) %>%
  group_by(SAMPLE_ID, PARAMETER_NAME) %>%
  summarize(CONCENTRATION = mean(CONCENTRATION, na.rm=TRUE),
            samples = n(),
            censored = sum(LAB_QUALIFIER=='U', na.rm=TRUE)) %>%
  ungroup() %>%
  rename(Contaminant = PARAMETER_NAME) %>%
  mutate(Contaminant = factor(Contaminant)) %>%
  
  #Create group variable to facilitate subsetting.
  mutate(cgroup = ifelse(Contaminant %in% sed_names,1,
                   ifelse(Contaminant %in% metal_names,2,
                      ifelse(Contaminant %in% pah_names,3,
                          ifelse(Contaminant %in% pcb_names,4,
                              ifelse(Contaminant %in% pesticide_names,
                                     5,0)))))) %>%
  mutate(cgroup = factor(cgroup, labels = c('Sediment', 'Metals', 'PAHs',
                                         'PCBs', 'Pesticides')))  %>% 
  mutate(Contaminant = fct_reorder(Contaminant, as.numeric(cgroup)))
```

    ## `summarise()` regrouping output by 'SAMPLE_ID' (override with `.groups` argument)

``` r
levels(sed_data_long$Contaminant)
```

    ##  [1] "% COARSE SAND"               "% FINE SAND"                
    ##  [3] "% MEDIUM SAND"               "FINES"                      
    ##  [5] "GRAVEL"                      "MOISTURE"                   
    ##  [7] "TOTAL ORGANIC CARBON (REP1)" "TOTAL ORGANIC CARBON (REP2)"
    ##  [9] "ARSENIC"                     "CADMIUM"                    
    ## [11] "CHROMIUM"                    "COPPER"                     
    ## [13] "LEAD"                        "MERCURY"                    
    ## [15] "NICKEL"                      "ZINC"                       
    ## [17] "ACENAPHTHENE"                "ACENAPHTHYLENE"             
    ## [19] "ANTHRACENE"                  "BENZ(A)ANTHRACENE"          
    ## [21] "BENZO(A)PYRENE"              "BENZO(B)FLUORANTHENE"       
    ## [23] "BENZO(GHI)PERYLENE"          "BENZO(K)FLUORANTHENE"       
    ## [25] "CHRYSENE"                    "DIBENZ(A,H)ANTHRACENE"      
    ## [27] "FLUORANTHENE"                "FLUORENE"                   
    ## [29] "INDENO(1,2,3-CD)PYRENE"      "NAPHTHALENE"                
    ## [31] "PHENANTHRENE"                "PYRENE"                     
    ## [33] "CL10-BZ#209"                 "CL2-BZ#8"                   
    ## [35] "CL3-BZ#18"                   "CL3-BZ#28"                  
    ## [37] "CL4-BZ#44"                   "CL4-BZ#49"                  
    ## [39] "CL4-BZ#52"                   "CL4-BZ#66"                  
    ## [41] "CL5-BZ#101"                  "CL5-BZ#105"                 
    ## [43] "CL5-BZ#118"                  "CL5-BZ#87"                  
    ## [45] "CL6-BZ#128"                  "CL6-BZ#138"                 
    ## [47] "CL6-BZ#153"                  "CL7-BZ#170"                 
    ## [49] "CL7-BZ#180"                  "CL7-BZ#183"                 
    ## [51] "CL7-BZ#184"                  "CL7-BZ#187"                 
    ## [53] "CL8-BZ#195"                  "CL9-BZ#206"                 
    ## [55] "4,4'-DDD"                    "4,4'-DDE"                   
    ## [57] "4,4'-DDT"                    "ALDRIN"                     
    ## [59] "CIS-CHLORDANE"               "CIS-NONACHLOR"              
    ## [61] "DIELDRIN"                    "ENDOSULFAN I"

### A Check for Half Censored Observations

Since in a few cases, we averaged two samples together, it is possible
that we have averaged a non-detect with a detection. It turns out we
did, but just once.

``` r
sum(sed_data_long$censored> 0 & sed_data_long$censored<sed_data_long$samples)
```

    ## [1] 1

``` r
sed_data_long[which(sed_data_long$censored> 0 &
                      sed_data_long$censored<sed_data_long$samples),]
```

    ## # A tibble: 1 x 6
    ##   SAMPLE_ID Contaminant CONCENTRATION samples censored cgroup    
    ##   <fct>     <fct>               <dbl>   <int>    <int> <fct>     
    ## 1 CSS-15    4,4'-DDT            0.563       2        1 Pesticides

``` r
the_data[the_data$SAMPLE_ID=='CSS-15' & the_data$PARAMETER_NAME=="4,4'-DDT",]
```

    ## # A tibble: 7 x 14
    ##   SAMPLE_ID CAS_NO PARAMETER_NAME CONCENTRATION LAB_QUALIFIER REPORTING_LIMIT
    ##   <fct>     <chr>  <chr>                  <dbl> <chr>                   <dbl>
    ## 1 CSS-15    50293  4,4'-DDT               0.744 P                       0.366
    ## 2 CSS-15    50293  4,4'-DDT              NA     U                       0.382
    ## 3 CSS-15    50293  4,4'-DDT              NA     <NA>                   NA    
    ## 4 CSS-15    50293  4,4'-DDT              NA     <NA>                   NA    
    ## 5 <NA>      <NA>   <NA>                  NA     <NA>                   NA    
    ## 6 <NA>      <NA>   <NA>                  NA     <NA>                   NA    
    ## 7 <NA>      <NA>   <NA>                  NA     <NA>                   NA    
    ## # ... with 8 more variables: PARAMETER_UNITS <chr>, `%_RECOVERY` <dbl>,
    ## #   RPD <dbl>, TEST <chr>, PARAMETER_QUALIFIER <chr>, PARAMETER_FILTERED <chr>,
    ## #   MDL <dbl>, DILUTION_FACTOR <dbl>

Note that these two DDT results were flagged as problematic because
differences between the two observations were greater than an acceptable
relative percent difference. That does not engender great confidence in
the data….

We have to pick this up again later, when we calculate values for
Pesticide Totals.

## Assemble Data with Maximum Likelihood Estimates of Non-detects

Here we replace non-detect with estimates of the conditional mean of
non-detects, based on a maximum likelihood procedure under a longnormal
distribution.

### Metals

No reorganization needed. All metals are below levels of concern, and
there are no suitable aggregate screening levels for metals or sum of
metals. So there is little point in reporting specific values, and no
easy way to summarize results. We could show one or two metals of
interest – like Mercury – or just report that all are below levels of
concern. We chose the latter option, so don’t include metals on the
graphics.

### PAHs

``` r
pah_res<- sed_data_long %>%
  filter (cgroup =='PAHs') %>%

  mutate(censored = censored>0) %>%

  group_by(Contaminant) %>%
  mutate(LikCensored = sub_cmeans(CONCENTRATION, censored)) %>%
  ungroup()  %>%
  group_by(SAMPLE_ID) %>%
  summarize(LNtotPAH = sum(LikCensored),
            .groups = 'drop')
```

### PCBs

``` r
pcb_res<- sed_data_long %>%
  filter (cgroup =='PCBs') %>%
  
  # CSP-8 had exceptionally high PCB detection limits
  filter(SAMPLE_ID != 'CSP-8') %>% 
  
  mutate(censored = censored>0) %>%

  group_by(Contaminant) %>%
  mutate(LikCensored = sub_cmeans(CONCENTRATION, censored)) %>%
  ungroup()  %>%
  group_by(SAMPLE_ID) %>%
  summarize(LNtotPCB = sum(LikCensored),
            .groups = 'drop')
```

### DDT Residues

We report only on Total DDT Residues. Other pesticides were observed too
rarely to be worth reporting.

#### Correcting for Half non-detect

When we average across the two values, we are averaging a non-detect
with a significantly higher observation. This only happened once in
these data, in the DDT results for Sample CSS-15.

To be accurate, we need to calculate estimates of the censored values
(ND, Half ND and Maximum Likelihood) on the original raw data and
average the results, rather than calculate estimates based on an average
of an observation and a reporting limit.

If we are looking at the full detection limit, the average of a sum is
the sum of the averages, and it makes no difference, but it should
matter for the other two estimators, especially for the maximum
likelihood estimator.

#### Calculate Average by Maximum Likelihood Estimator

``` r
est <- the_data %>%
  filter(PARAMETER_NAME=="4,4'-DDT") %>%
  filter(is.na(`%_RECOVERY`)) %>%
  filter(SAMPLE_ID != 'QC') %>%
  select(SAMPLE_ID, CONCENTRATION, REPORTING_LIMIT, LAB_QUALIFIER) %>%
  mutate(censored = LAB_QUALIFIER %in% c('U', 'J')) %>%
  mutate(CONCENTRATION = ifelse(censored, REPORTING_LIMIT, CONCENTRATION)) %>%
  mutate(lnest = sub_cmeans(CONCENTRATION, censored)) %>%
  filter(SAMPLE_ID == 'CSS-15') %>%
  pull(lnest)
(mle <- mean(est))
```

    ## [1] 0.4491413

``` r
rm(est)
```

#### Assemble Pesticides Data

``` r
pests_data_long <- sed_data_long %>%
  filter (cgroup =='Pesticides') %>%
  mutate(censored = censored>0) %>%
  group_by(Contaminant) %>%
  mutate(LikCensored = sub_cmeans(CONCENTRATION, censored)) %>%
  ungroup() %>%
  mutate(LikCensored = ifelse(SAMPLE_ID =='CSS-15' & Contaminant =="4,4'-DDT",
                              mle, LikCensored))
rm(mle)
```

#### Assemble Final DDT Residue Data

``` r
pesticide_res <- pests_data_long %>%
  group_by(SAMPLE_ID) %>%
  summarize(LNtotDDT = sum(LikCensored),
            .groups = 'drop')
```

# Generate Final Data for Plotting and Maps

## Combine PAH, PCB, and DDT Data

``` r
res <- site_info %>%
  left_join(pah_res,       by = "SAMPLE_ID") %>%
  left_join(pcb_res,       by = "SAMPLE_ID") %>%
  left_join(pesticide_res, by = "SAMPLE_ID") %>%
  
  rename_at(3:5, ~substr(.,nchar(.)-2, nchar(.))) %>%  # Pull last characters
  
  pivot_longer(cols = -c(SAMPLE_ID, sitename),
               names_to = 'Contaminant',
               values_to= 'MLE') %>%
  mutate(Contaminant = factor(Contaminant,
                              levels = c('DDT', 'PAH', 'PCB'),
                              labels = c('DDT Residues',
                                         'Total PAHs',
                                         'Total PCBs')))
```

# Cleanup

``` r
rm(parmnames, sed_names, metal_names, pah_names, pah_res, pcb_names, pcb_res,
   pesticide_names, pesticide_res, pests_data_long)
rm(the_data, sed_data_long)
```

# Build Screening Levels Tibble

``` r
pcb <- c(22.7, 180)
pah <- c(4022, 44792)
ddt <- c(1.58, 46.1)

sl <- tibble(Contaminant = rep(c('DDT Residues', 
                                 'Total PAHs', 'Total PCBs'), each = 2),    
             Threshold = rep(c('ERL','ERM'),3),
             Value = c(ddt,pah, pcb))
```

## Generate Screening Level Flags

``` r
erl <- sl %>% filter(Threshold == 'ERL') %>% select(-Threshold)
erm <- sl %>% filter(Threshold == 'ERM') %>% select(-Threshold)
res_screen <- res %>%
  mutate(ERL = erl$Value[match(Contaminant, erl$Contaminant)]) %>%
  mutate(ERM = erm$Value[match(Contaminant, erm$Contaminant)]) %>%
  mutate(SL = ifelse(MLE<ERL, 'Below ERL',
                     ifelse(MLE<ERM, 'Between ERL and ERM', 'Above ERM'))) %>%
  mutate(SL = factor(SL, levels = c('Below ERL',
                                     'Between ERL and ERM',
                                     'Above ERM'))) %>%
  select(-ERM, -ERL)
```

## A Wide Version for Export to GIS

The function pivot\_wider accepts two data columns, and handles them
intelligently, but the default names are awkward here. We use
“rename\_at only so we don’t have to exactly match the default name by
using”rename()".

``` r
res_screen_wide <- res_screen %>%
  pivot_wider(names_from = Contaminant, values_from = c(MLE,SL),
              id_cols = c(SAMPLE_ID, sitename)) %>%
  rename_at(3:4, ~substr(.,nchar(.)-3, nchar(.))) %>%  # Pull last characters
  rename_at(5, ~'DDTs') %>%
  rename_at(6:7, ~paste0(substr(.,nchar(.)-3, nchar(.)), 'SL')) %>%
  rename_at(8, ~'DDTsSL')
write.csv(res_screen_wide,'MLE_Results_Wide.csv')
```

# Graphic Development

## Draft

``` r
plt <- ggplot(res_screen, aes(Contaminant, MLE)) + 
  #geom_boxplot() +
  geom_point(aes(color = SL), size = 3, alpha = 0.5) +
  # geom_point(data = sl, aes(Contaminant, Value,
  #                               fill = Threshold, shape = Threshold),
  #                size = 4, alpha = .5) +

  scale_y_log10(labels=scales::comma) +
  
 #scale_shape_manual(values = c(24,25)) +
  scale_color_manual(name = '', values = cbep_colors()) +
  ylab('Concentration (ppb)') +
  xlab ('') +
  theme_cbep() +
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 
plt
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Final_Graphic_3_files/figure-gfm/draft_graphic-1.png)<!-- -->

So, what that shows is that despite DDT being outlawed for a generation,
concentrations of DDT residues in Portland Harbor are well above levels
of concern. Similarly, PAHs are usually above conservative screening
levels, and many sites had levels of PCBs above levels of concern.

## Reorder Factors

``` r
tmp <- res_screen %>%
  mutate(Contaminant = fct_relevel(Contaminant, 'Total PAHs', 'Total PCBs'))
levels(tmp$Contaminant)
```

    ## [1] "Total PAHs"   "Total PCBs"   "DDT Residues"

``` r
tmp_sl <- sl %>%
   mutate(Contaminant = fct_relevel(Contaminant, 'Total PAHs', 'Total PCBs'))
```

## Principal Graphic

``` r
plt <- ggplot(tmp, aes(Contaminant, MLE)) + 
  geom_point(aes(color = SL), size = 4, alpha = 0.5) +
  scale_y_log10(labels=scales::comma) +
  scale_color_manual(name = '', values = cbep_colors()) +
  #scale_color_viridis_d(begin=0, end=0.9, name = '') +  # The "end" parameter cuts out a very pale yellow.
  ylab('Concentration (ppb)') +
  xlab ('') +
  theme_cbep() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 
plt
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Final_Graphic_3_files/figure-gfm/final_graphic-1.png)<!-- -->

``` r
#ggsave('figures/Portland Harbor Contaminants.png', type = 'cairo',
#       width = 6, height = 5)
ggsave('figures/Portland Harbor Contaminants.pdf', device = cairo_pdf,
       width = 6, height = 5)
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

## Alternate Form Showing Correlated Levels

``` r
plt + geom_line(aes(as.numeric(Contaminant), MLE,
                    group = SAMPLE_ID), alpha = 0.2)
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Final_Graphic_3_files/figure-gfm/alternate_graphic-1.png)<!-- -->

That might work in a powerpoint, but it’s a bit noisy for SotB.

# Number and Proportion of Exceedences Table

``` r
res_screen %>%
  group_by(Contaminant, SL) %>%
  summarize(number = n(), .groups = 'drop_last')
```

    ## # A tibble: 9 x 3
    ## # Groups:   Contaminant [3]
    ##   Contaminant  SL                  number
    ##   <fct>        <fct>                <int>
    ## 1 DDT Residues Between ERL and ERM      8
    ## 2 DDT Residues Above ERM                8
    ## 3 Total PAHs   Below ERL                3
    ## 4 Total PAHs   Between ERL and ERM      6
    ## 5 Total PAHs   Above ERM                7
    ## 6 Total PCBs   Below ERL                7
    ## 7 Total PCBs   Between ERL and ERM      6
    ## 8 Total PCBs   Above ERM                2
    ## 9 Total PCBs   <NA>                     1
