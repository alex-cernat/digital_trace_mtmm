## Estimating measurement quality in digital trace data and surveys using the MultiTrait MultiMethod model

This is the code for: *Estimating measurement quality in digital trace data and surveys using the MultiTrait MultiMethod model* a paper which is currently under review at the Social Science Computer Review journal.

This is joined work by:

- Alexandru Cernat, University of Manchester
- Florian Keusch, Mannheim University
- Ruben Bach, Mannheim University
- Paulina K. Pankowska, Utrecht University 

Survey data can be publicly accessed from the GESIS archive and the citation is:

Bach, R. ; Keusch, F., Areal, J., Pankowska, P.; Cernat, A. (2023). Political Identities and News Consumption in Election Times (PINCET). [https://doi.org/10.7802/2524](https://doi.org/10.7802/2524)

The digital trace data can be requested from [Ruben Back](https://www.mzes.uni-mannheim.de/d7/en/profiles/ruben-l-bach).

### R session version

```r
sessionInfo()
```

```r
R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
[3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.utf8    

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] shinystan_2.6.0   shiny_1.7.4       viridis_0.6.2     viridisLite_0.4.1
 [5] blavaan_0.4-3     Rcpp_1.0.10       lavaan_0.6-13     corrplot_0.92    
 [9] forcats_1.0.0     stringr_1.5.0     dplyr_1.1.0       purrr_1.0.1      
[13] readr_2.1.3       tidyr_1.3.0       tibble_3.1.8      ggplot2_3.4.0    
[17] tidyverse_1.3.2  

loaded via a namespace (and not attached):
  [1] googledrive_2.0.0    colorspace_2.1-0     ellipsis_0.3.2      
  [4] markdown_1.5         base64enc_0.1-3      fs_1.6.0            
  [7] rstudioapi_0.14      listenv_0.9.0        rstan_2.21.8        
 [10] DT_0.27              fansi_1.0.4          mvtnorm_1.1-3       
 [13] lubridate_1.9.1      xml2_1.3.3           codetools_0.2-18    
 [16] mnormt_2.1.1         knitr_1.42           shinythemes_1.2.0   
 [19] bayesplot_1.10.0     jsonlite_1.8.4       broom_1.0.3         
 [22] dbplyr_2.3.0         compiler_4.2.2       httr_1.4.4          
 [25] backports_1.4.1      assertthat_0.2.1     Matrix_1.5-1        
 [28] fastmap_1.1.0        gargle_1.3.0         cli_3.6.0           
 [31] later_1.3.0          htmltools_0.5.4      prettyunits_1.1.1   
 [34] tools_4.2.2          igraph_1.4.0         coda_0.19-4         
 [37] gtable_0.3.1         glue_1.6.2           reshape2_1.4.4      
 [40] cellranger_1.1.0     vctrs_0.5.2          crosstalk_1.2.0     
 [43] xfun_0.37            globals_0.16.2       ps_1.7.2            
 [46] rvest_1.0.3          miniUI_0.1.1.1       timechange_0.2.0    
 [49] mime_0.12            CompQuadForm_1.4.3   lifecycle_1.0.3     
 [52] renv_0.16.0          gtools_3.9.4         googlesheets4_1.0.1 
 [55] future_1.31.0        zoo_1.8-11           scales_1.2.1        
 [58] colourpicker_1.2.0   hms_1.1.2            promises_1.2.0.1    
 [61] parallel_4.2.2       sandwich_3.0-2       inline_0.3.19       
 [64] gridExtra_2.3        loo_2.5.1            StanHeaders_2.21.0-7
 [67] stringi_1.7.12       dygraphs_1.1.1.6     pkgbuild_1.4.0      
 [70] rlang_1.0.6          pkgconfig_2.0.3      matrixStats_0.63.0  
 [73] lattice_0.20-45      rstantools_2.2.0     htmlwidgets_1.6.1   
 [76] processx_3.8.0       tidyselect_1.2.0     parallelly_1.34.0   
 [79] plyr_1.8.8           magrittr_2.0.3       R6_2.5.1            
 [82] generics_0.1.3       DBI_1.1.3            pillar_1.8.1        
 [85] haven_2.5.1          withr_2.5.0          xts_0.12.2          
 [88] future.apply_1.10.0  modelr_0.1.10        crayon_1.5.2        
 [91] nonnest2_0.5-5       utf8_1.2.3           tmvnsim_1.0-2       
 [94] tzdb_0.3.0           grid_4.2.2           readxl_1.4.1        
 [97] pbivnorm_0.6.0       callr_3.7.3          threejs_0.3.3       
[100] reprex_2.0.2         digest_0.6.31        xtable_1.8-4        
[103] httpuv_1.6.8         RcppParallel_5.1.6   stats4_4.2.2        
[106] munsell_0.5.0        shinyjs_2.1.0  
```
