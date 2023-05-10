# Sorting

This repository contains replication files for "Party-Ideology Sorting in the United States: Correlates and Consequences."

To run the replication files, clone the repo, [download](https://electionstudies.org/data-center/) the Stata versions of the 2012, 2016 and 2020 ANES and place them in the `data/` subdirectory, and then run `bash replication.sh`.

The results in the manuscript were produced in the following computing environment:

<details>
<summary>Output of R sessionInfo()</summary>
<br>

```
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.0

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] yardstick_1.1.0    workflowsets_1.0.0 workflows_1.1.2    tune_1.0.1         rsample_1.1.1      recipes_1.0.4      parsnip_1.0.3     
 [8] modeldata_1.1.0    infer_1.0.4        dials_1.1.0        scales_1.2.1       broom_1.0.3        tidymodels_1.0.0   modelsummary_1.3.0
[15] glmnetUtils_1.1.8  glmnet_4.1-6       Matrix_1.5-1       estimatr_1.0.0     haven_2.5.1        forcats_1.0.0      stringr_1.5.0     
[22] dplyr_1.1.1        purrr_1.0.1        readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       ggplot2_3.4.1      tidyverse_1.3.2   

loaded via a namespace (and not attached):
  [1] googledrive_2.0.0   colorspace_2.1-0    ellipsis_0.3.2      class_7.3-20        estimability_1.4.1  parameters_0.20.2  
  [7] fs_1.6.1            rstudioapi_0.14     farver_2.1.1        listenv_0.9.0       furrr_0.3.1         prodlim_2019.11.13 
 [13] fansi_1.0.4         mvtnorm_1.1-3       lubridate_1.9.2     xml2_1.3.3          codetools_0.2-18    splines_4.2.2      
 [19] knitr_1.42          Formula_1.2-4       jsonlite_1.8.4      dbplyr_2.3.0        effectsize_0.8.3    compiler_4.2.2     
 [25] httr_1.4.4          emmeans_1.8.5       backports_1.4.1     assertthat_0.2.1    fastmap_1.1.0       gargle_1.3.0       
 [31] cli_3.6.0           htmltools_0.5.4     tools_4.2.2         gtable_0.3.1        glue_1.6.2          tables_0.9.10      
 [37] Rcpp_1.0.10         cellranger_1.1.0    DiceDesign_1.9      vctrs_0.6.1         svglite_2.1.1       iterators_1.0.14   
 [43] insight_0.19.0      timeDate_4022.108   gower_1.0.1         xfun_0.38           globals_0.16.2      rvest_1.0.3        
 [49] timechange_0.2.0    lifecycle_1.0.3     renv_0.17.3         googlesheets4_1.0.1 future_1.31.0       MASS_7.3-58.1      
 [55] ipred_0.9-13        hms_1.1.2           parallel_4.2.2      RColorBrewer_1.1-3  rpart_4.1.19        stringi_1.7.12     
 [61] bayestestR_0.13.0   foreach_1.5.2       checkmate_2.1.0     lhs_1.1.6           hardhat_1.2.0       lava_1.7.1         
 [67] shape_1.4.6         rlang_1.1.0         pkgconfig_2.0.3     systemfonts_1.0.4   evaluate_0.20       lattice_0.20-45    
 [73] labeling_0.4.2      tidyselect_1.2.0    parallelly_1.34.0   magrittr_2.0.3      R6_2.5.1            generics_0.1.3     
 [79] DBI_1.1.3           pillar_1.8.1        withr_2.5.0         datawizard_0.6.5    survival_3.4-0      nnet_7.3-18        
 [85] performance_0.10.2  future.apply_1.10.0 modelr_0.1.10       crayon_1.5.2        utf8_1.2.3          tzdb_0.3.0         
 [91] rmarkdown_2.20      grid_4.2.2          readxl_1.4.2        reprex_2.0.2        digest_0.6.31       webshot_0.5.4      
 [97] munsell_0.5.0       GPfit_1.0-8         viridisLite_0.4.1   kableExtra_1.3.4   
```

</details>
