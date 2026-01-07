# Getting started

``` r
library("tidyverse")
library("rstatix")
library("magrittr")
library("GimmeMyStats")
```

## Clinical Dataset

``` r
set.seed(123)
n <- 150 # Number of patients
clinical_data <- tibble(
    Country = sample(c("France", "Germany", "UK", "Italy", "Spain"), n, replace = TRUE),
    Age = rnorm(n, mean = 60, sd = 10),
    Sex = sample(c("Male", "Female"), n, replace = TRUE),
    Cancer_Type = sample(c("Lung", "Breast", "Colorectal", "Healthy"), n, replace = TRUE),
    Cancer_Stage = sample(1:4, n, replace = TRUE),
    Weight = rnorm(n, mean = 75, sd = 15),
    Height = rnorm(n, mean = 170, sd = 10),
    Fatigue_Score = sample(0:10, n, replace = TRUE),
    Physician_Score = sample(0:10, n, replace = TRUE),
    CRP = rnorm(n, mean = 5, sd = 2),
    IL6 = rnorm(n, mean = 10, sd = 5),
    Leukocytes = rnorm(n, mean = 6.5, sd = 2),
    Neutrophils = rnorm(n, mean = 55, sd = 10),
    Lymphocytes = rnorm(n, mean = 35, sd = 8),
    KRAS_Mutation = sample(c("Mutated", "Wild-type"), n, replace = TRUE),
    Treatment_Response = sample(c("Complete", "Partial", "None"), n, replace = TRUE)
)
head(clinical_data)
#> # A tibble: 6 × 16
#>   Country   Age Sex    Cancer_Type Cancer_Stage Weight Height Fatigue_Score
#>   <chr>   <dbl> <chr>  <chr>              <int>  <dbl>  <dbl>         <int>
#> 1 UK       64.5 Female Lung                   3   61.6   180.             8
#> 2 UK       60.4 Male   Healthy                4   43.9   175.             5
#> 3 Germany  55.8 Female Lung                   1   77.3   178.             3
#> 4 Germany  39.5 Female Healthy                1   73.8   171.             1
#> 5 UK       71.3 Female Healthy                1   73.5   179.             6
#> 6 Spain    45.4 Female Breast                 4   78.2   184.             8
#> # ℹ 8 more variables: Physician_Score <int>, CRP <dbl>, IL6 <dbl>,
#> #   Leukocytes <dbl>, Neutrophils <dbl>, Lymphocytes <dbl>,
#> #   KRAS_Mutation <chr>, Treatment_Response <chr>
```

## Descriptive Statistics

We summarize categorical and multinomial variables using
`print_multinomial`.

``` r
print_multinomial(select(clinical_data, "Cancer_Type"))
#> # A tibble: 4 × 3
#>   Variables   Levels     Statistics
#>   <chr>       <fct>      <chr>     
#> 1 Cancer_Type Breast     31 (20.7%)
#> 2 Cancer_Type Colorectal 36 (24%)  
#> 3 Cancer_Type Healthy    39 (26%)  
#> 4 Cancer_Type Lung       44 (29.3%)
```

*Here, we see if the distribution of cancer types and treatment
responses is balanced across the dataset. If a category is
underrepresented, statistical comparisons may lack power.*

Binary variables can be summarized using `summary_binomial`.

``` r
summary_binomial(select(clinical_data, c("KRAS_Mutation", "Sex")))
#> # A tibble: 2 × 2
#>   Variables     Statistics          
#>   <chr>         <chr>               
#> 1 KRAS_Mutation Mutated : 64 (42.7%)
#> 2 Sex           Female : 78 (52%)
```

*Checking for imbalances in binary variables is crucial. If
`KRAS_Mutation` is highly imbalanced, conclusions regarding its
association with outcomes should be interpreted cautiously.*

For continuous variables, `summary_numeric` provides a robust summary.

``` r
print_numeric(select(clinical_data, c("Age", "Weight", "CRP")))
#> # A tibble: 3 × 10
#>   Variables `Mean+/-SD` `Median+/-IQR` `Q1-Q3` Range Kurtosis Skewness Normality
#>   <chr>     <chr>       <chr>          <chr>   <chr>    <dbl>    <dbl> <chr>    
#> 1 Age       59.9+/-9.9  58.8+/-12.8    53.5;6… 39.5…      0        0.5 *        
#> 2 CRP       4.9+/-2.2   5+/-3.1        3.3;6.5 -0.2…     -0.6      0   ns       
#> 3 Weight    73.6+/-14.6 73.7+/-20.7    63.3;84 32.9…     -0.2     -0.1 ns       
#> # ℹ 2 more variables: Zeros <int>, NAs <int>
summary_numeric(clinical_data$Age)
#> # A tibble: 1 × 2
#>   Variables `Median+/-IQR`
#>   <chr>     <chr>         
#> 1 x         58.8+/-12.8
```

*We verify if the distributions are symmetric or skewed. A strong skew
might indicate outliers or a non-normal distribution requiring
transformation before parametric testing.*

### **Identifying Outliers**

Outliers in continuous variables can affect statistical analyses. We use
different methods to detect them:

``` r
identify_outliers(clinical_data$CRP, method = "iqr")
#>       65      138 
#> 10.39679 -0.15288
```

``` r
identify_outliers(clinical_data$CRP, method = "percentiles")
#>          1          3          5          6          8          9         10 
#>  8.3134481  3.1290760  7.6413795  3.3149919  2.3978950  3.0335076  2.7260567 
#>         11         13         14         15         17         18         20 
#>  6.5718339  7.0840632  6.9596322  0.3681530  8.0072611  7.7231960  6.4619270 
#>         22         24         26         29         32         34         37 
#>  1.6904591  2.5296148  9.6856059  1.1423246  2.0109577  8.5924038  6.9686532 
#>         41         42         45         47         49         51         53 
#>  1.0635006  6.9962251  1.0653040  7.9710068  2.9247392  6.7639155  3.1759631 
#>         54         55         58         59         61         63         65 
#>  1.7084062  2.4285751  8.6285764  3.0199275  6.8558961  2.6936643 10.3967902 
#>         66         70         71         72         75         78         79 
#>  9.6581565  1.7176185  7.0088403  6.6258744  1.1835822  3.1078309  7.9568659 
#>         80         82         84         89         90         94         95 
#>  6.5516459  2.7315312  0.8203162  2.5309310  2.4729368  1.7363015  0.9751352 
#>         97         98        102        104        105        106        108 
#>  3.2022552  7.1503529  8.6788003  8.7396548  7.2354010  6.5513664  9.4546493 
#>        112        113        114        115        116        119        120 
#>  9.1914104  7.1803523  2.7092527  0.8903842  7.5161296  1.2543664  8.4874727 
#>        122        123        125        126        128        131        132 
#>  1.7135104  7.5253174  3.0938029  2.5521838  7.5803915  7.3143079  7.1365233 
#>        133        135        138        140        146        150 
#>  7.4986945  3.2458000 -0.1528800  6.8177517  2.1789440  2.0953922
```

``` r
identify_outliers(clinical_data$CRP, method = "hampel")
#>          1          5          8         15         17         18         22 
#>  8.3134481  7.6413795  2.3978950  0.3681530  8.0072611  7.7231960  1.6904591 
#>         24         26         29         32         34         41         45 
#>  2.5296148  9.6856059  1.1423246  2.0109577  8.5924038  1.0635006  1.0653040 
#>         47         54         55         58         65         66         70 
#>  7.9710068  1.7084062  2.4285751  8.6285764 10.3967902  9.6581565  1.7176185 
#>         75         79         84         89         90         94         95 
#>  1.1835822  7.9568659  0.8203162  2.5309310  2.4729368  1.7363015  0.9751352 
#>        102        104        108        112        115        116        119 
#>  8.6788003  8.7396548  9.4546493  9.1914104  0.8903842  7.5161296  1.2543664 
#>        120        122        123        126        128        133        138 
#>  8.4874727  1.7135104  7.5253174  2.5521838  7.5803915  7.4986945 -0.1528800 
#>        146        150 
#>  2.1789440  2.0953922
```

``` r
identify_outliers(clinical_data$CRP, method = "mad")
#>          1          5          8         15         17         18         22 
#>  8.3134481  7.6413795  2.3978950  0.3681530  8.0072611  7.7231960  1.6904591 
#>         24         26         29         32         34         41         45 
#>  2.5296148  9.6856059  1.1423246  2.0109577  8.5924038  1.0635006  1.0653040 
#>         47         54         55         58         65         66         70 
#>  7.9710068  1.7084062  2.4285751  8.6285764 10.3967902  9.6581565  1.7176185 
#>         75         79         84         89         90         94         95 
#>  1.1835822  7.9568659  0.8203162  2.5309310  2.4729368  1.7363015  0.9751352 
#>        102        104        108        112        115        116        119 
#>  8.6788003  8.7396548  9.4546493  9.1914104  0.8903842  7.5161296  1.2543664 
#>        120        122        123        126        128        133        138 
#>  8.4874727  1.7135104  7.5253174  2.5521838  7.5803915  7.4986945 -0.1528800 
#>        146        150 
#>  2.1789440  2.0953922
```

``` r
identify_outliers(select(clinical_data, CRP), method = "sd")
#>         15         26         29         34         41         45         58 
#>  0.3681530  9.6856059  1.1423246  8.5924038  1.0635006  1.0653040  8.6285764 
#>         65         66         75         84         95        102        104 
#> 10.3967902  9.6581565  1.1835822  0.8203162  0.9751352  8.6788003  8.7396548 
#>        108        112        115        119        120        138 
#>  9.4546493  9.1914104  0.8903842  1.2543664  8.4874727 -0.1528800
```

*Different methods have different sensitivity levels. The `iqr` method
identifies extreme values based on quartiles, while `mad` and `hampel`
are robust to skewed distributions. `sd` assumes normality and may not
be ideal for skewed data.*

## Correlation Analysis

``` r
mcor_test(clinical_data[, c("CRP", "IL6", "Leukocytes")], method = "pearson")
#>                    CRP        IL6  Leukocytes
#> CRP         1.00000000 0.05415309 -0.04326444
#> IL6         0.05415309 1.00000000  0.07126276
#> Leukocytes -0.04326444 0.07126276  1.00000000
```

``` r
mcor_test(
    clinical_data[, c("CRP", "IL6", "Leukocytes")],
    clinical_data[, c("Physician_Score", "Fatigue_Score")],
    method = "spearman",
    p.value = TRUE,
    method_adjust = "bonferroni"
)
#> $estimate
#>                          CRP         IL6  Leukocytes
#> Physician_Score -0.003373907 -0.02593512 0.005017972
#> Fatigue_Score    0.125182200  0.00348041 0.109012944
#> 
#> $p.value
#>                     CRP IL6 Leukocytes
#> Physician_Score 1.00000   1          1
#> Fatigue_Score   0.76154   1          1
```

*Pearson’s correlation assumes linear relationships, whereas Spearman’s
is rank-based and better suited for skewed or non-linear associations.
If Pearson’s r differs significantly from Spearman’s rho, the
relationship might not be linear.*

## Group Comparisons

### **ANOVA (Parametric)**

``` r
anova_res <- anova_test(data = clinical_data, Age ~ Country)
print_test(anova_res)
#> [1] "Anova, F(4, 145) = 2, p = 0.13"
```

*A significant ANOVA result suggests at least one group mean differs. If
non-significant, we fail to reject the null hypothesis that all means
are equal.*

### **Kruskal-Wallis (Non-Parametric)**

``` r
kruskal_res <- kruskal_test(data = clinical_data, CRP ~ Cancer_Type)
print_test(kruskal_res)
#> [1] "Kruskal-Wallis, K(3) = 0, p = 0.96"
```

*This test is an alternative to ANOVA when normality is violated. A
significant result means at least one group median differs.*

### **Wilcoxon Test (Two Groups)**

``` r
wilcox_res <- wilcox_test(data = clinical_data, IL6 ~ KRAS_Mutation)
print_test(wilcox_res)
#> [1] "Wilcoxon, W = 2450, p = 0.25"
```

*A significant result suggests IL6 levels differ significantly between
`KRAS_Mutation` groups.*

## Chi-Square and Fisher’s Exact Test

``` r
chi2_res <- chisq_test(table(clinical_data$Cancer_Type, clinical_data$Treatment_Response))
print_chi2_test(chi2_res)
#> [1] "X2(6) = 6, P = 0.421, N = 150"
```

*Chi-square tests independence between categorical variables. A
significant result suggests an association between cancer type and
treatment response.*

``` r
post_hoc_chi2(clinical_data$Cancer_Type, method = "chisq")
#> # A tibble: 6 × 9
#>       n statistic    df     p p.signif group1     group2       FDR fdr.signif
#>   <int>     <dbl> <dbl> <dbl> <chr>    <chr>      <chr>      <dbl> <chr>     
#> 1    67     0.373     1 0.541 ns       Breast     Colorectal 0.7   ns        
#> 2    70     0.914     1 0.339 ns       Breast     Healthy    0.7   ns        
#> 3    75     2.25      1 0.133 ns       Breast     Lung       0.7   ns        
#> 4    75     0.12      1 0.729 ns       Colorectal Healthy    0.729 ns        
#> 5    80     0.8       1 0.371 ns       Colorectal Lung       0.7   ns        
#> 6    83     0.301     1 0.583 ns       Healthy    Lung       0.7   ns
```

*Post-hoc tests determine which specific categories differ, useful when
the chi-square test is significant.*

## Session Information

    #> R version 4.5.2 (2025-10-31)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.3 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] GimmeMyStats_1.0.0 magrittr_2.0.4     rstatix_0.7.3      lubridate_1.9.4   
    #>  [5] forcats_1.0.1      stringr_1.6.0      dplyr_1.1.4        purrr_1.2.0       
    #>  [9] readr_2.1.6        tidyr_1.3.2        tibble_3.3.0       ggplot2_4.0.1     
    #> [13] tidyverse_2.0.0   
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] gtable_0.3.6        xfun_0.55           bslib_0.9.0        
    #>  [4] lattice_0.22-7      numDeriv_2016.8-1.1 tzdb_0.5.0         
    #>  [7] vctrs_0.6.5         tools_4.5.2         Rdpack_2.6.4       
    #> [10] generics_0.1.4      proxy_0.4-29        pkgconfig_2.0.3    
    #> [13] Matrix_1.7-4        RColorBrewer_1.1-3  S7_0.2.1           
    #> [16] desc_1.4.3          lifecycle_1.0.4     compiler_4.5.2     
    #> [19] farver_2.1.2        textshaping_1.0.4   lmerTest_3.1-3     
    #> [22] carData_3.0-5       htmltools_0.5.9     class_7.3-23       
    #> [25] sass_0.4.10         yaml_2.3.12         Formula_1.2-5      
    #> [28] nloptr_2.2.1        pillar_1.11.1       pkgdown_2.2.0      
    #> [31] car_3.1-3           jquerylib_0.1.4     MASS_7.3-65        
    #> [34] cachem_1.1.0        reformulas_0.4.3    boot_1.3-32        
    #> [37] abind_1.4-8         nlme_3.1-168        tidyselect_1.2.1   
    #> [40] digest_0.6.39       stringi_1.8.7       splines_4.5.2      
    #> [43] fastmap_1.2.0       grid_4.5.2          cli_3.6.5          
    #> [46] utf8_1.2.6          broom_1.0.11        e1071_1.7-17       
    #> [49] withr_3.0.2         scales_1.4.0        backports_1.5.0    
    #> [52] timechange_0.3.0    rmarkdown_2.30      lme4_1.1-38        
    #> [55] ragg_1.5.0          hms_1.1.4           evaluate_1.0.5     
    #> [58] knitr_1.51          rbibutils_2.4       rlang_1.1.6        
    #> [61] Rcpp_1.1.0          glue_1.8.0          minqa_1.2.8        
    #> [64] jsonlite_2.0.0      R6_2.6.1            systemfonts_1.3.1  
    #> [67] fs_1.6.6
