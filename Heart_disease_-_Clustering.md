R Notebook
================

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.3

``` r
setwd("C:/Users/J/Documents/R Projects/Data/heart-disease-uci")
heart_disease <- read.csv('heart.csv', sep=',')

#check for all variables are numeric
lapply(heart_disease, min)
```

    ## $ï..age
    ## [1] 29
    ## 
    ## $sex
    ## [1] 0
    ## 
    ## $cp
    ## [1] 0
    ## 
    ## $trestbps
    ## [1] 94
    ## 
    ## $chol
    ## [1] 126
    ## 
    ## $fbs
    ## [1] 0
    ## 
    ## $restecg
    ## [1] 0
    ## 
    ## $thalach
    ## [1] 71
    ## 
    ## $exang
    ## [1] 0
    ## 
    ## $oldpeak
    ## [1] 0
    ## 
    ## $slope
    ## [1] 0
    ## 
    ## $ca
    ## [1] 0
    ## 
    ## $thal
    ## [1] 0
    ## 
    ## $target
    ## [1] 0

``` r
summary(heart_disease)
```

    ##      ï..age           sex               cp           trestbps    
    ##  Min.   :29.00   Min.   :0.0000   Min.   :0.000   Min.   : 94.0  
    ##  1st Qu.:47.50   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:120.0  
    ##  Median :55.00   Median :1.0000   Median :1.000   Median :130.0  
    ##  Mean   :54.37   Mean   :0.6832   Mean   :0.967   Mean   :131.6  
    ##  3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:140.0  
    ##  Max.   :77.00   Max.   :1.0000   Max.   :3.000   Max.   :200.0  
    ##       chol            fbs            restecg          thalach     
    ##  Min.   :126.0   Min.   :0.0000   Min.   :0.0000   Min.   : 71.0  
    ##  1st Qu.:211.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:133.5  
    ##  Median :240.0   Median :0.0000   Median :1.0000   Median :153.0  
    ##  Mean   :246.3   Mean   :0.1485   Mean   :0.5281   Mean   :149.6  
    ##  3rd Qu.:274.5   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:166.0  
    ##  Max.   :564.0   Max.   :1.0000   Max.   :2.0000   Max.   :202.0  
    ##      exang           oldpeak         slope             ca        
    ##  Min.   :0.0000   Min.   :0.00   Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:1.000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.80   Median :1.000   Median :0.0000  
    ##  Mean   :0.3267   Mean   :1.04   Mean   :1.399   Mean   :0.7294  
    ##  3rd Qu.:1.0000   3rd Qu.:1.60   3rd Qu.:2.000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :6.20   Max.   :2.000   Max.   :4.0000  
    ##       thal           target      
    ##  Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median :2.000   Median :1.0000  
    ##  Mean   :2.314   Mean   :0.5446  
    ##  3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :3.000   Max.   :1.0000

``` r
#check for duplicates
dim(heart_disease)
```

    ## [1] 303  14

``` r
heart_disease <- heart_disease[!duplicated(heart_disease),]
dim(heart_disease)
```

    ## [1] 302  14

``` r
# Remove id
heart_disease = heart_disease[ , !(names(heart_disease) %in% c("id"))]
#renaming age variable
names(heart_disease)[1] <-"age"
names(heart_disease)  
```

    ##  [1] "age"      "sex"      "cp"       "trestbps" "chol"     "fbs"     
    ##  [7] "restecg"  "thalach"  "exang"    "oldpeak"  "slope"    "ca"      
    ## [13] "thal"     "target"

``` r
scaled = scale(heart_disease,center = TRUE, scale = TRUE)

summary(scaled)
```

    ##       age               sex                cp              trestbps       
    ##  Min.   :-2.8095   Min.   :-1.4624   Min.   :-0.93366   Min.   :-2.14097  
    ##  1st Qu.:-0.7096   1st Qu.:-1.4624   1st Qu.:-0.93366   1st Qu.:-0.66061  
    ##  Median : 0.1193   Median : 0.6815   Median : 0.03529   Median :-0.09125  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.00000  
    ##  3rd Qu.: 0.7272   3rd Qu.: 0.6815   3rd Qu.: 1.00424   3rd Qu.: 0.47812  
    ##  Max.   : 2.4955   Max.   : 0.6815   Max.   : 1.97320   Max.   : 3.89431  
    ##       chol              fbs             restecg           thalach       
    ##  Min.   :-2.3283   Min.   :-0.4178   Min.   :-1.0009   Min.   :-3.4305  
    ##  1st Qu.:-0.6859   1st Qu.:-0.4178   1st Qu.:-1.0009   1st Qu.:-0.7125  
    ##  Median :-0.1159   Median :-0.4178   Median : 0.9002   Median : 0.1279  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.5459   3rd Qu.:-0.4178   3rd Qu.: 0.9002   3rd Qu.: 0.7174  
    ##  Max.   : 6.1349   Max.   : 2.3858   Max.   : 2.8012   Max.   : 2.2892  
    ##      exang            oldpeak            slope               ca         
    ##  Min.   :-0.6972   Min.   :-0.8981   Min.   :-2.2674   Min.   :-0.7137  
    ##  1st Qu.:-0.6972   1st Qu.:-0.8981   1st Qu.:-0.6448   1st Qu.:-0.7137  
    ##  Median :-0.6972   Median :-0.2093   Median :-0.6448   Median :-0.7137  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 1.4296   3rd Qu.: 0.4795   3rd Qu.: 0.9779   3rd Qu.: 0.2796  
    ##  Max.   : 1.4296   Max.   : 4.4401   Max.   : 0.9779   Max.   : 3.2595  
    ##       thal             target       
    ##  Min.   :-3.7756   Min.   :-1.0883  
    ##  1st Qu.:-0.5131   1st Qu.:-1.0883  
    ##  Median :-0.5131   Median : 0.9158  
    ##  Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 1.1181   3rd Qu.: 0.9158  
    ##  Max.   : 1.1181   Max.   : 0.9158

``` r
############ Kmeans

set.seed(123)


first_clust = kmeans(scaled, centers = 5, nstart = 1)
first_clust$centers
```

    ##          age        sex         cp    trestbps        chol        fbs
    ## 1  0.7216503 -0.3189916  0.9719458  0.62140709  0.31946317  0.5167758
    ## 2  0.6032578 -0.2280355 -0.6987611  0.44533496  0.51965462  0.8566044
    ## 3 -0.7495207 -0.1720903  0.1698695 -0.42443395 -0.21773447 -0.3139164
    ## 4  0.1263900  0.4341445 -0.6230970 -0.09343926 -0.02997444 -0.3458661
    ## 5  0.3427520  0.4950933 -0.2174770  0.04985436 -0.45491577  0.0698273
    ##       restecg    thalach       exang    oldpeak       slope         ca
    ## 1 -0.36719916  0.1228543 -0.41361741 -0.1863010  0.08543117 -0.2998529
    ## 2 -0.42480650 -0.1942201  0.20508014  0.6021581 -0.39890673  1.6942660
    ## 3  0.30168616  0.6910993 -0.48057137 -0.5990983  0.52715389 -0.4654024
    ## 4  0.02275828 -0.7307260  0.88425934  0.5667350 -0.47833739  0.1012863
    ## 5  0.07362229 -0.8088736  0.04255995  0.5132230 -0.50366311  0.1931967
    ##         thal     target
    ## 1 -0.1325168  0.5483699
    ## 2  0.5743596 -0.8454094
    ## 3 -0.3469964  0.8044528
    ## 4  0.8671487 -0.9855582
    ## 5 -1.7897757 -0.6526541

``` r
set.seed(1234)
second_clust = kmeans(scaled, centers = 5, nstart = 1)
second_clust$centers
```

    ##           age        sex          cp    trestbps        chol        fbs
    ## 1  0.39901100 -1.3305028  0.24399012 -0.01329004  0.50520563 -0.1158282
    ## 2 -0.83377673  0.3639007  0.08314237 -0.44481866 -0.38966777 -0.2793042
    ## 3  0.13772558  0.5974480  1.23223273  0.38098949 -0.26464068  0.5167758
    ## 4  0.68196286  0.1455340 -0.80152863  0.35389132  0.29993586  0.4105794
    ## 5  0.07491524  0.3300555 -0.82246722  0.03102430 -0.01599639 -0.2339112
    ##       restecg    thalach      exang    oldpeak      slope         ca
    ## 1 -0.09422895  0.0946983 -0.4681500 -0.4437152  0.4037212 -0.3469708
    ## 2  0.43076930  0.7551091 -0.5133919 -0.6631412  0.6773997 -0.4194164
    ## 3 -0.32992381  0.2773386 -0.3635757  0.2752577 -0.4220462 -0.1294343
    ## 4 -0.48241385 -0.4406423  0.4145350  0.8024037 -0.5341280  1.6566411
    ## 5  0.15221136 -1.0176267  1.1855298  0.5444593 -0.5915617 -0.1600857
    ##         thal      target
    ## 1 -0.5382388  0.79246226
    ## 2 -0.1909197  0.61888545
    ## 3  0.1265646  0.05126776
    ## 4  0.3395580 -1.04278556
    ## 5  0.4763062 -0.95691573

``` r
heart_disease[,"first_clust"] = first_clust$cluster
heart_disease[,"second_clust"] = second_clust$cluster


plot_one = ggplot(heart_disease, aes(x =age, y = chol, color = as.factor(first_clust))) + 
  geom_point()
plot_one 
```

![](Heart_disease_-_Clustering_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
plot_two = ggplot(heart_disease, aes(x = age, y = chol, color = as.factor(second_clust))) +  geom_point()
plot_two
```

![](Heart_disease_-_Clustering_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
############ Hierarchical clustering

###### Complete linkage
hier_clust_1 = hclust(dist(scaled), method= "complete")

#dendrogram
plot(hier_clust_1)
```

![](Heart_disease_-_Clustering_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# pruning
hc_1_assign <- cutree(hier_clust_1, 5)

######## Single linkage

hier_clust_2 = hclust(dist(scaled), method = "single")
plot(hier_clust_2)
```

![](Heart_disease_-_Clustering_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# Getting cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, 5)

# Adding cluster assignments
heart_disease['hc_clust'] = hc_1_assign

# remove sex, first_clust, second_clust
hd_simple = heart_disease[, !(names(heart_disease) %in% c("sex","first_clust","second_clust"))]

# summary stats
clust_summary = do.call(data.frame, aggregate(. ~ hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary
```

    ##   hc_clust  age.avg   age.sd    cp.avg     cp.sd trestbps.avg trestbps.sd
    ## 1        1 56.78512 8.196754 0.5702479 1.0068635     133.5289    17.86434
    ## 2        2 47.59184 7.547563 1.4183673 0.8958492     124.8265    12.40388
    ## 3        3 59.26866 7.316611 1.2089552 0.9775869     133.5970    17.40956
    ## 4        4 62.00000 5.567764 0.6666667 1.1547005     133.0000    17.52142
    ## 5        5 57.15385 5.535434 0.0000000 0.0000000     154.1538    24.46033
    ##   chol.avg  chol.sd    fbs.avg    fbs.sd restecg.avg restecg.sd
    ## 1 246.5785 42.28214 0.12396694 0.3309141   0.4380165  0.5146611
    ## 2 233.5714 44.02225 0.02040816 0.1421189   0.7244898  0.4490680
    ## 3 252.1045 53.84690 0.32835821 0.4731602   0.4328358  0.5286885
    ## 4 460.0000 90.07219 0.00000000 0.0000000   0.0000000  0.0000000
    ## 5 265.0769 55.05976 0.46153846 0.5188745   0.4615385  0.7762500
    ##   thalach.avg thalach.sd exang.avg  exang.sd oldpeak.avg oldpeak.sd
    ## 1    138.1074  22.773816 0.5537190 0.4991729   1.4280992  1.1230333
    ## 2    164.6837  16.069178 0.1530612 0.3618977   0.5193878  0.7659854
    ## 3    149.9701  19.986336 0.1044776 0.3081877   0.5835821  0.6756808
    ## 4    154.6667   5.033223 0.3333333 0.5773503   2.5000000  1.3076697
    ## 5    139.0769  19.133137 0.6923077 0.4803845   3.4384615  1.4233402
    ##   slope.avg  slope.sd    ca.avg     ca.sd thal.avg   thal.sd target.avg
    ## 1 1.1983471 0.5263053 1.0413223 1.0115722 2.611570 0.5968235  0.1157025
    ## 2 1.6122449 0.5856119 0.3163265 0.8920836 2.153061 0.4391191  0.9387755
    ## 3 1.6567164 0.5090764 0.5820896 0.8375515 1.940299 0.5471856  0.8507463
    ## 4 1.0000000 0.0000000 1.6666667 1.5275252 3.000000 0.0000000  0.3333333
    ## 5 0.3846154 0.5063697 1.2307692 1.2351684 2.538462 0.7762500  0.0000000
    ##   target.sd
    ## 1 0.3211978
    ## 2 0.2409742
    ## 3 0.3590278
    ## 4 0.5773503
    ## 5 0.0000000

``` r
# Plotting age and chol
plot_one = ggplot(heart_disease, aes(x = age, y = chol, color = as.factor(hc_clust))) + 
  geom_point()
plot_one 
```

![](Heart_disease_-_Clustering_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Plotting oldpeak and trestbps
plot_two = ggplot(heart_disease, aes(x= oldpeak , y= trestbps, color = as.factor(hc_clust))) + 
  geom_point()
plot_two
```

![](Heart_disease_-_Clustering_files/figure-markdown_github/unnamed-chunk-5-2.png)
