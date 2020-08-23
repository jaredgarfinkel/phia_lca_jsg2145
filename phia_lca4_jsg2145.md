phia\_lca4\_jsg2145
================
Jared Garfinkel
8/21/2020

## read in data

``` r
biomarker <- read_csv(file = "./data/Shims22016adultbio.csv",
                      col_names = TRUE, 
                      col_types = NULL,
                      locale = default_locale(), 
                      na = c("", "NA"), 
                      quoted_na = TRUE,
                      quote = "\"", 
                      comment = "", 
                      trim_ws = TRUE, 
                      skip = 0,
                      n_max = Inf, 
                      progress = show_progress(), 
                      skip_empty_rows = TRUE)
```

``` r
bio_dat <- biomarker %>% 
  filter(hiv1statusfinalsurvey == 1) %>%
  select(personid, 
         resultvlc) %>%
  mutate(resultvlc = recode(resultvlc, 
                    "< LLOD" = "1",
                    "< LLOQ: 20" = "20",
                    "< LLOQ: 40" = "40",
                    "< LLOQ: 400" = "400",
                    "< LLOQ: 839" = "839",
                    "> ULOQ 10000000" = "10000000"),
         resultvlc = as.numeric(resultvlc),
         vlunder200 = if_else(resultvlc < 200, 1, 2),
         across(.funs = as_factor))
```

The biomarker data set is 3055 observations of 3 variables, “personid”,
“resultvlc”, and “vlunder200”.

It was filtered on hiv1statusfinalsurvey = 1.

``` r
# Read public-release datasets
# Independent dataset
# hivstatusfinal not filtered
independent <- read_csv(file = "./data/Shims22016adultind.csv") %>% 
  filter(age > 14,
         bt_status == 1,
         hivstatusfinal == 1) 
```

After including only those who are 15 years old and over who had a blood
test and were HIV+, the independent data set is 3003 observations of 750
variables. We select only those related to the coding of
“awareselfreported” variable and “personid.”

``` r
ind_dat_lca = independent %>%
  select(personid, known_hiv_status, hivtstever, hivtstrslt, hivpsbp, hivrtpg, hivrslr) %>% 
  tibble() %>% 
  mutate(across(.fns = as.numeric),
         across(everything(), ~recode(.x, "1" = 1, 
                                      "2" = 0, 
                                      "99" = 0, 
                                      "-8" = 0, 
                                      "3" = 0, 
                                      "4" = 0,
                                      "-9" = 0)),
         across(everything(), ~replace_na(.x, 0)))

# ind_dat_lca
# independent %>% 
#   filter(gender == 1) %>% 
#   nrow()
```

## suspect zero inflated data in pregnancy related questions (hivpsbp, hivrtpg, hivrslr)

Because the variables hivpsbp, hivrtpg and hivrslr are related to
pregnancy, there are 972 men respondents who are not asked these
questions. This is problematic because these patients are not 0s, but
ineligible for the outcome. This results in zero-inflated poisson data.

``` r
skimr::skim(ind_dat_lca)
```

|                                                  |               |
| :----------------------------------------------- | :------------ |
| Name                                             | ind\_dat\_lca |
| Number of rows                                   | 3003          |
| Number of columns                                | 7             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| numeric                                          | 7             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: numeric**

| skim\_variable     | n\_missing | complete\_rate | mean |   sd | p0 | p25 | p50 | p75 | p100 | hist  |
| :----------------- | ---------: | -------------: | ---: | ---: | -: | --: | --: | --: | ---: | :---- |
| personid           |          0 |              1 | 0.00 | 0.00 |  0 |   0 |   0 |   0 |    0 | ▁▁▇▁▁ |
| known\_hiv\_status |          0 |              1 | 0.86 | 0.34 |  0 |   1 |   1 |   1 |    1 | ▁▁▁▁▇ |
| hivtstever         |          0 |              1 | 0.97 | 0.16 |  0 |   1 |   1 |   1 |    1 | ▁▁▁▁▇ |
| hivtstrslt         |          0 |              1 | 0.86 | 0.35 |  0 |   1 |   1 |   1 |    1 | ▁▁▁▁▇ |
| hivpsbp            |          0 |              1 | 0.12 | 0.32 |  0 |   0 |   0 |   0 |    1 | ▇▁▁▁▁ |
| hivrtpg            |          0 |              1 | 0.03 | 0.17 |  0 |   0 |   0 |   0 |    1 | ▇▁▁▁▁ |
| hivrslr            |          0 |              1 | 0.00 | 0.02 |  0 |   0 |   0 |   0 |    1 | ▇▁▁▁▁ |

# lca

``` r
set.seed(719)
ind_lca2 = randomLCA::randomLCA(ind_dat_lca, calcSE = TRUE)
lca_probs_ind2 = outcomeProbs(ind_lca2)
```

``` r
set.seed(719)
aware_lca_r = randomLCA::randomLCA(ind_dat_lca, random = TRUE, calcSE = TRUE)
lca_probs_r = outcomeProbs(aware_lca_r, boot = TRUE)
# summary(aware_lca_r)
lca_probs_r
```

    ## Class  1 
    ##                     Outcome p        2.5 %       97.5 %
    ## personid         9.664293e-07 4.112092e-07 2.188224e-06
    ## known_hiv_status 9.999990e-01 9.999979e-01 9.999996e-01
    ## hivtstever       9.996138e-01 3.477012e-01 9.999922e-01
    ## hivtstrslt       9.969173e-01 9.887489e-01 9.988466e-01
    ## hivpsbp          1.332827e-01 1.219336e-01 1.466603e-01
    ## hivrtpg          3.466934e-02 2.836531e-02 4.265535e-02
    ## hivrslr          3.861506e-04 6.722377e-06 3.665570e-01
    ## Class  2 
    ##                     Outcome p        2.5 %       97.5 %
    ## personid         6.129686e-06 5.525539e-06 6.657598e-06
    ## known_hiv_status 6.144217e-06 4.897194e-08 1.123715e-05
    ## hivtstever       8.034360e-01 7.648350e-01 8.361495e-01
    ## hivtstrslt       6.129789e-06 3.295263e-06 1.069449e-05
    ## hivpsbp          6.144143e-06 5.551027e-06 6.688241e-06
    ## hivrtpg          6.129685e-06 2.526507e-06 1.370821e-05
    ## hivrslr          6.129685e-06 2.563470e-06 1.351162e-05

### extract classes

``` r
set.seed(719)
lca_out_ind = postClassProbs(aware_lca_r) %>% 
  janitor::clean_names() %>% 
  mutate(class = if_else(class_1 > class_2, 1, 0))
```

``` r
ind_dat_out = ind_dat_lca %>% 
  left_join(lca_out_ind, copy = TRUE, by = c("known_hiv_status", "hivtstever", "hivtstrslt", "hivpsbp", "hivrtpg", "hivrslr"))
```

``` r
xtabs(~hivpsbp + class, data = ind_dat_out)
```

    ##        class
    ## hivpsbp    0    1
    ##       0  407 2250
    ##       1    0  346

There are 2250 false negatives in the hivpsbp variable. The sensitivity
is therefore 346/2596, 0.13.

# dealing with zero-inflated poisson data

## filter for women only??

This data only include women to reduce the zero inflation given that
more women will have been asked about pregnancy. However, only a
fraction of women have been pregnant.

``` r
ind_w <- read_csv(file = "./data/Shims22016adultind.csv") %>% 
  filter(age > 14,
         bt_status == 1,
         hivstatusfinal == 1,
         gender == 2) %>% 
  select(known_hiv_status, hivtstever, hivtstrslt, hivpsbp, hivrtpg, hivrslr)
```

There are 2031 observations in our women only data set.

``` r
ind_dat_w_lca = ind_w %>%
  tibble() %>% 
  mutate(across(.fns = as.numeric),
         across(everything(), ~recode(.x, "1" = 1, 
                                      "2" = 0, 
                                      "99" = 0, 
                                      "-8" = 0, 
                                      "3" = 0, 
                                      "4" = 0,
                                      "-9" = 0)),
         across(everything(), ~replace_na(.x, 0)))

skimr::skim(ind_dat_w_lca)
```

|                                                  |                  |
| :----------------------------------------------- | :--------------- |
| Name                                             | ind\_dat\_w\_lca |
| Number of rows                                   | 2031             |
| Number of columns                                | 6                |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                  |
| Column type frequency:                           |                  |
| numeric                                          | 6                |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                  |
| Group variables                                  | None             |

Data summary

**Variable type: numeric**

| skim\_variable     | n\_missing | complete\_rate | mean |   sd | p0 | p25 | p50 | p75 | p100 | hist  |
| :----------------- | ---------: | -------------: | ---: | ---: | -: | --: | --: | --: | ---: | :---- |
| known\_hiv\_status |          0 |              1 | 0.89 | 0.31 |  0 |   1 |   1 |   1 |    1 | ▁▁▁▁▇ |
| hivtstever         |          0 |              1 | 0.99 | 0.11 |  0 |   1 |   1 |   1 |    1 | ▁▁▁▁▇ |
| hivtstrslt         |          0 |              1 | 0.89 | 0.31 |  0 |   1 |   1 |   1 |    1 | ▁▁▁▁▇ |
| hivpsbp            |          0 |              1 | 0.17 | 0.38 |  0 |   0 |   0 |   0 |    1 | ▇▁▁▁▂ |
| hivrtpg            |          0 |              1 | 0.04 | 0.21 |  0 |   0 |   0 |   0 |    1 | ▇▁▁▁▁ |
| hivrslr            |          0 |              1 | 0.00 | 0.02 |  0 |   0 |   0 |   0 |    1 | ▇▁▁▁▁ |

### run LCA without random effects

``` r
set.seed(719)
ind_lca2w = randomLCA::randomLCA(ind_dat_w_lca, calcSE = TRUE)
lca_probs_ind2w = outcomeProbs(ind_lca2w)
```

### add random effects

``` r
set.seed(719)
aware_lca_rw = randomLCA::randomLCA(ind_dat_w_lca, random = TRUE, calcSE = TRUE)
lca_probs_rw = outcomeProbs(aware_lca_rw, boot = TRUE)
# summary(aware_lca_r)
lca_probs_rw
```

    ## Class  1 
    ##                     Outcome p        2.5 %    97.5 %
    ## known_hiv_status 0.9999986113 9.999701e-01 0.9999998
    ## hivtstever       0.9994479601 3.566326e-01 0.9999921
    ## hivtstrslt       0.9955933667 9.837255e-01 0.9983969
    ## hivpsbp          0.1905292154 1.728760e-01 0.2081651
    ## hivrtpg          0.0495606341 4.087971e-02 0.0607979
    ## hivrslr          0.0005521227 5.794568e-06 0.7101342
    ## Class  2 
    ##                     Outcome p        2.5 %       97.5 %
    ## known_hiv_status 1.152809e-05 1.670383e-08 2.117951e-05
    ## hivtstever       8.976639e-01 8.449721e-01 9.303900e-01
    ## hivtstrslt       1.157974e-05 4.124351e-06 4.891633e-05
    ## hivpsbp          1.152831e-05 4.088271e-06 4.850129e-05
    ## hivrtpg          1.158003e-05 3.049110e-06 6.234327e-05
    ## hivrslr          1.158003e-05 4.128323e-06 4.894538e-05

### extract classes

``` r
set.seed(719)
lca_out_ind_w = postClassProbs(aware_lca_rw) %>% 
  janitor::clean_names() %>% 
  mutate(class = if_else(class_1 > class_2, 1, 0))
```

``` r
ind_dat_out_w = ind_dat_w_lca %>% 
  left_join(lca_out_ind_w, copy = TRUE, by = c("known_hiv_status", "hivtstever", "hivtstrslt", "hivpsbp", "hivrtpg", "hivrslr"))
```

``` r
xtabs(~hivpsbp + class, data = ind_dat_out_w)
```

    ##        class
    ## hivpsbp    0    1
    ##       0  215 1470
    ##       1    0  346

#### There were 1470 false negatives in hivpsbp. The sensitivity of this test is 346/1816, 0.19, slightly higher, but still practically uninformative.

### The same analysis without random effects

``` r
set.seed(719)
lca_out_ind_w2 = postClassProbs(ind_lca2w) %>% 
  janitor::clean_names() %>% 
  mutate(class = if_else(class_1 > class_2, 1, 0))
```

``` r
ind_dat_out_w2 = ind_dat_w_lca %>% 
  left_join(lca_out_ind_w2, copy = TRUE, by = c("known_hiv_status", "hivtstever", "hivtstrslt", "hivpsbp", "hivrtpg", "hivrslr"))
```

``` r
xtabs(~hivpsbp + class, data = ind_dat_out_w2)
```

    ##        class
    ## hivpsbp    0    1
    ##       0  215 1470
    ##       1    0  346

## transform the pregnancy variables?

### There are three questions related to pregnancy included in this model.

``` r
pgyes = ind_dat_w_lca %>%
  filter(hivpsbp == 1 | hivrtpg == 1 | hivrslr == 1) # 437 rows
```

#### There are 437 patients who had hiv results from one of these three variables.

### We combine the three variables into one.

``` r
ind_dat_w_lca2 = ind_dat_w_lca %>% 
  mutate(hivpg = if_else(hivpsbp == 1 | hivrtpg == 1 | hivrslr == 1, 1, 0)) %>% 
  select(-hivpsbp, -hivrtpg, -hivrslr)
```

``` r
set.seed(719)
ind_lca3 = randomLCA::randomLCA(ind_dat_w_lca2, calcSE = TRUE)
lca_probs_ind3 = outcomeProbs(ind_lca3)
```

``` r
set.seed(719)
aware_lca_r3 = randomLCA::randomLCA(ind_dat_w_lca2, random = TRUE, calcSE = TRUE)
lca_probs_r3 = outcomeProbs(aware_lca_r3, boot = TRUE)
# summary(aware_lca_r)
lca_probs_r3
```

    ## Class  1 
    ##                     Outcome p        2.5 %       97.5 %
    ## known_hiv_status 1.160260e-05 8.865961e-09 4.463415e-05
    ## hivtstever       8.976651e-01 8.447718e-01 9.301263e-01
    ## hivtstrslt       1.161160e-05 2.665465e-06 5.381860e-05
    ## hivpg            1.160269e-05 2.993379e-06 5.412988e-05
    ## Class  2 
    ##                  Outcome p     2.5 %    97.5 %
    ## known_hiv_status 0.9999986 0.9999890 0.9999997
    ## hivtstever       0.9994480 0.4996814 0.9999921
    ## hivtstrslt       0.9955933 0.8685787 0.9996058
    ## hivpg            0.2406393 0.2190586 0.2615708

### extract classes

``` r
set.seed(719)
lca_out_ind3 = postClassProbs(aware_lca_r3) %>% 
  janitor::clean_names() %>% 
  mutate(class = if_else(class_1 > class_2, 0, 1))
```

``` r
ind_dat_out3 = ind_dat_w_lca2 %>% 
  left_join(lca_out_ind3, copy = TRUE, by = c("known_hiv_status", "hivtstever", "hivtstrslt", "hivpg"))
```

``` r
xtabs(~hivpg + class, data = ind_dat_out3)
```

    ##      class
    ## hivpg    0    1
    ##     0  215 1379
    ##     1    0  437

#### There are 1379 false negatives. The sensitivity is therefore 437/1816, 0.24. Still better, but not useful.

## remove the pregnancy variables, add back vlunder200?

``` r
full_df = left_join(bio_dat, independent, by = "personid")

full_df_lca = full_df %>% 
  mutate(across(.fns = as.numeric),
       across(everything(), ~recode(.x, "1" = 1, 
                                    "2" = 0, 
                                    "99" = 0, 
                                    "-8" = 0, 
                                    "3" = 0, 
                                    "4" = 0,
                                    "-9" = 0)),
       across(everything(), ~replace_na(.x, 0))) %>% 
  select(known_hiv_status, hivtstever, hivtstrslt, vlunder200)
```

#### Our full dataset has 3055 rows and 4 variables. Three of these variables we keep from our previous model and 1 is added from the biomarker dataset. “vlunder200” is suspected to be a proxy for awareness because most of those who are aware will be on art in Eswatini.

### no random effects

``` r
set.seed(719)
expanded_lca = randomLCA::randomLCA(full_df_lca, calcSE = TRUE)
lca_probs_ind2w = outcomeProbs(expanded_lca)
```

### include random effects

``` r
set.seed(719)
aware_lca_full = randomLCA::randomLCA(full_df_lca, random = TRUE, calcSE = TRUE)
lca_probs_full = outcomeProbs(aware_lca_full, boot = TRUE)
# summary(aware_lca_r)
lca_probs_full
```

    ## Class  1 
    ##                     Outcome p        2.5 %       97.5 %
    ## known_hiv_status 5.528986e-03 1.498788e-04 7.133662e-01
    ## hivtstever       7.118378e-01 6.728008e-01 7.458302e-01
    ## hivtstrslt       5.415665e-06 4.870004e-06 5.974019e-06
    ## vlunder200       2.306198e-01 1.932576e-01 2.730716e-01
    ## Class  2 
    ##                  Outcome p     2.5 %    97.5 %
    ## known_hiv_status 0.9999990 0.9999990 0.9999991
    ## hivtstever       0.9999990 0.9999990 0.9999990
    ## hivtstrslt       0.9978973 0.9735050 0.9996860
    ## vlunder200       0.7937521 0.7774083 0.8086553

### extract classes

``` r
set.seed(719)
lca_out_full = postClassProbs(aware_lca_full) %>% 
  janitor::clean_names() %>% 
  mutate(class = if_else(class_1 > class_2, 0, 1))
```

``` r
full_dat_out = full_df_lca %>% 
  left_join(lca_out_full, copy = TRUE, by = c("known_hiv_status", "hivtstever", "hivtstrslt", "vlunder200"))
```

``` r
xtabs(~vlunder200 + class, data = full_dat_out)
```

    ##           class
    ## vlunder200    0    1
    ##          0  356  534
    ##          1  106 2059

#### There are 534 false negatives in “vlunder200”. The sensitivity is 2059/2593, 0.79, a significant improvement. Including a 4th variable allows one to use the random effects option of the randomLCA package in R.

### extract classes without random effects

``` r
set.seed(719)
lca_out_exp = postClassProbs(expanded_lca) %>% 
  janitor::clean_names() %>% 
  mutate(class = if_else(class_1 > class_2, 0, 1))
```

``` r
exp_dat_out = full_df_lca %>% 
  left_join(lca_out_exp, copy = TRUE, by = c("known_hiv_status", "hivtstever", "hivtstrslt", "vlunder200"))
```

``` r
xtabs(~vlunder200 + class, data = exp_dat_out)
```

    ##           class
    ## vlunder200    0    1
    ##          0  356  534
    ##          1  106 2059
