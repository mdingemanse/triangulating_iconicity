Coding, consistency & calibration
================
\[anonymized for review\]
Updated 2024-04-03

## Coding consistency

Code notebook for combining coding from two coders, working out
consistency, and recording coding/recoding decisions.

## Setup

``` r
# Packages
list.of.packages <- c("tidyverse","readxl","writexl","irr")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# useful functions
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)
```

## Data

Let’s first load the original data and generate derivative XLS files to
aid in the calibration.

``` r
d1 = read_excel("data/ideophones_coded_MD_original.xlsx") |> arrange(filename)
d2 = read_excel("data/ideophones_coded_SP_original.xlsx") |> arrange(filename)
d2 <- d2[,-c(1:7)] # remove non-unique fields except filename prior to merge

d <- merge(d1,d2,by="filename")

d.F <- d |>
  dplyr::select(c("filename","ideophone"),starts_with("F_")) |>
  dplyr::select(-starts_with("F_notes"),everything()) # put note fields at end
#write_xlsx(d.F,path="data\\ideophones_F_consistency.xlsx")

d.M <- d |>
  dplyr::select(c("filename","ideophone"),starts_with("M_")) |>
  dplyr::select(-starts_with("M_notes"),everything()) # put note fields at end
#write_xlsx(d.M,path="data\\ideophones_M_consistency.xlsx")

# function to get the names of coding categories (note: this presupposes there is a coder _MD, change as needed)
get_coding_categories <- function(data) {
  coder_1_names <- data |> dplyr::select(ends_with("_MD")) |> names()
  gsub('MD','',coder_1_names)
}
```

## Consistency

We compute coding consistency for the original coding to establish which
categories of the coding scheme are usable as is and which need
recalibration or reformulation. We use `irr` package to compute %
agreement and Cohen’s Kappa.

``` r
# build functions to make it less repetitive
get_agree <- function(data,cat) {
  # takes a dataset and a coding category, looks up fields of that coding category, and computes agreement
  agree(dplyr::select(data,starts_with(cat)))$value
}
get_kappa <- function(data,cat) {
  # takes a dataset and a coding category, looks up fields of that coding category, and computes kappa
  kappa2(dplyr::select(data,starts_with(cat)))$value
}

# sanity check: does the function work? yes
get_agree(d.F,cat='F_redup_')
```

    ## [1] 99.16318

``` r
# get list of coding categories and create dataframes for the consistency values
F_categories <- get_coding_categories(d.F)
F_consistency <- as.data.frame(F_categories) |> dplyr::rename(category = F_categories)
M_categories <- get_coding_categories(d.M)
M_consistency <- as.data.frame(M_categories) |> dplyr::rename(category = M_categories)

F_consistency$agree <- NA
F_consistency$kappa <- NA
for (i in 1:(length(F_categories)-1)) { 
  F_consistency$agree[i] <- (get_agree(data=d.F,cat=F_categories[i]))
  F_consistency$kappa[i] <- (get_kappa(data=d.F,cat=F_categories[i]))
}
F_consistency
```

    ##            category    agree     kappa
    ## 1          F_redup_ 99.16318 0.9794479
    ## 2   F_monosyllabic_ 99.16318 0.9629342
    ## 3   F_partialredup_ 95.39749 0.7078564
    ## 4 F_closedsyllable_ 99.58159 0.9912489
    ## 5   F_vowelquality_ 95.39749 0.9281635
    ## 6          F_voice_ 72.38494 0.5876941
    ## 7     F_finalvowel_ 95.81590 0.8147000
    ## 8     F_intonation_ 87.02929 0.4484068
    ## 9          F_notes_       NA        NA

``` r
mean.na(F_consistency$kappa)
```

    ## [1] 0.8025565

For Form, mean agreement in the original coding is **93%** and the
lowest Kappa values are for F_intonation, F_voice, and F_partialredup,
pointing to the need to resolve inconsistencies and sharpen those coding
categories.

``` r
M_consistency$agree <- NA
M_consistency$kappa <- NA
for (i in 1:(length(M_categories)-1)) {
  M_consistency$agree[i] <- (get_agree(data=d.M,cat=M_categories[i]))
  M_consistency$kappa[i] <- (get_kappa(data=d.M,cat=M_categories[i]))
}
M_consistency
```

    ##          category    agree     kappa
    ## 1        M_sound_ 98.74477 0.9612076
    ## 2 M_distribution_ 86.13445 0.7133367
    ## 3    M_irregular_ 84.10042 0.4503752
    ## 4       M_weight_ 59.41423 0.3496325
    ## 5     M_punctual_ 95.39749 0.7069446
    ## 6     M_durative_ 79.07950 0.2208893
    ## 7       M_abrupt_ 96.65272 0.7469561
    ## 8        M_notes_       NA        NA

``` r
mean.na(M_consistency$agree)
```

    ## [1] 85.64623

For Meaning, mean agreement in the original coding is **86%** and the
lowest Kappa values are for M_durative, M_weight, and M_irregular,
pointing to the need to resolve inconsistencies and sharpen those coding
categories.

## Outcomes

Using the consistency scores as a guide, we revisited the most
problematic categories to (i) recalibrate coders’ understanding of the
categories, (ii) reformulate instructions and (iii) in some cases rename
the coding category to reflect the revisions.

### Form

1.  *F_redupmod* was renamed (originally F_partialredup) and the coding
    instructions were sharpened to reflect the fact that we want to
    capture both partial and modified forms of reduplication.
2.  *F_voice*: coding instructions were clarified to note that this
    question privileges consonants that crosslinguistically often enter
    into voicing oppositions (plosives and fricatives) as well as
    nasals, documented for their iconic uses across languages. This is
    close to Westermann’s use of this feature. We also considered a more
    continuous version of this feature that would weight or classify
    consonants based on their place on the sonority scale, but decided
    against it because the coding was already too fine-grained.
3.  *F_finallength* was renamed (originally F_finalvowel) to capture
    word final lengthening of any final sound, whether final vowel or
    nasal, glide or trill.
4.  *F_intonation*: coding instructions were clarified to include a
    decision procedure for forms with falling or rising intonation.

### Meaning

1.  *M_irregularity*: coding instructions were modified to indicate that
    the primary semantic property here is one of irregularity in the
    sense of imbalance (a highly regular ridged pattern would not
    qualify)
2.  *M_weight*: coding instructions were sharpened to focus on binary
    oppositions of size, weight and brightness and to exclude
    Westermann’s hard/soft distinction, which we found orthogonal and
    conflicting in some cases. This also means that the middle value
    essentially means neutral or “not applicable”. It may be advisable
    to further break this up this feature, as there is no guarantee that
    size, weight and brightness pattern similarly across languages.
3.  *M_long* was renamed (originally M_durative): coding instructions
    were broadened to capture percepts that are long in time and/or
    space (in line with M_distribution), and sharpened to clarify that
    for the temporal dimension it targets only events.

The original coding sheet is available as `predico_coding_scheme_1.pdf`
and the updated coding sheet as `predico_coding_scheme_2.pdf`.

## Calibrated coding

After reformulating and revising the coding scheme, we discussed all
inconsistent cases to reach a consensus and recoded accordingly. Here we
check whether we now indeed have a canonical, ground truth set of coded
data.

``` r
d1 = read_excel("data/ideophones_coded_MD.xlsx") |> arrange(filename)
d2 = read_excel("data/ideophones_coded_SP.xlsx") |> arrange(filename)
d2 <- d2[,-c(1:7)] # remove non-unique fields except filename prior to merge

d <- merge(d1,d2,by="filename")

d.F <- d |>
  dplyr::select(c("filename","ideophone","meaning","meaning_NL"),starts_with("F_")) |>
  dplyr::select(-starts_with("F_notes"),everything()) # put note fields at end

d.M <- d |>
  dplyr::select(c("filename","ideophone","meaning","meaning_NL"),starts_with("M_")) |>
  dplyr::select(-starts_with("M_notes"),everything()) # put note fields at end

# get list of coding categories and create dataframes for the consistency values
F_categories <- get_coding_categories(d.F)
F_consistency <- as.data.frame(F_categories) |> dplyr::rename(category = F_categories)
M_categories <- get_coding_categories(d.M)
M_consistency <- as.data.frame(M_categories) |> dplyr::rename(category = M_categories)

F_consistency$agree <- NA
F_consistency$kappa <- NA
for (i in 1:(length(F_categories)-1)) { # -1 to exclude the notes column
  F_consistency$agree[i] <- (get_agree(data=d.F,cat=F_categories[i]))
  F_consistency$kappa[i] <- (get_kappa(data=d.F,cat=F_categories[i]))
}
F_consistency
```

    ##            category     agree     kappa
    ## 1          F_redup_  99.58159 0.9897684
    ## 2   F_monosyllabic_  99.16318 0.9629342
    ## 3       F_redupmod_ 100.00000 1.0000000
    ## 4 F_closedsyllable_ 100.00000 1.0000000
    ## 5   F_vowelquality_  98.74477 0.9803787
    ## 6          F_voice_ 100.00000 1.0000000
    ## 7    F_finallength_  98.32636 0.9258800
    ## 8     F_intonation_ 100.00000 1.0000000
    ## 9          F_notes_        NA        NA

A consistency check at this point shows we’re nearly there but we’re not
100% consistent on F_redup, F_monosyllabic and F_vowelquality and
F_finallength. We can export only those columns to have a quick check.
However, as we do not aim for 100% consistency, this is more for
informative purposes.

``` r
inconsistent <- c('F_redup','F_monosyllabic','F_vowelquality','F_finallength')
d.F.inconsistent <- d.F |>
  dplyr::select(filename,ideophone,meaning,meaning_NL,matches(paste(inconsistent,collapse="|"))) |>
  mutate(F_redup_C = ifelse(F_redup_MD == F_redup_SP,1,0),
         F_monosyllabic_C = ifelse(F_monosyllabic_MD == F_monosyllabic_SP,1,0),
         F_vowelquality_C = ifelse(F_vowelquality_MD == F_vowelquality_SP,1,0),
         F_finallength_C = ifelse(F_finallength_MD == F_finallength_SP,1,0)) |>
  mutate(check = F_redup_C + F_monosyllabic_C + F_vowelquality_C + F_finallength_C) |>
  filter(check < 4)
d.F.inconsistent <- d.F.inconsistent[, order(names(d.F.inconsistent))]

write_xlsx(d.F.inconsistent,path="data/ideophones_F_consistency_2ndround.xlsx")
```

### Consensus version

The final result of the coding consistency is a consensus or ground
truth file `ideophones_coded.xlsx`, to be used in the next step of the
analyses. We do this this by taking the (calibrated and revised) data
from one coder.

``` r
d.coded <- read_excel("data/ideophones_coded_SP.xlsx") |> 
  arrange(filename) |>
  dplyr::select(-matches('notes'))
names(d.coded) <- gsub('_SP','',names(d.coded))

notes <- d |> dplyr::select(filename,matches('notes'))

d.coded <- left_join(d.coded,notes)
```

## Adding congruency measures

We add congruency measures by combining F and M features as described in
the coding scheme.

``` r
# note that we overwrite the old d with the new consensus version here

# using ifelse() statements that essentially say, if these conditions are true, use 1, otherwise 0
d <- d.coded |>
  mutate(C_modality = ifelse(M_sound == 1,1,0),
         C_iterative = ifelse(F_redup == 1 & M_distribution == 1,1,0),
         C_irregular = ifelse(F_redupmod == 1 & M_irregular == 1,1,0),
         C_closure = ifelse(F_closedsyllable == 1 & M_abrupt == 1,1,0),
         C_punctual = ifelse(F_monosyllabic == 1 & M_punctual == 1,1,0),
         C_long = ifelse(F_finallength == 1 & M_long == 1,1,0),
         C_weight_voice = ifelse((F_voice == 0 & M_weight == 0) | (F_voice == 2 & M_weight == 2),1,0),
         C_weight_vowel = ifelse((F_vowelquality == 0 & M_weight == 0) | (F_vowelquality == 2 & M_weight == 2),1,0),
         C_weight_tone = ifelse((F_intonation == 0 & M_weight == 2) | (F_intonation == 2 & M_weight == 0),1,0)
         )

# some mappings are mutually exclusive, so we combine them

#sanity check: are they indeed mutually exclusive? these calls should come up empty
d |> 
  mutate(C_aspect = C_iterative + C_punctual) |>
  dplyr::select(ideophone,meaning_NL,language,C_iterative,C_punctual,C_aspect) |>
  filter(C_aspect > 1)
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: ideophone <chr>, meaning_NL <chr>, language <chr>,
    ## #   C_iterative <dbl>, C_punctual <dbl>, C_aspect <dbl>

``` r
d |>
  mutate(C_length = C_long + C_closure) |>
  dplyr::select(ideophone,meaning_NL,language,C_iterative,C_closure,C_length) |>
  filter(C_length > 1)
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: ideophone <chr>, meaning_NL <chr>, language <chr>,
    ## #   C_iterative <dbl>, C_closure <dbl>, C_length <dbl>

``` r
d <- d |>
  mutate(C_aspect = C_iterative + C_punctual,
         C_length = C_long + C_closure)


# C_weight_ subfeatures are linked to one and the same meaning feature M_weight so they are recombined into C_magnitude
# (note: this may be subject to revision, as the subfeatures may not deserve equal weight)

d <- d |>
  mutate(C_weight = C_weight_voice + C_weight_vowel + C_weight_tone,
         C_magnitude = ifelse(C_weight == 0,0,1)) # i.e. a magnitude value that is 1 if any weight feature was congruent


# add cumulative iconicity scores (and some others)
# any1 = 1 if an ideophone has at least 1 congruency feature of any type
# atleast2 = if an ideophone has at least 2 congruency features of any type
# ternary = simple way of conflating the (smaller) upper regions: 1=1, 2=2, >2 = 3
d <- d |>
  mutate(C_cumulative = C_modality + C_aspect + C_length + C_irregular + C_magnitude) |>
  mutate(C_any1 = ifelse(C_cumulative == 0,0,1),
         C_atleast2 = ifelse(C_cumulative > 1,1,0),
         C_ternary = ifelse(C_cumulative == 0,0,
                            ifelse(C_cumulative == 1,1,
                                   ifelse(C_cumulative == 2,2,3)))) # i.e. 1, 2 or 3
```

Finally, we write this to XLSX and CSV files. (Commented out here to
avoid overwriting anytime we knit the markdown.)

``` r
#write_xlsx(d,path="data\\ideophones_coded.xlsx")
#write.csv(d,file="data\\ideophones_coded.csv")
```
