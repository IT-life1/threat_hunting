# Практическая работа 006
IT-life1@yandex.ru

# Исследование вредоносной активности в домене Windows

## Цель работы

1.  Закрепить навыки исследования данных журнала Windows Active
    Directory
2.  Изучить структуру журнала системы Windows Active Directory
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Операционная система: Windows 10
2.  Среда разработки: RStudio
3.  Версия интерпретатора R: 4.5.1

## Ход работы

1.  Импортируем данные –
    https://storage.yandexcloud.net/iamcth-data/dataset.tar.gz. и
    https://learn.microsoft.com/en-us/windows-server/identity/adds/
    plan/appendix-l–events-to-monitor; подготовим данные для дальнейшего
    анализа.

2.  Проведем анализ датасетов с точками доступа: 2.1 Раскройте датафрейм
    избавившись от вложенных датафреймов. Для обнаружения таких можно
    использовать функцию dplyr::glimpse() , а для раскрытия вложенности
    – tidyr::unnest() . Обратите внимание, что при раскрытии теряются
    внешние названия колонок – это можно предотвратить если использовать
    параметр tidyr::unnest(…, names_sep = ). 2.2 Минимизируйте
    количество колонок в датафрейме – уберите колоки с единственным
    значением параметра. 2.3 Какое количество хостов представлено в
    данном датасете? 2.4 Подготовьте датафрейм с расшифровкой Windows
    Event_ID, приведите типы данных к типу их значений. 2.5 Есть ли в
    логе события с высоким и средним уровнем значимости? Сколько их?

### Шаг 1

#### Импортируем исходный датасет

![](./imgs/img1.png)

Установим необходимые библиотеки

``` r
library(jsonlite)
```

    Warning: package 'jsonlite' was built under R version 4.5.2

``` r
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.5.2

    Warning: package 'ggplot2' was built under R version 4.5.2

    Warning: package 'readr' was built under R version 4.5.2

    Warning: package 'dplyr' was built under R version 4.5.2

    Warning: package 'forcats' was built under R version 4.5.2

    Warning: package 'lubridate' was built under R version 4.5.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.1.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter()  masks stats::filter()
    ✖ purrr::flatten() masks jsonlite::flatten()
    ✖ dplyr::lag()     masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(readr)
library(janitor)
```

    Warning: package 'janitor' was built under R version 4.5.2


    Attaching package: 'janitor'

    The following objects are masked from 'package:stats':

        chisq.test, fisher.test

``` r
library(jsonlite)
library(dplyr)
library(stringr)
tar_path <- "dataset.tar.gz"

temp_dir <- tempdir()
untar(
  tarfile = tar_path,
  exdir = temp_dir
)

json_files <- list.files(temp_dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
json_path <- file.path(temp_dir, "caldera_attack_evals_round1_day1_2019-10-20201108.json")
events <- stream_in(file(json_path), verbose = FALSE)
```

#### Подготовим и импортируем коды журнала Windows

``` r
library(xml2)
```

    Warning: package 'xml2' was built under R version 4.5.2

``` r
library(rvest)
```

    Warning: package 'rvest' was built under R version 4.5.2


    Attaching package: 'rvest'

    The following object is masked from 'package:readr':

        guess_encoding

``` r
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

webpage_url <- "https://learn.microsoft.com/en-us/windows-server/identity/ad-ds/plan/appendix-l--events-to-monitor"
webpage <- read_html(webpage_url)
event_df_raw <- html_table(webpage)[[1]]
event_df <- event_df_raw %>%
  mutate(
    `Current Windows Event ID` = as.numeric(`Current Windows Event ID`),
    `Legacy Windows Event ID` = as.numeric(`Legacy Windows Event ID`),
    `Potential criticality` = as.character(`Potential Criticality`),
    `Event Summary` = as.character(`Event Summary`)
  )
```

    Warning: There were 2 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `Current Windows Event ID = as.numeric(`Current Windows Event
      ID`)`.
    Caused by warning:
    ! NAs introduced by coercion
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
head(event_df,10)
```

    # A tibble: 10 × 5
       `Current Windows Event ID` `Legacy Windows Event ID` `Potential Criticality`
                            <dbl>                     <dbl> <chr>                  
     1                       4618                        NA High                   
     2                       4649                        NA High                   
     3                       4719                       612 High                   
     4                       4765                        NA High                   
     5                       4766                        NA High                   
     6                       4794                        NA High                   
     7                       4897                       801 High                   
     8                       4964                        NA High                   
     9                       5124                        NA High                   
    10                         NA                       550 Medium to High         
    # ℹ 2 more variables: `Event Summary` <chr>, `Potential criticality` <chr>

### Шаг 2

#### Посмотрим, что внутри датасета

``` r
glimpse(events)
```

    Rows: 101,904
    Columns: 9
    $ `@timestamp` <chr> "2019-10-20T20:11:06.937Z", "2019-10-20T20:11:07.101Z", "…
    $ `@metadata`  <df[,4]> <data.frame[26 x 4]>
    $ event        <df[,4]> <data.frame[26 x 4]>
    $ log          <df[,1]> <data.frame[26 x 1]>
    $ message      <chr> "A token right was adjusted.\n\nSubject:\n\tSecurity I…
    $ winlog       <df[,16]> <data.frame[26 x 16]>
    $ ecs          <df[,1]> <data.frame[26 x 1]>
    $ host         <df[,1]> <data.frame[26 x 1]>
    $ agent        <df[,5]> <data.frame[26 x 5]>

#### Подготовим датасет для дальнейшей работы

``` r
events1 <- events %>%
  tidyr::unnest_wider(event, names_sep = "")
```

``` r
events2 <- events1 %>%
  tidyr::unnest_wider(winlog, names_sep = "")
```

``` r
events3 <- events2 %>%
  tidyr::unnest_wider(host, names_sep = "") %>%
  tidyr::unnest_wider(agent, names_sep = "") %>%
  tidyr::unnest_wider(ecs, names_sep = "")
```

#### Посмотрим на итоговые поля в распаршеном датасете

``` r
names(events3)
```

     [1] "@timestamp"          "@metadata"           "eventcreated"       
     [4] "eventkind"           "eventcode"           "eventaction"        
     [7] "log"                 "message"             "winlogevent_data"   
    [10] "winlogevent_id"      "winlogprovider_name" "winlogapi"          
    [13] "winlogrecord_id"     "winlogcomputer_name" "winlogprocess"      
    [16] "winlogkeywords"      "winlogprovider_guid" "winlogchannel"      
    [19] "winlogtask"          "winlogopcode"        "winlogversion"      
    [22] "winloguser"          "winlogactivity_id"   "winloguser_data"    
    [25] "ecsversion"          "hostname"            "agentephemeral_id"  
    [28] "agenthostname"       "agentid"             "agentversion"       
    [31] "agenttype"          

#### Минимизируем количество колонок в датафрейме, убрав колонки с единственным значением параметра.

``` r
events_min <- events3 %>%
  select(-`@metadata`) %>%
  select(where(~ dplyr::n_distinct(.x) > 1))

glimpse(events_min)
```

    Rows: 101,904
    Columns: 21
    $ `@timestamp`        <chr> "2019-10-20T20:11:06.937Z", "2019-10-20T20:11:07.1…
    $ eventcreated        <chr> "2019-10-20T20:11:09.988Z", "2019-10-20T20:11:09.9…
    $ eventcode           <int> 4703, 4673, 10, 10, 10, 10, 11, 10, 10, 10, 10, 7,…
    $ eventaction         <chr> "Token Right Adjusted Events", "Sensitive Privileg…
    $ log                 <df[,1]> <data.frame[26 x 1]>
    $ message             <chr> "A token right was adjusted.\n\nSubject:\n\tSec…
    $ winlogevent_data    <df[,234]> <data.frame[26 x 234]>
    $ winlogevent_id      <int> 4703, 4673, 10, 10, 10, 10, 11, 10, 10, 10, 10, 7,…
    $ winlogprovider_name <chr> "Microsoft-Windows-Security-Auditing", "Micro…
    $ winlogrecord_id     <int> 50588, 104875, 226649, 153525, 163488, 153526, 134…
    $ winlogcomputer_name <chr> "HR001.shire.com", "HFDC01.shire.com", "IT001.shir…
    $ winlogprocess       <df[,2]> <data.frame[26 x 2]>
    $ winlogkeywords      <list<list>> ["Audit Success"], ["Audit Failure"], [<NULL>], [<…
    $ winlogprovider_guid <chr> "{54849625-5478-4994-a5ba-3e3b0328c30d}", "{548496…
    $ winlogchannel       <chr> "security", "Security", "Microsoft-Windows-Sysm…
    $ winlogtask          <chr> "Token Right Adjusted Events", "Sensitive P…
    $ winlogopcode        <chr> "Info", "Info", "Info", "Info", "Info", "Info", "I…
    $ winlogversion       <int> NA, NA, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, NA, 3,…
    $ winloguser          <df[,4]> <data.frame[26 x 4]>
    $ winlogactivity_id   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ winloguser_data     <df[,30]> <data.frame[26 x 30]>

#### Посчитаем количество хостов представлено в данном датасете.

``` r
hosts_count <- events_min %>%
  summarise(n_hosts = n_distinct(winlogcomputer_name))

hosts_count
```

    # A tibble: 1 × 1
      n_hosts
        <int>
    1       5

#### Посмотрим на список уникальных хостов.

``` r
hosts_list <- events_min %>%
  distinct(winlogcomputer_name)

hosts_list
```

    # A tibble: 5 × 1
      winlogcomputer_name
      <chr>              
    1 HR001.shire.com    
    2 HFDC01.shire.com   
    3 IT001.shire.com    
    4 ACCT001.shire.com  
    5 FILE001.shire.com  

#### Подготовим датафрейм с расшифровкой Windows Event_ID.

``` r
event_df_clean <- event_df_raw %>%
  janitor::clean_names() %>% 
  transmute(
    event_id        = as.numeric(current_windows_event_id),
    legacy_event_id = as.numeric(legacy_windows_event_id),
    criticality     = as.character(potential_criticality),
    summary         = as.character(event_summary)
  ) %>%
  filter(!is.na(event_id))
```

    Warning: There were 2 warnings in `transmute()`.
    The first warning was:
    ℹ In argument: `event_id = as.numeric(current_windows_event_id)`.
    Caused by warning:
    ! NAs introduced by coercion
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
glimpse(event_df_clean)
```

    Rows: 371
    Columns: 4
    $ event_id        <dbl> 4618, 4649, 4719, 4765, 4766, 4794, 4897, 4964, 5124, …
    $ legacy_event_id <dbl> NA, NA, 612, NA, NA, NA, 801, NA, NA, 517, NA, NA, NA,…
    $ criticality     <chr> "High", "High", "High", "High", "High", "High", "High"…
    $ summary         <chr> "A monitored security event pattern has occurred.", "A…

``` r
head(event_df_clean, 10)
```

    # A tibble: 10 × 4
       event_id legacy_event_id criticality    summary                              
          <dbl>           <dbl> <chr>          <chr>                                
     1     4618              NA High           A monitored security event pattern h…
     2     4649              NA High           A replay attack was detected. May be…
     3     4719             612 High           System audit policy was changed.     
     4     4765              NA High           SID History was added to an account. 
     5     4766              NA High           An attempt to add SID History to an …
     6     4794              NA High           An attempt was made to set the Direc…
     7     4897             801 High           Role separation enabled.             
     8     4964              NA High           Special groups have been assigned to…
     9     5124              NA High           A security setting was updated on th…
    10     1102             517 Medium to High The audit log was cleared.           

#### Соединими два датафрейма в один.

``` r
events_joined <- events_min %>%
  left_join(event_df_clean, by = c("eventcode" = "event_id"))

events_joined %>%
  select(`@timestamp`, winlogcomputer_name, eventcode, criticality, summary) %>%
  head(10)
```

    # A tibble: 10 × 5
       `@timestamp`             winlogcomputer_name eventcode criticality summary   
       <chr>                    <chr>                   <dbl> <chr>       <chr>     
     1 2019-10-20T20:11:06.937Z HR001.shire.com          4703 <NA>        <NA>      
     2 2019-10-20T20:11:07.101Z HFDC01.shire.com         4673 Low         A privile…
     3 2019-10-20T20:11:09.052Z IT001.shire.com            10 <NA>        <NA>      
     4 2019-10-20T20:11:10.985Z HR001.shire.com            10 <NA>        <NA>      
     5 2019-10-20T20:11:11.249Z ACCT001.shire.com          10 <NA>        <NA>      
     6 2019-10-20T20:11:15.017Z HR001.shire.com            10 <NA>        <NA>      
     7 2019-10-20T20:11:15.438Z FILE001.shire.com          11 <NA>        <NA>      
     8 2019-10-20T20:11:15.522Z IT001.shire.com            10 <NA>        <NA>      
     9 2019-10-20T20:11:15.522Z IT001.shire.com            10 <NA>        <NA>      
    10 2019-10-20T20:11:16.460Z IT001.shire.com            10 <NA>        <NA>      

#### Проверим есть ли в логе события с высоким и средним уровнем значимости и их количество.

``` r
severity_stats <- events_joined %>%
  filter(!is.na(criticality)) %>%
  count(criticality, sort = TRUE)

severity_stats
```

    # A tibble: 1 × 2
      criticality     n
      <chr>       <int>
    1 Low          2436

##### Как видим из предыдущего результата в датафрейме есть только LOW критичные события.

``` r
high_events <- events_joined %>%
  filter(str_detect(criticality, regex("high", ignore_case = TRUE)))

medium_events <- events_joined %>%
  filter(str_detect(criticality, regex("medium", ignore_case = TRUE)))

n_high   <- nrow(high_events)
n_medium <- nrow(medium_events)

n_high
```

    [1] 0

``` r
n_medium
```

    [1] 0
