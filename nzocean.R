if (FALSE) {
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(lubridate)
  nzocean <-
    read_csv('../ocean-acidification-19982016.csv',
             show_col_types = FALSE) %>%
    transmute(date = as.Date(Date, format = '%d-%b-%y'),
              mth = factor(format(date, '%m')),
              name = recode(Variable,
                            'mean_surface_temperature_degree_celsius' = 'temp',
                            'mean_salinity_unitless_(parts_per_1000,_‰)' = 'sal',
                            'mean_pCO2_microatmospheres' = 'pco2',
                            'At_mmol_kg_1_(micromoles_per_kilogram_of_seawater)' = 'DROP',
                            'dissolved_inorganic_carbon_mmol_kg_1_(micromoles_per_kilogram_of_seawater)' = 'dioc',
                            'pH___total_scale,_at_in_situ_temperatur' = 'pH'),
              value = Value) %>%
    filter(name != 'DROP') %>%
    pivot_wider()
  nzocean
  
  soi <-
    read_csv('../el-nino-southern-oscillation-1876-2019.csv',
             show_col_types = FALSE) %>%
    filter(enso_year >= as.integer(format(min(nzocean$date), '%Y')) - 1) %>%
    transmute(date = paste(year,
                           gsub('^(\\d{1})$', '0\\1', month),
                           '01',
                           sep = '-'),
              date = as.Date(date),
              soi) %>%
    arrange(desc(date)) %>%
    mutate(prev = list(.),
           prev = map2(prev,
                       date,
                       ~ ..1 %>%
                         filter(date <= ..2) %>%
                         mutate(i = row_number(),
                                el_nino = cumall(soi <= -1.0),
                                la_nina = cumall(soi >= 1.0),
                                neutral = cumall(abs(soi) < 0.5),
                                phase = if_else(neutral, 'Neutral', ''),
                                phase = if_else(el_nino, 'El Niño', phase),
                                phase = if_else(la_nina, 'La Niña', phase)) %>%
                         filter(i >= 3 & phase != '') %>%
                         slice_tail(n = 1) %>%
                         select(phase, end = date))) %>%
    unnest(prev, keep_empty = TRUE) %>%
    mutate(across(c(phase, end),
                  ~ if_else(is.na(.x) & date >= lag(end, 1), lag(.x, 1), .x)),
           across(c(phase, end),
                  ~ if_else(is.na(.x) & date >= lag(end, 1), lag(.x, 1), .x)),
           n_mths = 1 + (interval(end, date) %/% months(1)),
           year_month = format(date, '%Y-%m'),
           phase = if_else(is.na(phase), 'Fluctuating', phase),
           date = NULL,
           end = NULL,
           n_mths = NULL)
  soi
  
  nzocean <-
    nzocean %>%
    mutate(year_month = format(date, '%Y-%m')) %>%
    full_join(soi, by = 'year_month') %>%
    mutate(date = if_else(is.na(date),
                          as.Date(paste0(year_month, '-01')),
                          date)) %>%
    select(-year_month, -mth) %>%
    arrange(date) %>%
    filter(date >= min(nzocean$date) & date <= max(nzocean$date)) %>%
    as.data.frame() %>%
    mutate(phase = factor(phase, levels = c('Fluctuating',
                                            'Neutral',
                                            'La Niña',
                                            'El Niño')))
  nzocean
  
  save(nzocean, file = 'data/nzocean.rda')
}