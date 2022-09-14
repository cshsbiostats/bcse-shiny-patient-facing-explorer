library(tidyverse)

main <- local({
  main <-
    readxl::read_excel('data/B35data_Main_AE_QOL 8-25-2020.xlsx', sheet = 'Main') %>%
    janitor::clean_names()
  
  main <- main %>%
    select(patientid, trt, toff)
  
})

data <- local({
  data <-
    readxl::read_excel('data/B35data_Main_AE_QOL 8-25-2020.xlsx', sheet = 'QOL (long fmt)')
  
  data <- data %>% janitor::clean_names()
  
  data <- data %>%
    select(patientid, hotfl:othpro, time_point) %>%
    pivot_longer(hotfl:othpro)
  
  data <- data %>%
    mutate(
      time_point = str_extract(time_point, '\\d+'),
      time_point = ifelse(is.na(time_point), 0, time_point),
      time_point = as.numeric(time_point)
    )
  
  symptom_checklists <- data %>%
    count(name) %>%
    pull(name)
  
  data <- data %>%
    complete(.,
             nesting(patientid),
             time_point = seq(0, 72, 6),
             name = symptom_checklists)
  
  data <- data %>%
    rename('symptom' = name,
           'response' = value)
  
})

data <- data %>% 
  left_join(main, by = "patientid") %>% 
  mutate(
    response = case_when(
      is.na(response) & time_point >= toff ~ 'Off trial',
      is.na(response) & time_point < toff ~ 'No Response',
      TRUE ~ response
    )
  )

symptom_labels <- 
tibble::tribble(
                    ~symptom,                                           ~symptom_labels,
                     "hotfl",                                             "Hot Flashes",
                    "headac",                                               "Headaches",
                    "bladlc", "Difficulty with bladder control–when laughing or crying",
                    "bladot",          "Difficulty with bladder control-at other times",
                    "vagdis",                                       "Vaginal discharge",
                    "vagbld",                            "Vaginal bleeding or spotting",
                    "genitc",                              "Genital itching/irritation",
                    "vagdry",                                         "Vaginal dryness",
                    "painin",                                   "Pain with intercourse",
                    "painbr",                                             "Breast pain",
                     "brsts",                             "Difficulty with breast skin",
                     "brstt",                           "Breast sensitivity/tenderness",
                   "genache",                                 "General aches and pains",
                    "jointp",                                             "Joint pains",
                     "stiff",                                        "Muscle stiffness",
                    "wtgain",                                             "Weight gain",
                    "wtloss",                                             "Weight loss",
                    "unhapp",                      "Unhappy with appearance of my body",
                    "decapp",                                      "Decreased appetite",
                    "forget",                                           "Forgetfulness",
                    "excite",                                            "Excitability",
                    "shortt",                                            "Short temper",
                      "naps",                      "Tendency to take naps; stay in bed",
                   "nightsw",                                            "Night sweats",
                    "coldsw",                                             "Cold sweats",
                   "diffcon",                                "Difficulty concentrating",
                    "easdis",                                       "Easily distracted",
                    "trousl",                                        "Trouble sleeping",
                    "eaawak",                                         "Early awakening",
                    "othpro",                                      "Any other problems"
                   )

data <- data %>% 
  left_join(symptom_labels, by = "symptom")

write_csv(data, 'data/shiny_qol_data.csv')