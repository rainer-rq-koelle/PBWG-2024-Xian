## code to prepare `DATASET-FLT-VAR-NM` dataset goes here

# NM flight table downloaded to local data drive
# read and extract global connections for 2023 and 2024

# load utility functions
source("~/RProjects/PBWG-2024-Xian/R/rqutils-zip.R")

# folder to NM
rprojs <- here::here() |> dirname()
nm_fn  <- here::here(rprojs, "__DATA","NM-flight-table")

# ----------- data prep utility functions -----------------------------
# extract payload for project
trim_flt <- function(.nm_flt){
    tmp <- .nm_flt |>
        dplyr::select(
             SAM_ID = ID, FLTID = AIRCRAFT_ID
            , REG   = REGISTRATION, ICAO24 = AIRCRAFT_ADDRESS
            , TYPE  = AIRCRAFT_TYPE_ICAO_ID, WTC = WK_TBL_CAT
            , ADEP, ADES, ADES_FILED
            , AOBT = AOBT_3
            , AIBT = ARVT_3
            , TXIT_3 = TAXI_TIME_3
            , FLT_DUR_3, RTE_LEN_3
            )
}

# ----------- end data prep utility functions --------------------------

# load 2023 data ------------ 2023 -------------
rq <- read_zip(nm_fn, "NM-flt-2023.zip") |> dplyr::bind_rows()
rq <- rq |> trim_flt()
rq |> arrow::write_parquet(sink = here::here("data", "nm-flt-2023.parquet"))

rq <- read_zip(nm_fn, "NM-flt-2024-Q12.zip") |> dplyr::bind_rows()
rq2 <- arrow::read_parquet(paste0(nm_fn,"/NM-flt-2024-Q3.gz.parquet"))
rq <- rq |> dplyr::bind_rows(rq2)
rq |>
    trim_flt() |>
    arrow::write_parquet(sink = here::here("data", "nm-flt-2024-123.parquet"))

#usethis::use_data(DATASET-FLT-VAR-NM, overwrite = TRUE)
