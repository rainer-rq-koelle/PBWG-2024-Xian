## code to prepare `DATASET` dataset goes here

# load utility functions
source("~/RProjects/PBWG-2024-Xian/R/rqutils-zip.R")

# folder to APDF data
rprojs  <- here::here() |> dirname()
apdf_fn <- here::here(rprojs, "__DATA","APDF")

# load APDF and extract sample data
start_date <- lubridate::ymd("2024-07-01")
end_date   <- start_date + lubridate::days(14)

# load look up airports
apts_pbwg <- readxl::read_xlsx("./data/pbwg-apts.xlsx")
apts_eur  <- apts_pbwg |> filter(REG_3 == "EUR") |> pull(ICAO)

# my APDFs
fns <- check_zip_content(apdf_fn, "apdf-2024-JanSep.zip") |> pull(Name)

get_apdf_sample <- function(
          .fn                                   # filename of zip content
        , .start_date = start_date, .end_date = end_date # dates to trim
        , .apts_pbwg = apts_pbwg, .apts_eur = apts_eur   # study airports
        ){
    this_apt   <- substr(.fn, 1, 4)
    other_eur  <- .apts_eur |> setdiff(this_apt)
    other_pbwg <- .apts_pbwg$ICAO

    tmp <-  read_zip(apdf_fn, "apdf-2024-JanSep.zip", .files = .fn)
    tmp <- tmp |>
        dplyr::mutate(
            DOF = dplyr::case_when(
                          !is.na(BLOCK_TIME_UTC) ~ lubridate::date(BLOCK_TIME_UTC)
                         , is.na(BLOCK_TIME_UTC) ~ lubridate::date(MVT_TIME_UTC)
                         , .default = NA
                         )
         , .after = APDS_ID) |>
        dplyr::filter(dplyr::between(DOF, .start_date, .end_date))

    #CONTINUE - filter airports

    return(tmp)
}

