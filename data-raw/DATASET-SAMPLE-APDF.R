## code to prepare `DATASET` dataset goes here

# load utility functions
source("~/RProjects/PBWG-2024-Xian/R/rqutils-zip.R")

# folder to APDF data
rprojs  <- here::here() |> dirname()
apdf_fn <- here::here(rprojs, "__DATA","APDF")

# load APDF and extract sample data
start_date <- lubridate::ymd("2024-06-17")
end_date   <- start_date + lubridate::days(27)

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
    other_pbwg <- .apts_pbwg$ICAO |> setdiff(.apts_eur)

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

    #filter PBWG 'other' airports
    arrs <- tmp |> dplyr::filter(SRC_PHASE == "ARR") |> dplyr::filter(ADEP_ICAO %in% other_pbwg)
    deps <- tmp |> dplyr::filter(SRC_PHASE == "DEP") |> dplyr::filter(ADES_ICAO %in% other_pbwg)
    tmp  <- dplyr::bind_rows(arrs, deps) |> dplyr::arrange(BLOCK_TIME_UTC)

    # trim to payload
    tmp <- tmp |>
        dplyr::select(
            DOF, FLTID = AP_C_FLTID, REG = AP_C_REG, CLASS = AC_CLASS, TYPE = AP_C_ARCTYP
            ,ADEP = ADEP_ICAO, ADES = ADES_ICAO
            ,PHASE = SRC_PHASE
            ,BLOCK_TIME = BLOCK_TIME_UTC, MVT_TIME = MVT_TIME_UTC
            ,SCHED_TIME = SCHED_TIME_UTC
            ,RWY = AP_C_RWY, STND = AP_C_STND
            ,C40_TIME = C40_CROSS_TIME,C40_BRG = C40_BEARING, C40_LAT = C40_CROSS_LAT, C40_LON = C40_CROSS_LON, C40_FL = C40_CROSS_FL
            ,C100_TIME = C100_CROSS_TIME,C100_BRG = C100_BEARING, C100_LAT = C100_CROSS_LAT, C100_LON = C100_CROSS_LON, C100_FL = C100_CROSS_FL
            ,PREV_BLOCK = PREV_BLOCK_TIME_UTC
            ,SAM_ID = IM_SAMAD_ID
        )
    return(tmp)
}

# iterate over all PBWG EUR airports to create APDF snapshot
apdf_pbwg <- fns |>
    purrr::map( ~ get_apdf_sample(.x))

apdf_pbwg |> dplyr::bind_rows() |> arrow::write_parquet("./data/sample-apdf-eur-pbwg.parquet")
