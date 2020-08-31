#' Package Constants
#' @export MR
MR <- new.env()

###################
# CONSTANTS #
###################
MR$DL_GREEN      <- "#108A00"
MR$DL_BLUE       <- "#0174BB"
MR$DL_LIGHT_BLUE <- "#E7EDEE"
MR$DL_DARK_BLUE  <- "#00274C"
MR$DL_GRAY       <- "#878A8F"
MR$DL_MAUVE      <- "#853754"
MR$DL_ORANGE     <- "#BA5827"
MR$DL_FILL       <- "#FFFFFF"

MR$REPORT_PAL <- c(medicaid=MR$DL_GREEN,
                   non_medicaid=MR$DL_MAUVE,
                   IUD=MR$DL_DARK_BLUE,
                   Nexplanon=MR$DL_BLUE,
                   PPTL=MR$DL_LIGHT_BLUE,
                   Other=MR$DL_GRAY,
                   Medicaid=MR$DL_GREEN,
                   'non-Medicaid'=MR$DL_MAUVE)

MR$ASCRIBEE_TITLES <- c(UMich="Michigan Medicine",
                        Hurley="Hurley Medical Center",
                        Munson="Munson Healthcare")


#' @importFrom tibble tibble
make_measure_names <- function(){
  tibble::tibble( measure = c("M1", "M2", "M3", "M4", "M5",
                      "M6", "M7", "M8", "M9", "M10",
                      "M11", "M12", "M13", "M14", "M15",
                      "M16", "M17", "M18", "M19", "M20",
                      "M21"),
          short_name = c("Provision 3 PP", "Provision 60 PP", "LARC 3 PP", "LARC 60 PP", "Couseling",
                         "Choice documented", "Prefer IPLARC", "Preference provision", "LARC PP", "IUD",
                         "Nexplanon", "PPTL", "Other", "Delivered", "",
                         "IUD", "Nexplanon", "PPTL", "Other", "IUD",
                         "Nexplanon")
        )
}

MR$MEASURE_NAMES <- make_measure_names()
