###################
# CONSTANTS #
###################
DL_GREEN      <- "#108A00"
DL_BLUE       <- "#0174BB"
DL_LIGHT_BLUE <- "#E7EDEE"
DL_DARK_BLUE  <- "#00274C"
DL_GRAY       <- "#878A8F"
DL_MAUVE      <- "#853754"
DL_ORANGE     <- "#BA5827"
DL_FILL       <- "#FFFFFF"

REPORT_PAL <- c(medicaid=DL_GREEN,
                non_medicaid=DL_MAUVE,
                IUD=DL_DARK_BLUE,
                Nexplanon=DL_BLUE,
                PPTL=DL_LIGHT_BLUE,
                Other=DL_GRAY,
                Medicaid=DL_GREEN,
                'non-Medicaid'=DL_MAUVE)

ASCRIBEE_TITLES <- c(UMich="Michigan Medicine",
                     Hurley="Hurley Medical Center",
                     Munson="Munson Healthcare")

MEASURE_NAMES <- tibble( measure = c("M1", "M2", "M3", "M4", "M5", 
                                     "M6", "M7", "M8", "M9", "M10",
                                     "M11", "M12", "M13", "M14", "M15", 
                                     "M16", "M17", "M18", "M19", "M20",
                                     "M21"),
                         short_name = c("Provision 3 PP", "Provision 60 PP", "LARC 3 PP ", "LARC 60 PP", "Couseling", 
                                        "Choice documented ", "Prefer IPLARC", "Preference provision", "LARC PP", "IUD",
                                        "Nexplanon", "PPTL", "Other", "Delivered", "", 
                                        "IUD", "Nexplanon", "PPTL", "Other", "IUD", 
                                        "Nexplanon")
                         )
