library(knitr)
library(tikzDevice)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(kableExtra)


# Useful for generating figure ids
nameit <- function(){paste0('fig', paste0(sample(c(0:9, LETTERS[1:6]), 6, T), collapse=''))}

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
                Other=DL_GRAY)

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

####################
# Common Functions #
####################
dl_annotate <- function(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
                        ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, na.rm = FALSE, ...) {
  args_list <- list(...)
  args_list["color"] <- "#00274C"
  args_list["geom"] <- geom
  args_list["x"] <- x
  args_list["y"] <- list(y)
  do.call(annotate, args = args_list)
}

top_performer_theme <- function(){
  theme_classic() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.margin = unit(c(-.1,-.1,-.1,-.1),"npc") )
}

single_line_theme <- function(){
  theme_classic() +
    theme(axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text = element_text(color="#00274c"),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position = "none")
}

# Common Circle Plot
circle_plot <- function(maptg_data, measure_id, middle_label="" ){
  plotting_attrs <- tibble(obs=c("numerator","gap","denominator"),
                           ring=c(50,50,58),
                           width=c(16,16,6),
                           fill_color=c(DL_DARK_BLUE, DL_LIGHT_BLUE, DL_LIGHT_BLUE))
  
  plot_data <- maptg_data %>% 
    filter(measure == measure_id, ascribee == RECIP) %>%
    group_by(measure) %>%
    summarize(
      numerator = sum(numerator, na.rm=TRUE),
      denominator = sum(denominator, na.rm=TRUE),
      gap = denominator - numerator) %>%
    pivot_longer(cols=c("denominator","numerator", "gap"), names_to = "obs", values_to = "value") %>%
    left_join(plotting_attrs)
  
  numer <- plot_data %>% filter(obs=="numerator") %>% pull(value)
  denom <- plot_data %>% filter(obs=="denominator") %>% pull(value)
  perf_label <- paste(floor(100*numer/denom), "%",sep="")
    
  ggplot(plot_data, aes(x=ring, y=value, fill=fill_color, width=width)) +
    geom_col() +
    scale_fill_identity() +
    scale_x_continuous(limits=c(0,75)) +
    coord_polar(theta="y", direction=-1) +
    dl_annotate("text", x=10, y=denom/2, label=perf_label, size=9, color=DL_DARK_BLUE, fontface=2) +
    dl_annotate("text", x=8, y=0, label=middle_label, size=2.5, color=DL_DARK_BLUE) +
    dl_annotate("text", x=20, y=0, label=paste(numer, denom, sep="/"),
                size=4, color=DL_DARK_BLUE) +
    top_performer_theme()
}

######################
# Process Input Data #
######################
full_maptg_data <- readr::read_csv('data/maptg.csv', trim_ws=T)

# Trim to date range
START_DATE <- lubridate::ymd('2020-01-01')
END_DATE <- lubridate::ymd('2020-04-01')
maptg_data <- full_maptg_data %>% filter( time >= START_DATE, time < END_DATE)

OBS_END_DATE <- maptg_data$time %>% max

START_MONTH <- format(START_DATE, "%b")
END_MONTH <- format(END_DATE, "%b")
END_YEAR <- format(END_DATE, "%Y")

# Set recipient of report
RECIP <- "Hurley"

###########################
# Generate Report Content #
###########################

#### SUM: Women Delievered
m14_sum <- maptg_data %>% 
  filter(measure == "M14", ascribee == RECIP) %>% 
  pull(numerator) %>% 
  sum()

#### FIGURE
# stacked bar
plot_data <- maptg_data %>% 
  filter(measure =="M14", ascribee == RECIP) %>%
  group_by(time) %>%
  mutate(denominator = sum(numerator),
         payer_rate = numerator/denominator)

fig7F5D31 <- ggplot(plot_data, aes(x=time, y=payer_rate)) +
  geom_bar(aes(fill=group), stat='identity', color=DL_DARK_BLUE) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  ylab("% Women Delivered") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = REPORT_PAL, guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom",  axis.title.x = element_blank(),
        legend.title = element_blank(), legend.box.spacing = unit(0,"mm")) 
  
content_id <- deparse(substitute(fig7F5D31))
info7F5D3 <- paste(content_id, "m14")

#### FIGURE
# circle
# M1	Provision 3 PP
figADA835A <- circle_plot(maptg_data, "M1", "")
content_id <- deparse(substitute(fig7F5D31))
infoADA835A <- paste(content_id, "m1")

#### FIGURE
plot_data <- maptg_data %>% 
  filter(measure == "M1", ascribee == RECIP) %>%
  group_by(ascribee, time, measure) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(denominator, na.rm=TRUE),
    rate = numerator/denominator) %>%
  mutate(
    recipient = ifelse(ascribee == RECIP, T, F),
    perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
    arrow = ifelse(recipient, "show", "noshow"),
    pcolor = ifelse(recipient, DL_DARK_BLUE, DL_GRAY)
    ) 

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")
  
fig707A6E <- ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
  geom_point(mapping = aes(y = rate + 0.07, shape=arrow), size=4, color=DL_DARK_BLUE) +
  geom_line(size=1, lineend="round") +
  geom_point(size=2, fill=DL_FILL, shape=21, stroke=1.2) +
  scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
  geom_label(mapping = aes(label=perf_label), nudge_y = 0.1, fill=DL_DARK_BLUE,
             color=DL_FILL, label.r = unit(0, "lines"), label.size=0) +
  single_line_theme() +
  scale_color_manual(labels = plot_data$ascribee, 
                     values = plot_data$pcolor, guide = guide_legend(title=NULL)) +
  theme(legend.position="bottom")

content_id <- deparse(substitute(fig707A6E))
info707A6E <- paste(content_id, "m1")

content_id <- deparse(substitute(fig7F5D31))
info7F5D3 <- paste(content_id, "m14")

#### FIGURE
fig9C0A4F <- circle_plot(maptg_data, "M3", "")
content_id <- deparse(substitute(fig9C0A4F))
info9C0A4F <- paste(content_id, "m3")

### FIGURE

## TODO side by side figures facetted together or cowplotted
plot_data <- maptg_data %>% 
  filter(measure %in% c("M20", "M21"), ascribee == RECIP) %>%
  group_by(group, measure) %>%
  summarize(numerator = sum(numerator),
            denominator = sum(pull(., numerator))) %>%
  mutate(ratio = numerator/denominator) %>%
  left_join(MEASURE_NAMES, by="measure")

figBE214E <- ggplot(plot_data, aes(y=ratio)) +
  geom_bar(aes(fill=short_name,x="Device"), stat='identity') +
  geom_bar(aes(fill=group,x="Payer"), stat='identity') +
  ylab("% of Immediate LARC Provided") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = REPORT_PAL, guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank(),
        legend.text = element_text(size=8), legend.key.size = unit(4, "mm"))  +
  guides(fill=guide_legend(nrow=2))

content_id <- deparse(substitute(figBE214E))
infoBE214E <- paste(content_id, "m20,21")

#### FIGURE
fig540727 <- circle_plot(maptg_data, "M5", "")
content_id <- deparse(substitute(fig540727))
info540727 <- paste(content_id, "m5")

#### FIGURE
plot_data <- maptg_data %>% 
  filter(measure == "M5", ascribee == RECIP) %>%
  group_by(ascribee,time, measure) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(denominator, na.rm=TRUE),
    rate = numerator/denominator) %>%
  mutate(
    recipient = ifelse(ascribee == RECIP, T, F),
    perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
    arrow = ifelse(recipient, "show", "noshow"),
    pcolor = ifelse(recipient, DL_DARK_BLUE, DL_GRAY)
    ) 

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")
  
fig5BF5D0 <- ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
  geom_point(mapping = aes(y = rate + 0.07, shape=arrow), size=4, color=DL_DARK_BLUE) +
  geom_line(size=1, lineend="round") +
  geom_point(size=2, fill=DL_FILL, shape=21, stroke=1.2) +
  scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
  geom_label(mapping = aes(label=perf_label), nudge_y = 0.1, fill=DL_DARK_BLUE,
             color=DL_FILL, label.r = unit(0, "lines"), label.size=0) +
  single_line_theme() +
  scale_color_manual(labels = plot_data$ascribee, 
                     values = plot_data$pcolor, guide = guide_legend(title=NULL)) +
  theme(legend.position="bottom")

content_id <- deparse(substitute(fig5BF5D0))
info5BF5D0 <- paste(content_id, "m5")

#### TABLE DATA
tbl_82C4A3 <- maptg_data %>% 
  filter(ascribee == RECIP,
         measure %in% c("M10", "M11","M12","M13")) %>%
  group_by(measure) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(denominator, na.rm=TRUE),
    rate = numerator/denominator,
    count = paste(numerator, denominator, sep="/")) %>%
  left_join(MEASURE_NAMES, by="measure") %>%
  select(short_name, count, rate)

#### FIGURE
# Preference of LARC by payer
plot_data <- maptg_data %>% 
  filter(ascribee == RECIP,
         measure %in% c("M10", "M11","M12","M13"))  %>%
  group_by(group, measure) %>%
  summarize(numerator = sum(numerator),
            denominator = sum(denominator)) %>%
  mutate(rate = numerator/denominator) %>%
  left_join(MEASURE_NAMES, by="measure")

fig1903AB <- ggplot(plot_data, aes(x=group, y=rate)) +
  geom_bar(aes(fill=short_name), stat='identity', color=DL_DARK_BLUE) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = REPORT_PAL, guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom", axis.title = element_blank(), 
        title = element_text(size=10), legend.title = element_blank(), 
        legend.text = element_text(size=8), legend.key.size = unit(4, "mm"),
        legend.box.spacing = unit(0,"mm")) +
  guides(fill=guide_legend(nrow=2))

content_id <- deparse(substitute(fig1903AB))
info1903AB <- paste(content_id, "m10,11,12,13")

#### FIGURE
figBDBC81 <- circle_plot(maptg_data, "M8", "")
content_id <- deparse(substitute(figBDBC81))
infoBDBC81 <- paste(content_id, "m8")

#### FIGURE
pref_pal <- c("Provided"=DL_DARK_BLUE, "Preferred"=DL_MAUVE)

plot_data <- maptg_data %>% 
  filter(ascribee == RECIP,
         measure %in% c("M16","M17","M18","M19"))  %>%
  group_by(measure) %>%
  summarize(numerator = sum(numerator),
            denominator = sum(denominator)) %>%
  left_join(MEASURE_NAMES, by="measure") %>%
  mutate(rate = numerator/denominator,
         short_name = as.factor(short_name),
         mpos = as.numeric(short_name))

figE8F578 <- ggplot(plot_data, aes(x=short_name)) +
  geom_bar(aes(y=numerator, color="Provided"), fill=DL_GRAY, stat='identity') +
  geom_linerange(aes(y=denominator, xmin=mpos-0.4, xmax=mpos+0.4, color="Preferred"), size=3) +
  scale_color_manual(values=pref_pal) +
  scale_fill_manual(values=pref_pal) +
  theme_minimal() +
  theme(legend.position="right", axis.title.x = element_blank(), title = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1, face="bold"),
        legend.title = element_blank(), legend.text = element_text(size=8), legend.key.size = unit(4, "mm") ) +
  guides(color=guide_legend(override.aes=list(fill="white"))) +
  ylab("Number of Women")

content_id <- deparse(substitute(figE8F578))
infoE8F578 <- paste(content_id, "m16,17,18,19")

###################
# Generate Report #
###################
template_path <- "report.Rnw"

output_path <- "report.pdf"
build_dir <- tempdir()

options(tinytex.engine="pdflatex")
knitr::knit2pdf(input = template_path, pdf_file=output_path, clean=TRUE)

# TODO: Build report_env to directly control what variables are accessible in the report.
#knitr::knit2pdf(input = template_path, envir=report_env, pdf_file= output_path)
  