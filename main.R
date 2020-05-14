library(knitr)
library(tikzDevice)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(kableExtra)


# Useful for generating figure ids
nameit <- function(){paste0('fig_', paste0(sample(c(0:9, LETTERS[1:6]), 6, T), collapse=''))}

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
                           fill_color=c(DL_BLUE, DL_LIGHT_BORDER, DL_LIGHT_BORDER))
  
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
    
  fig_mid_left <-ggplot(plot_data, aes(x=ring, y=value, fill=fill_color, width=width)) +
    geom_col() +
    scale_fill_identity() +
    scale_x_continuous(limits=c(0,75)) +
    coord_polar(theta="y", direction=-1) +
    dl_annotate("text", x=10, y=denom/2, label=perf_label, size=9, color=DL_BLUE, fontface=2) +
    dl_annotate("text", x=8, y=0, label=middle_label, size=2.5, color=DL_BLUE) +
    dl_annotate("text", x=20, y=0, label=paste(numer, denom, sep="/"),
                size=4, color=DL_BLUE) +
    top_performer_theme()
}

###################
# CONSTANTS #
###################

DL_GREEN        <- "#108A00"
DL_LIGHT_BLUE   <- "#0174BB"
DL_LIGHT_BORDER <- "#e7edee"
DL_BLUE         <- "#00274C"
DL_FILL         <- "#FFFFFF"
DL_GRAY         <- "#878A8F"

MEASURE_NAMES <- tibble( measure = c("M1", "M2", "M3", "M4", "M5", 
                                     "M6", "M7", "M8", "M9", "M10",
                                     "M11", "M12", "M13", "M14", "M15", 
                                     "M16", "M17", "M18", "M19", "M20",
                                     "M21"),
                         short_name = c("Provision 3 PP", "Provision 60 PP", "LARC 3 PP ", "LARC 60 PP", "Couseling", 
                                        "Choice documented ", "Prefer IPLARC", "Preference provision", "LARC PP", "Choice IUD",
                                        "Nexplanon", "PPTL", "Other", "Delivered", "", 
                                        "IUD", "Nexplanon", "PPTL", "Other", "IUD", 
                                        "Nexplanon")
                         )


######################
# Process Input Data #
######################
maptg_data <- readr::read_csv('data/maptg.csv', trim_ws=T)
#  ascribee,time,group,measure,numerator,denominator
#  Hurley,2020-01-01,Medicaid,M1,14,65

# ID	Short Name
# M1	Provision 3 PP
# M2	Provision 60 PP
# M3	LARC 3 PP 
# M4	LARC 60 PP
# M5	Couseling
# M6	Choice documented 
# M7	Prefer IPLARC
# M8	Preference provision
# M9	LARC PP
# M10	Choice IUD
# M11	Choice Nexplanon
# M12	Choice PPTL
# M13	Choice other
# M14	Delivered
# M15	Prenatal



###########################
# Generate Report Content #
###########################

RECIP <- "Hurley"

#### SUM: Women Delievered
# Sum measures across groups for women delivered
m14_sum <- maptg_data %>% filter(measure == "M14") %>% pull(numerator) %>% sum()

#### FIGURE
# stacked bar
plot_data <- maptg_data %>% 
  filter(measure =="M14") %>%
  group_by(time) %>%
  mutate(denominator = sum(numerator),
         payer_rate = numerator/denominator)

fig_7F5D31 <- ggplot(plot_data, aes(x=time, y=payer_rate)) +
  geom_bar(aes(fill=group), stat='identity', color=DL_BLUE) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  ylab("% Women Delivered") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c(DL_GREEN, DL_LIGHT_BLUE), guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank()) +
  ggtitle("Measure 14")

#### FIGURE
# circle
# M1	Provision 3 PP
#p1_fig_mid_left <- circle_plot(maptg_data, "M1", "Measure 1")
fig_ADA835A <- circle_plot(maptg_data, "M1", "Measure 1")


#### FIGURE
# title
plot_data <- maptg_data %>% 
  filter(measure == "M1") %>%
  group_by(ascribee,time, measure) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(denominator, na.rm=TRUE),
    rate = numerator/denominator) %>%
  mutate(
    recipient = ifelse(ascribee == RECIP, T, F),
    perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
    arrow = as.factor(ifelse(recipient, "show", "noshow")),
    pcolor = ifelse(recipient, DL_BLUE, DL_GRAY)
    ) 

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")
  
fig_707A6E <- ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
  geom_point(mapping = aes(y = rate + 0.07, shape=arrow), size=4, color=DL_BLUE) +
  geom_line(size=1, lineend="round") +
  geom_point(size=2, fill=DL_FILL, shape=21, stroke=1.2) +
  scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
  geom_label(mapping = aes(label=perf_label), nudge_y = 0.1, fill=DL_BLUE,
             color=DL_FILL, label.r = unit(0, "lines"), label.size=0) +
  single_line_theme() +
  scale_color_manual(labels = plot_data$ascribee, 
                     values = plot_data$pcolor, guide = guide_legend(title=NULL)) +
  theme(legend.position="bottom") +
  ggtitle("Measure 1")

#### FIGURE
fig_9C0A4F <- circle_plot(maptg_data, "M3", "Measure 3")

### FIGURE

## TODO side by side figures facetted together or cowplotted
plot_data <- maptg_data %>% 
  filter(measure %in% c("M20", "M21"), ascribee == RECIP) %>%
  group_by(group, measure) %>%
  summarize(numerator = sum(numerator),
            denominator = sum(pull(., numerator))) %>%
  mutate(ratio = numerator/denominator) %>%
  left_join(MEASURE_NAMES, by="measure")

fig_BE214E <- ggplot(plot_data, aes(y=ratio)) +
  geom_bar(aes(fill=short_name,x="Device"), stat='identity') +
  geom_bar(aes(fill=group,x="Payer"), stat='identity') +
  ylab("% of Immediate LARC Provided") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  #scale_fill_manual(values = c(DL_GREEN, DL_LIGHT_BLUE), guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank(),
        legend.text = element_text(size=8), legend.key.size = unit(4, "mm"))  +
  guides(fill=guide_legend(nrow=2)) +
  ggtitle("Measure 20,21")

#### FIGURE
fig_540727 <- circle_plot(maptg_data, "M5", "Measure 5")

#### FIGURE
plot_data <- maptg_data %>% 
  filter(measure == "M5") %>%
  group_by(ascribee,time, measure) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(denominator, na.rm=TRUE),
    rate = numerator/denominator) %>%
  mutate(
    recipient = ifelse(ascribee == RECIP, T, F),
    perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
    arrow = as.factor(ifelse(recipient, "show", "noshow")),
    pcolor = ifelse(recipient, DL_BLUE, DL_GRAY)
    ) 

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")
  
fig_5BF5D0 <- ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
  geom_point(mapping = aes(y = rate + 0.07, shape=arrow), size=4, color=DL_BLUE) +
  geom_line(size=1, lineend="round") +
  geom_point(size=2, fill=DL_FILL, shape=21, stroke=1.2) +
  scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
  geom_label(mapping = aes(label=perf_label), nudge_y = 0.1, fill=DL_BLUE,
             color=DL_FILL, label.r = unit(0, "lines"), label.size=0) +
  single_line_theme() +
  scale_color_manual(labels = plot_data$ascribee, 
                     values = plot_data$pcolor, guide = guide_legend(title=NULL)) +
  theme(legend.position="bottom") +
  ggtitle("Measure 5")

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

fig_1903AB <- ggplot(plot_data, aes(x=group, y=rate)) +
  geom_bar(aes(fill=short_name), stat='identity', color=DL_BLUE) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  theme_minimal() +
  theme(legend.position="bottom", axis.title = element_blank(), title = element_text(size=10),
        legend.title = element_blank(), legend.text = element_text(size=8), legend.key.size = unit(4, "mm") ) +
  ggtitle("M10-13") +
  guides(fill=guide_legend(nrow=2))

#### FIGURE
fig_BDBC81 <- circle_plot(maptg_data, "M8", "Measure 8")

#### FIGURE
PALLETTE <- c("Provided"=DL_BLUE, "Preferred"=DL_GREEN)

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

fig_E8F578 <- ggplot(plot_data, aes(x=short_name)) +
  geom_bar(aes(y=numerator, color="Provided"), fill=DL_LIGHT_BLUE, stat='identity') +
  geom_linerange(aes(y=denominator, xmin=mpos-0.4, xmax=mpos+0.4, color="Preferred"), size=3) +
  scale_color_manual(name="foo", values=PALLETTE) +
  scale_fill_manual(name="foo", values=PALLETTE) +
  theme_minimal() +
  theme(legend.position="right", axis.title.x = element_blank(), title = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1, face="bold"),
        legend.title = element_blank(), legend.text = element_text(size=8), legend.key.size = unit(4, "mm") ) +
  ggtitle("M16-19") +
  guides(color=guide_legend(override.aes=list(fill="white"))) +
  ylab("Number of Women")

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
  