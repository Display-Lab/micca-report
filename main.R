library(knitr)
library(tikzDevice)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

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


###################
# COLOR CONSTANTS #
###################

DL_GREEN        <- "#108A00"
DL_LIGHT_BLUE   <- "#0174BB"
DL_LIGHT_BORDER <- "#e7edee"
DL_BLUE         <- "#00274C"
DL_FILL         <- "#FFFFFF"
DL_GRAY         <- "#878A8F"

###########################
# Generate Report Content #
###########################

RECIP <- "Hurley"

#### TOP LEFT
# Sum measures across groups for women delivered
m14_sum <- maptg_data %>% filter(measure == "M14") %>% pull(numerator) %>% sum()

#### TOP RIGHT
# stacked bar
plot_data <- maptg_data %>% 
  filter(measure =="M14") %>%
  group_by(time) %>%
  mutate(denominator = sum(numerator),
         payer_rate = numerator/denominator)

fig_top_right <- ggplot(plot_data, aes(x=time, y=payer_rate)) +
  geom_bar(aes(fill=group), stat='identity', color=DL_BLUE) +
  scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
  ylab("% Women Delivered") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c(DL_GREEN, DL_LIGHT_BLUE), guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank()) 

#### MID LEFT
# circle
# M1	Provision 3 PP
# M2	Provision 60 PP

plotting_attrs <- tibble(obs=c("numerator","gap","denominator"),
                         ring=c(50,50,58),
                         width=c(16,16,6),
                         fill_color=c(DL_BLUE, DL_LIGHT_BORDER, DL_LIGHT_BORDER))

plot_data <- maptg_data %>% 
  select(-group) %>%
  filter(measure == "M1" | measure == "M2",
         ascribee == RECIP) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(unique(denominator), na.rm=TRUE),
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
  dl_annotate("text", x=8, y=0, label="M1 + M2", size=2.5, color=DL_BLUE) +
  dl_annotate("text", x=20, y=0, label=paste(numer, denom, sep="/"),
              size=4, color=DL_BLUE) +
  top_performer_theme()

#### MID RIGHT
plot_data <- maptg_data %>% 
  select(-group) %>%
  filter(measure == "M1" | measure == "M2") %>%
  group_by(ascribee,time) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(unique(denominator), na.rm=TRUE),
    rate = numerator/denominator) %>%
  mutate(
    recipient = ifelse(ascribee == RECIP, T, F),
    perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
    arrow = as.factor(ifelse(recipient, "show", "noshow")),
    pcolor = ifelse(recipient, DL_BLUE, DL_GRAY)
    ) 
plot_data

# y axis labels
breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
labels_y <- c("20%", "40%", "60%", "80%", "100%")
  
fig_mid_right <- ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
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
  theme(legend.position="bottom")

#### BOTTOM LEFT
plotting_attrs <- tibble(obs=c("numerator","gap","denominator"),
                         ring=c(50,50,58),
                         width=c(20,20,10),
                         fill_color=c(DL_BLUE, DL_LIGHT_BORDER, DL_LIGHT_BORDER))

plot_data <- maptg_data %>% 
  select(-group) %>%
  filter(measure == "M3" | measure == "M4",
         ascribee == RECIP) %>%
  summarize(
    numerator = sum(numerator, na.rm=TRUE),
    denominator = sum(unique(denominator), na.rm=TRUE),
    gap = denominator - numerator) %>%
    pivot_longer(cols=c("denominator","numerator", "gap"), names_to = "obs", values_to = "value") %>%
  left_join(plotting_attrs)

numer <- plot_data %>% filter(obs=="numerator") %>% pull(value)
denom <- plot_data %>% filter(obs=="denominator") %>% pull(value)
perf_label <- paste(floor(100*numer/denom), "%",sep="")
  
fig_bot_left <-ggplot(plot_data, aes(x=ring, y=value, fill=fill_color, width=width)) +
  geom_col() +
  scale_fill_identity() +
  scale_x_continuous(limits=c(0,75)) +
  coord_polar(theta="y", direction=-1) +
  dl_annotate("text", x=10, y=denom/2, label=perf_label, size=9, color=DL_BLUE, fontface=2) +
  dl_annotate("text", x=8, y=0, label="", size=5, color=DL_BLUE) +
  dl_annotate("text", x=20, y=0, label=paste(numer, denom, sep="/"),
              size=4, color=DL_BLUE) +
  top_performer_theme() 


### BOTTOM RIGHT
plot_data <- maptg_data %>% 
  filter(measure %in% c("M3","M4")) %>%
  group_by(ascribee, group) %>%
  summarize(numerator = sum(numerator),
            denominator = sum(pull(., numerator)))%>%
  mutate(rate = numerator/denominator)

fig_bot_right <- ggplot(plot_data, aes(x=group, y=rate)) +
  geom_bar(aes(fill=group), stat='identity', color=DL_BLUE) +
  ylab("% of LARC Provided") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = c(DL_GREEN, DL_LIGHT_BLUE), guide=guide_legend()) +
  theme_minimal() +
  theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank()) 

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
  