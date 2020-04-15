library(knitr)
library(tikzDevice)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)


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
          plot.background=element_rect(fill="pink"),
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
          plot.background=element_rect(fill="orange"),
          legend.position = "none")
}

######################
# Process Input Data #
######################
raw_data <- readr::read_csv('micca-data.csv', trim_ws=T)

# de-multiplex measure groups
measure_lookup <- sapply(raw_data$measure, str_split, "-", n=2)
payer_vals <- sapply(measure_lookup, `[`, 1)
observation_vals <- sapply(measure_lookup, `[`, 2)

fmt_data <- raw_data %>%
  mutate(
    month=factor(format(`date`, "%b"), levels=month.abb, ordered = T),
    payer=payer_vals,
    obs=observation_vals)

###########################
# Generate Report Figures #
###########################
###### 
df <- fmt_data %>% filter(obs == "delivered") 

f_wmn_delv_bar <- ggplot(df, aes(x=month, y=denominator)) +
  geom_bar(aes(fill=payer), stat='identity') +
  ylab("Women Delivered") +
  theme(legend.position="bottom", plot.background = element_rect(fill="yellow"),
        axis.title.x = element_blank(), legend.title = element_blank())

######
df <- fmt_data %>% filter(obs == "postpartum-contra-counsel") 

denom <- sum(df$denominator)
numer <- sum(df$numerator)

plot_data <- data.frame(
  group = c("background", "performance", "background"),
  value = c(denom, numer, denom - numer),
  ring = c(58, 50, 50),
  width = c(6,16,16)
)

color_set <- c("#00274C", "#e7edee", "#e7edee")
percentage <- paste(floor(100*(numer / denom)), "%", sep="")
f_counsel_circ <- ggplot(plot_data, aes(x=ring, y=value, fill=group, width=width)) +
  geom_col(position=position_fill(), fill=color_set) +
  scale_x_continuous(limits=c(0, max(plot_data$ring) + max(plot_data$width)/2)) +
  coord_polar(theta="y", direction=-1) +
  dl_annotate("text", x=10, y=0, label=percentage, size=12, color="#00274C", fontface=2) +
  dl_annotate("text", x=26, y=.5, label="patients", size=3, color="#00274C") +
  dl_annotate("text", x=18, y=.5, label=paste(numer, denom, sep="/"), size=4, color="#00274C") +
  top_performer_theme() 

#####
df <- fmt_data %>% filter(obs == "documented-contra-choice")

denom <- sum(df$denominator)
numer <- sum(df$numerator)

plot_data <- data.frame(
  group = c("background", "performance", "background"),
  value = c(denom, numer, denom - numer),
  ring = c(58, 50, 50),
  width = c(6,16,16)
)

color_set <- c("#00274C", "#e7edee", "#e7edee")
percentage <- paste(floor(100*(numer / denom)), "%", sep="")
f_document_circ <- ggplot(plot_data, aes(x=ring, y=value, fill=group, width=width)) +
  geom_col(position=position_fill(), fill=color_set) +
  scale_x_continuous(limits=c(0,max(plot_data$width+plot_data$ring))) +
  coord_polar(theta="y", direction=-1) +
  top_performer_theme() +
  dl_annotate("text", x=10, y=0, label=percentage, size=12, color="#00274C", fontface=2) +
  dl_annotate("text", x=26, y=.5, label="patients", size=3, color="#00274C") +
  dl_annotate("text", x=18, y=.5, label=paste(numer, denom, sep="/"), size=4, color="#00274C")

cap_document_circ <- "Women who delivered this quarter receiving prenatal contraceptive counseling"

########################
# Generate Report Data #
########################
total_delivered <- fmt_data %>% filter(obs == "delivered") %>% pull(denominator) %>% sum


###################
# Generate Report #
###################
template_path <- "micca-template.Rnw"

output_path <- "micca-report.pdf"
build_dir <- tempdir()

options(tinytex.engine="pdflatex")
#knitr::knit2pdf(input = template_path, envir=report_env, pdf_file= output_path)
knitr::knit2pdf(input = template_path, pdf_file=output_path)
