# Load libraries and set theme ----
pacman::p_load("tidyverse", "colorspace", "lubridate", "patchwork", "reshape2", "scales", "ggrepel", "ggfocus", "directlabels", "viridis")

report::cite_packages() # Create a citation for all used packages
devtools::session_info()
packageVersion("tidyverse", "patchwork") #Get package version
version$version.string # Get R version

c("tidyverse", "colorspace", "lubridate", "patchwork") %>%
  map(citation) %>%
  print(style = "text")

# Set themes

theme_set(theme_minimal() +
            theme(text = element_text(size = 12),
                  axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                  axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
                  legend.key.size = unit(0.5, "cm"),
                  legend.text = element_text(size = 10),
                  panel.border = element_rect(color = "#E0E0E0", size = 0.5, fill = NA),
                  strip.background = element_rect(fill = "#FAFAFA", color = "#E0E0E0"),
                  panel.spacing = unit(2, "lines")))


palette_colors <- c(
  `red`         = "#F44336",
  `pink`        = "#E91E63",
  `purple`      = "#9C27B0",
  `deep purple` = "#673AB7",
  `indigo`      = "#3F51B5",
  `blue`        = "#2196F3",
  `light blue`  = "#03A9F4",
  `cyan`        = "#00BCD4",
  `teal`        = "#009688",
  `green`       = "#4CAF50",
  `light green` = "#8BC34A",
  `lime`        = "#CDDC39",
  `yellow`      = "#FFEB3B",
  `amber`       = "#FFC107",
  `orange`      = "#FF9800",
  `deep orange` = "#FF5722",
  `brown`       = "#795548",
  `grey`        = "#9E9E9E",
  `blue grey`   = "#607D8B",
  'covid' = "#00C1B2",
  'noncovid' = "#ED90A4")

# Retrieve a specific color from the palette
palette <- function(color) {
  cols <- c(color)
  if (is.null(cols)) {
    return(NA)
  } else {
    return(unname(palette_colors[color]))
  }
}

# Data import -------------

biorxiv_preprints_basic_20131101_20251231 <- read_csv("output/biorxiv_preprints_basic_20131101_20251231.csv") 
biorxiv_preprints_basic_reviews_20131101_20251231 <- read_csv("output/biorxiv_preprints_basic_reviews_20131101_20251231.csv") 
biorxiv_preprints_enhanced_openalex_data_20131101_20251231 <- read_csv("output/biorxiv_enhanced_openalex_20131101_20251231.csv")
biorxiv_published_20131101_20251231 <- read_csv("output/biorxiv_13_25_published.csv")
medrxiv_published_13_25 <- read_csv("output/medrxiv_published_13_25.csv")

biorxiv_usage_data_20131101_20251231 <- read_csv("output/biorxiv_usage_data_20131101_20251231")
biorxiv_review_data_20131101_202512311 <- read_csv("output/biorxiv_review_data_20131101_20251231.csv")
biorxiv_preprints_enhanced_review_data_20131101_20251231 <- read_csv("output/biorxiv_preprints_enhanced_review_data_20131101_20251231.csv")
biorxiv_openalex_20131101_20251231 <- read_csv("output/biorxiv_openalex_20131101_20251231.csv")

epmc_preprints <- read_csv("output/epmc_2013_2025_complete.csv")

biorxiv_usage_data_clean <- read_csv("output/biorxiv_usage_data_clean.csv")
biorxiv_usage_data_20131101_20251231 <- read_csv("output/biorxiv_usage_data_20131101_20251231")
medrxiv_data_2019_2025 <- read_csv("output/medrxiv_data_19-25.csv")

summary_data <- read_csv("data/biorxiv_category_summary_data.csv") 
summary_data_medrxiv <- read_csv("data/medrxiv_category_summary_data.csv") 
biorxiv_usage_summary <- read_csv("data/biorxiv_usage.csv")
medrxiv_usage_summary <- read_csv("data/medrxiv_usage.csv")
openrxiv_usage_summary <- read_csv("data/openrxiv_usage.csv")
openrxiv_summary <- read_csv("data/openrxiv_summary.csv")


summary_data %>% 
  group_by(Category) %>% 
  summarise_at(vars(Articles), list( name = sum)) 


# Preprints over time, EuPMC -----
epmc_preprints %>% 
  group_by(preprint_server) %>% 
  count(posting_year)  %>% 
  mutate(preprint_server = case_when(
    preprint_server == "bioRxiv" ~ "biorxiv",
    preprint_server == "medRxiv" ~ "medrxiv",
    preprint_server == "Research Square" ~ "Research Square",
    preprint_server == "SSRN" ~ "SSRN",
    preprint_server == "Preprints.org" ~ "Preprints.org",
  #  preprint_server == "ChemRxiv" ~ "ChemRxiv",
    TRUE ~ "other"
    )) %>% #View()
#  filter(posting_year >= "2018") %>% 
  ggplot(aes(x = posting_year, y = n, fill = preprint_server, group = preprint_server, label = preprint_server)) +
    geom_area(alpha=0.8) +
    labs(title = "Preprints since 2013",
         x = "Year", 
         y = "Number of preprints posted", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data") +
    theme_minimal() +
  scale_fill_manual(values = qualitative_hcl(n = 6, palette = "Dynamic")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) #+
  ggsave("figs/euPMC_preprints_posted.png", height = 8, width = 16)

# Cumulative preprints over time, EuPMC -----
epmc_preprints %>% 
    group_by(preprint_server) %>% 
    arrange(posting_year) %>% 
    count(posting_year)  %>% 
    mutate(running_total = cumsum(n)) %>% 
    mutate(preprint_server = case_when(
      preprint_server == "bioRxiv" ~ "biorxiv",
      preprint_server == "medRxiv" ~ "medrxiv",
      preprint_server == "Research Square" ~ "Research Square",
      preprint_server == "SSRN" ~ "SSRN",
      preprint_server == "Preprints.org" ~ "Preprints.org",
      TRUE ~ "other"
    )) %>% #View()
    ggplot(aes(x = posting_year, y = running_total, fill = preprint_server, group = preprint_server, label = preprint_server)) +
    geom_area(alpha=0.8) +
    labs(title = "Preprints since 2013",
         x = "Year", 
         y = "Cumulative number of preprints", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data") +
    theme_minimal() +
    scale_fill_manual(values = qualitative_hcl(n = 6, palette = "Dynamic")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
    annotate(geom="segment", x = 2020, y = 650000, xend = 2022.5) +
    annotate(geom="text", x = 2021, y = 680000, 
           label=" COVID-19") +
    geom_vline(xintercept = 2020, linetype="dotted", 
             color = "blue", linewidth=0.5) +
    geom_vline(xintercept = 2022.5, linetype="dotted", 
               color = "blue", linewidth=0.5) #+
  ggsave("figs/euPMC_preprints_posted_cumulative.png", height = 8, width = 16)  

# Preprints over time, bioRixv -----
# Prepare the data for labels: Filter for the last (maximum) Year in each Category
  label_data <- summary_data %>%
    group_by(Category) %>%
    filter(Year == max(Year)) %>%
    ungroup()
  
summary_data %>%
  ggplot(aes(x = Year, y = Articles, color = Category, group = Category, label = Category)) +
  geom_line(linewidth=1, linetype=1, show.legend = TRUE) +
  labs(title = "Preprints posted to bioRxiv",
       x = "Year", 
       y = "Number of preprints posted", 
       fill = 'Category',
       subtitle = "bioRxiv data") +
  theme_minimal() +
#  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
                     limits = c(min(summary_data$Year), max(summary_data$Year) + 1.5)) +
  scale_color_manual(values = qualitative_hcl(28, palette = "Set2")) + 
  annotate(geom="text", x = 2013, y = 800, 
         label=" Launch \n  of bioRxiv") +
  annotate(geom="point", x =  2013, y = 0, size=10, shape=21, fill="transparent") +
#  geom_dl(aes(label = Category), method = list(dl.trans(x = x + 0.2), "last.points"), cex = 0.8) +
  geom_text_repel(
    data = label_data, # Use the filtered data for labels
    aes(label = Category),
    size = 3,
    # Adjust position to be slightly to the right of the line end
    hjust = 0,
    nudge_x = 0.5,
    # Ensure they don't overlap with each other or the lines
    box.padding = 0.5,
    point.padding = 0.5,
    min.segment.length = 0, # Draw segments even for short distances
    show.legend = FALSE) #+ 
  ggsave("figs/biorxiv_preprints_category_time.png", height = 8, width = 14)

  summary_data %>% 
    group_by(Category) %>% 
    summarise_at(vars(Articles), list( total = sum)) %>% View()
  summarize(mean(total))
  
summary_data %>% 
    group_by(Category) %>% 
    summarise_at(vars(Articles), list( total = sum)) %>% #View()
    ggplot(aes(x = Category, y = total, fill = Category)) +
    geom_col() +
  theme_minimal() +
  labs(title = "Preprints posted to bioRxiv since 2013",
       x = "Category", 
       y = "Total number of preprints posted",
       subtitle = "bioRxiv data") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) #+
  ggsave("figs/total_preprints_posted_counts.png", height = 7, width = 10)


# Preprints over time, medRixv -----
  # Prepare the data for labels: Filter for the last (maximum) Year in each Category
  #  label_data <- summary_data %>%
label_data_medrxiv <- summary_data_medrxiv %>%
    group_by(Category) %>%
    filter(Year == max(Year)) %>%
    ungroup()
  
summary_data_medrxiv %>%
    ggplot(aes(x = Year, y = Articles, color = Category, group = Category, label = Category)) +
    geom_line(linewidth=1, linetype=1, show.legend = TRUE) +
    labs(title = "Preprints posted to bioRxiv",
         x = "Year", 
         y = "Number of preprints posted", 
         fill = 'Category',
         subtitle = "bioRxiv data") +
    theme_minimal() +
    #  guides(fill = guide_legend(nrow = 3)) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks=c(2019, 2020, 2021, 2022, 2023, 2024, 2025),
                       limits = c(min(summary_data_medrxiv$Year), max(summary_data_medrxiv$Year) + 1.5)) +
    scale_color_manual(values = qualitative_hcl(51, palette = "Set2")) + 
    annotate(geom="text", x = 2019, y = 800, 
             label=" Launch \n  of medRxiv") +
    annotate(geom="point", x =  2019, y = 0, size=10, shape=21, fill="transparent") +
    #  geom_dl(aes(label = Category), method = list(dl.trans(x = x + 0.2), "last.points"), cex = 0.8) +
    geom_text_repel(
      data = label_data_medrxiv, # Use the filtered data for labels
      aes(label = Category),
      size = 3,
      # Adjust position to be slightly to the right of the line end
      hjust = 0,
      nudge_x = 0.5,
      # Ensure they don't overlap with each other or the lines
      box.padding = 0.5,
      point.padding = 0.5,
      min.segment.length = 0, # Draw segments even for short distances
      show.legend = FALSE) #+ 
  ggsave("figs/medrxiv_preprints_category_time.png", height = 8, width = 14)
  
summary_data_medrxiv %>% 
    group_by(Category) %>% 
    summarise_at(vars(Articles), list( total = sum)) %>% #View()
  summarize(mean(total))
  
summary_data_medrxiv %>% 
    group_by(Category) %>% 
    summarise_at(vars(Articles), list( total = sum)) %>% #View()
    ggplot(aes(x = Category, y = total, fill = Category)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Preprints posted to medRxiv since 2013",
         x = "Category", 
         y = "Total number of preprints posted",
         subtitle = "medRxiv report data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) #+
  ggsave("figs/total_preprints_posted_counts_medrxiv.png", height = 7, width = 10)  
  
# Plots -----

## Europe PMC data ----

### Preprints per server ----
epmc_preprints %>% 
    count(preprint_server)  %>% 
    mutate(preprint_server = case_when(
      preprint_server == "bioRxiv" ~ "biorxiv",
      preprint_server == "medRxiv" ~ "medrxiv",
      preprint_server == "Research Square" ~ "Research Square",
      preprint_server == "SSRN" ~ "SSRN",
      preprint_server == "Preprints.org" ~ "Preprints.org",
      TRUE ~ "other"
    )) %>% #View()
    ggplot(aes(reorder(x = preprint_server, -n), y = n, fill = preprint_server, group = preprint_server, label = preprint_server)) +
    geom_col(alpha=0.8) +
    labs(title = "Preprints since 2013",
         x = "Preprint server", 
         y = "Number of preprints posted", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data") +
    theme(axis.text.x = element_text(angle = 45)) +
    theme_minimal() +
    scale_fill_manual(values = qualitative_hcl(n = 7, palette = "Set2")) +
    theme(legend.position = "none") #+
  ggsave("figs/euPMC_preprints_per_server.png", height = 4, width = 6)
  
### licences ----  
epmc_preprints %>% 
    group_by(preprint_server) %>% 
    count(license)  %>% 
    na.omit() %>% 
    mutate(preprint_server = case_when(
      preprint_server == "bioRxiv" ~ "biorxiv",
      preprint_server == "medRxiv" ~ "medrxiv",
      preprint_server == "Research Square" ~ "Research Square",
      preprint_server == "SSRN" ~ "SSRN",
      preprint_server == "Preprints.org" ~ "Preprints.org",
      TRUE ~ "other"
    )) %>% #View()
    ggplot(aes(x = license, y = n, fill = preprint_server, group = preprint_server, label = preprint_server)) +
    geom_col(alpha=0.8) +
    labs(title = "Preprint licences used across preprint servers",
         x = "Licence", 
         y = "Count (preprint licence)", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data (licence info not present for every preprint)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    scale_fill_manual(values = qualitative_hcl(n = 7, palette = "Set2")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1)) #+
  ggsave("figs/euPMC_preprints_licence.png", height = 4, width = 8)  
  
### Evaluations ----  
epmc_preprints %>% 
#    group_by(preprint_server) %>% 
    filter(preprint_server == "bioRxiv") %>% 
    count(evaluated)  %>% 
    na.omit() %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% 
    # mutate(preprint_server = case_when(
    #   preprint_server == "bioRxiv" ~ "bioRxiv",
    #   preprint_server == "medRxiv" ~ "medRxiv",
    #   preprint_server == "Research Square" ~ "Research Square",
    #   preprint_server == "SSRN" ~ "SSRN",
    #   preprint_server == "Preprints.org" ~ "Preprints.org",
    #   preprint_server == "ChemRxiv" ~ "ChemRxiv",
    #   TRUE ~ "other"
    # )) %>% View()
    ggplot(aes(x = evaluated, y = proportion, fill = evaluated)) +
    geom_col(alpha=0.8) +
    labs(title = "bioRxiv preprints evaluated",
         x = "Evaluated", 
         y = "Percentage (preprints evaluated)", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_text(aes(label = proportion), hjust = 0.5, nudge_y = -2) +
    scale_fill_manual(values = qualitative_hcl(n = 7, palette = "Set2")) +
    theme(legend.position = "none") #+
  ggsave("figs/euPMC_preprints_evaluated_biorxiv.png", height = 4, width = 4)    

  
epmc_preprints %>% 
  mutate(preprint_server = case_when(
      preprint_server == "bioRxiv" ~ "bioRxiv",
      preprint_server == "medRxiv" ~ "medRxiv",
     preprint_server == "Research Square" ~ "Research Square",
     preprint_server == "Preprints.org" ~ "Preprints.org",
       TRUE ~ "other")) %>% 
 # filter(preprint_server != "other") %>% 
  group_by(preprint_server) %>% 
  count(evaluated)  %>% 
  na.omit() %>% 
  mutate(proportion = (n/sum(n)) * 100) %>% #View()
  ggplot(aes(x = preprint_server, y = proportion, fill = evaluated)) +
  geom_col(alpha=0.8, position = "dodge") +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  labs(title = "bioRxiv preprints evaluated",
       x = "Preprint Server", 
       y = "Percentage (preprints evaluated)", 
       subtitle = "Europe PMC data; excludes journal-organised peer review") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = qualitative_hcl(n = 7, palette = "Set2")) +
  theme(legend.position = "right") #+
ggsave("figs/euPMC_preprints_evaluated_server.png", height = 4, width = 6)    

epmc_preprints %>% 
  mutate(preprint_server = case_when(
    preprint_server == "bioRxiv" ~ "bioRxiv",
    preprint_server == "medRxiv" ~ "medRxiv",
    preprint_server == "Research Square" ~ "Research Square",
    preprint_server == "Preprints.org" ~ "Preprints.org",
    TRUE ~ "other")) %>% 
  # filter(preprint_server != "other") %>% 
  group_by(posting_year, preprint_server) %>% 
  count(evaluated) %>% 
  na.omit() %>% 
  group_by(posting_year, preprint_server) %>% 
  mutate(proportion = (n / sum(n)) * 100) %>% 
  filter(evaluated == "Y" | evaluated == "yes") %>% 
  ggplot(aes(x = posting_year, y = proportion, color = preprint_server)) +
  geom_line(linewidth = 1) +
  geom_point(alpha = 0.5) +
  labs(title = "Preprint Evaluations Over Time",
       subtitle = "Europe PMC data",
       x = "Year", 
       y = "Percentage of preprints evaluated (%)", 
       color = 'Preprint Server') +
  scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
  scale_color_manual(values = qualitative_hcl(n = 5, palette = "Set2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) #+
ggsave("figs/euPMC_preprints_evaluated_server_time.png", height = 4, width = 6)    

### Citations ----
epmc_preprints %>% 
  mutate(preprint_server = case_when(
    preprint_server == "bioRxiv" ~ "bioRxiv",
    preprint_server == "medRxiv" ~ "medRxiv",
    preprint_server == "Research Square" ~ "Research Square",
    preprint_server == "Preprints.org" ~ "Preprints.org",
    TRUE ~ "other")) %>% 
  group_by(posting_year, preprint_server) %>% 
  summarise(total_citations = sum(citations, na.rm = TRUE), .groups = "drop") %>% 
  arrange(preprint_server, posting_year) %>% 
  group_by(preprint_server) %>% 
  mutate(cumulative_citations = cumsum(total_citations)) %>% 
  mutate(date_fixed = as.Date(paste0(posting_year, "-01-01"))) %>% 
  ggplot(aes(x = date_fixed, y = cumulative_citations, color = preprint_server)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Cumulative Citations per Preprint Server",
       subtitle = "Total citation growth over time (Europe PMC data)",
       x = "Year", 
       y = "Total Citations (Cumulative)", 
       color = 'Preprint Server') +
  scale_color_manual(
    values = qualitative_hcl(n = 5, palette = "Set2"),
    labels = function(x) str_wrap(x, width = 15)
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom") #+
  ggsave("figs/euPMC_preprints_cited_server_time.png", height = 4, width = 6)    

    
## biorxiv data -----  
### Licence -----
biorxiv_preprints_basic_20131101_20251231 %>% 
  count(license)  %>% 
  mutate(proportion = (n/sum(n)) * 100) %>% #View()
  mutate(license = factor(license,
                          levels = c("cc0", "cc_by", "cc_by_nc", "cc_by_nd", 
                                     "cc_by_nc_nd", "cc_no", "cc0_ng"),
                          labels = c("CC0", "CC BY", "CC BY-NC", "CC BY-ND", 
                                     "CC BY-NC-ND", "None", "CC0 NG?"))) %>%
  na.omit() %>% 
  ggplot(aes(x = license, y = proportion, fill = license)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
#  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
  labs(title = "Licenses used for bioRxiv preprints",
       x = "License", 
       y = "Percentage",
      subtitle = "bioRxiv data") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #+
  ggsave("figs/biorxiv_licence.png", height = 6, width = 8)


### Licence data over time ----
 # 1. Prepare the plotting data first
  license_data <- biorxiv_preprints_basic_20131101_20251231 %>%
    group_by(posted_year, license) %>%
    count() %>%
    ungroup() %>%
    mutate(license = factor(license,
                            levels = c("cc0", "cc_by", "cc_by_nc", "cc_by_nd", 
                                       "cc_by_nc_nd", "cc_no", "cc0_ng"),
                            labels = c("CC0", "CC BY", "CC BY-NC", "CC BY-ND", 
                                       "CC BY-NC-ND", "None", "CC0 NG?"))) %>%
    na.omit()
  # 2. Create labeling data (Fix: y must be the count 'n', not the license name)
  label_data_licence <- license_data %>%
    group_by(license) %>%
    filter(posted_year == max(posted_year))
  # 3. Generate the Plot
  ggplot(license_data, aes(x = posted_year, y = n, color = license, group = license)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    # Fix: Replaced summary_data reference with plot_data
    scale_x_continuous(breaks = seq(2013, 2025, 1),
                       limits = c(2013, 2026.5)) + 
    scale_color_manual(values = qualitative_hcl(7, palette = "Set2")) + 
    labs(title = "Licenses used for bioRxiv preprints over time",
         subtitle = "bioRxiv data (2013-2025)",
         x = "Posted Year", 
         y = "Number of Preprints") +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    # Fix: Ensure label coordinates match the line ends
    geom_label_repel(data = label_data_licence, 
                     aes(x = posted_year, y = n, label = license),
                     nudge_x = 0.5, 
                     direction = "y",
                     hjust = 0,
                     segment.size = 0.2,
                     inherit.aes = FALSE) #+
  ggsave("figs/biorxiv_licence_per_year.png", height = 6, width = 12)    
  
### Corresponding author institute -----
biorxiv_preprints_basic_20131101_20251231 %>% 
    count(author_corresponding_institution)  %>% #View()
    filter(n > 855) %>% # Match to only filter top ten
    ggplot(aes(x = reorder(author_corresponding_institution, -n), y = n, fill = author_corresponding_institution)) +
    geom_col() +
    theme_minimal() +
    scale_fill_manual(values = qualitative_hcl(n = 11, palette = "Set2")) +
    labs(title = "Top 10 institutions posting bioRxiv preprints",
         x = "corresponding author institution", 
         y = "count",
         subtitle = "bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) #+
  ggsave("figs/biorxiv_top_institution.png", height = 6, width = 8)  

### Corresponding author  -----
biorxiv_preprints_basic_20131101_20251231 %>% 
    count(author_corresponding)  %>% #View()
    na.omit() %>% 
    filter(n > 34) %>% 
    ggplot(aes(x = reorder(author_corresponding, -n), y = n, fill = author_corresponding)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Top 10 authors posting bioRxiv preprints",
         x = "corresponding author", 
         y = "count",
         subtitle = "bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/biorxiv_top_author.png", height = 6, width = 8)   
  
### Versioning  -----
biorxiv_preprints_basic_20131101_20251231 %>% 
    count(n_versions)  %>% 
    mutate(n_versions = case_when(
      n_versions == 1 ~ "1",
      n_versions == 2 ~ "2", 
      n_versions == 3 ~ "3",
      n_versions == 4 ~ "4",
      n_versions > 4 ~ "5+")) %>% 
    na.omit() %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% 
    ggplot(aes(x = n_versions, y = proportion, fill = n_versions)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Number of preprint versions",
         x = "Versions", 
         y = "Percentage",
         subtitle = "bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/biorxiv_versions.png", height = 4, width = 4)     

### Preprint type  -----  
biorxiv_preprints_basic_20131101_20251231 %>% 
    group_by(posted_year) %>%
    count() %>% View()
  
## bioRxiv Usage ------  
  
biorxiv_usage_data_20131101_20251231 %>% 
    group_by(posted_year) %>%
    summarise(total_views = sum(sum_views_total, na.rm = TRUE)) %>%
    ggplot(aes(x = posted_year, y = total_views)) +
    geom_line(color = "#2c3e50", linewidth = 1) +
    geom_point(color = "#e74c3c", size = 3) +
    scale_y_continuous(labels = scales::comma) + # Format numbers with commas
    scale_x_continuous(breaks = seq(2013, 2025, 1)) +
    labs(
      title = "Total bioRxiv Views per Year",
      subtitle = "Data from Nov 2013 to Dec 2025",
      x = "Year",
      y = "Total Views",
      caption = "Source: bioRxiv Usage Data"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank())
  
  
### Usage summary, totals -----
biorxiv_usage_summary %>% 
    select(month, abstract_views, full_text_views, pdf_downloads) %>% 
    mutate(afp_views = rowSums(across(where(is.numeric))),
           year = format(as.Date(biorxiv_usage_summary$month, format="%Y-%m-%d"),"%Y")) %>% #View()
    group_by(year) %>% 
    ggplot(aes(x = year, y = afp_views, fill = year)) +
    geom_col(alpha=0.8) +
    labs(title = "Preprint downloads and views",
         x = "Year", 
         y = "Total preprints views and downloads", 
         fill = 'Category',
         subtitle = "bioRxiv reports data") +
    theme_minimal() +
    scale_y_continuous(
      labels = label_comma(),           # Removes scientific notation
      breaks = seq(0, 2000000000, by = 25000000)) +  # 25M increments
  scale_color_manual(values = qualitative_hcl(13, palette = "Set2")) +
  theme(legend.position = "none") #+
  ggsave("figs/biorxiv_usage_summary.png", height = 4, width = 6)     
    
  
### Usage summary - cumulative totals -----
usage_plot  <- biorxiv_usage_summary %>% 
      # Create a total for each specific month
      mutate(
        afp_views = abstract_cumulative + full_text_cumulative + pdf_cumulative,
        year_num = as.numeric(format(as.Date(month), "%Y")),
        date_obj = as.Date(month)
      ) %>% 
      group_by(year_num) %>% 
      filter(date_obj == max(date_obj)) %>% 
      ungroup() 
    
ggplot(usage_plot, aes(x = year_num, y = afp_views)) +
      geom_line(color = "#2c3e50", linewidth = 1) + 
      geom_point(aes(color = factor(year_num)), size = 3) +
      # Make the labels pretty
      scale_y_continuous(labels = scales::label_comma()) +
      scale_x_continuous(breaks = seq(min(usage_plot$year_num), max(usage_plot$year_num), by = 1)) +
      scale_color_manual(values = qualitative_hcl(nrow(usage_plot), palette = "Set2")) +
      labs(
        title = "Total Cumulative bioRxiv Usage",
        subtitle = "Data represents the status at the end of each year",
        x = "Year", 
        y = "Total Cumulative Views/Downloads"
      ) +
      theme_minimal() +
      theme(legend.position = "none") #+
   ggsave("figs/biorxiv_usage_summary_cumulative.png", height = 4, width = 8)     

   
###  Usage summary - cumulative totals by type -----  
# 1. Reshape and Process Data
   usage_plot_2 <- biorxiv_usage_summary %>%
     mutate(year = format(as.Date(biorxiv_usage_summary$month, format="%Y-%m-%d"),"%Y")) %>% 
     select(year, abstract_views, full_text_views, pdf_downloads) %>%
     pivot_longer(
       cols = c(abstract_views, full_text_views, pdf_downloads), 
       names_to = "category", 
       values_to = "view_count"
     ) %>%
     group_by(category, year) %>%
     summarise(annual_sum = sum(view_count, na.rm = TRUE), .groups = "drop_last") %>%
     mutate(cumulative_views = cumsum(annual_sum)) %>%
     ungroup()
# 2. Plot
   ggplot(usage_plot_2, aes(x = factor(year), y = cumulative_views, fill = category)) +
     geom_col(alpha = 0.9) + # This automatically stacks the categories
     scale_y_continuous(
       labels = label_comma(),           # No scientific notation
       breaks = seq(0, 1000000000, by = 25000000)) +  
     scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
     labs(
       title = "Cumulative Preprint Usage by Category",
       x = "Year",
       y = "Total views and downloads",
       fill = "Metric Type") +
     theme_minimal() +
     theme(legend.position = "bottom",
       panel.grid.minor = element_blank()) #+
   ggsave("figs/biorxiv_usage_breakdown_cumulative.png", height = 5, width = 8) 
 
###  Usage summary -  totals by type -----  
   # 1. Reshape and Process Data
   usage_plot_3 <- biorxiv_usage_summary %>%
     mutate(year = format(as.Date(biorxiv_usage_summary$month, format="%Y-%m-%d"),"%Y")) %>% 
     select(year, abstract_views, full_text_views, pdf_downloads) %>%
     pivot_longer(
       cols = c(abstract_views, full_text_views, pdf_downloads), 
       names_to = "category", 
       values_to = "view_count"
     ) %>%
     group_by(category, year) %>%
     summarise(annual_sum = sum(view_count, na.rm = TRUE), .groups = "drop_last") %>%
     ungroup()
# 2. Plot
   ggplot(usage_plot_3, aes(x = factor(year), y = annual_sum, fill = category)) +
     geom_col(alpha = 0.9) + # This automatically stacks the categories
     scale_y_continuous(
       labels = label_comma(),           # No scientific notation
       breaks = seq(0, 1000000000, by = 25000000)) +  
     scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
     labs(
       title = "Preprint Usage by type of view",
       x = "Year",
       y = "Total views and downloads",
       fill = "Metric Type") +
     theme_minimal() +
     theme(legend.position = "bottom",
           panel.grid.minor = element_blank()) #+
   ggsave("figs/biorxiv_usage_breakdown_yearly.png", height = 5, width = 8)      
   
### Total over time -----
biorxiv_usage_data_20131101_20251231 %>% 
    group_by(posted_year) %>% 
    ggplot(aes(x = posted_year, y = sum_views_total, fill = posted_year, group = posted_year)) +
    geom_col(alpha=0.8) +
    labs(title = "Preprint licences used across preprint servers",
         x = "Licence", 
         y = "Count (preprint licence)", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data (licence info not present for every preprint)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
 #   scale_fill_manual(values = qualitative_hcl(n = 7, palette = "Set2")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1)) #+
    ggsave("figs/euPMC_preprints_licence.png", height = 4, width = 8) 
  
## biorxiv review data  ----
biorxiv_preprints_basic_reviews_20131101_20251231 %>% 
  mutate(provider = if_else(str_detect(provider, "PCI"), "PCI", provider)) %>% 
  count(provider) %>% # View()
  na.omit() %>% 
  ggplot(aes(x = provider, y = n, fill = provider)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Preprint evaluations",
       x = "Review provider", 
       y = "Number of preprints reviewed",
       subtitle = "bioRxiv data") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
ggsave("figs/biorxiv_review_providers.png", height = 4, width = 6)  
  
### reviews by category ----
biorxiv_preprints_basic_reviews_20131101_20251231 %>%
  filter(!is.na(provider)) %>% 
  mutate(category = stringr::str_to_sentence(category)) %>% 
  count(category)  %>% 
  na.omit() %>% #View()
  ggplot(aes(x = category, y = n, fill = category)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Non-journal organised reviews",
       x = "Category", 
       y = "Number of preprints reviewed",
       subtitle = "bioRxiv data") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
ggsave("figs/biorxiv_review_categories.png", height = 4, width = 6)  

### Time between posting preprint and review ----
biorxiv_preprints_basic_reviews_20131101_20251231 %>% 
  mutate(provider = if_else(str_detect(provider, "PCI"), "PCI", provider)) %>% 
  mutate(delay_in_days = as.numeric(ymd(Review_date) -  ymd(date))) %>% #View()
  mutate(pub_bracket = cut(as.numeric(delay_in_days), 
                           seq(0, 360, by = 10), 
                           labels=seq(0, 350, by = 10))) %>% 
  group_by(provider) %>% 
  count(pub_bracket) %>%  
  mutate(prop = n*100 / sum(n)) %>%
  filter(prop > 8) %>% # to make data more readable
  filter(!is.na(pub_bracket)) %>%
  filter(!is.na(provider)) %>% 
  ggplot(aes(x = pub_bracket, y = prop, fill=provider, color=provider)) +
  geom_bar(alpha = 0.25, width = 1, stat = "identity", position="identity") +
  scale_x_discrete(breaks = as.character(seq(0, 350, by = 50))) +
  labs(x = "Time from preprint posting to evaluation (days)",
       y = "% of all preprints",
       subtitle = "Excluding the smallest services",
       title = "Delay between preprint posting and evaluation") +
#  scale_color_manual(values = c(qualitative_hcl(2, palette = "Set2"), "grey60")) +
#  scale_fill_manual(values = c(qualitative_hcl(2, palette = "Set2"), "grey60")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8)) #+
  ggsave("figs/biorxiv_review_delay_posting_to_review.png", height = 4, width = 8)  

### reviewed preprints by year of posting -----
biorxiv_preprints_basic_reviews_20131101_20251231 %>%
  #  mutate(posted_year = format(as.Date(biorxiv_preprints_basic_reviews_20131101_20251231$preprint_date, format="%Y-%m-%d"),"%Y")) %>% 
   filter(Review_date) # filter to only select preprints that are evaluated
     count(posted_year)  %>% 
    na.omit() %>% #View()
    ggplot(aes(x = posted_year, y = n, fill = posted_year)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints evaluated",
         x = "Year of preprint posting", 
         y = "Number of preprints published",
         subtitle = "bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/biorxiv_evaluated_posting_year.png", height = 4, width = 6)    

  
## bioRxiv published data ----
  
### location of published preprints ----
biorxiv_published_20131101_20251231 %>% 
    count(published_journal)  %>% #View()
    filter(n > 1000) %>% 
    na.omit() %>% 
    ggplot(aes(x = reorder(published_journal, -n), y = n, fill = published_journal)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints published",
         x = "Category", 
         y = "Number of preprints published",
         subtitle = "venues with > 1000 published preprints; bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/biorxiv_published_journal.png", height = 4, width = 8)  
  
### published preprints by year of posting -----
biorxiv_published_20131101_20251231 %>%
    mutate(posted_year = format(as.Date(biorxiv_published_20131101_20251231$preprint_date, format="%Y-%m-%d"),"%Y")) %>% 
    count(posted_year)  %>% 
    na.omit() %>% #View()
    ggplot(aes(x = posted_year, y = n, fill = posted_year)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints published",
         x = "Year of preprint posting", 
         y = "Number of preprints published",
         subtitle = "bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/biorxiv_published_posting_year.png", height = 4, width = 6)  
  
### published preprints by category  -----
biorxiv_published_20131101_20251231 %>%
    mutate(preprint_category = stringr::str_to_sentence(preprint_category)) %>% 
    count(preprint_category)  %>% 
    na.omit() %>% #View()
    ggplot(aes(x = preprint_category, y = n, fill = preprint_category)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints published",
         x = "Category", 
         y = "Number of preprints published",
         subtitle = "bioRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/biorxiv_published_categories.png", height = 4, width = 6)  
  
### Time to publish ----
biorxiv_published_20131101_20251231 %>% 
  mutate(days = as.Date(published_date) - as.Date(preprint_date)) %>%
    ggplot() +
    geom_histogram(aes(as.numeric(days)),
                   binwidth = 1,
                   fill = "#cccccc") +
    labs(x = "Days between preprint deposition and journal publication",
         y= "Number of articles",
         title ="Time to publication") +
    coord_cartesian(xlim = c(-100, 1000)) +
    theme_minimal() +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      plot.title = element_text(face = "bold")) #+
    ggsave("figs/biorxiv_published_delay.png", height = 4, width = 6)  
  
biorxiv_published_20131101_20251231 %>% 
      mutate(days = as.Date(published_date) - as.Date(preprint_date)) %>% 
  #summarise(avg_value = mean(days, na.rm = TRUE))
  summarise(avg_value = median(days, na.rm = TRUE))

### bin delay
biorxiv_published_20131101_20251231 %>%
  mutate(diff_days = as.numeric(as.Date(published_date) - as.Date(preprint_date)),
    day_bin = cut(diff_days, 
                  breaks = c(-Inf, 0, 30, 90, 180, 365, Inf),
                  labels = c("Same day/Before", "1 Month", "1-3 Months", 
                             "3-6 Months", "6-12 Months", "> 1 Year"))) %>%
  count(day_bin) %>%
  na.omit() %>% 
  ggplot(aes(x = day_bin, y = n, fill = day_bin)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Time from Preprint to Publication",
       x = "Time Range",
       y = "Number of Papers",
       subtitle = "bioRxiv data") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = "none") #+
  ggsave("figs/biorxiv_published_delay_bin.png", height = 4, width = 4)  

  
## medRxiv data -----
  
medrxiv_data_2019_2025 %>% 
    count(category) %>% #View()
    ggplot(aes(x = category, y = n, fill = category)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Preprints posted to medRxiv since 2019",
         x = "Category", 
         y = "Total number of preprints posted",
         subtitle = "medRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
  ggsave("figs/total_preprints_posted_counts_medrxiv.png", height = 6, width = 8)  
  
### Licence -----
medrxiv_data_2019_2025 %>% 
    count(license)  %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% #View()
    mutate(license = factor(license,
                            levels = c("cc0", "cc_by", "cc_by_nc", "cc_by_nd", 
                                       "cc_by_nc_nd", "cc_no", "cc0_ng"),
                            labels = c("CC0", "CC BY", "CC BY-NC", "CC BY-ND", 
                                       "CC BY-NC-ND", "None", "CC0 NG?"))) %>%
    na.omit() %>% 
    ggplot(aes(x = license, y = proportion, fill = license)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    #  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
    labs(title = "Licenses used for medRxiv preprints",
         x = "License", 
         y = "Percentage",
         subtitle = "medRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #+
  ggsave("figs/medrxiv_licence.png", height = 6, width = 8) 

medrxiv_data_2019_2025 %>% 
    mutate(license = case_when(
      str_detect(license, "(?i)cc-by$|cc-by-4.0|public-domain|cc0") ~ "Open",
      TRUE ~ "Closed"
    )) %>% 
    group_by(posted_year, license) %>% 
    count() %>% 
    group_by(posted_year) %>% 
    mutate(proportion = (n / sum(n)) * 100) %>% 
    ungroup() %>% 
    filter(!is.na(posted_year)) %>% 
    ggplot(aes(x = posted_year, y = proportion, color = license, group = license)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Open" = "#2ECC71", "Closed" = "#E74C3C")) + # Green for Open, Red for Closed
    scale_x_continuous(breaks=c(2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
    labs(
      title = "Shift Toward Open Licensing on medRxiv",
      subtitle = "Open: CC-BY and Public Domain | Closed: All other license types; medRxiv data",
      x = "Publication Year",
      y = "Percentage of Preprints (%)",
      color = "License Category") +
    theme_minimal() +
    theme(legend.position = "bottom") #+
  ggsave("figs/openalex_biorxiv_licence_Open_Closed_year.png", height = 4, width = 6)  
  
### Versioning  -----
medrxiv_data_2019_2025 %>% 
    count(n_versions)  %>% 
    mutate(n_versions = case_when(
      n_versions == 1 ~ "1",
      n_versions == 2 ~ "2", 
      n_versions == 3 ~ "3",
      n_versions == 4 ~ "4",
      n_versions > 4 ~ "5+")) %>% 
    na.omit() %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% 
    ggplot(aes(x = n_versions, y = proportion, fill = n_versions)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Number of preprint versions",
         x = "Versions", 
         y = "Percentage",
         subtitle = "medRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/medrxiv_versions.png", height = 4, width = 4)  

## medRxiv published data ----
  
### location of published preprints ----
medrxiv_published_13_25 %>% 
    count(published_journal)  %>% #View()
    filter(n > 199) %>% 
    na.omit() %>% 
    ggplot(aes(x = reorder(published_journal, -n), y = n, fill = published_journal)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints published",
         x = "Category", 
         y = "Number of preprints published",
         subtitle = "venues with >= 200 published preprints; medRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/medrxiv_published_journal.png", height = 4, width = 8)  
  
### published preprints by category  -----
medrxiv_published_13_25 %>%
    mutate(preprint_category = stringr::str_to_sentence(preprint_category)) %>% 
    count(preprint_category)  %>% 
    na.omit() %>% #View()
    ggplot(aes(x = preprint_category, y = n, fill = preprint_category)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints published",
         x = "Category", 
         y = "Number of preprints published",
         subtitle = "medRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/medrxiv_published_categories.png", height = 4, width = 8) 
  
### published preprints by year of posting -----
medrxiv_published_13_25 %>%
    mutate(posted_year = format(as.Date(medrxiv_published_13_25$preprint_date, format="%Y-%m-%d"),"%Y")) %>% 
    count(posted_year)  %>% 
    na.omit() %>% #View()
    ggplot(aes(x = posted_year, y = n, fill = posted_year)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    labs(title = "Preprints published",
         x = "Year of preprint posting", 
         y = "Number of preprints published",
         subtitle = "medRxiv data") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  ggsave("figs/medrxiv_published_posting_year.png", height = 4, width = 6)  
  
  ### Time to publish ----
medrxiv_published_13_25 %>% 
    mutate(days = as.Date(published_date) - as.Date(preprint_date)) %>%
    ggplot() +
    geom_histogram(aes(as.numeric(days)),
                   binwidth = 1,
                   fill = "#cccccc") +
    labs(x = "Days between preprint deposition and journal publication",
         y= "Number of articles",
         title ="Time to publication") +
    coord_cartesian(xlim = c(-100, 1000)) +
    theme_minimal() +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          plot.title = element_text(face = "bold")) #+
  ggsave("figs/medrxiv_published_delay.png", height = 4, width = 6)  
  
medrxiv_published_13_25 %>% 
    mutate(days = as.Date(published_date) - as.Date(preprint_date)) %>% 
 #   summarise(avg_value = mean(days, na.rm = TRUE))
    summarise(avg_value = median(days, na.rm = TRUE))
  
  ### bin delay
medrxiv_published_13_25 %>%
    mutate(diff_days = as.numeric(as.Date(published_date) - as.Date(preprint_date)),
           day_bin = cut(diff_days, 
                         breaks = c(-Inf, 0, 30, 90, 180, 365, Inf),
                         labels = c("Same day/Before", "1 Month", "1-3 Months", 
                                    "3-6 Months", "6-12 Months", "> 1 Year"))) %>%
    count(day_bin) %>%
    na.omit() %>% 
    ggplot(aes(x = day_bin, y = n, fill = day_bin)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Time from Preprint to Publication",
         x = "Time Range",
         y = "Number of Papers",
         subtitle = "medRxiv data") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    theme(legend.position = "none") #+
  ggsave("figs/medrxiv_published_delay_bin.png", height = 4, width = 4)    
    
  
## medRxiv Usage ------  
  
### Usage summary, totals -----
  medrxiv_usage_summary %>% 
    select(month, abstract_views, full_text_views, pdf_downloads) %>% 
    mutate(afp_views = rowSums(across(where(is.numeric))),
           year = format(as.Date(medrxiv_usage_summary$month, format="%Y-%m-%d"),"%Y")) %>% #View()
    group_by(year) %>% 
    ggplot(aes(x = year, y = afp_views, fill = year)) +
    geom_col(alpha=0.8) +
    labs(title = "Preprint downloads and views",
         x = "Year", 
         y = "Total preprints views and downloads", 
         fill = 'Category',
         subtitle = "bioRxiv reports data") +
    theme_minimal() +
    scale_y_continuous(
      labels = label_comma(),           # Removes scientific notation
      breaks = seq(0, 2000000000, by = 25000000)) +  # 25M increments
    scale_color_manual(values = qualitative_hcl(13, palette = "Set2")) +
    theme(legend.position = "none") #+
  ggsave("figs/medrxiv_usage_summary.png", height = 4, width = 6)     
  
  
###  Usage summary - cumulative totals by type -----  
  # 1. Reshape and Process Data
  usage_plot_2m <- medrxiv_usage_summary %>%
    mutate(year = format(as.Date(medrxiv_usage_summary$month, format="%Y-%m-%d"),"%Y")) %>% 
    select(year, abstract_views, full_text_views, pdf_downloads) %>%
    pivot_longer(
      cols = c(abstract_views, full_text_views, pdf_downloads), 
      names_to = "category", 
      values_to = "view_count"
    ) %>%
    group_by(category, year) %>%
    summarise(annual_sum = sum(view_count, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(cumulative_views = cumsum(annual_sum)) %>%
    ungroup()
  # 2. Plot
  ggplot(usage_plot_2m, aes(x = factor(year), y = cumulative_views, fill = category)) +
    geom_col(alpha = 0.9) + # This automatically stacks the categories
    scale_y_continuous(
      labels = label_comma(),           # No scientific notation
      breaks = seq(0, 1000000000, by = 25000000)) +  
    scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
    labs(
      title = "Cumulative Preprint Usage by Category",
      x = "Year",
      y = "Total views and downloads",
      fill = "Metric Type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank()) #+
  ggsave("figs/medrxiv_usage_breakdown_cumulative.png", height = 5, width = 8) 
  
###  Usage summary -  totals by type -----  
  # 1. Reshape and Process Data
  usage_plot_3m <- medrxiv_usage_summary %>%
    mutate(year = format(as.Date(medrxiv_usage_summary$month, format="%Y-%m-%d"),"%Y")) %>% 
    select(year, abstract_views, full_text_views, pdf_downloads) %>%
    pivot_longer(
      cols = c(abstract_views, full_text_views, pdf_downloads), 
      names_to = "category", 
      values_to = "view_count"
    ) %>%
    group_by(category, year) %>%
    summarise(annual_sum = sum(view_count, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()
  # 2. Plot
  ggplot(usage_plot_3m, aes(x = factor(year), y = annual_sum, fill = category)) +
    geom_col(alpha = 0.9) + # This automatically stacks the categories
    scale_y_continuous(
      labels = label_comma(),           # No scientific notation
      breaks = seq(0, 1000000000, by = 25000000)) +  
    scale_fill_manual(values = qualitative_hcl(3, palette = "Set2")) +
    labs(
      title = "Preprint Usage by type of view",
      x = "Year",
      y = "Total views and downloads",
      fill = "Metric Type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank()) #+
  ggsave("figs/medrxiv_usage_breakdown_yearly.png", height = 5, width = 8)      
  
  ### Total over time -----
  biorxiv_usage_data_20131101_20251231 %>% 
    group_by(posted_year) %>% 
    ggplot(aes(x = posted_year, y = sum_views_total, fill = posted_year, group = posted_year)) +
    geom_col(alpha=0.8) +
    labs(title = "Preprint licences used across preprint servers",
         x = "Licence", 
         y = "Count (preprint licence)", 
         fill = 'Preprint server',
         subtitle = "Europe PMC data (licence info not present for every preprint)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    #   scale_fill_manual(values = qualitative_hcl(n = 7, palette = "Set2")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1)) #+
  ggsave("figs/euPMC_preprints_licence.png", height = 4, width = 8)   

# OpenRxiv Usage summary -----
openrxiv_usage_summary %>% 
    mutate(month = as.Date(month), 
      year = format(month, "%Y"),
      afp_views = rowSums(across(c(abstract_views, full_text_views, pdf_downloads)), na.rm = TRUE)) %>% 
    group_by(year, source) %>% 
    summarise(total_views = sum(afp_views, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = year, y = total_views, fill = source)) +
    geom_col(alpha = 0.8, position = "stack") +
    scale_y_continuous(labels = label_comma()) + # Dynamic breaks are usually better
    scale_fill_discrete_qualitative(palette = "Set2") +
    labs(title = "openRxiv downloads and views",
         subtitle = "openRxiv reports data",
         x = "Year", 
         y = "Total Views & Downloads", 
         fill = 'Server Source') +
    theme_minimal() +
    theme(legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1))  #+
  ggsave("figs/openrxiv_usage_summary.png", height = 4, width = 6)    

openrxiv_usage_summary %>% 
    mutate(month = as.Date(month), 
      year = format(month, "%Y")) %>% 
    pivot_longer(
      cols = c(abstract_views, full_text_views, pdf_downloads), 
      names_to = "view_type", 
      values_to = "count") %>% 
    group_by(year, source, view_type) %>% 
    summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>% 
    mutate(view_type = str_replace_all(view_type, "_", " ") %>% str_to_title()) %>% 
    ggplot(aes(x = year, y = total, fill = source)) +
    geom_col(alpha = 0.8) +
    # Use scales = "free_y" if view counts differ drastically in magnitude
    facet_grid(view_type ~ source, scales = "free_y") + 
    scale_y_continuous(labels = label_comma()) +
    scale_fill_discrete_qualitative(palette = "Set2") +
    labs(title = "openRxiv Usage Breakdown",
         subtitle = "Yearly totals faceted by metric type and server source",
         x = "Year", 
         y = "Count", 
         fill = 'Server Source') +
    theme_minimal() +
    theme(legend.position = "none", # Legend is redundant because of facet labels
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold")) #+
ggsave("figs/openrxiv_usage_summary_facet.png", height = 6, width = 6)    

    
openrxiv_usage_summary %>% 
    mutate(date_obj = as.Date(month),
      year_num = year(date_obj),
      afp_views = rowSums(across(c(abstract_views, full_text_views, pdf_downloads)), na.rm = TRUE)
    ) %>% 
    group_by(year_num, source) %>% 
    summarise(yearly_total = sum(afp_views, na.rm = TRUE), .groups = "drop") %>% 
    arrange(year_num) %>% 
    group_by(source) %>% 
    mutate(cumulative_total = cumsum(yearly_total)) %>% #View()
    ungroup() %>% 
    ggplot(aes(x = year_num, y = cumulative_total, color = source)) +
    geom_line(linewidth = 1) +
    geom_point(alpha = 0.8, size = 3) + 
    scale_y_continuous(labels = label_comma()) +
    scale_x_continuous(breaks = seq(min(year(as.Date(openrxiv_usage_summary$month))), 
                                    max(year(as.Date(openrxiv_usage_summary$month))), by = 1)) +
    scale_color_discrete_qualitative(palette = "Set2") +
    labs(title = "Yearly Growth of openRxiv Preprint Usage",
         subtitle = "Total views and downloads calculated at year-end \n(openRxiv data)",
         x = "Year", 
         y = "Cumulative Views & Downloads", 
         color = 'Server Source') +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) #+   
  ggsave("figs/openrxiv_usage_summary_line.png", height = 4, width = 6)    

# Plot individual servers and combined    
  # 1. Aggregate metrics by YEAR and calculate cumulative totals
  usage_yearly <- openrxiv_usage_summary %>% 
    mutate(
      date_obj = as.Date(month),
      year_num = year(date_obj),
      # Sum the specific metric columns for the row total
      afp_views = rowSums(across(c(abstract_views, full_text_views, pdf_downloads)), na.rm = TRUE)
    ) %>% 
    # Group by Year and Source to get the annual sum
    group_by(year_num, source) %>% 
    summarise(yearly_sum = sum(afp_views, na.rm = TRUE), .groups = "drop") %>% 
    # Sort and calculate the running total (Cumulative)
    arrange(year_num) %>% 
    group_by(source) %>% 
    mutate(cumulative_total = cumsum(yearly_sum)) %>% 
    ungroup()
  
  # 2. Create a "TOTAL" data frame that sums all sources per year
  total_yearly <- usage_yearly %>%
    group_by(year_num) %>%
    summarise(cumulative_total = sum(cumulative_total)) %>%
    mutate(source = "TOTAL")
  
  # 3. Combine and Plot
bind_rows(usage_yearly, total_yearly) %>%
    # Factor source so 'TOTAL' appears first in the legend
    mutate(source = factor(source, levels = c("TOTAL", setdiff(unique(source), "TOTAL")))) %>%
    ggplot(aes(x = year_num, y = cumulative_total, color = source, linetype = (source == "TOTAL"))) +
    geom_line(aes(linewidth = (source == "TOTAL"))) +
    geom_point(size = 3) + # Points emphasize the yearly data markers
    scale_y_continuous(labels = label_comma()) +
    # Ensure every year is labeled on the x-axis
    scale_x_continuous(breaks = seq(min(usage_yearly$year_num), max(usage_yearly$year_num), by = 1)) +
    # Custom colors: Total is black, others use the Set2 palette
    scale_color_manual(values = c("TOTAL" = "#66c2a5", 
                                  "bioRxiv" = "#fc8d62", 
                                  "medRxiv" = "#8da0cb", 
                                  "other" = "#8da0cb")) + 
    scale_linewidth_manual(values = c("TRUE" = 1, "FALSE" = 0.7), guide = "none") +
    scale_linetype_manual(values = c("TRUE" = "dashed", "FALSE" = "solid"), guide = "none") +
    labs(title = "Cumulative Growth of openRxiv Preprint Usage",
         subtitle = "Points represent the total at the end of each calendar year; \nopenRxiv report data",
         x = "Year", 
         y = "Cumulative Views & Downloads", 
         color = 'Source') +
    theme_minimal() +
    theme(legend.position = "bottom",
      panel.grid.minor = element_blank()) #+
  ggsave("figs/openrxiv_usage_summary_line_total.png", height = 4, width = 6)    
  
# openRxiv summary ----
# 1. Process the monthly data into annual totals per server
summary_plot_data <- openrxiv_summary %>%
    mutate(date_obj = my(Month), 
      Year = year(date_obj)) %>%
    group_by(Year, source) %>%
    summarise(Annual_New_Papers = sum(`New Papers`, na.rm = TRUE), 
      .groups = "drop")
  
# 2. Get the latest year for labeling
summary_label_data <- summary_plot_data %>%
    group_by(source) %>%
    filter(Year == max(Year)) %>%
    ungroup()
  
# 3. Plot
ggplot(summary_plot_data, aes(x = Year, y = Annual_New_Papers, color = source, group = source)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_x_continuous(
      breaks = seq(min(summary_plot_data$Year), max(summary_plot_data$Year)),
      limits = c(min(summary_plot_data$Year), max(summary_plot_data$Year) + 1.5)) +
    scale_color_manual(values = qualitative_hcl(length(unique(summary_plot_data$source)), palette = "Set2")) +
    labs(
      title = "Annual New Preprints by Server",
      subtitle = "openRxiv report data",
      x = "Year",
      y = "Total New Papers per Year",
      color = "Server") +
    theme_minimal() +
    theme(legend.position = "none") +
    # Add direct labels to the end of the lines
    geom_text_repel(
      data = summary_label_data,
      aes(label = source),
      size = 4,
      fontface = "bold",
      hjust = 0,
      nudge_x = 0.5,
      segment.color = "grey50") #+
  ggsave("figs/openrxiv_new_papers_summary_line_total.png", height = 4, width = 6)    


 

    
## OpenAlex data ----- 

### OpenAlex Citations ----
biorxiv_openalex_20131101_20251231 %>% 
    group_by(publication_year) %>% 
    summarise(total = sum(cited_by_count, na.rm = TRUE)) %>% 
    ggplot(aes(x = publication_year, y = total, fill = factor(publication_year))) +
    geom_col() +
    theme_minimal() +
    labs(title = "Citations to preprints posted to bioRxiv since 2013",
         x = "Year of preprint posting", 
         y = "Total number of citations received",
         subtitle = "OpenAlex data") +
    scale_x_continuous(breaks = 2013:2025) +
    scale_y_continuous(labels = label_comma()) +
    scale_fill_discrete_qualitative(palette = "Set2") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
    ggsave("figs/openalex_biorxiv_citations_yearly.png", height = 6, width = 8)

biorxiv_openalex_20131101_20251231 %>% 
      group_by(publication_year) %>% 
      summarise(total = sum(cited_by_count, na.rm = TRUE), .groups = "drop") %>% 
      arrange(publication_year) %>% 
      mutate(cumulative_total = cumsum(total)) %>% 
      ggplot(aes(x = publication_year, y = cumulative_total)) +
      # Adding an area fill makes cumulative growth easier to see
      geom_area(fill = "#fc8d62", alpha = 0.3) + 
      geom_line(color = "#fc8d62", linewidth = 1.2) +
      geom_point(color = "#fc8d62", size = 2) +
      theme_minimal() +
      labs(title = "Cumulative Citations to bioRxiv Preprints",
           subtitle = "Running total of citations by year of preprint posting (OpenAlex data)",
           x = "Year of preprint posting", 
           y = "Cumulative Citations Received") +
      scale_x_continuous(breaks = 2013:2025) +
      scale_y_continuous(labels = label_comma()) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.grid.minor = element_blank()) #+
  ggsave("figs/openalex_biorxiv_citations_yearly_cumulative.png", height = 4, width = 6)

biorxiv_preprints_enhanced_openalex_20131101_20251231 %>% 
  mutate(category = stringr::str_to_sentence(category)) %>% 
  group_by(category) %>% 
  summarise(total = sum(cited_by_count, na.rm = TRUE), .groups = "drop") %>% 
  na.omit() %>% #View()
  ggplot(aes(x = category, y = total, fill = category)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Citations to preprints across bioRxiv categories",
       x = "Category", 
       y = "Preprint citations",
       subtitle = "bioRxiv data") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
ggsave("figs/biorxiv_citations_categories.png", height = 4, width = 6)  
    
## OpenAlex licences
biorxiv_openalex_20131101_20251231 %>% 
    count(best_oa_location.license)  %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% #View()
    na.omit() %>% 
    ggplot(aes(x = best_oa_location.license, y = proportion, fill = best_oa_location.license)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    #  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
    labs(title = "Licenses used for bioRxiv preprints",
         x = "License", 
         y = "Percentage",
         subtitle = "OpenAlex data") +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #+
  ggsave("figs/openalex_biorxiv_licence.png", height = 6, width = 8)

biorxiv_openalex_20131101_20251231 %>% 
  group_by(publication_year, best_oa_location.license) %>% 
    count() %>% 
    group_by(publication_year) %>% 
    mutate(proportion = (n / sum(n)) * 100) %>% 
    ungroup() %>%  
    ggplot(aes(x = factor(publication_year), y = proportion, fill = best_oa_location.license)) +
    # Using position = "stack" to show the composition of 100% each year
    geom_col(alpha = 0.9) + 
    theme_minimal() +
    scale_fill_discrete_qualitative(
      palette = "Set 2", 
      labels = function(x) str_wrap(x, width = 15)) +
    labs(title = "Evolution of bioRxiv License Choices",
         subtitle = "Percentage of preprints by license type per year (OpenAlex data)",
         x = "Publication Year", 
         y = "Percentage (%)",
         fill = "License Type") +
    # Formatting for readability
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()) #+
  ggsave("figs/openalex_biorxiv_licence_year.png", height = 6, width = 8)

biorxiv_openalex_20131101_20251231 %>% 
    mutate(license_type = case_when(
      str_detect(best_oa_location.license, "(?i)cc-by$|cc-by-4.0|public-domain|cc0") ~ "Open",
      TRUE ~ "Closed"
    )) %>% 
    group_by(publication_year, license_type) %>% 
    count() %>% 
    group_by(publication_year) %>% 
    mutate(proportion = (n / sum(n)) * 100) %>% 
    ungroup() %>% 
    filter(!is.na(publication_year)) %>% 
    ggplot(aes(x = publication_year, y = proportion, color = license_type, group = license_type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Open" = "#2ECC71", "Closed" = "#E74C3C")) + # Green for Open, Red for Closed
  scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
      labs(
      title = "Shift Toward Open Licensing on bioRxiv",
      subtitle = "Open: CC-BY and Public Domain | Closed: All other license types; OpenAlex data",
      x = "Publication Year",
      y = "Percentage of Preprints (%)",
      color = "License Category") +
    theme_minimal() +
    theme(legend.position = "bottom") #+
ggsave("figs/openalex_biorxiv_licence_Open_Closed_year.png", height = 4, width = 6)

   
### OpenAlex references within preprints ----
# biorxiv_openalex_20131101_20251231 %>% 
#     mutate(referenced_works_count = case_when(
#       referenced_works_count <= 10 ~ "<10",
#       referenced_works_count >10 & referenced_works_count <=30 ~ "11-30", 
#       referenced_works_count >30 & referenced_works_count <=50 ~ "31-50",
#       referenced_works_count >50 & referenced_works_count <=70~ "51-70",
#       referenced_works_count >70 & referenced_works_count <=90~ "71-90",
#       referenced_works_count >90 ~ "90+")) %>% 
#     group_by(publication_year, referenced_works_count) %>% 
#     count(referenced_works_count)  %>% #View()
#     #na.omit() %>% 
#  #   mutate(proportion = (n/sum(n)) * 100) %>% 
#     ggplot(aes(x = referenced_works_count, y = n, fill = referenced_works_count)) +
#     geom_col() +
#     theme_minimal() +
#     labs(title = "Number of references with preprints posted to bioRxiv",
#          x = "Number of references within preprints", 
#          y = "Count (preprints)",
#          subtitle = "openAlex data") +
#     theme(legend.position = "none") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
#     ggsave("figs/openalex_biorxiv_references.png", height = 6, width = 8)    
    

## Published ------  
preprint_data_pub <- read_csv("./data/preprint_data_pub.csv") 
  
p6 <- preprint_data_pub %>% 
 # filter(version == "1") %>% # To prevent double-counting of multiple versions
  filter(category == "Molecular Biology" | category =="Biochemistry") %>% 
  mutate(Published = case_when(published == "Not Published" ~ "No",
                               TRUE ~ "Yes"))  %>% 
    group_by(category) %>% 
    count(Published)  %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% #View()
    ggplot(aes(x = Published, y = proportion, fill = category)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Biochemistry & Molecular Biology \n preprints published",
         x = "Published", 
         y = "Percentage") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/published.png", height = 4, width = 4)   
  
  
preprint_data_pub %>% 
    group_by(category) %>%  
    mutate(Published = case_when(published == "Not Published" ~ "No",
                                 TRUE ~ "Yes"))  %>% 
    count(category, Published)  %>% 
    mutate(proportion = (n/sum(n)) * 100) %>%
    ggplot(aes(x = category, y = proportion, fill = Published)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Physiology preprints published",
         x = "Published", 
         y = "Percentage") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/published.png", height = 4, width = 4)   
  
preprint_data_pub %>% 
    mutate(Published = case_when(published == "Not Published" ~ "No",
                                 TRUE ~ "Yes"))  %>% 
    count(Published)  %>% 
    mutate(proportion = (n/sum(n)) * 100) %>% 
    ggplot(aes(x = Published, y = proportion, fill = Published)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Physiology preprints published",
         x = "Published", 
         y = "Percentage") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/published.png", height = 4, width = 4)   
  
  preprint_data %>% 
#    filter(category == "Physiology") %>% 
    count(category)  %>% 
    mutate(proportion = (n/sum(n)) * 100) %>%  View()
  ggplot(aes(x = version, y = proportion, fill = version)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Physiology preprint versions",
         x = "Versions", 
         y = "Percentage") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) #+
  ggsave("figs/version.png", height = 6, width = 8)     
  
  
## Reviews ----  
# Data provided by sciety
preprints_evaluated %>% 
    filter(date != "NA") %>%  
    group_by(category) %>% 
    count(group)  %>% 
    ggplot(aes(x = category, y = n, fill = category)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    #  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
    labs(title = "Reviews written for bioRxiv preprints",
         subtitle = "Data provided by Sciety",
         x = "Category", 
         y = "count") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
  ggsave("figs/Sciety_reviews.png", height = 6, width = 8)   
 
preprints_evaluated %>% 
    filter(date != "NA") %>%  
    group_by(category) %>% 
    count(group)  %>% 
    ggplot(aes(x = group, y = n, fill = group)) +
    geom_col(position = position_stack()) +
  #  scale_y_continuous(limits = c(0, 45000), breaks = seq(0, 10000, by = 1000)) +
    scale_y_continuous(limits = c(0, 45000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 20000, 30000, 40000)) +
    theme_minimal() +
    #  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
    labs(title = "Preprint peer reviews",
         subtitle = "Data provided by Sciety",
         x = "Review platform", 
         y = "count") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
  ggsave("figs/Sciety_reviews_platform.png", height = 6, width = 8)   
     
  
# Reviews data provided by PREreview  
preprint_data_review %>% 
    filter(Service == "PREreview") %>% 
  #  filter(category == "Molecular Biology" | category =="Biochemistry") %>% 
    group_by(category) %>% 
    count(Service)  %>% 
  #  mutate(proportion = (n/sum(n)) * 100) %>% #View()
    na.omit() %>% 
    ggplot(aes(x = category, y = n, fill = category)) +
    geom_col(position = position_dodge()) +
    theme_minimal() +
    #  scale_fill_manual(values = qualitative_hcl(n = 2, palette = "Set2")) +
    labs(title = "Reviews written for bioRxiv preprints",
         subtitle = "PREreview data only",
         x = "Category", 
         y = "count") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
    ggsave("figs/PREreviews.png", height = 6, width = 8)    
    
  
# Field plot over time------
p5 <- summary_data  %>%
    mutate(highlight = case_when(
      Category == "Molecular Biology" ~ "Yes",
      Category =="Biochemistry" ~ "Yes1",
      TRUE ~ 'No')) %>% 
    ggplot(aes(x = Year, y = Articles, color = highlight, group = Category, alpha = highlight, label = Category)) +
    geom_line(linewidth=1, linetype=1, show.legend = TRUE) +
    labs(title = "Preprints posted to bioRxiv",
         x = "Year", 
         y = "Number of preprints posted", 
         fill = 'Category') +
    scale_color_focus(c("Yes", "Yes1"), color_focus = c("blue", "red"), color_other = "gray") +
    scale_alpha_focus(c("Yes","Yes1"), alpha_focus = 1, alpha_other = 0.4) + 
    theme_minimal() +
    guides(fill = guide_legend(nrow = 3)) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  #  annotate(geom="point", x =  2023, y = 0, size=10, shape=21, fill="transparent") +
    annotate(geom = "curve", x = 2022.75, y = 2800, xend = 2022.75, yend = 1800, 
             curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x =2022.75, y = 2900, label = "Biochemistry", hjust = "left") +
    annotate(geom = "curve", x = 2021.5, y = 2400, xend = 2022, yend = 1500, 
             curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x =2021.5, y = 2500, label = "Molecular \n Biology", hjust = "left") +
    geom_text_repel(data =. %>% filter(Year == last(Year)),
                    aes(label = Category, color = "Black"), 
                    nudge_x = 0.5, 
                    size = 4,
                    segment.linetype = "dotted",
                    xlim = c(2013, NA),
                    na.rm = TRUE) #+ 
    ggsave("figs/biochem+Mol_bio_preprints_posted.png", height = 8, width = 12)
  

    

# General preprint usage ----


# Plot the cumulative number of PDF downloads per month
# Here month dates are returned already in YYYY-MM-DD format
biorxiv_usage(interval = "m", format = "df") %>%
  mutate(month = as.Date(month)) %>%
  ggplot() +
  geom_bar(aes(x = month, y = pdf_cumulative),
           fill = "#cccccc",
           stat = "identity") +
  labs(x = "",
       y= "PDF downloads (cumulative)",
       title ="Number of bioRxiv PDF downloads over time") +
  scale_x_date(date_breaks = "3 months",
               date_minor_breaks = "3 months",
               date_labels = "%b-%y",
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.title = element_text(face = "bold")
  ) #+ 
ggsave("figs/biorxiv_cumulative_downloads.png", height = 4, width = 6)


# Plot the cumulative number of new preprints deposited per month
# Note that month dates are returned in YYYY-MM format - here we convert
# month dates to YYYY-MM-DD format to make plotting easier
biorxiv_summary(interval = "m", format = "df") %>%
  mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d")) %>%
  ggplot() +
  geom_bar(aes(x = month, y = new_papers_cumulative),
           fill = "#cccccc",
           stat = "identity") +
  labs(x = "",
       y= "Submissions",
       title ="Cumulative new bioRxiv submissions") +
  scale_x_date(date_breaks = "3 months",
               date_minor_breaks = "3 months",
               date_labels = "%b-%y",
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.title = element_text(face = "bold")
  ) #+ 
ggsave("figs/biorxiv_cumulative_posting.png", height = 4, width = 6)

# Calculate the number of days between preprint deposition and 
# journal publication. Plot results as a histogram.
biorxiv_published(from = "2013-11-01", to = "2024-12-31", 
                  limit = "*", format = "df") %>%
  mutate(days = as.Date(published_date) - as.Date(preprint_date)) %>%
  ggplot() +
  geom_histogram(aes(as.numeric(days)),
                 binwidth = 1,
                 fill = "#cccccc") +
  labs(x = "Days between preprint deposition and journal publication",
       y= "Number of articles",
       title ="Time to publication") +
  coord_cartesian(xlim = c(-100, 1000)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    plot.title = element_text(face = "bold")
  ) 


# Patchwork -----

# fig 1
layout2 <- "
AAAAAA
BBBCCC
"

p1 + p2 + p3 + plot_layout(design = layout2) +
  plot_annotation(tag_levels = "A") +
  ggsave("./figs/Figure_X.png", width = 16, height = 10)

# fig 2

layout <- "
AAAAAA
AAAAAA
BBCCDD
"

p5 + p6 + p7 + p8 + plot_layout(design = layout) +
#  plot_layout(guides = 'collect') & theme(legend.position = 'bottom') +
  plot_annotation(tag_levels = "A") +
  ggsave("./figs/Figure_2_FEBS.png", width = 14, height = 14)

