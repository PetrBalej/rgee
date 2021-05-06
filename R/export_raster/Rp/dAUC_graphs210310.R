synonyms <- list(
  "Spatula clypeata" = "Anas clypeata",
  "Phylloscopus sibilatrix" = "Phylloscopus sibillatrix",
  "Spatula querquedula" = "Anas querquedula",
  "Mareca penelope" = "Anas penelope",
  "Calidris pugnax" = "Philomachus pugnax",
  "Dryobates minor" = "Dendrocopos minor",
  # nové oproti traits
  "Acanthis cabaret" = "Acanthis flammea",
  "Mareca strepera" = "Anas strepera",
  "Clanga pomarina" = "Aquila pomarina",
  "Tetrastes bonasia" = "Bonasa bonasia",
  "Linaria cannabina" = "Carduelis cannabina",
  "Acanthis flammea" = "Carduelis flammea",
  "Dendrocoptes medius" = "Dendrocopos medius",
  "Dryobates minor" = "Dendrocopos minor",
  "Ardea alba" = "Egretta alba",
  "Ichthyaetus melanocephalus" = "Larus melanocephalus",
  "Poecile montanus" = "Parus montanus",
  "Saxicola rubicola" = "Saxicola torquata",
  "Lyrurus tetrix" = "Tetrao tetrix"
)

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

pairs <- list(
  c("SchoenerD_1", "SchoenerD_2"),
  c("auc_gbif", "auc_all"), c("auc_gbif", "auc_ndop"),
  c("TSS_gbif", "TSS_all"), c("TSS_gbif", "TSS_ndop"),
  c("Jaccard_gbif", "Jaccard_all"), c("Jaccard_gbif", "Jaccard_ndop"),
  c("Sorensen_gbif", "Sorensen_all"), c("Sorensen_gbif", "Sorensen_ndop"),
  c("WarrenI_1", "WarrenI_2"),
  c("HellingerDist_1", "HellingerDist_2")
)

for (p in pairs) {
  print(paste0(p[1], "/", p[2]))

  p_1 <- strsplit(p[1], "_")
  p_2 <- strsplit(p[2], "_")
  print(p_1[[1]][1])
  print(p_1[[1]][2])
  print(p_2[[1]][1])
  print(p_2[[1]][2])


  title <- paste0(p_1[[1]][1], " (", p_1[[1]][2], "+", p_2[[1]][2], ")")
  appendix <- paste0(p_1[[1]][1], " (", p_1[[1]][2], "+", p_2[[1]][2], ")")

  # title <- paste0(p[1], "/", p[2])
  # appendix <- paste0(p[1], "-", p[2])



  # read AUC table
  # dfAUCorig <- read.csv2("AUCall100.csv", header = TRUE)

  # dfAUCorig <- read_csv(paste0(wd, "/../export/schuzka2-total-gbif-ndop6/topX-final.csv"))
  dfAUCorig_10000 <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/gOverlap_10000.csv")
  dfAUCorig_1000 <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/gOverlap_1000.csv")

  dfAUCorig <- dfAUCorig_10000 %>% add_row(dfAUCorig_1000)


  # dfAUCorig_count <- as.integer(count(dfAUCorig) / 4)
  dfAUCorig_count <- as.integer(count(dfAUCorig) / 4) * 4


  auc_limit <- 0.7
  # select important columns, rename columns with AUC differences to Q1 and Q2 (first and second research question)
  # creates a "long" table from a "wide" one
  # dfAUC <- select(dfAUCorig, (c("species_", "species", "spec", "recnum", "grain", "Q1" = "dAUC1_2", "Q2" = "dAUC1_3"))) %>%
  dfAUCfiltr <- dfAUCorig %>%
    # dopočet pořadí START
    # arrange(recnum, pixel_size) %>%
    # mutate(recnum_n = rep(dfAUCorig_count:1, each = 4)) %>%
    # mutate(recnum_n = rep(dfAUCorig_count:1, each = 1)) %>%
    # dopočet pořadí END
    # filter(Training.samples > 50) %>%
    # filter(Test.samples > 30) %>%
    # select(species1_, species1, species1_short, recnum, recnum_n, pixel_size, Q1, Q2, species2, Training.samples, Training.samples.all, Training.AUC, Training.AUC.all) ### %>%
    ### gather("Q1", "Q2", key = "Question", value = "dAUC")
    filter(auc_gbif >= auc_limit) %>%
    filter(auc_all >= auc_limit) %>%
    filter(auc_ndop >= auc_limit)


  c_orig <- as_tibble(dfAUCorig %>% group_by(pixel_size) %>% count(pixel_size))
  c_filter <- as_tibble(dfAUCfiltr %>% group_by(pixel_size) %>% count(pixel_size))
  reduction <- as_tibble(c_orig %>% mutate(n_filter = c_filter$n) %>% mutate(perc = (n_filter * 100 / n)))

  dfAUC <- dfAUCfiltr %>% gather(p[1], p[2], key = "Question", value = "dAUC")

  cnt.all <- count(dfAUCorig)
  cnt.filtr <- count(dfAUCfiltr)
  cnt.perc <- (cnt.filtr * 100) / cnt.all
  caption <- paste0("AUC > ", auc_limit)


  # dfAUC <- dfAUCorig %>%
  #   filter(Training.samples > 30) %>%
  #   filter(Test.samples > 20) %>%
  #   select(species1_, species1, species1_short, recnum, pixel_size, Q1, Q2, species2, Training.samples, Training.samples.all) %>%
  #   gather("Training.samples", "Training.samples.all", key = "Question", value = "dAUC")



  glimpse(dfAUC) # check data

  # read bird traits table (traits according to Kolecek et al 2010)
  dftraits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";") %>% select("species", "Habitat", "Migration", "Distribution", "Protection")
  glimpse(dftraits) # check data

  # join bird traits to dfAUC
  df <- dfAUC %>% left_join(dftraits, by = c("species1" = "species"))
  glimpse(df) # check data

  # anti1 <- dfAUC %>% anti_join(dftraits, by = c("species1" = "species")) %>% distinct(species1)
  # anti <- dftraits %>% anti_join(dfAUC, by = c("species" = "species1")) %>% distinct(species)
  # glimpse(anti1) # check data
  # glimpse(anti) # check data



  # print(anti1, n = 100)
  # print(anti, n = 300)

  dfAUC1 <- dfAUC %>%
    inner_join(dftraits, by = c("species1" = "species")) %>%
    drop_na(Habitat)
  anti <- dfAUC %>% anti_join(dftraits, by = c("species1" = "species"))

  dfAUC2 <- anti %>%
    inner_join(dftraits, by = c("species2" = "species")) %>%
    drop_na(Habitat)

  dfAUC <- bind_rows(dfAUC1, dfAUC2)

  df <- dfAUC
  # dfAUC %>% distinct(species1)


  # head(df, 100)
  # head(df[90:120, ], 100) # check data



  # labels
  titfreqpixel_size <- paste0("Dependence of ", title, " on species frequency level (according to grain)")
  titQ1 <- "Q1: difference between global GIF and GIF model with local (NDOP) validation"
  titQ2 <- "Q2: difference between global GIF and adjusted GBIF+NDOP model"
  titQ1Q2 <- "Q1: global (GIF) model with local (NDOP) validation; Q2: GBIF + NDOP model"
  labelfreq <- "species frequency level"
  labely <- title # "AUC difference"

  tithab <- paste0("Dependence of ", title, " on habitat and grain")
  labelhab <- "Habitat type: A - farmland, open sites, F - forests, trees, U - urban, W - wetland"

  titprot <- paste0("Dependence of ", title, " on protection statut and grain")
  labelprot <- "Protection statut: N—non-protected, E—endangered, H — highly endangered, C — critically endangered"

  titdist <- paste0("Dependence of ", title, " on European distribution and grain")
  labeldist <- "European distribution: C — central, N — northern, S — southern, W — widespread"

  titmigr <- paste0("Dependence of ", title, " on migration type and grain")
  labelmigr <- "Migration type: L - ?, P - , R - , S - , NA - nedostupné"


  pdf(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/pdf_outputs/", appendix, ".pdf"))

  grid.arrange(top = caption, tableGrob(reduction))
  # grid.table(reduction)


  frequency <- c("recnum", "occ_count_ndop", "presence_ndop")
  for (f in frequency) {
    #----------------------------------------------------------------------------------------------------------------
    # check if there is a dependence of AUC diff on species frequency level
    pic_lm <- ggplot(df, aes(x = eval(parse(text = f)), y = as.numeric(dAUC))) +
      geom_point() +
      geom_smooth(method = loess) +
      facet_grid(Question ~ pixel_size) +
      labs(title = titfreqpixel_size, subtitle = titQ1Q2, x = paste0(labelfreq, " (normalized by \'", f, "\')"), y = labely, caption = caption)
    print(pic_lm)
    # .... spíš žádná závislost není, což je fajn; u Q2 možná rozdíl v AUC trochu klesá s vyšším počtem záznamů o druhu,
    # .... ale asi spíš neprůkazně (někdy otestovat, ale teď není čas)
    # .... zároveň je na grafech vidět, že
    # .... u Q1 jsou dost velké rozdíly v dosažených AUC - model validovaný na NDOP výrazně horší (nižší AUC)
    # .... tj. kvalita dlobálních modelů je na national level špatná
    # .... tj. global data nepostihují dostatečně national niche ?
    # .... u Q2 jsou rozdíly mezi globálním modelem z GBIF a modelem z GBIF+NDOP menší,
    # .... přičemž u většiny druhů se přidáním NDOP AUC zmenšilo (což nevím, jak interpretovat, nevím, jak jsi to validoval),
    # .... asi zatím tak, že přidání national dat nic modely nezlepší, spíš naopak? Má to vysvětlení?
  }

  #----------------------------------------------------------------------------------------------------------------
  # "Dependence of dAUC on habitat and grain"
  pic_box <- ggplot(df, aes(x = Habitat, y = as.numeric(dAUC), fill = Habitat)) +
    geom_boxplot() +
    facet_grid(Question ~ pixel_size) +
    labs(title = tithab, subtitle = titQ1Q2, x = labelhab, y = labely)
  print(pic_box)
  # .... rozdíly v habitatech u Q1 budou: nejhorší jsou globální modely oproti national validaci pro farmland a lesní ptáky
  # .... o něco lepší pro birds of urban environment, nejmenší pro wetland birds
  # .... zdá se, že s vyšším grain:
  # .... se national modely jeví spíš horší
  # .... a zároveň se rozdíly mezi ptáky různých habitatů spíš stírají (100 a 200 vypadá podobně, u 1000 změna - všechny habitaty podobně blbé)
  # .... ověřit trend ještě na 2000 a/nebo 5000?

  #----------------------------------------------------------------------------------------------------------------
  # "Dependence of dAUC on protection level and grain"
  pic_box <- ggplot(df, aes(x = Protection, y = as.numeric(dAUC), fill = Protection)) +
    geom_boxplot() +
    facet_grid(Question ~ pixel_size) +
    labs(title = titprot, subtitle = titQ1Q2, x = labelprot, y = labely)
  print(pic_box)
  # .... žádný jasný trend, že modely pro chráněné a nechráněné byly lepší či horší?
  # .... obdobně jako u Habitat se zdá, že s vyšším grain spíš horší national a stírají se rozdíly mezi skupinami?

  #----------------------------------------------------------------------------------------------------------------
  # "Dependence of dAUC on European distribution and grain"
  pic_box <- ggplot(df, aes(x = Distribution, y = as.numeric(dAUC), fill = Distribution)) +
    geom_boxplot() +
    facet_grid(Question ~ pixel_size) +
    labs(title = titdist, subtitle = titQ1Q2, x = labeldist, y = labely)
  print(pic_box)
  # ... tady pokud nějaké rozdíly, tak spíš ve větším grain?



  #----------------------------------------------------------------------------------------------------------------
  # to samé se dá udělat pro typ migrace, ale to asi nemá smysl ukazovat, protože není důvod, proč by na tom mělo něco záležet
  pic_box <- ggplot(df, aes(x = Migration, y = as.numeric(dAUC), fill = Migration)) +
    geom_boxplot() +
    facet_grid(Question ~ pixel_size) +
    labs(title = titmigr, subtitle = titQ1Q2, x = labelmigr, y = labely)
  print(pic_box)

  dev.off()
}




###
# Test významnosti rozdílů mezi daty
###

# # https://www.datanovia.com/en/lessons/anova-in-r/#one-way-independent-measures

# adata <- df %>%
#   filter(pixel_size == 100) %>%
#   select(species1_, Habitat, Q1) %>%
#   mutate_at(vars(Habitat), factor)

# print(summary(adata))

# print(adata %>%
#   group_by(Habitat) %>%
#   get_summary_stats(Q1, type = "mean_sd"))

# print(ggboxplot(adata, x = "Habitat", y = "Q1"))



# # print(adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1))

# # # outliers - odebrání1
# # outliers <- adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1) %>%
# #   select(species1_) %>%
# #   pull(species1_)

# # adata %<>% filter(!species1_ %in% as.vector(outliers))
# # # outliers - odebrání1



# # print(adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1))

# # # outliers - odebrání2
# # outliers <- adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1) %>%
# #   select(species1_) %>%
# #   pull(species1_)

# # adata %<>% filter(!species1_ %in% as.vector(outliers))
# # # outliers - odebrání2




# print(adata %>%
#   group_by(Habitat) %>%
#   identify_outliers(Q1))



# print(ggboxplot(adata, x = "Habitat", y = "Q1"))


# # Build the linear model
# model <- lm(Q1 ~ Habitat, data = adata)
# # Create a QQ plot of residuals
# print(ggqqplot(residuals(model)))

# # Compute Shapiro-Wilk test of normality
# print("Compute Shapiro-Wilk test of normality")
# print(shapiro_test(residuals(model)))

# print("Check normality assumption by groups. Computing Shapiro-Wilk test for each group level. If the data is normally distributed, the p-value should be greater than 0.05.")
# print(adata %>%
#   group_by(Habitat) %>%
#   shapiro_test(Q1))


# print(ggqqplot(adata, "Q1", facet.by = "Habitat"))

# print(plot(model, 1))

# print(adata %>% levene_test(Q1 ~ Habitat))

# # nemůžu klasickou ANOVA
# welch_anova <- adata %>% welch_anova_test(Q1 ~ Habitat)
# print(welch_anova)

# # můžu Tukey? - ne (jen pro normální data), jinak musím použít Games-Howell
# pwc <- adata %>% tukey_hsd(Q1 ~ Habitat)
# print(pwc)



# # Pairwise comparisons (Games-Howell)
# pwc2 <- adata %>% games_howell_test(Q1 ~ Habitat)
# # Visualization: box plots with p-values
# pwc2 <- pwc2 %>% add_xy_position(x = "Habitat", step.increase = 1)
# print(ggboxplot(adata, x = "Habitat", y = "Q1") +
#   stat_pvalue_manual(pwc2, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(welch_anova, detailed = TRUE),
#     caption = get_pwc_label(pwc2)
#   ))

# print(pwc2)


# ###
# # If you have doubt about the normality of the data, you can use the Kruskal-Wallis test, which is the non-parametric alternative to one-way ANOVA test.
# # https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# # print(adata %>% kruskal_test(Q1 ~ Habitat))
# # print(adata %>% kruskal_effsize(Q1 ~ Habitat))