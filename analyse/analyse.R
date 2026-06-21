require("readxl")
require("dplyr")
require("tidyr")
require("ggplot2")
require("stringr")
require("psych")
require("lmtest")
require("sandwich")
require("readr")
require("tibble")
require("openxlsx")

options(repos = c(CRAN = "https://cran.r-project.org"))
plot_width <- 5.91
plot_height <- 2.36
plot_height_tall <- 2.36
plot_device <- grDevices::svg
plot_theme <- theme_minimal(base_size = 10, base_family = "Arial")

script_dir <- getwd()
root_dir <- if(basename(script_dir) == "analyse") dirname(script_dir) else script_dir
input_path <- file.path(root_dir, "data", "12.05.2026.xlsx")
out_dir <- file.path(root_dir, "Output")
sec_out_dir <- file.path(root_dir, "second_Output")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(sec_out_dir)) dir.create(sec_out_dir, recursive = TRUE)

raw <- readxl::read_excel(input_path)
data <- raw %>% rename_with(~ str_replace(.x, "^[A-Z]\\[([A-Z]\\d+)\\]$", "\\1"))

v_cols <- intersect(paste0("V", 1:5), names(data))
i_cols <- intersect(paste0("I", 1:3), names(data))
e_cols <- intersect(paste0("E", 1:3), names(data))
k_cols <- intersect(paste0("K", 1:3), names(data))

data <- data %>% mutate(across(all_of(c(v_cols, i_cols, e_cols, k_cols, "D2")), ~ parse_number(as.character(.x))))
insta_col <- names(data)[str_detect(tolower(names(data)), "insta")][1]
if(!is.na(insta_col)) {
  data$Instagram_Nutzung <- parse_number(as.character(data[[insta_col]]))
}

valid_min <- 1; valid_max <- 5
item_cols <- c(v_cols, i_cols, e_cols, k_cols)

out_range_counts <- tibble(
  Item = item_cols,
  Anzahl_ausserhalb_1_5 = sapply(item_cols, function(c) sum(!is.na(data[[c]]) & (data[[c]] < valid_min | data[[c]] > valid_max)))
)
write.csv(out_range_counts, file.path(out_dir, "out_of_range_filtered.csv"), row.names = FALSE)

for (col in item_cols) {
  data[[col]][!is.na(data[[col]]) & (data[[col]] < valid_min | data[[col]] > valid_max)] <- NA_real_
}

row_mean_min <- function(df, cols, min_n) {
  vals <- df[cols]; non_na <- rowSums(!is.na(vals))
  m <- rowMeans(vals, na.rm = TRUE); m[non_na < min_n] <- NA; m
}

data <- data %>% mutate(
  trustworthiness = row_mean_min(., v_cols, 3),
  info_quality = row_mean_min(., i_cols, 2),
  attitude_ugc = row_mean_min(., e_cols, 2),
  purchase_intent = row_mean_min(., k_cols, 2)
)

data_export <- data %>%
  rename(Vertrauenswuerdigkeit = trustworthiness, Informationsqualitaet = info_quality, 
         Einstellung_UGC = attitude_ugc, Kaufabsicht = purchase_intent) %>%
  rename_with(~ case_when(.x == "D1" ~ "Geschlecht", .x == "D2" ~ "Alter", TRUE ~ .x))

write.csv(data_export, file.path(out_dir, "clean_data.csv"), row.names = FALSE)
openxlsx::write.xlsx(data_export, file.path(out_dir, "clean_data.xlsx"))

analysis_df <- data %>% drop_na(trustworthiness, info_quality, attitude_ugc, purchase_intent)
analysis_df_de <- analysis_df %>% rename(Vertrauenswuerdigkeit = trustworthiness, Informationsqualitaet = info_quality, Einstellung_UGC = attitude_ugc, Kaufabsicht = purchase_intent)

scale_df_de <- analysis_df_de %>% select(Vertrauenswuerdigkeit, Informationsqualitaet, Einstellung_UGC, Kaufabsicht)
desc <- psych::describe(scale_df_de)
write.csv(desc, file.path(out_dir, "descriptives.csv"), row.names = TRUE)

desc_smry <- tibble(Skala = names(scale_df_de), Mittelwert = sapply(scale_df_de, function(x) mean(x, na.rm=T)), SD = sapply(scale_df_de, function(x) sd(x, na.rm=T)))
p_desc <- ggplot(desc_smry, aes(x=Skala, y=Mittelwert)) + geom_col(fill="#2c7fb8") + geom_errorbar(aes(ymin=Mittelwert-SD, ymax=Mittelwert+SD), width=0.2) + plot_theme + labs(y="Mittelwert (SD)", x=NULL) + theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(file.path(out_dir, "descriptives_means_sd.svg"), p_desc, width=plot_width, height=plot_height, device=plot_device)

calc_alpha <- function(df, cols, lbl) {
  if(length(cols)<2) return(tibble(Skala=lbl, Anzahl_Items=length(cols), Cronbach_Alpha=NA))
  tibble(Skala=lbl, Anzahl_Items=length(cols), Cronbach_Alpha=psych::alpha(df[cols], check.keys=T)$total$raw_alpha)
}
alpha_tbl <- bind_rows(
  calc_alpha(data, v_cols, "Vertrauenswuerdigkeit"), calc_alpha(data, i_cols, "Informationsqualitaet"),
  calc_alpha(data, e_cols, "Einstellung_UGC"), calc_alpha(data, k_cols, "Kaufabsicht")
)
write.csv(alpha_tbl, file.path(out_dir, "reliability.csv"), row.names = FALSE)

corr <- psych::corr.test(scale_df_de, use="pairwise")
write.csv(corr$r, file.path(out_dir, "correlations.csv"))
write.csv(corr$p, file.path(out_dir, "correlations_p_values.csv"))

safe_cor <- function(df, x, y) {
  if (!all(c(x,y) %in% names(df))) return(list(n=0, r=NA, p=NA))
  sub <- drop_na(df[c(x,y)]); if(nrow(sub)<3) return(list(n=nrow(sub), r=NA, p=NA))
  res <- cor.test(sub[[x]], sub[[y]]); list(n=nrow(sub), r=res$estimate, p=res$p.value)
}

age_att <- safe_cor(data, "D2", "attitude_ugc")
age_pur <- safe_cor(data, "D2", "purchase_intent")
age_inst <- safe_cor(data, "D2", "Instagram_Nutzung")
age_corr_tbl <- tibble(Analyse=c("Alter x Einstellung_UGC","Alter x Kaufabsicht","Alter x Instagram_Nutzung"), n=c(age_att$n, age_pur$n, age_inst$n), r=c(age_att$r, age_pur$r, age_inst$r), p_Wert=c(age_att$p, age_pur$p, age_inst$p))
write.csv(age_corr_tbl, file.path(out_dir, "age_correlations.csv"), row.names = FALSE)

if ("D1" %in% names(data)) {
  data_g <- data %>% mutate(G_num = parse_number(as.character(D1)))
  g_pur <- safe_cor(data_g, "G_num", "purchase_intent")
  g_inst <- safe_cor(data_g, "G_num", "Instagram_Nutzung")
} else {
  g_pur <- list(n=0, r=NA, p=NA); g_inst <- list(n=0, r=NA, p=NA)
}
gender_corr_tbl <- tibble(Analyse=c("Geschlecht x Kaufabsicht", "Geschlecht x Instagram_Nutzung"), n=c(g_pur$n, g_inst$n), r=c(g_pur$r, g_inst$r), p_Wert=c(g_pur$p, g_inst$p))
write.csv(gender_corr_tbl, file.path(out_dir, "gender_correlations.csv"), row.names = FALSE)

corr_vars_ext <- c("Vertrauenswuerdigkeit", "Informationsqualitaet", "Einstellung_UGC", "Kaufabsicht", "Alter", "Instagram_Nutzung")
corr_input <- data_export %>% select(any_of(corr_vars_ext))
if(ncol(corr_input)>=2) {
  corr_ext <- psych::corr.test(corr_input, use="pairwise")
  write.csv(corr_ext$r, file.path(out_dir, "correlations_extended.csv"))
  write.csv(corr_ext$p, file.path(out_dir, "correlations_extended_p_values.csv"))
}

scale_long <- scale_df_de %>% pivot_longer(cols=everything(), names_to="scale", values_to="value") %>% drop_na()
hist_freqs <- scale_long %>% count(scale, value, name="Haeufigkeit")
write.csv(hist_freqs, file.path(out_dir, "scale_histograms_frequencies.csv"), row.names = FALSE)

hist_plot <- ggplot(scale_long, aes(x=value)) + geom_histogram(binwidth=0.5, fill="#2c7fb8", color="white") + facet_wrap(~scale) + plot_theme + labs(x="Skalenwert", y="Haeufigkeit")
ggsave(file.path(out_dir, "scale_histograms.svg"), hist_plot, width=plot_width, height=plot_height_tall, device=plot_device)

plot_scat <- function(df, x, y, f) {
  p <- ggplot(df, aes_string(x=x, y=y)) + geom_point(alpha=0.7) + geom_smooth(method="lm", color="#1b7837") + plot_theme + labs(x=x, y=y)
  ggsave(file.path(out_dir, f), p, width=plot_width, height=plot_height, device=plot_device)
}
plot_scat(analysis_df_de, "Vertrauenswuerdigkeit", "Kaufabsicht", "scatter_trustworthiness_purchase.svg")
plot_scat(analysis_df_de, "Informationsqualitaet", "Kaufabsicht", "scatter_info_quality_purchase.svg")
plot_scat(analysis_df_de, "Einstellung_UGC", "Kaufabsicht", "scatter_attitude_purchase.svg")

model <- lm(purchase_intent ~ trustworthiness + info_quality + attitude_ugc, data=analysis_df)
model_std <- lm(scale(purchase_intent) ~ scale(trustworthiness) + scale(info_quality) + scale(attitude_ugc), data=analysis_df)

calc_tol <- function(df, tgt, preds) {
  fml <- as.formula(paste(tgt, "~", paste(setdiff(preds, tgt), collapse="+")))
  r2 <- summary(lm(fml, data=df))$r.squared
  tibble(Praediktor=tgt, R2=r2, Toleranz=1-r2)
}
preds <- c("trustworthiness", "info_quality", "attitude_ugc")
tol_tbl <- bind_rows(lapply(preds, calc_tol, df=analysis_df, preds=preds)) %>%
  mutate(Praediktor = recode(Praediktor, trustworthiness="Vertrauenswuerdigkeit", info_quality="Informationsqualitaet", attitude_ugc="Einstellung_UGC"))
write.csv(tol_tbl, file.path(out_dir, "multicollinearity_tolerance.csv"), row.names=FALSE)

resid_plot <- ggplot(tibble(F=fitted(model), R=residuals(model)), aes(x=F, y=R)) + geom_point() + geom_hline(yintercept=0, linetype="dashed") + plot_theme + labs(x="Fitted",y="Residuen")
ggsave(file.path(out_dir, "residuals_vs_fitted.svg"), resid_plot, width=plot_width, height=plot_height, device=plot_device)

mahal_df <- analysis_df %>% select(all_of(preds))
if(nrow(mahal_df) > length(preds)) {
  cm <- cov(mahal_df)
  md2 <- mahalanobis(mahal_df, colMeans(mahal_df), cm)
  cut <- qchisq(0.999, df=length(preds))
  mahal_tbl <- tibble(Row=1:nrow(mahal_df), Mahalanobis_D2=md2, Cutoff=cut, Ausreisser=md2>cut)
  write.csv(mahal_tbl, file.path(out_dir, "mahalanobis_outliers.csv"), row.names=FALSE)
  p_mahal <- ggplot(mahal_tbl, aes(x=Mahalanobis_D2)) + geom_histogram(fill="#a6cee3", bins=30) + geom_vline(xintercept=cut, linetype="dashed", color="red") + plot_theme
  ggsave(file.path(out_dir, "mahalanobis_distances.svg"), p_mahal, width=plot_width, height=plot_height, device=plot_device)
}

rec_pred <- function(x) recode(x, "(Intercept)"="Konstante", "trustworthiness"="Vertrauenswuerdigkeit", "info_quality"="Informationsqualitaet", "attitude_ugc"="Einstellung_UGC", "scale(trustworthiness)"="Vertrauenswuerdigkeit", "scale(info_quality)"="Informationsqualitaet", "scale(attitude_ugc)"="Einstellung_UGC")
coef_tbl <- as.data.frame(summary(model)$coefficients) %>% rownames_to_column("Praediktor") %>% mutate(Praediktor=rec_pred(Praediktor)) %>% rename(Schaetzung=Estimate, Std_Fehler=`Std. Error`, t_Wert=`t value`, p_Wert=`Pr(>|t|)`)
write.csv(coef_tbl, file.path(out_dir, "regression_coefficients.csv"), row.names=FALSE)
coef_std_tbl <- as.data.frame(summary(model_std)$coefficients) %>% rownames_to_column("Praediktor") %>% mutate(Praediktor=rec_pred(Praediktor)) %>% rename(Schaetzung=Estimate, Std_Fehler=`Std. Error`, t_Wert=`t value`, p_Wert=`Pr(>|t|)`)
write.csv(coef_std_tbl, file.path(out_dir, "regression_coefficients_standardized.csv"), row.names=FALSE)

coef_plot_df <- coef_std_tbl %>% filter(Praediktor!="Konstante") %>% mutate(L=Schaetzung-1.96*Std_Fehler, U=Schaetzung+1.96*Std_Fehler)
p_coef <- ggplot(coef_plot_df, aes(x=Praediktor, y=Schaetzung)) + geom_col(fill="#984ea3") + geom_errorbar(aes(ymin=L, ymax=U), width=0.2) + geom_hline(yintercept=0, linetype="dashed") + plot_theme + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(y="Standardisierte Koeffizienten", x=NULL)
ggsave(file.path(out_dir, "regression_coefficients_standardized.svg"), p_coef, width=plot_width, height=plot_height, device=plot_device)

hypo <- tibble(Hypothese=c("H1","H2","H3"), Praediktor=c("Vertrauenswuerdigkeit","Einstellung_UGC","Informationsqualitaet"), Schaetzung=coef_tbl$Schaetzung[match(c("Vertrauenswuerdigkeit","Einstellung_UGC","Informationsqualitaet"), coef_tbl$Praediktor)], p_Wert=coef_tbl$p_Wert[match(c("Vertrauenswuerdigkeit","Einstellung_UGC","Informationsqualitaet"), coef_tbl$Praediktor)])
write.csv(hypo, file.path(out_dir, "hypotheses_results.csv"), row.names=FALSE)

smry_txt <- capture.output(summary(model))
writeLines(c("Lineare Regression (abhaengige Variable: Kaufabsicht)", smry_txt), file.path(out_dir, "regression_summary.txt"))
writeLines(c("Lineare Regression (abhaengige Variable: Kaufabsicht)", smry_txt), file.path(out_dir, "multiple Regression.txt"))

smp_sz <- tibble(Gesamt_n=nrow(data), Analyse_n=nrow(analysis_df), Fehlende_Skalenwerte=sum(!complete.cases(scale_df_de)), Anzahl_ausserhalb_1_5=sum(out_range_counts$Anzahl_ausserhalb_1_5))
write.csv(smp_sz, file.path(out_dir, "sample_sizes.csv"), row.names=FALSE)

if("D1" %in% names(data)) {
  g_smry <- data %>% filter(!is.na(D1)) %>% group_by(D1) %>% summarise(Anzahl=n(), Vertrauenswuerdigkeit=mean(trustworthiness,na.rm=T), Informationsqualitaet=mean(info_quality,na.rm=T), Einstellung_UGC=mean(attitude_ugc,na.rm=T), Kaufabsicht=mean(purchase_intent,na.rm=T)) %>% rename(Geschlecht=D1)
  write.csv(g_smry, file.path(out_dir, "scale_means_by_gender.csv"), row.names=FALSE)
}

# Second output / VIF & effects
vif_eff <- tol_tbl %>% mutate(VIF=1/Toleranz, Bedenklich_ab_2=VIF>=2, Cohens_f2=NA, Cohens_f=NA)
r2_mod <- summary(model)$r.squared
mod_f2 <- r2_mod/(1-r2_mod)
mod_f <- sqrt(mod_f2)
eff <- tibble(Praediktor="Gesamtmodell", R2=r2_mod, Toleranz=NA, VIF=NA, Bedenklich_ab_2=NA, Cohens_f2=mod_f2, Cohens_f=mod_f)
write.csv(bind_rows(vif_eff, eff), file.path(sec_out_dir, "vif_und_effektstaerke.csv"), row.names=FALSE)
writeLines(sprintf("Cohen's f2 = %.3f\nCohen's f = %.3f", mod_f2, mod_f), file.path(out_dir, "effect_size_f.txt"))

g_lbl <- function(x) case_when(x==1~"Frauen", x==2~"Maenner", TRUE~NA_character_)
if("D1" %in% names(data)) {
  g_cnt <- data %>% drop_na(D1) %>% mutate(G=factor(g_lbl(D1), c("Frauen","Maenner"))) %>% count(G) %>% mutate(P=n/sum(n)*100, L=sprintf("%d\n(%.1f%%)", n, P))
  if(nrow(g_cnt)>0) {
    p_g <- ggplot(g_cnt, aes(x=G, y=n, fill=G)) + geom_col(width=0.65) + geom_text(aes(label=L), vjust=-0.5) + scale_fill_manual(values=c("Frauen"="#e41a1c", "Maenner"="#377eb8")) + plot_theme + theme(legend.position="none") + labs(title="Geschlechterverteilung", x=NULL, y="Anzahl")
    ggsave(file.path(sec_out_dir, "geschlechterverteilung.svg"), p_g, width=plot_width, height=plot_height, device=plot_device)
  }
}
if("D2" %in% names(data)) {
  a_cnt <- data %>% drop_na(D2) %>% select(Alter=D2)
  if(nrow(a_cnt)>0) {
    p_a <- ggplot(a_cnt, aes(x=Alter)) + geom_histogram(binwidth=5, fill="#80b1d3", color="white") + geom_vline(xintercept=mean(a_cnt$Alter), linetype="dashed", color="red") + plot_theme + labs(title="Altersverteilung", x="Alter", y="Anzahl")
    ggsave(file.path(sec_out_dir, "altersverteilung.svg"), p_a, width=plot_width, height=plot_height, device=plot_device)
  }
}
if("D1" %in% names(data)) {
  pg_smry <- data %>% filter(!is.na(D1), !is.na(purchase_intent)) %>% mutate(G=factor(g_lbl(D1), c("Frauen","Maenner"))) %>% group_by(G) %>% summarise(M=mean(purchase_intent), SD=sd(purchase_intent), n=n()) %>% mutate(L=sprintf("M = %.2f\nSD = %.2f", M, SD))
  if(nrow(pg_smry)>0) {
    p_kauf <- ggplot(pg_smry, aes(x=G, y=M, fill=G)) + geom_col(width=0.65) + geom_errorbar(aes(ymin=M-SD, ymax=M+SD), width=0.18) + geom_text(aes(label=L), vjust=-0.45) + scale_fill_manual(values=c("Frauen"="#fb9a99", "Maenner"="#a6cee3")) + coord_cartesian(ylim=c(0,5)) + plot_theme + theme(legend.position="none") + labs(title="Kaufabsicht nach Geschlecht", x=NULL, y="Kaufabsicht")
    ggsave(file.path(sec_out_dir, "kaufabsicht_nach_geschlecht.svg"), p_kauf, width=plot_width, height=plot_height_tall, device=plot_device)
  }
}
