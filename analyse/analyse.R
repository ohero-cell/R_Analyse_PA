## UGC-Umfrageanalyse
## Liest Excel-Daten, berechnet Skalenwerte, Reliabilitaet, Korrelationen und Regressionen
## Schreibt Tabellen und Grafiken in den Output-Ordner

required_packages <- c(
	"readxl",
	"dplyr",
	"tidyr",
	"ggplot2",
	"stringr",
	"psych",
	"readr",
	"tibble"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
	install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
root_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
input_path <- file.path(root_dir, "data", "12.05.2026.xlsx")
output_dir <- file.path(root_dir, "Output")

if (!file.exists(input_path)) {
	stop("Eingabedatei nicht gefunden: ", input_path)
}

if (!dir.exists(output_dir)) {
	dir.create(output_dir, recursive = TRUE)
}

raw <- readxl::read_excel(input_path)

data <- raw %>%
	rename_with(~ stringr::str_replace(.x, "^[A-Z]\\[([A-Z]\\d+)\\]$", "\\1"))

v_cols <- intersect(paste0("V", 1:5), names(data))
i_cols <- intersect(paste0("I", 1:3), names(data))
e_cols <- intersect(paste0("E", 1:3), names(data))
k_cols <- intersect(paste0("K", 1:3), names(data))

if (length(v_cols) == 0 || length(i_cols) == 0 || length(e_cols) == 0 || length(k_cols) == 0) {
	stop("Ein oder mehrere Item-Bloecke fehlen. Bitte die Spaltennamen in der Excel-Datei pruefen.")
}

to_numeric <- function(x) {
	readr::parse_number(as.character(x))
}

data <- data %>%
	mutate(across(all_of(c(v_cols, i_cols, e_cols, k_cols)), to_numeric)) %>%
	mutate(
		D2 = if ("D2" %in% names(data)) readr::parse_number(as.character(D2)) else D2
	)

item_cols <- c(v_cols, i_cols, e_cols, k_cols)
valid_min <- 1
valid_max <- 5

out_of_range_counts <- tibble::tibble(
	item = item_cols,
	n_out_of_range = vapply(
		item_cols,
		function(col) {
			sum(!is.na(data[[col]]) & (data[[col]] < valid_min | data[[col]] > valid_max))
		},
		numeric(1)
	)
)

if (length(item_cols) > 0) {
	for (col in item_cols) {
		data[[col]][!is.na(data[[col]]) & (data[[col]] < valid_min | data[[col]] > valid_max)] <- NA_real_
	}
}

row_mean_with_min <- function(df, cols, min_non_missing) {
	values <- as.data.frame(df[cols])
	non_missing <- rowSums(!is.na(values))
	means <- rowMeans(values, na.rm = TRUE)
	means[non_missing < min_non_missing] <- NA_real_
	means
}

data <- data %>%
	mutate(
		trustworthiness = row_mean_with_min(., v_cols, min_non_missing = 3),
		info_quality = row_mean_with_min(., i_cols, min_non_missing = 2),
		attitude_ugc = row_mean_with_min(., e_cols, min_non_missing = 2),
		purchase_intent = row_mean_with_min(., k_cols, min_non_missing = 2)
	)

analysis_df <- data %>%
	drop_na(trustworthiness, info_quality, attitude_ugc, purchase_intent)

scale_labels <- c(
	trustworthiness = "Vertrauenswuerdigkeit",
	info_quality = "Informationsqualitaet",
	attitude_ugc = "Einstellung_UGC",
	purchase_intent = "Kaufabsicht"
)

scale_df <- data %>%
	select(trustworthiness, info_quality, attitude_ugc, purchase_intent)

scale_df_de <- scale_df %>%
	rename(
		Vertrauenswuerdigkeit = trustworthiness,
		Informationsqualitaet = info_quality,
		Einstellung_UGC = attitude_ugc,
		Kaufabsicht = purchase_intent
	)

analysis_df_de <- analysis_df %>%
	rename(
		Vertrauenswuerdigkeit = trustworthiness,
		Informationsqualitaet = info_quality,
		Einstellung_UGC = attitude_ugc,
		Kaufabsicht = purchase_intent
	)

data_export <- data
if ("D1" %in% names(data_export)) {
	data_export <- data_export %>% rename(Geschlecht = D1)
}
if ("D2" %in% names(data_export)) {
	data_export <- data_export %>% rename(Alter = D2)
}

data_export <- data_export %>%
	rename(
		Vertrauenswuerdigkeit = trustworthiness,
		Informationsqualitaet = info_quality,
		Einstellung_UGC = attitude_ugc,
		Kaufabsicht = purchase_intent
	)

write.csv(data_export, file.path(output_dir, "clean_data.csv"), row.names = FALSE)

out_of_range_counts_de <- out_of_range_counts %>%
	rename(Item = item, Anzahl_ausserhalb_1_5 = n_out_of_range)

write.csv(out_of_range_counts_de, file.path(output_dir, "out_of_range_filtered.csv"), row.names = FALSE)

desc <- psych::describe(scale_df_de)
write.csv(desc, file.path(output_dir, "descriptives.csv"), row.names = TRUE)

calc_alpha <- function(df, cols, label) {
	if (length(cols) < 2) {
		return(tibble::tibble(scale = label, n_items = length(cols), alpha = NA_real_))
	}
	a <- psych::alpha(df[cols], warnings = FALSE, check.keys = TRUE)
	tibble::tibble(scale = label, n_items = length(cols), alpha = a$total$raw_alpha)
}

alpha_table <- dplyr::bind_rows(
	calc_alpha(data, v_cols, scale_labels[["trustworthiness"]]),
	calc_alpha(data, i_cols, scale_labels[["info_quality"]]),
	calc_alpha(data, e_cols, scale_labels[["attitude_ugc"]]),
	calc_alpha(data, k_cols, scale_labels[["purchase_intent"]])
)

alpha_table_de <- alpha_table %>%
	rename(Skala = scale, Anzahl_Items = n_items, Cronbach_Alpha = alpha)

write.csv(alpha_table_de, file.path(output_dir, "reliability.csv"), row.names = FALSE)

corr <- psych::corr.test(scale_df_de, use = "pairwise")
corr_mat <- corr$r
p_mat <- corr$p

write.csv(corr_mat, file.path(output_dir, "correlations.csv"))
write.csv(p_mat, file.path(output_dir, "correlations_p_values.csv"))

corr_df <- as.data.frame(as.table(corr_mat)) %>%
	rename(var1 = Var1, var2 = Var2, r = Freq)

corr_plot <- ggplot(corr_df, aes(x = var1, y = var2, fill = r)) +
	geom_tile(color = "white") +
	geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
	scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0) +
	labs(title = "Korrelationen der Skalen", fill = "r") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank())

ggsave(file.path(output_dir, "correlation_heatmap.png"), corr_plot, width = 7, height = 6, dpi = 300)

scale_long <- scale_df_de %>%
	pivot_longer(cols = everything(), names_to = "scale", values_to = "value")

hist_plot <- ggplot(scale_long, aes(x = value)) +
	geom_histogram(binwidth = 0.5, fill = "#2c7fb8", color = "white") +
	facet_wrap(~ scale, ncol = 2) +
	coord_cartesian(xlim = c(valid_min, valid_max)) +
	scale_x_continuous(breaks = valid_min:valid_max) +
	theme_minimal() +
	labs(x = "Skalenwert", y = "Haeufigkeit")

ggsave(file.path(output_dir, "scale_histograms.png"), hist_plot, width = 8, height = 6, dpi = 300)

plot_scatter <- function(df, x_var, y_var, file_name, x_label, y_label) {
	p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
		geom_point(alpha = 0.7) +
		geom_smooth(method = "lm", se = TRUE, color = "#1b7837") +
		coord_cartesian(xlim = c(valid_min, valid_max), ylim = c(valid_min, valid_max)) +
		scale_x_continuous(breaks = valid_min:valid_max) +
		scale_y_continuous(breaks = valid_min:valid_max) +
		theme_minimal() +
		labs(x = x_label, y = y_label)
	ggsave(file.path(output_dir, file_name), p, width = 6, height = 4, dpi = 300)
}

plot_scatter(
	analysis_df_de,
	"Vertrauenswuerdigkeit",
	"Kaufabsicht",
	"scatter_trustworthiness_purchase.png",
	"Vertrauenswuerdigkeit",
	"Kaufabsicht"
)
plot_scatter(
	analysis_df_de,
	"Informationsqualitaet",
	"Kaufabsicht",
	"scatter_info_quality_purchase.png",
	"Informationsqualitaet",
	"Kaufabsicht"
)
plot_scatter(
	analysis_df_de,
	"Einstellung_UGC",
	"Kaufabsicht",
	"scatter_attitude_purchase.png",
	"Einstellung_UGC",
	"Kaufabsicht"
)

model <- lm(purchase_intent ~ trustworthiness + info_quality + attitude_ugc, data = analysis_df)
model_std <- lm(scale(purchase_intent) ~ scale(trustworthiness) + scale(info_quality) + scale(attitude_ugc), data = analysis_df)

coef_table <- summary(model)$coefficients
coef_table_std <- summary(model_std)$coefficients

recode_predictor <- function(x) {
	dplyr::recode(
		x,
		"(Intercept)" = "Konstante",
		"trustworthiness" = scale_labels[["trustworthiness"]],
		"info_quality" = scale_labels[["info_quality"]],
		"attitude_ugc" = scale_labels[["attitude_ugc"]],
		"scale(trustworthiness)" = scale_labels[["trustworthiness"]],
		"scale(info_quality)" = scale_labels[["info_quality"]],
		"scale(attitude_ugc)" = scale_labels[["attitude_ugc"]],
		.default = x
	)
}

coef_df <- as.data.frame(coef_table) %>%
	tibble::rownames_to_column(var = "Praediktor") %>%
	mutate(Praediktor = recode_predictor(Praediktor)) %>%
	rename(
		Schaetzung = Estimate,
		Std_Fehler = `Std. Error`,
		t_Wert = `t value`,
		p_Wert = `Pr(>|t|)`
	)

coef_df_std <- as.data.frame(coef_table_std) %>%
	tibble::rownames_to_column(var = "Praediktor") %>%
	mutate(Praediktor = recode_predictor(Praediktor)) %>%
	rename(
		Schaetzung = Estimate,
		Std_Fehler = `Std. Error`,
		t_Wert = `t value`,
		p_Wert = `Pr(>|t|)`
	)

write.csv(coef_df, file.path(output_dir, "regression_coefficients.csv"), row.names = FALSE)
write.csv(coef_df_std, file.path(output_dir, "regression_coefficients_standardized.csv"), row.names = FALSE)

model_summary <- summary(model)
fstat <- model_summary$fstatistic
f_p_value <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

sink(file.path(output_dir, "regression_summary.txt"))
cat("Lineare Regression (abhaengige Variable: Kaufabsicht)\n")
cat("Modell: Kaufabsicht ~ Vertrauenswuerdigkeit + Informationsqualitaet + Einstellung_UGC\n\n")
cat(sprintf("R2 = %.3f, adjustiertes R2 = %.3f\n", model_summary$r.squared, model_summary$adj.r.squared))
cat(sprintf("F(%d, %d) = %.2f, p = %.4f\n\n", fstat[2], fstat[3], fstat[1], f_p_value))
cat("Koeffizienten (nicht standardisiert)\n")
print(coef_df)
cat("\nKoeffizienten (standardisiert)\n")
print(coef_df_std)
cat("\nInterpretation: Ein positiver Koeffizient bedeutet, dass hoehere Werte der jeweiligen Skala\n")
cat("mit hoeherer Kaufabsicht einhergehen. p-Werte < 0.05 gelten als statistisch signifikant.\n")
sink()

hypotheses <- tibble::tibble(
	Hypothese = c("H1", "H2", "H3"),
	Praediktor = c(scale_labels[["trustworthiness"]], scale_labels[["attitude_ugc"]], scale_labels[["info_quality"]]),
	Schaetzung = c(coef(model)["trustworthiness"], coef(model)["attitude_ugc"], coef(model)["info_quality"]),
	p_Wert = c(coef_table["trustworthiness", 4], coef_table["attitude_ugc", 4], coef_table["info_quality", 4])
)

write.csv(hypotheses, file.path(output_dir, "hypotheses_results.csv"), row.names = FALSE)

sample_sizes <- tibble::tibble(
	Gesamt_n = nrow(data),
	Analyse_n = nrow(analysis_df),
	Fehlende_Skalenwerte = sum(!complete.cases(scale_df)),
	Anzahl_ausserhalb_1_5 = sum(out_of_range_counts$n_out_of_range)
)

write.csv(sample_sizes, file.path(output_dir, "sample_sizes.csv"), row.names = FALSE)

if ("D1" %in% names(data)) {
	gender_summary <- data %>%
		group_by(D1) %>%
		summarise(
			Anzahl = n(),
			Vertrauenswuerdigkeit = mean(trustworthiness, na.rm = TRUE),
			Informationsqualitaet = mean(info_quality, na.rm = TRUE),
			Einstellung_UGC = mean(attitude_ugc, na.rm = TRUE),
			Kaufabsicht = mean(purchase_intent, na.rm = TRUE),
			.groups = "drop"
		) %>%
		rename(Geschlecht = D1)

	write.csv(gender_summary, file.path(output_dir, "scale_means_by_gender.csv"), row.names = FALSE)
}
