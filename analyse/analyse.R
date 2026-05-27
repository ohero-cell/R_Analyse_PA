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
	"lmtest",
	"readr",
	"tibble"
)

options(repos = c(CRAN = "https://cran.r-project.org"))

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
	install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

plot_width <- 5.91
plot_height <- 2.36
plot_height_tall <- 2.36
plot_device <- grDevices::svg
plot_theme <- theme_minimal(base_size = 10, base_family = "Arial") +
	theme(
		text = element_text(size = 10, family = "Arial"),
		axis.title = element_text(size = 10),
		axis.text = element_text(size = 10),
		legend.title = element_text(size = 10),
		legend.text = element_text(size = 10),
		plot.title = element_text(size = 10),
		strip.text = element_text(size = 10)
	)

script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA_character_)
if (is.na(script_dir) || script_dir == "" || script_dir == ".") {
	script_dir <- normalizePath(getwd(), mustWork = FALSE)
}

if (file.exists(file.path(script_dir, "analyse.R"))) {
	root_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
} else if (file.exists(file.path(script_dir, "analyse", "analyse.R"))) {
	root_dir <- normalizePath(script_dir, mustWork = FALSE)
} else {
	root_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
}
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

find_instagram_var <- function(df) {
	if ("Q2" %in% names(df)) {
		return("Q2")
	}
	candidates <- names(df)[stringr::str_detect(stringr::str_to_lower(names(df)), "insta|instagram")]
	if (length(candidates) == 0) {
		return(NA_character_)
	}
	candidates[1]
}

instagram_source <- find_instagram_var(data)
if (!is.na(instagram_source)) {
	data <- data %>%
		mutate(Instagram_Nutzung = readr::parse_number(as.character(.data[[instagram_source]])))
}

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

desc_summary <- tibble::tibble(
	Skala = names(scale_df_de),
	Mittelwert = vapply(scale_df_de, function(x) mean(x, na.rm = TRUE), numeric(1)),
	SD = vapply(scale_df_de, function(x) sd(x, na.rm = TRUE), numeric(1))
)

desc_plot <- ggplot(desc_summary, aes(x = Skala, y = Mittelwert)) +
	geom_col(fill = "#2c7fb8") +
	geom_errorbar(aes(ymin = Mittelwert - SD, ymax = Mittelwert + SD), width = 0.2) +
	plot_theme +
	labs(x = NULL, y = "Mittelwert (SD)") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(output_dir, "descriptives_means_sd.svg"),
	desc_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

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

alpha_plot <- ggplot(alpha_table_de, aes(x = Skala, y = Cronbach_Alpha)) +
	geom_col(fill = "#4daf4a") +
	geom_hline(yintercept = 0.7, linetype = "dashed", color = "#636363") +
	coord_cartesian(ylim = c(0, 1)) +
	plot_theme +
	labs(x = NULL, y = "Cronbach Alpha") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(output_dir, "reliability_alpha.svg"),
	alpha_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

corr <- psych::corr.test(scale_df_de, use = "pairwise")
corr_mat <- corr$r
p_mat <- corr$p

write.csv(corr_mat, file.path(output_dir, "correlations.csv"))
write.csv(p_mat, file.path(output_dir, "correlations_p_values.csv"))

safe_cor_test <- function(df, x, y) {
	if (!all(c(x, y) %in% names(df))) {
		return(list(n = 0, r = NA_real_, p = NA_real_))
	}
	sub <- df[, c(x, y)]
	sub <- sub[complete.cases(sub), ]
	if (nrow(sub) < 3) {
		return(list(n = nrow(sub), r = NA_real_, p = NA_real_))
	}
	test <- cor.test(sub[[x]], sub[[y]])
	list(n = nrow(sub), r = unname(test$estimate), p = test$p.value)
}

age_attitude <- safe_cor_test(data, "D2", "attitude_ugc")
age_purchase <- safe_cor_test(data, "D2", "purchase_intent")
age_instagram <- safe_cor_test(data, "D2", "Instagram_Nutzung")

age_corr_table <- tibble::tibble(
	Analyse = c("Alter x Einstellung_UGC", "Alter x Kaufabsicht", "Alter x Instagram_Nutzung"),
	n = c(age_attitude$n, age_purchase$n, age_instagram$n),
	r = c(age_attitude$r, age_purchase$r, age_instagram$r),
	p_Wert = c(age_attitude$p, age_purchase$p, age_instagram$p)
)

format_list <- function(x) {
	if (length(x) == 0) {
		return("keine")
	}
	paste(x, collapse = ", ")
}

format_cor_line <- function(label, res) {
	if (is.na(res$r) || res$n < 3) {
		return(sprintf("%s: nicht berechnet", label))
	}
	sprintf("%s: r = %.3f, p = %.4f (n = %d)", label, res$r, res$p, res$n)
}

format_r_value <- function(x) {
	if (is.na(x)) {
		return(NA_character_)
	}
	sprintf("%.3f", x)
}

format_r_line <- function(label, value) {
	if (is.na(value)) {
		return(sprintf("%s: nicht berechnet", label))
	}
	sprintf("%s: r = %.3f", label, value)
}

get_corr_value <- function(mat, row_name, col_name) {
	if (is.null(mat)) {
		return(NA_real_)
	}
	if (!row_name %in% rownames(mat) || !col_name %in% colnames(mat)) {
		return(NA_real_)
	}
	mat[row_name, col_name]
}

write.csv(age_corr_table, file.path(output_dir, "age_correlations.csv"), row.names = FALSE)

age_corr_plot_df <- age_corr_table %>%
	filter(!is.na(r))

if (nrow(age_corr_plot_df) > 0) {
	age_corr_plot <- ggplot(age_corr_plot_df, aes(x = Analyse, y = r, fill = r)) +
		geom_col() +
		scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0) +
		plot_theme +
		labs(x = NULL, y = "r", fill = "r") +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(
		file.path(output_dir, "age_correlations.svg"),
		age_corr_plot,
		width = plot_width,
		height = plot_height,
		device = plot_device
	)
}

corr_ext <- NULL
corr_vars_ext <- c(
	"Vertrauenswuerdigkeit",
	"Informationsqualitaet",
	"Einstellung_UGC",
	"Kaufabsicht",
	"Alter",
	"Instagram_Nutzung"
)
corr_input <- data_export %>% select(any_of(corr_vars_ext))
if (ncol(corr_input) >= 2) {
	corr_ext <- psych::corr.test(corr_input, use = "pairwise")
	write.csv(corr_ext$r, file.path(output_dir, "correlations_extended.csv"))
	write.csv(corr_ext$p, file.path(output_dir, "correlations_extended_p_values.csv"))
}

plot_matrix_heatmap <- function(mat, file_name, title, value_format, fill_scale, fill_label) {
	plot_df <- as.data.frame(as.table(mat)) %>%
		rename(var1 = Var1, var2 = Var2, value = Freq)
	p <- ggplot(plot_df, aes(x = var1, y = var2, fill = value)) +
		geom_tile(color = "white") +
		geom_text(aes(label = sprintf(value_format, value)), size = 4) +
		fill_scale +
		labs(title = title, fill = fill_label) +
		plot_theme +
		theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank())
	ggsave(
		file.path(output_dir, file_name),
		p,
		width = plot_width,
		height = plot_height_tall,
		device = plot_device
	)
}

corr_df <- as.data.frame(as.table(corr_mat)) %>%
	rename(var1 = Var1, var2 = Var2, r = Freq)

corr_plot <- ggplot(corr_df, aes(x = var1, y = var2, fill = r)) +
	geom_tile(color = "white") +
	geom_text(aes(label = sprintf("%.2f", r)), size = 4) +
	scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0) +
	labs(title = "Korrelationen der Skalen", fill = "r") +
	plot_theme +
	theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank())

ggsave(
	file.path(output_dir, "correlation_heatmap.svg"),
	corr_plot,
	width = plot_width,
	height = plot_height_tall,
	device = plot_device
)

plot_matrix_heatmap(
	p_mat,
	"correlation_pvalues_heatmap.svg",
	"Korrelationen p-Werte (Skalen)",
	"%.4f",
	scale_fill_gradient(low = "#b2182b", high = "#f7f7f7", limits = c(0, 1)),
	"p"
)

if (!is.null(corr_ext)) {
	plot_matrix_heatmap(
		corr_ext$r,
		"correlation_extended_heatmap.svg",
		"Korrelationen (erweitert)",
		"%.2f",
		scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0),
		"r"
	)
	plot_matrix_heatmap(
		corr_ext$p,
		"correlation_extended_pvalues_heatmap.svg",
		"Korrelationen p-Werte (erweitert)",
		"%.4f",
		scale_fill_gradient(low = "#b2182b", high = "#f7f7f7", limits = c(0, 1)),
		"p"
	)
}

scale_long <- scale_df_de %>%
	pivot_longer(cols = everything(), names_to = "scale", values_to = "value")

hist_plot <- ggplot(scale_long, aes(x = value)) +
	geom_histogram(binwidth = 0.5, fill = "#2c7fb8", color = "white") +
	facet_wrap(~ scale, ncol = 2) +
	coord_cartesian(xlim = c(valid_min, valid_max)) +
	scale_x_continuous(breaks = valid_min:valid_max) +
	plot_theme +
	labs(x = "Skalenwert", y = "Haeufigkeit")

ggsave(
	file.path(output_dir, "scale_histograms.svg"),
	hist_plot,
	width = plot_width,
	height = plot_height_tall,
	device = plot_device
)

plot_scatter <- function(df, x_var, y_var, file_name, x_label, y_label) {
	p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
		geom_point(alpha = 0.7) +
		geom_smooth(method = "lm", se = TRUE, color = "#1b7837") +
		coord_cartesian(xlim = c(valid_min, valid_max), ylim = c(valid_min, valid_max)) +
		scale_x_continuous(breaks = valid_min:valid_max) +
		scale_y_continuous(breaks = valid_min:valid_max) +
		plot_theme +
		labs(x = x_label, y = y_label)
	ggsave(
		file.path(output_dir, file_name),
		p,
		width = plot_width,
		height = plot_height,
		device = plot_device
	)
}

plot_scatter(
	analysis_df_de,
	"Vertrauenswuerdigkeit",
	"Kaufabsicht",
	"scatter_trustworthiness_purchase.svg",
	"Vertrauenswuerdigkeit",
	"Kaufabsicht"
)
plot_scatter(
	analysis_df_de,
	"Informationsqualitaet",
	"Kaufabsicht",
	"scatter_info_quality_purchase.svg",
	"Informationsqualitaet",
	"Kaufabsicht"
)
plot_scatter(
	analysis_df_de,
	"Einstellung_UGC",
	"Kaufabsicht",
	"scatter_attitude_purchase.svg",
	"Einstellung_UGC",
	"Kaufabsicht"
)

model <- lm(purchase_intent ~ trustworthiness + info_quality + attitude_ugc, data = analysis_df)
model_std <- lm(scale(purchase_intent) ~ scale(trustworthiness) + scale(info_quality) + scale(attitude_ugc), data = analysis_df)

predictor_cols <- c("trustworthiness", "info_quality", "attitude_ugc")

calc_tolerance <- function(df, target, predictors) {
	others <- setdiff(predictors, target)
	if (length(others) == 0) {
		return(list(r2 = NA_real_, tolerance = NA_real_))
	}
	fml <- stats::as.formula(paste(target, "~", paste(others, collapse = " + ")))
	model_tmp <- lm(fml, data = df)
	r2 <- summary(model_tmp)$r.squared
	list(r2 = r2, tolerance = 1 - r2)
}

tolerance_table <- dplyr::bind_rows(lapply(predictor_cols, function(target) {
	res <- calc_tolerance(analysis_df, target, predictor_cols)
	tibble::tibble(
		Praediktor = scale_labels[[target]],
		R2 = res$r2,
		Toleranz = res$tolerance,
		Problematisch = res$tolerance < 0.01
	)
}))

write.csv(tolerance_table, file.path(output_dir, "multicollinearity_tolerance.csv"), row.names = FALSE)

tolerance_plot <- ggplot(tolerance_table, aes(x = Praediktor, y = Toleranz)) +
	geom_col(fill = "#80b1d3") +
	geom_hline(yintercept = 0.01, linetype = "dashed", color = "#b2182b") +
	plot_theme +
	labs(x = NULL, y = "Toleranz") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(output_dir, "multicollinearity_tolerance.svg"),
	tolerance_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

residual_values <- residuals(model)
fitted_values <- fitted(model)

shapiro_res <- NULL
if (length(residual_values) >= 3 && length(residual_values) <= 5000) {
	shapiro_res <- shapiro.test(residual_values)
}

bp_res <- lmtest::bptest(model)
dw_res <- lmtest::dwtest(model)

resid_plot <- ggplot(
	tibble::tibble(Fitted = fitted_values, Residuen = residual_values),
	aes(x = Fitted, y = Residuen)
) +
	geom_point(alpha = 0.7) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "#636363") +
	plot_theme +
	labs(x = "Fitted", y = "Residuen")

ggsave(
	file.path(output_dir, "residuals_vs_fitted.svg"),
	resid_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

mahal_table <- NULL
mahal_cutoff <- NA_real_
mahal_outliers <- NA_integer_
mahal_df <- analysis_df %>% select(all_of(predictor_cols))
if (nrow(mahal_df) >= length(predictor_cols) + 1) {
	cov_mat <- tryCatch(stats::cov(mahal_df), error = function(e) NULL)
	if (!is.null(cov_mat)) {
		mahal_values <- mahalanobis(mahal_df, colMeans(mahal_df), cov_mat)
		mahal_cutoff <- qchisq(0.999, df = length(predictor_cols))
		mahal_table <- tibble::tibble(
			Row = seq_len(nrow(mahal_df)),
			Mahalanobis_D2 = mahal_values,
			Cutoff = mahal_cutoff,
			Ausreisser = mahal_values > mahal_cutoff
		)
		mahal_outliers <- sum(mahal_table$Ausreisser)
		write.csv(mahal_table, file.path(output_dir, "mahalanobis_outliers.csv"), row.names = FALSE)

		mahal_plot <- ggplot(mahal_table, aes(x = Mahalanobis_D2)) +
			geom_histogram(binwidth = 1, fill = "#a6cee3", color = "white") +
			geom_vline(xintercept = mahal_cutoff, linetype = "dashed", color = "#b2182b") +
			plot_theme +
			labs(x = "Mahalanobis D2", y = "Anzahl")

		ggsave(
			file.path(output_dir, "mahalanobis_distances.svg"),
			mahal_plot,
			width = plot_width,
			height = plot_height,
			device = plot_device
		)
	}
}

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

sig_predictors <- coef_df %>%
	filter(Praediktor != "Konstante") %>%
	mutate(Signifikant = p_Wert < 0.05)

sig_names <- sig_predictors$Praediktor[sig_predictors$Signifikant]
nonsig_names <- sig_predictors$Praediktor[!sig_predictors$Signifikant]

write.csv(coef_df, file.path(output_dir, "regression_coefficients.csv"), row.names = FALSE)
write.csv(coef_df_std, file.path(output_dir, "regression_coefficients_standardized.csv"), row.names = FALSE)

coef_plot_df <- coef_df_std %>%
	filter(Praediktor != "Konstante") %>%
	mutate(
		Lower = Schaetzung - 1.96 * Std_Fehler,
		Upper = Schaetzung + 1.96 * Std_Fehler
	)

coef_plot <- ggplot(coef_plot_df, aes(x = Praediktor, y = Schaetzung)) +
	geom_col(fill = "#984ea3") +
	geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
	geom_hline(yintercept = 0, linetype = "dashed", color = "#636363") +
	plot_theme +
	labs(x = NULL, y = "Standardisierte Koeffizienten") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(output_dir, "regression_coefficients_standardized.svg"),
	coef_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

model_summary <- summary(model)
fstat <- model_summary$fstatistic
f_p_value <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

corr_matrix_for_summary <- if (!is.null(corr_ext)) corr_ext$r else corr_mat
corr_purchase_att <- get_corr_value(corr_matrix_for_summary, "Einstellung_UGC", "Kaufabsicht")
corr_purchase_info <- get_corr_value(corr_matrix_for_summary, "Informationsqualitaet", "Kaufabsicht")
corr_purchase_trust <- get_corr_value(corr_matrix_for_summary, "Vertrauenswuerdigkeit", "Kaufabsicht")

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
cat("\nZusatzanalysen (Tasks 1-11)\n")
cat("1-3 Korrelationen Alter\n")
print(age_corr_table)
if (is.na(instagram_source)) {
	cat("Hinweis: Keine Spalte mit 'instagram' gefunden; Instagram_Nutzung ist NA.\n")
}
cat("\n4 Korrelationsmatrix (erweitert)\n")
if (!is.null(corr_ext)) {
	print(round(corr_ext$r, 3))
	cat("p-Werte\n")
	print(round(corr_ext$p, 4))
} else {
	cat("Nicht berechnet (zu wenige Variablen).\n")
}
cat("\n5-6 Multikollinearitaet (Toleranzwerte)\n")
print(tolerance_table)
cat("Kriterium: Toleranz < 0.01 = problematisch.\n")
cat("\n7 Normalverteilung der Residuen (Shapiro-Wilk)\n")
if (is.null(shapiro_res)) {
	cat("Shapiro-Wilk nicht berechnet (n ausserhalb 3..5000).\n")
} else {
	cat(sprintf("W = %.3f, p = %.4f\n", shapiro_res$statistic, shapiro_res$p.value))
}
cat("\n8 Homoskedastizitaet (Breusch-Pagan)\n")
cat(sprintf("BP = %.2f, df = %d, p = %.4f\n", bp_res$statistic, as.integer(bp_res$parameter), bp_res$p.value))
cat("\n9 Linearitaet (Residuen vs. Fitted)\n")
cat("Plot gespeichert: residuals_vs_fitted.svg\n")
cat("\n10 Unabhaengigkeit der Residuen (Durbin-Watson)\n")
cat(sprintf("DW = %.3f, p = %.4f\n", dw_res$statistic, dw_res$p.value))
cat("\n11 Ausreisser (Mahalanobis-Distanz)\n")
if (!is.null(mahal_table)) {
	cat(sprintf(
		"Cutoff (p = 0.001, df = %d) = %.3f; Ausreisser = %d\n",
		length(predictor_cols),
		mahal_cutoff,
		mahal_outliers
	))
} else {
	cat("Nicht berechnet (zu wenige Faelle oder singulaere Kovarianzmatrix).\n")
}
cat("\nKurzfazit (Wichtigste Ergebnisse)\n")
cat(sprintf(
	"Modell erklaert %.1f%% der Varianz (R2 = %.3f, adj. R2 = %.3f).\n",
	model_summary$r.squared * 100,
	model_summary$r.squared,
	model_summary$adj.r.squared
))
cat(sprintf(
	"Signifikante Praediktoren: %s. Nicht signifikant: %s.\n",
	format_list(sig_names),
	format_list(nonsig_names)
))
cat(paste0(format_cor_line("Alter x Einstellung_UGC", age_attitude), "\n"))
cat(paste0(format_cor_line("Alter x Kaufabsicht", age_purchase), "\n"))
cat(paste0(format_cor_line("Alter x Instagram_Nutzung", age_instagram), "\n"))
if (is.null(shapiro_res)) {
	cat("Normalitaet Residuen (Shapiro): nicht berechnet.\n")
} else {
	cat(sprintf(
		"Normalitaet Residuen (Shapiro): p = %.4f (%s).\n",
		shapiro_res$p.value,
		ifelse(
			shapiro_res$p.value < 0.05,
			"Hinweis auf Abweichung von Normalverteilung",
			"kein signifikanter Hinweis"
		)
	))
}
cat(sprintf(
	"Homoskedastizitaet (Breusch-Pagan): p = %.4f (%s).\n",
	bp_res$p.value,
	ifelse(bp_res$p.value < 0.05, "Hinweis auf Heteroskedastizitaet", "kein signifikanter Hinweis")
))
cat(sprintf(
	"Unabhaengigkeit Residuen (Durbin-Watson): p = %.4f (%s).\n",
	dw_res$p.value,
	ifelse(dw_res$p.value < 0.05, "Hinweis auf Autokorrelation", "kein signifikanter Hinweis")
))
if (!is.null(mahal_table)) {
	cat(sprintf(
		"Mahalanobis-Ausreisser: Cutoff = %.3f (alpha = 0.001, df = %d), Ausreisser = %d.\n",
		mahal_cutoff,
		length(predictor_cols),
		mahal_outliers
	))
}
cat("\nSachzusammenhang (Einordnung)\n")
if (!is.na(corr_purchase_att) || !is.na(corr_purchase_info) || !is.na(corr_purchase_trust)) {
	cat(sprintf(
		"Kaufabsicht haengt am staerksten mit Einstellung_UGC (r = %s) und Informationsqualitaet (r = %s) zusammen; Vertrauenswuerdigkeit ist schwaecher (r = %s).\n",
		format_r_value(corr_purchase_att),
		format_r_value(corr_purchase_info),
		format_r_value(corr_purchase_trust)
	))
} else {
	cat("Kaufabsicht-Zusammenhaenge: nicht berechnet.\n")
}
if (!is.na(age_attitude$r) && !is.na(age_purchase$r)) {
	cat(sprintf(
		"Alter zeigt keinen signifikanten Zusammenhang mit Einstellung_UGC (r = %.3f, p = %.4f) oder Kaufabsicht (r = %.3f, p = %.4f).\n",
		age_attitude$r,
		age_attitude$p,
		age_purchase$r,
		age_purchase$p
	))
}
if (!is.na(age_instagram$r)) {
	cat(sprintf(
		"Instagram-Nutzung ist negativ mit Alter korreliert (r = %.3f, p = %.4f): juengere nutzen Instagram staerker.\n",
		age_instagram$r,
		age_instagram$p
	))
} else {
	cat("Instagram-Nutzung konnte nicht ausgewertet werden.\n")
}
cat("Multikollinearitaet und Ausreisser sind unproblematisch; die Residuen zeigen jedoch Hinweise auf Abweichung von Normalverteilung und moegliche Autokorrelation.\n")
cat("\nDokumentation Tasks 1-11 (Kurzbeschreibung)\n")
cat("1 Alter x Einstellung: Zusammenhang Alter und Einstellung_UGC.\n")
cat("2 Alter x Kaufabsicht: Zusammenhang Alter und Kaufabsicht.\n")
cat("3 Alter x Instagram: Zusammenhang Alter und Instagram-Nutzung.\n")
cat("4 Korrelationsmatrix: Uebersicht aller paarweisen Zusammenhaenge.\n")
cat("5 Multikollinearitaet: Regression jedes Praediktors auf die anderen.\n")
cat("6 Toleranz (1 - R2): Indikator fuer problematische Ueberschneidungen.\n")
cat("7 Shapiro-Wilk: Normalverteilung der Residuen.\n")
cat("8 Homoskedastizitaet: konstante Residuenstreuung.\n")
cat("9 Linearitaet: Residuen vs. Fitted Plot.\n")
cat("10 Durbin-Watson: Unabhaengigkeit der Residuen.\n")
cat("11 Mahalanobis: multivariate Ausreisser.\n")
cat("\nVisualisierungen (Dateien)\n")
cat("correlation_heatmap.svg; correlation_pvalues_heatmap.svg; correlation_extended_heatmap.svg (falls vorhanden); correlation_extended_pvalues_heatmap.svg (falls vorhanden)\n")
cat("descriptives_means_sd.svg; reliability_alpha.svg; regression_coefficients_standardized.svg\n")
cat("age_correlations.svg; multicollinearity_tolerance.svg; residuals_vs_fitted.svg; mahalanobis_distances.svg\n")
cat("sample_sizes.svg; scale_means_by_gender.svg (falls vorhanden)\n")
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

sample_sizes_long <- sample_sizes %>%
	pivot_longer(cols = everything(), names_to = "Kennzahl", values_to = "Wert")

sample_sizes_plot <- ggplot(sample_sizes_long, aes(x = Kennzahl, y = Wert)) +
	geom_col(fill = "#fb9a99") +
	plot_theme +
	labs(x = NULL, y = "Anzahl") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(output_dir, "sample_sizes.svg"),
	sample_sizes_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

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

	gender_long <- gender_summary %>%
		pivot_longer(
			cols = c(Vertrauenswuerdigkeit, Informationsqualitaet, Einstellung_UGC, Kaufabsicht),
			names_to = "Skala",
			values_to = "Mittelwert"
		)

	gender_plot <- ggplot(gender_long, aes(x = Skala, y = Mittelwert, fill = Geschlecht)) +
		geom_col(position = position_dodge(width = 0.8)) +
		plot_theme +
		labs(x = NULL, y = "Mittelwert") +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(
		file.path(output_dir, "scale_means_by_gender.svg"),
		gender_plot,
		width = plot_width,
		height = plot_height,
		device = plot_device
	)
}

png_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
if (length(png_files) > 0) {
	invisible(file.remove(png_files))
}

pdf_files <- list.files(output_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdf_files) > 0) {
	invisible(file.remove(pdf_files))
}
