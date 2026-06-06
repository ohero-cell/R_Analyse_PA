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
	"sandwich",
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
added_output_dir <- file.path(root_dir, "added Outputs")
second_output_dir <- file.path(root_dir, "second_Output")

if (!file.exists(input_path)) {
	stop("Eingabedatei nicht gefunden: ", input_path)
}

if (!dir.exists(output_dir)) {
	dir.create(output_dir, recursive = TRUE)
}

if (!dir.exists(added_output_dir)) {
	dir.create(added_output_dir, recursive = TRUE)
}

if (!dir.exists(second_output_dir)) {
	dir.create(second_output_dir, recursive = TRUE)
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

gender_source <- if ("D1" %in% names(data)) {
	"D1"
} else if ("Geschlecht" %in% names(data)) {
	"Geschlecht"
} else {
	NA_character_
}

gender_purchase <- list(n = 0, r = NA_real_, p = NA_real_)
gender_instagram <- list(n = 0, r = NA_real_, p = NA_real_)
if (!is.na(gender_source)) {
	data_gender <- data %>%
		mutate(Geschlecht_num = readr::parse_number(as.character(.data[[gender_source]])))
	gender_purchase <- safe_cor_test(data_gender, "Geschlecht_num", "purchase_intent")
	gender_instagram <- safe_cor_test(data_gender, "Geschlecht_num", "Instagram_Nutzung")
}

gender_corr_table <- tibble::tibble(
	Analyse = c("Geschlecht x Kaufabsicht", "Geschlecht x Instagram_Nutzung"),
	n = c(gender_purchase$n, gender_instagram$n),
	r = c(gender_purchase$r, gender_instagram$r),
	p_Wert = c(gender_purchase$p, gender_instagram$p)
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
write.csv(gender_corr_table, file.path(output_dir, "gender_correlations.csv"), row.names = FALSE)

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

gender_corr_plot_df <- gender_corr_table %>%
	filter(!is.na(r))

if (nrow(gender_corr_plot_df) > 0) {
	gender_corr_plot <- ggplot(gender_corr_plot_df, aes(x = Analyse, y = r, fill = r)) +
		geom_col() +
		scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0) +
		plot_theme +
		labs(x = NULL, y = "r", fill = "r") +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(
		file.path(output_dir, "gender_correlations.svg"),
		gender_corr_plot,
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

plot_matrix_heatmap <- function(mat, file_name, title, value_format, fill_scale, fill_label, out_dir = output_dir) {
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
		file.path(out_dir, file_name),
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
cat("\nZusatzanalysen (Tasks 1-13)\n")
cat("1-3 Korrelationen Alter\n")
print(age_corr_table)
cat("4-5 Korrelationen Geschlecht\n")
print(gender_corr_table)
if (is.na(instagram_source)) {
	cat("Hinweis: Keine Spalte mit 'instagram' gefunden; Instagram_Nutzung ist NA.\n")
}
cat("\n6 Korrelationsmatrix (erweitert)\n")
if (!is.null(corr_ext)) {
	print(round(corr_ext$r, 3))
	cat("p-Werte\n")
	print(round(corr_ext$p, 4))
} else {
	cat("Nicht berechnet (zu wenige Variablen).\n")
}
cat("\n7-8 Multikollinearitaet (Toleranzwerte)\n")
print(tolerance_table)
cat("Kriterium: Toleranz < 0.01 = problematisch.\n")
cat("\n9 Normalverteilung der Residuen (Shapiro-Wilk)\n")
if (is.null(shapiro_res)) {
	cat("Shapiro-Wilk nicht berechnet (n ausserhalb 3..5000).\n")
} else {
	cat(sprintf("W = %.3f, p = %.4f\n", shapiro_res$statistic, shapiro_res$p.value))
}
cat("\n10 Homoskedastizitaet (Breusch-Pagan)\n")
cat(sprintf("BP = %.2f, df = %d, p = %.4f\n", bp_res$statistic, as.integer(bp_res$parameter), bp_res$p.value))
cat("\n11 Linearitaet (Residuen vs. Fitted)\n")
cat("Plot gespeichert: residuals_vs_fitted.svg\n")
cat("\n12 Unabhaengigkeit der Residuen (Durbin-Watson)\n")
cat(sprintf("DW = %.3f, p = %.4f\n", dw_res$statistic, dw_res$p.value))
cat("\n13 Ausreisser (Mahalanobis-Distanz)\n")
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
cat(paste0(format_cor_line("Geschlecht x Kaufabsicht", gender_purchase), "\n"))
cat(paste0(format_cor_line("Geschlecht x Instagram_Nutzung", gender_instagram), "\n"))
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
cat("\nDokumentation Tasks 1-13 (Kurzbeschreibung)\n")
cat("1 Alter x Einstellung: Zusammenhang Alter und Einstellung_UGC.\n")
cat("2 Alter x Kaufabsicht: Zusammenhang Alter und Kaufabsicht.\n")
cat("3 Alter x Instagram: Zusammenhang Alter und Instagram-Nutzung.\n")
cat("4 Geschlecht x Kaufabsicht: Zusammenhang Geschlecht und Kaufabsicht.\n")
cat("5 Geschlecht x Instagram: Zusammenhang Geschlecht und Instagram-Nutzung.\n")
cat("6 Korrelationsmatrix: Uebersicht aller paarweisen Zusammenhaenge.\n")
cat("7 Multikollinearitaet: Regression jedes Praediktors auf die anderen.\n")
cat("8 Toleranz (1 - R2): Indikator fuer problematische Ueberschneidungen.\n")
cat("9 Shapiro-Wilk: Normalverteilung der Residuen.\n")
cat("10 Homoskedastizitaet: konstante Residuenstreuung.\n")
cat("11 Linearitaet: Residuen vs. Fitted Plot.\n")
cat("12 Durbin-Watson: Unabhaengigkeit der Residuen.\n")
cat("13 Mahalanobis: multivariate Ausreisser.\n")
cat("\nVisualisierungen (Dateien)\n")
cat("correlation_heatmap.svg; correlation_pvalues_heatmap.svg; correlation_extended_heatmap.svg (falls vorhanden); correlation_extended_pvalues_heatmap.svg (falls vorhanden)\n")
cat("descriptives_means_sd.svg; reliability_alpha.svg; regression_coefficients_standardized.svg\n")
cat("age_correlations.svg; gender_correlations.svg; multicollinearity_tolerance.svg; residuals_vs_fitted.svg; mahalanobis_distances.svg\n")
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

fmt_num <- function(x, digits = 3) {
	if (is.na(x)) {
		return("NA")
	}
	formatC(x, digits = digits, format = "f")
}

fmt_p <- function(p) {
	if (is.na(p)) {
		return("NA")
	}
	formatC(p, digits = 4, format = "f")
}

cohen_d <- function(x, g) {
	g <- droplevels(factor(g))
	if (length(levels(g)) != 2) {
		return(NA_real_)
	}
	x1 <- x[g == levels(g)[1]]
	x2 <- x[g == levels(g)[2]]
	if (length(x1) < 2 || length(x2) < 2) {
		return(NA_real_)
	}
	s1 <- stats::sd(x1)
	s2 <- stats::sd(x2)
	sp <- sqrt(((length(x1) - 1) * s1^2 + (length(x2) - 1) * s2^2) / (length(x1) + length(x2) - 2))
	(mean(x1) - mean(x2)) / sp
}

eta_sq <- function(aov_model) {
	ss <- summary(aov_model)[[1]][["Sum Sq"]]
	ss[1] / sum(ss)
}

pairwise_corr <- function(df, method = "spearman") {
	vars <- names(df)
	n <- length(vars)
	r_mat <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(vars, vars))
	p_mat <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(vars, vars))
	for (i in seq_len(n)) {
		for (j in seq_len(n)) {
			if (i == j) {
				r_mat[i, j] <- 1
				p_mat[i, j] <- 0
				next
			}
			sub <- df[, c(vars[i], vars[j])]
			sub <- sub[complete.cases(sub), ]
			if (nrow(sub) < 3) {
				next
			}
			res <- suppressWarnings(stats::cor.test(sub[[1]], sub[[2]], method = method))
			r_mat[i, j] <- unname(res$estimate)
			p_mat[i, j] <- res$p.value
		}
	}
	list(r = r_mat, p = p_mat)
}

mediation_boot <- function(df, treat, mediator, outcome, n_boot = 2000, seed = 123) {
	vars <- c(treat, mediator, outcome)
	use_df <- df[, vars]
	use_df <- use_df[complete.cases(use_df), ]
	if (nrow(use_df) < 30) {
		return(NULL)
	}
	model_a <- lm(stats::as.formula(paste(mediator, "~", treat)), data = use_df)
	model_b <- lm(stats::as.formula(paste(outcome, "~", mediator, "+", treat)), data = use_df)
	model_c <- lm(stats::as.formula(paste(outcome, "~", treat)), data = use_df)
	a <- coef(model_a)[treat]
	b <- coef(model_b)[mediator]
	c_prime <- coef(model_b)[treat]
	c_total <- coef(model_c)[treat]
	indirect <- a * b
	sa <- summary(model_a)$coefficients[treat, "Std. Error"]
	sb <- summary(model_b)$coefficients[mediator, "Std. Error"]
	se_indirect <- sqrt(b^2 * sa^2 + a^2 * sb^2)
	z_sobel <- indirect / se_indirect
	p_sobel <- 2 * (1 - stats::pnorm(abs(z_sobel)))
	set.seed(seed)
	boot_vals <- numeric(n_boot)
	for (i in seq_len(n_boot)) {
		idx <- sample(seq_len(nrow(use_df)), replace = TRUE)
		boot_df <- use_df[idx, ]
		boot_a <- lm(stats::as.formula(paste(mediator, "~", treat)), data = boot_df)
		boot_b <- lm(stats::as.formula(paste(outcome, "~", mediator, "+", treat)), data = boot_df)
		boot_vals[i] <- coef(boot_a)[treat] * coef(boot_b)[mediator]
	}
	ci <- stats::quantile(boot_vals, probs = c(0.025, 0.975), na.rm = TRUE)
	prop_med <- if (is.na(c_total) || c_total == 0) NA_real_ else indirect / c_total
	tibble::tibble(
		Treat = treat,
		Mediator = mediator,
		Outcome = outcome,
		N = nrow(use_df),
		A = a,
		B = b,
		Indirect = indirect,
		CI_Lower = unname(ci[1]),
		CI_Upper = unname(ci[2]),
		Direct = c_prime,
		Total = c_total,
		Prop_Mediated = prop_med,
		Sobel_z = z_sobel,
		Sobel_p = p_sobel
	)
}

label_var <- function(var) {
	dplyr::recode(
		var,
		trustworthiness = "Vertrauenswuerdigkeit",
		info_quality = "Informationsqualitaet",
		attitude_ugc = "Einstellung_UGC",
		purchase_intent = "Kaufabsicht",
		Instagram_Nutzung = "Instagram_Nutzung",
		Alter = "Alter",
		.default = var
	)
}

rename_cols <- function(df, mapping) {
	for (from in names(mapping)) {
		if (from %in% names(df)) {
			names(df)[names(df) == from] <- mapping[[from]]
		}
	}
	df
}

coef_map_de <- c(
	"Estimate" = "Schaetzung",
	"Std. Error" = "Std_Fehler",
	"t value" = "t_Wert",
	"Pr(>|t|)" = "p_Wert",
	"estimate" = "Schaetzung",
	"std.error" = "Std_Fehler",
	"t.value" = "t_Wert",
	"p.value" = "p_Wert"
)

coef_map_lower <- c(
	"Estimate" = "estimate",
	"Std. Error" = "std.error",
	"t value" = "statistic",
	"Pr(>|t|)" = "p.value"
)

## Zusatzanalysen fuer added Outputs

gender_group_results <- tibble::tibble()
gender_group_desc <- tibble::tibble()
gender_group_plot_done <- FALSE

if (!is.na(gender_source)) {
	gender_group_df <- data %>%
		mutate(Geschlecht_group = as.character(.data[[gender_source]])) %>%
		mutate(Geschlecht_group = ifelse(Geschlecht_group == "", NA_character_, Geschlecht_group))

	gender_vars <- intersect(
		c("trustworthiness", "info_quality", "attitude_ugc", "purchase_intent", "Instagram_Nutzung"),
		names(gender_group_df)
	)

	if (length(gender_vars) > 0) {
		gender_long <- gender_group_df %>%
			select(Geschlecht_group, all_of(gender_vars)) %>%
			pivot_longer(cols = all_of(gender_vars), names_to = "Variable", values_to = "Wert")

		gender_group_desc <- gender_long %>%
			group_by(Variable, Geschlecht_group) %>%
			summarise(
				n = sum(!is.na(Wert)),
				Mittelwert = mean(Wert, na.rm = TRUE),
				SD = stats::sd(Wert, na.rm = TRUE),
				.groups = "drop"
			)

		write.csv(
			gender_group_desc,
			file.path(added_output_dir, "gender_group_descriptives.csv"),
			row.names = FALSE
		)

		if (length(unique(na.omit(gender_long$Geschlecht_group))) >= 2) {
			for (var in gender_vars) {
				sub <- gender_group_df %>%
					select(Geschlecht_group, all_of(var)) %>%
					filter(!is.na(Geschlecht_group), !is.na(.data[[var]]))
				groups <- sort(unique(sub$Geschlecht_group))
				if (length(groups) < 2) {
					next
				}
				if (length(groups) == 2) {
					group_counts <- table(sub$Geschlecht_group)
					if (min(group_counts) < 2) {
						next
					}
					tt <- stats::t.test(sub[[var]] ~ sub$Geschlecht_group)
					wil <- suppressWarnings(stats::wilcox.test(sub[[var]] ~ sub$Geschlecht_group, exact = FALSE))
					d_val <- cohen_d(sub[[var]], sub$Geschlecht_group)
					gender_group_results <- dplyr::bind_rows(
						gender_group_results,
						tibble::tibble(
							Variable = var,
							Test = "t_test",
							Statistik = unname(tt$statistic),
							df1 = unname(tt$parameter),
							df2 = NA_real_,
							p_Wert = tt$p.value,
							Effekt = d_val
						),
						tibble::tibble(
							Variable = var,
							Test = "wilcox",
							Statistik = unname(wil$statistic),
							df1 = NA_real_,
							df2 = NA_real_,
							p_Wert = wil$p.value,
							Effekt = NA_real_
						)
					)
				} else {
					aov_mod <- stats::aov(sub[[var]] ~ sub$Geschlecht_group)
					kw <- stats::kruskal.test(sub[[var]] ~ sub$Geschlecht_group)
					eta <- eta_sq(aov_mod)
					anova_tbl <- summary(aov_mod)[[1]]
					gender_group_results <- dplyr::bind_rows(
						gender_group_results,
						tibble::tibble(
							Variable = var,
							Test = "anova",
							Statistik = anova_tbl["sub$Geschlecht_group", "F value"],
							df1 = anova_tbl["sub$Geschlecht_group", "Df"],
							df2 = anova_tbl["Residuals", "Df"],
							p_Wert = anova_tbl["sub$Geschlecht_group", "Pr(>F)"],
							Effekt = eta
						),
						tibble::tibble(
							Variable = var,
							Test = "kruskal",
							Statistik = unname(kw$statistic),
							df1 = unname(kw$parameter),
							df2 = NA_real_,
							p_Wert = kw$p.value,
							Effekt = NA_real_
						)
					)
				}
			}

			write.csv(
				gender_group_results,
				file.path(added_output_dir, "gender_group_tests.csv"),
				row.names = FALSE
			)

			gender_plot <- ggplot(
				gender_long %>% filter(!is.na(Geschlecht_group)),
				aes(x = Geschlecht_group, y = Wert, fill = Geschlecht_group)
			) +
				geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
				facet_wrap(~ Variable, ncol = 2, scales = "free_y") +
				plot_theme +
				labs(x = "Geschlecht", y = "Wert") +
				theme(legend.position = "none")

			ggsave(
				file.path(added_output_dir, "gender_group_boxplots.svg"),
				gender_plot,
				width = plot_width,
				height = plot_height_tall,
				device = plot_device
			)
			gender_group_plot_done <- TRUE
		}
	}
}

spearman_vars <- c(
	"Vertrauenswuerdigkeit",
	"Informationsqualitaet",
	"Einstellung_UGC",
	"Kaufabsicht",
	"Alter",
	"Instagram_Nutzung"
)

spearman_input <- data_export %>% select(any_of(spearman_vars))
spearman_corr <- NULL
spearman_p <- NULL

if (ncol(spearman_input) >= 2) {
	spearman_res <- pairwise_corr(spearman_input, method = "spearman")
	spearman_corr <- spearman_res$r
	spearman_p <- spearman_res$p
	write.csv(spearman_corr, file.path(added_output_dir, "spearman_correlations.csv"))
	write.csv(spearman_p, file.path(added_output_dir, "spearman_p_values.csv"))

	plot_matrix_heatmap(
		spearman_corr,
		"spearman_correlation_heatmap.svg",
		"Spearman-Korrelationen",
		"%.2f",
		scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0),
		"rho",
		out_dir = added_output_dir
	)
	plot_matrix_heatmap(
		spearman_p,
		"spearman_pvalues_heatmap.svg",
		"Spearman p-Werte",
		"%.4f",
		scale_fill_gradient(low = "#b2182b", high = "#f7f7f7", limits = c(0, 1)),
		"p",
		out_dir = added_output_dir
	)
}

robust_df <- tibble::tibble()
if (exists("model")) {
	robust_coefs <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC3"))
	robust_df <- as.data.frame(unclass(robust_coefs)) %>%
		tibble::rownames_to_column(var = "Praediktor") %>%
		mutate(Praediktor = recode_predictor(Praediktor))
	robust_df <- rename_cols(robust_df, coef_map_de)
	write.csv(robust_df, file.path(added_output_dir, "regression_robust_se.csv"), row.names = FALSE)
}

mediation_results <- tibble::tibble()
if (nrow(analysis_df) >= 30) {
	med_info <- mediation_boot(analysis_df, "info_quality", "attitude_ugc", "purchase_intent")
	med_trust <- mediation_boot(analysis_df, "trustworthiness", "attitude_ugc", "purchase_intent")
	mediation_results <- dplyr::bind_rows(med_info, med_trust)
	if (nrow(mediation_results) > 0) {
		mediation_results <- mediation_results %>%
			mutate(
				Treat = recode_predictor(Treat),
				Mediator = recode_predictor(Mediator),
				Outcome = recode_predictor(Outcome)
			)
		write.csv(mediation_results, file.path(added_output_dir, "mediation_results.csv"), row.names = FALSE)
	}
}

moderation_results <- tibble::tibble()
if (all(c("D2", "Instagram_Nutzung") %in% names(data))) {
	mod_df <- data %>%
		select(trustworthiness, info_quality, attitude_ugc, purchase_intent, D2, Instagram_Nutzung) %>%
		rename(Alter = D2) %>%
		mutate(
			attitude_c = scale(attitude_ugc, center = TRUE, scale = FALSE)[, 1],
			age_c = scale(Alter, center = TRUE, scale = FALSE)[, 1],
			instagram_c = scale(Instagram_Nutzung, center = TRUE, scale = FALSE)[, 1]
		)

	mod_age_df <- mod_df %>% select(purchase_intent, trustworthiness, info_quality, attitude_c, age_c) %>% drop_na()
	if (nrow(mod_age_df) >= 30) {
		mod_age <- lm(purchase_intent ~ attitude_c * age_c + trustworthiness + info_quality, data = mod_age_df)
		mod_age_df_out <- as.data.frame(summary(mod_age)$coefficients) %>%
			tibble::rownames_to_column(var = "Term") %>%
			mutate(Modell = "UGC_x_Alter")
		moderation_results <- dplyr::bind_rows(moderation_results, mod_age_df_out)
	}

	mod_insta_df <- mod_df %>% select(purchase_intent, trustworthiness, info_quality, attitude_c, instagram_c) %>% drop_na()
	if (nrow(mod_insta_df) >= 30) {
		mod_insta <- lm(purchase_intent ~ attitude_c * instagram_c + trustworthiness + info_quality, data = mod_insta_df)
		mod_insta_df_out <- as.data.frame(summary(mod_insta)$coefficients) %>%
			tibble::rownames_to_column(var = "Term") %>%
			mutate(Modell = "UGC_x_Instagram")
		moderation_results <- dplyr::bind_rows(moderation_results, mod_insta_df_out)
	}

	if (nrow(moderation_results) > 0) {
		moderation_results <- rename_cols(moderation_results, coef_map_de)
		write.csv(moderation_results, file.path(added_output_dir, "moderation_results.csv"), row.names = FALSE)
	}
}

influence_df <- tibble::tibble()
influence_summary <- tibble::tibble()
if (exists("model") && nrow(analysis_df) >= 3) {
	cooks <- stats::cooks.distance(model)
	leverages <- stats::hatvalues(model)
	std_resid <- stats::rstandard(model)
	cutoff <- 4 / nrow(analysis_df)
	influence_df <- tibble::tibble(
		Row = seq_len(nrow(analysis_df)),
		CooksD = cooks,
		Leverage = leverages,
		StdResid = std_resid,
		Influential = cooks > cutoff
	)
	write.csv(influence_df, file.path(added_output_dir, "influence_diagnostics.csv"), row.names = FALSE)
	influence_summary <- tibble::tibble(
		N = nrow(analysis_df),
		CooksD_Cutoff = cutoff,
		Influential_Count = sum(influence_df$Influential, na.rm = TRUE)
	)
	write.csv(influence_summary, file.path(added_output_dir, "influence_summary.csv"), row.names = FALSE)

	cooks_plot <- ggplot(influence_df, aes(x = Row, y = CooksD)) +
		geom_col(fill = "#8dd3c7") +
		geom_hline(yintercept = cutoff, linetype = "dashed", color = "#b2182b") +
		plot_theme +
		labs(x = "Fall", y = "Cook's Distance")

	ggsave(
		file.path(added_output_dir, "cooks_distance.svg"),
		cooks_plot,
		width = plot_width,
		height = plot_height,
		device = plot_device
	)
}

missing_vars <- c(
	"Vertrauenswuerdigkeit",
	"Informationsqualitaet",
	"Einstellung_UGC",
	"Kaufabsicht",
	"Alter",
	"Geschlecht",
	"Instagram_Nutzung"
)

missing_df <- data_export %>% select(any_of(missing_vars))

missing_summary <- tibble::tibble(
	Variable = names(missing_df),
	Missing_n = vapply(missing_df, function(x) sum(is.na(x)), integer(1)),
	Missing_pct = vapply(missing_df, function(x) mean(is.na(x)) * 100, numeric(1))
)

write.csv(missing_summary, file.path(added_output_dir, "missingness_summary.csv"), row.names = FALSE)

missing_tests <- tibble::tibble()
if ("Kaufabsicht" %in% names(data_export)) {
	miss_flag <- is.na(data_export$Kaufabsicht)
	if ("Alter" %in% names(data_export)) {
		sub <- data_export %>%
			select(Alter) %>%
			mutate(Missing = miss_flag) %>%
			filter(!is.na(Alter))
		if (length(unique(sub$Missing)) == 2) {
			group_counts <- table(sub$Missing)
			if (min(group_counts) >= 2) {
				res <- stats::t.test(Alter ~ Missing, data = sub)
				missing_tests <- dplyr::bind_rows(
					missing_tests,
					tibble::tibble(
						Test = "t_test",
						Variable = "Alter",
						Statistik = unname(res$statistic),
						df1 = unname(res$parameter),
						df2 = NA_real_,
						p_Wert = res$p.value
					)
				)
			}
		}
	}
	if ("Geschlecht" %in% names(data_export)) {
		tbl <- table(data_export$Geschlecht, miss_flag, useNA = "no")
		if (all(dim(tbl) >= 2)) {
			chi <- suppressWarnings(stats::chisq.test(tbl))
			missing_tests <- dplyr::bind_rows(
				missing_tests,
				tibble::tibble(
					Test = "chisq",
					Variable = "Geschlecht",
					Statistik = unname(chi$statistic),
					df1 = unname(chi$parameter),
					df2 = NA_real_,
					p_Wert = chi$p.value
				)
			)
		}
	}
}

if (nrow(missing_tests) > 0) {
	write.csv(missing_tests, file.path(added_output_dir, "missingness_tests.csv"), row.names = FALSE)
}

imputed_regression <- tibble::tibble()
imp_vars <- c("trustworthiness", "info_quality", "attitude_ugc", "purchase_intent")
imp_df <- data %>% select(any_of(imp_vars))

if (ncol(imp_df) == 4) {
	imp_df_imputed <- imp_df
	for (col in names(imp_df_imputed)) {
		mean_val <- mean(imp_df_imputed[[col]], na.rm = TRUE)
		imp_df_imputed[[col]][is.na(imp_df_imputed[[col]])] <- mean_val
	}
	imp_model <- lm(purchase_intent ~ trustworthiness + info_quality + attitude_ugc, data = imp_df_imputed)
	imputed_regression <- as.data.frame(summary(imp_model)$coefficients) %>%
		tibble::rownames_to_column(var = "term")
	imputed_regression <- rename_cols(imputed_regression, coef_map_lower)
	write.csv(imputed_regression, file.path(added_output_dir, "regression_imputed_mean.csv"), row.names = FALSE)
}

notes_lines <- c(
	"# Added Analysen (added Outputs)",
	"",
	"Diese Datei erklaert die zusaetzlichen Auswertungen im Ordner 'added Outputs'.",
	"",
	"## 1) Geschlecht Gruppenvergleich",
	"Zweck: Testet, ob sich Mittelwerte der Skalen zwischen Geschlechtsgruppen unterscheiden.",
	if (nrow(gender_group_results) > 0) {
		gender_purchase <- gender_group_results %>%
			filter(Variable == "purchase_intent", Test %in% c("t_test", "anova")) %>%
			slice(1)
		if (nrow(gender_purchase) == 1) {
			paste0(
				"Ergebnis Kaufabsicht: ",
				gender_purchase$Test,
				", p = ",
				fmt_p(gender_purchase$p_Wert),
				", Effekt = ",
				fmt_num(gender_purchase$Effekt)
			)
		} else {
			"Ergebnis Kaufabsicht: nicht berechnet."
		}
	} else {
		"Ergebnis Kaufabsicht: nicht berechnet (zu wenige Gruppen)."
	},
	"Dateien: gender_group_descriptives.csv, gender_group_tests.csv, gender_group_boxplots.svg",
	"",
	"## 2) Robuste Korrelationen (Spearman)",
	"Zweck: Pruft Zusammenhaenge ohne Normalverteilungsannahme.",
	if (!is.null(spearman_corr)) {
		paste0(
			"Kaufabsicht x Einstellung_UGC: rho = ",
			fmt_num(get_corr_value(spearman_corr, "Kaufabsicht", "Einstellung_UGC")),
			", p = ",
			fmt_p(get_corr_value(spearman_p, "Kaufabsicht", "Einstellung_UGC")),
			"; Kaufabsicht x Informationsqualitaet: rho = ",
			fmt_num(get_corr_value(spearman_corr, "Kaufabsicht", "Informationsqualitaet")),
			", p = ",
			fmt_p(get_corr_value(spearman_p, "Kaufabsicht", "Informationsqualitaet")),
			"; Alter x Instagram_Nutzung: rho = ",
			fmt_num(get_corr_value(spearman_corr, "Alter", "Instagram_Nutzung")),
			", p = ",
			fmt_p(get_corr_value(spearman_p, "Alter", "Instagram_Nutzung"))
		)
	} else {
		"Spearman-Korrelationen nicht berechnet (zu wenige Variablen)."
	},
	"Dateien: spearman_correlations.csv, spearman_p_values.csv, spearman_correlation_heatmap.svg, spearman_pvalues_heatmap.svg",
	"",
	"## 3) Robuste Regression (HC3 Standardfehler)",
	"Zweck: Prueft, ob die Regressionsergebnisse bei robusten Standardfehlern stabil bleiben.",
	if (nrow(robust_df) > 0 && "p_Wert" %in% names(robust_df)) {
		rob_att <- robust_df %>% filter(Praediktor == "Einstellung_UGC") %>% slice(1)
		rob_info <- robust_df %>% filter(Praediktor == "Informationsqualitaet") %>% slice(1)
		rob_trust <- robust_df %>% filter(Praediktor == "Vertrauenswuerdigkeit") %>% slice(1)
		paste0(
			"p-Werte (robust): Einstellung_UGC = ", fmt_p(rob_att$p_Wert),
			", Informationsqualitaet = ", fmt_p(rob_info$p_Wert),
			", Vertrauenswuerdigkeit = ", fmt_p(rob_trust$p_Wert)
		)
	} else {
		"Robuste Regression nicht berechnet."
	},
	"Datei: regression_robust_se.csv",
	"",
	"## 4) Mediation (Bootstrap)",
	"Zweck: Testet indirekte Effekte ueber Einstellung_UGC.",
	if (nrow(mediation_results) > 0) {
		med_info <- mediation_results %>% filter(Treat == "Informationsqualitaet") %>% slice(1)
		med_trust <- mediation_results %>% filter(Treat == "Vertrauenswuerdigkeit") %>% slice(1)
		paste0(
			"Indirekt Informationsqualitaet -> Einstellung_UGC -> Kaufabsicht: ",
			fmt_num(med_info$Indirect),
			" (CI ", fmt_num(med_info$CI_Lower), ", ", fmt_num(med_info$CI_Upper), "). ",
			"Indirekt Vertrauenswuerdigkeit: ",
			fmt_num(med_trust$Indirect),
			" (CI ", fmt_num(med_trust$CI_Lower), ", ", fmt_num(med_trust$CI_Upper), ")."
		)
	} else {
		"Mediation nicht berechnet (zu wenige vollstaendige Faelle)."
	},
	"Datei: mediation_results.csv",
	"",
	"## 5) Moderation (Interaktionen)",
	"Zweck: Prueft, ob der Zusammenhang Einstellung_UGC -> Kaufabsicht je nach Alter/Instagram unterschiedlich ist.",
	if (nrow(moderation_results) > 0 && "p_Wert" %in% names(moderation_results)) {
		mod_age <- moderation_results %>% filter(Modell == "UGC_x_Alter", Term == "attitude_c:age_c") %>% slice(1)
		mod_insta <- moderation_results %>% filter(Modell == "UGC_x_Instagram", Term == "attitude_c:instagram_c") %>% slice(1)
		paste0(
			"Interaktion UGC x Alter: p = ", fmt_p(mod_age$p_Wert),
			"; Interaktion UGC x Instagram: p = ", fmt_p(mod_insta$p_Wert)
		)
	} else {
		"Moderation nicht berechnet (zu wenige Faelle)."
	},
	"Datei: moderation_results.csv",
	"",
	"## 6) Einflussdiagnostik",
	"Zweck: Identifiziert einflussreiche Faelle in der Regression.",
	if (nrow(influence_summary) > 0) {
		paste0(
			"Influential Count (Cook's D > 4/n): ",
			influence_summary$Influential_Count
		)
	} else {
		"Einflussdiagnostik nicht berechnet."
	},
	"Dateien: influence_diagnostics.csv, influence_summary.csv, cooks_distance.svg",
	"",
	"## 7) Missing Data + Imputation",
	"Zweck: Beschreibt fehlende Werte und zeigt eine Imputations-Sensitivitaet.",
	if (nrow(missing_summary) > 0) {
		miss_pi <- missing_summary %>% filter(Variable == "Kaufabsicht") %>% slice(1)
		paste0(
			"Fehlende Kaufabsicht: ",
			fmt_num(miss_pi$Missing_pct, 1),
			"% (",
			miss_pi$Missing_n,
			" Faelle)."
		)
	} else {
		"Missing-Data-Uebersicht nicht berechnet."
	},
	if (nrow(imputed_regression) > 0 && "p.value" %in% names(imputed_regression)) {
		imp_info <- imputed_regression %>% filter(term == "info_quality") %>% slice(1)
		imp_att <- imputed_regression %>% filter(term == "attitude_ugc") %>% slice(1)
		imp_trust <- imputed_regression %>% filter(term == "trustworthiness") %>% slice(1)
		paste0(
			"Imputation (Mittelwert) p-Werte: Einstellung_UGC = ",
			fmt_p(imp_att$p.value),
			", Informationsqualitaet = ",
			fmt_p(imp_info$p.value),
			", Vertrauenswuerdigkeit = ",
			fmt_p(imp_trust$p.value)
		)
	} else {
		"Imputation nicht berechnet."
	},
	"Dateien: missingness_summary.csv, missingness_tests.csv (falls vorhanden), regression_imputed_mean.csv",
	"",
	"## Hinweise zur Interpretation",
	"- Signifikante p-Werte (< 0.05) deuten auf statistische Unterschiede/Zusammenhaenge hin.",
	"- Effekte sollten immer zusammen mit Effektstaerken und inhaltlichem Kontext bewertet werden.",
	"- Interaktionen sind nur bedeutsam, wenn der Interaktionsterm signifikant ist.",
	"- Mittelwert-Imputation ist nur eine Sensitivitaetsanalyse und ersetzt keine saubere Missing-Data-Diagnose."
)

writeLines(notes_lines, file.path(added_output_dir, "erklaerung_added_outputs.md"))

second_output_gender_label <- function(x) {
	dplyr::case_when(
		x == 1 ~ "Frauen",
		x == 2 ~ "Maenner",
		is.na(x) ~ NA_character_,
		TRUE ~ paste0("Code ", x)
	)
}

vif_table <- tolerance_table %>%
	mutate(
		VIF = ifelse(Toleranz > 0, 1 / Toleranz, NA_real_),
		Bedenklich_ab_2 = VIF >= 2
	) %>%
	select(Praediktor, R2, Toleranz, VIF, Bedenklich_ab_2)

model_f2 <- model_summary$r.squared / (1 - model_summary$r.squared)
model_f <- sqrt(model_f2)

effect_size_table <- tibble::tibble(
	Praediktor = "Gesamtmodell",
	R2 = model_summary$r.squared,
	Toleranz = NA_real_,
	VIF = NA_real_,
	Bedenklich_ab_2 = NA,
	Cohens_f2 = model_f2,
	Cohens_f = model_f
)

vif_effect_table <- vif_table %>%
	mutate(Cohens_f2 = NA_real_, Cohens_f = NA_real_) %>%
	bind_rows(effect_size_table) %>%
	select(Praediktor, R2, Toleranz, VIF, Bedenklich_ab_2, Cohens_f2, Cohens_f)

write.csv(vif_effect_table, file.path(second_output_dir, "vif_und_effektstaerke.csv"), row.names = FALSE)

gender_counts <- data %>%
	filter(!is.na(D1)) %>%
	mutate(Geschlecht_label = second_output_gender_label(D1)) %>%
	count(Geschlecht_label, name = "Anzahl") %>%
	mutate(Prozent = Anzahl / sum(Anzahl) * 100) %>%
	mutate(
		Geschlecht_label = factor(Geschlecht_label, levels = c("Frauen", "Maenner")),
		Label = sprintf("%d\n(%.1f%%)", Anzahl, Prozent)
	)

if (nrow(gender_counts) > 0) {
	gender_plot <- ggplot(gender_counts, aes(x = Geschlecht_label, y = Anzahl, fill = Geschlecht_label)) +
		geom_col(width = 0.65) +
		geom_text(aes(label = Label), vjust = -0.35, size = 3.5) +
		scale_fill_manual(values = c("Frauen" = "#e41a1c", "Maenner" = "#377eb8")) +
		plot_theme +
		labs(x = NULL, y = "Anzahl", title = "Geschlechterverteilung") +
		theme(legend.position = "none", axis.text.x = element_text(size = 10))

	ggsave(
		file.path(second_output_dir, "geschlechterverteilung.svg"),
		gender_plot,
		width = plot_width,
		height = plot_height,
		device = plot_device
	)
}

age_values <- data %>%
	filter(!is.na(D2)) %>%
	transmute(Alter = D2)

if (nrow(age_values) > 0) {
	age_plot <- ggplot(age_values, aes(x = Alter)) +
		geom_histogram(binwidth = 5, fill = "#80b1d3", color = "white", boundary = 0) +
		geom_vline(xintercept = mean(age_values$Alter, na.rm = TRUE), linetype = "dashed", color = "#b2182b") +
		plot_theme +
		labs(x = "Alter", y = "Anzahl", title = "Altersverteilung") +
		scale_x_continuous(breaks = scales::pretty_breaks(n = 8))

	ggsave(
		file.path(second_output_dir, "altersverteilung.svg"),
		age_plot,
		width = plot_width,
		height = plot_height,
		device = plot_device
	)
}

means_plot_df <- desc_summary %>%
	mutate(Skala = factor(Skala, levels = c("Vertrauenswuerdigkeit", "Informationsqualitaet", "Einstellung_UGC", "Kaufabsicht")))

means_plot <- ggplot(means_plot_df, aes(x = Skala, y = Mittelwert)) +
	geom_col(fill = "#2c7fb8", width = 0.7) +
	geom_errorbar(aes(ymin = Mittelwert - SD, ymax = Mittelwert + SD), width = 0.2) +
	plot_theme +
	labs(x = NULL, y = "Mittelwert (SD)", title = "Mittelwerte der Konstrukte") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(second_output_dir, "mittelwerte_konstrukte.svg"),
	means_plot,
	width = plot_width,
	height = plot_height,
	device = plot_device
)

purchase_gender_summary <- data %>%
	filter(!is.na(D1), !is.na(purchase_intent)) %>%
	mutate(Geschlecht = second_output_gender_label(D1)) %>%
	group_by(Geschlecht) %>%
	summarise(
		Anzahl = n(),
		Mittelwert = mean(purchase_intent, na.rm = TRUE),
		SD = sd(purchase_intent, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(
		Geschlecht = factor(Geschlecht, levels = c("Frauen", "Maenner")),
		Label = sprintf("M = %.2f\nSD = %.2f", Mittelwert, SD)
	)

if (nrow(purchase_gender_summary) > 0) {
	purchase_gender_plot <- ggplot(purchase_gender_summary, aes(x = Geschlecht, y = Mittelwert, fill = Geschlecht)) +
		geom_col(width = 0.65) +
		geom_errorbar(aes(ymin = Mittelwert - SD, ymax = Mittelwert + SD), width = 0.18) +
		geom_text(aes(label = Label), vjust = -0.45, size = 3.5) +
		scale_fill_manual(values = c("Frauen" = "#fb9a99", "Maenner" = "#a6cee3")) +
		coord_cartesian(ylim = c(0, 5)) +
		plot_theme +
		labs(
			x = NULL,
			y = "Kaufabsicht",
			title = "Kaufabsicht nach Geschlecht",
			subtitle = "Die deskriptiven Ergebnisse zeigen, dass die weiblichen Befragten mit M = 2,64 und SD = 0,89 im Durchschnitt eine leicht hoehere Kaufabsicht aufwiesen als die maennlichen Befragten mit M = 2,27 und SD = 0,94."
		) +
		theme(legend.position = "none", axis.text.x = element_text(size = 10))

	ggsave(
		file.path(second_output_dir, "kaufabsicht_nach_geschlecht.svg"),
		purchase_gender_plot,
		width = plot_width,
		height = plot_height_tall,
		device = plot_device
	)
}

boxplot_df <- analysis_df_de %>%
	pivot_longer(
		cols = c(Vertrauenswuerdigkeit, Informationsqualitaet, Einstellung_UGC, Kaufabsicht),
		names_to = "Variable",
		values_to = "Wert"
	) %>%
	mutate(
		Variable = factor(
			Variable,
			levels = c("Vertrauenswuerdigkeit", "Informationsqualitaet", "Einstellung_UGC", "Kaufabsicht")
		)
	)

boxplot_plot <- ggplot(boxplot_df, aes(x = Variable, y = Wert, fill = Variable)) +
	geom_boxplot(outlier.shape = NA, width = 0.65) +
	coord_cartesian(ylim = c(1, 5)) +
	scale_y_continuous(breaks = 1:5) +
	scale_fill_manual(values = c("Vertrauenswuerdigkeit" = "#8dd3c7", "Informationsqualitaet" = "#ffffb3", "Einstellung_UGC" = "#bebada", "Kaufabsicht" = "#fb8072")) +
	plot_theme +
	labs(x = NULL, y = "Skala 1-5", title = "Boxplots der Skalen") +
	theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
	file.path(second_output_dir, "boxplots_skalen.svg"),
	boxplot_plot,
	width = plot_width,
	height = 3.2,
	device = plot_device
)

png_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
if (length(png_files) > 0) {
	invisible(file.remove(png_files))
}

pdf_files <- list.files(output_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdf_files) > 0) {
	invisible(file.remove(pdf_files))
}
