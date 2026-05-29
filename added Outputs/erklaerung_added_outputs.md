# Added Analysen (added Outputs)

Diese Datei erklaert die zusaetzlichen Auswertungen im Ordner 'added Outputs'.

## 1) Geschlecht Gruppenvergleich
Zweck: Testet, ob sich Mittelwerte der Skalen zwischen Geschlechtsgruppen unterscheiden.
Ergebnis Kaufabsicht: t_test, p = 0.0507, Effekt = 0.409
Dateien: gender_group_descriptives.csv, gender_group_tests.csv, gender_group_boxplots.svg

## 2) Robuste Korrelationen (Spearman)
Zweck: Pruft Zusammenhaenge ohne Normalverteilungsannahme.
Kaufabsicht x Einstellung_UGC: rho = 0.479, p = 0.0000; Kaufabsicht x Informationsqualitaet: rho = 0.394, p = 0.0000; Alter x Instagram_Nutzung: rho = -0.098, p = 0.2624
Dateien: spearman_correlations.csv, spearman_p_values.csv, spearman_correlation_heatmap.svg, spearman_pvalues_heatmap.svg

## 3) Robuste Regression (HC3 Standardfehler)
Zweck: Prueft, ob die Regressionsergebnisse bei robusten Standardfehlern stabil bleiben.
p-Werte (robust): Einstellung_UGC = 0.0001, Informationsqualitaet = 0.0017, Vertrauenswuerdigkeit = 0.1737
Datei: regression_robust_se.csv

## 4) Mediation (Bootstrap)
Zweck: Testet indirekte Effekte ueber Einstellung_UGC.
Indirekt Informationsqualitaet -> Einstellung_UGC -> Kaufabsicht: 0.168 (CI 0.078, 0.284). Indirekt Vertrauenswuerdigkeit: 0.187 (CI 0.096, 0.294).
Datei: mediation_results.csv

## 5) Moderation (Interaktionen)
Zweck: Prueft, ob der Zusammenhang Einstellung_UGC -> Kaufabsicht je nach Alter/Instagram unterschiedlich ist.
Interaktion UGC x Alter: p = 0.2881; Interaktion UGC x Instagram: p = 0.4749
Datei: moderation_results.csv

## 6) Einflussdiagnostik
Zweck: Identifiziert einflussreiche Faelle in der Regression.
Influential Count (Cook's D > 4/n): 7
Dateien: influence_diagnostics.csv, influence_summary.csv, cooks_distance.svg

## 7) Missing Data + Imputation
Zweck: Beschreibt fehlende Werte und zeigt eine Imputations-Sensitivitaet.
Fehlende Kaufabsicht: 19.9% (33 Faelle).
Imputation (Mittelwert) p-Werte: Einstellung_UGC = 0.0000, Informationsqualitaet = 0.0026, Vertrauenswuerdigkeit = 0.0965
Dateien: missingness_summary.csv, missingness_tests.csv (falls vorhanden), regression_imputed_mean.csv

## Hinweise zur Interpretation
- Signifikante p-Werte (< 0.05) deuten auf statistische Unterschiede/Zusammenhaenge hin.
- Effekte sollten immer zusammen mit Effektstaerken und inhaltlichem Kontext bewertet werden.
- Interaktionen sind nur bedeutsam, wenn der Interaktionsterm signifikant ist.
- Mittelwert-Imputation ist nur eine Sensitivitaetsanalyse und ersetzt keine saubere Missing-Data-Diagnose.
