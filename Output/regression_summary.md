# Regression Summary (Kaufabsicht)

## Modell
- Abhaengige Variable: Kaufabsicht
- Modell: Kaufabsicht ~ Vertrauenswuerdigkeit + Informationsqualitaet + Einstellung_UGC

## Modellfit
- R2 = 0.318
- Adjustiertes R2 = 0.302
- F(3, 129) = 20.06, p = 0.0000
- Hinweis: p = 0.0000 bedeutet p < 0.0001 (gerundet).

## Koeffizienten (nicht standardisiert)

| Praediktor | Schaetzung | Std_Fehler | t_Wert | p_Wert |
| --- | --- | --- | --- | --- |
| Konstante | -0.4712222 | 0.43631212 | -1.080012 | 2.821521e-01 |
| Vertrauenswuerdigkeit | 0.1540825 | 0.10563906 | 1.458575 | 1.471127e-01 |
| Informationsqualitaet | 0.3012799 | 0.11109450 | 2.711924 | 7.601696e-03 |
| Einstellung_UGC | 0.3694932 | 0.08159195 | 4.528550 | 1.335284e-05 |

## Koeffizienten (standardisiert)

| Praediktor | Schaetzung | Std_Fehler | t_Wert | p_Wert |
| --- | --- | --- | --- | --- |
| Konstante | -5.447296e-16 | 0.07243007 | -7.520767e-15 | 1.000000e+00 |
| Vertrauenswuerdigkeit | 1.266386e-01 | 0.08682354 | 1.458575e+00 | 1.471127e-01 |
| Informationsqualitaet | 2.298219e-01 | 0.08474494 | 2.711924e+00 | 7.601696e-03 |
| Einstellung_UGC | 3.620349e-01 | 0.07994500 | 4.528550e+00 | 1.335284e-05 |

## Interpretation (Kurz)
- Positive Koeffizienten bedeuten, dass hoehere Werte der Skala mit hoeherer Kaufabsicht einhergehen.
- Signifikant sind Informationsqualitaet und Einstellung_UGC.
- Vertrauenswuerdigkeit ist im Modell nicht signifikant.

## Zusatzanalysen (Tasks 1-13)

### 1-3 Korrelationen Alter

| Analyse | n | r | p_Wert |
| --- | --- | --- | --- |
| Alter x Einstellung_UGC | 134 | 0.0596 | 0.4941 |
| Alter x Kaufabsicht | 133 | 0.0763 | 0.3827 |
| Alter x Instagram_Nutzung | 134 | -0.1950 | 0.0243 |

### 4-5 Korrelationen Geschlecht

| Analyse | n | r | p_Wert |
| --- | --- | --- | --- |
| Geschlecht x Kaufabsicht | 133 | -0.1770 | 0.0418 |
| Geschlecht x Instagram_Nutzung | 134 | -0.1100 | 0.2078 |

### 6 Korrelationsmatrix (erweitert) - r

|  | Vertrauenswuerdigkeit | Informationsqualitaet | Einstellung\_UGC | Kaufabsicht | Alter | Instagram\_Nutzung |
| --- | --- | --- | --- | --- | --- | --- |
| Vertrauenswuerdigkeit | 1.000 | 0.496 | 0.367 | 0.379 | -0.035 | 0.046 |
| Informationsqualitaet | 0.496 | 1.000 | 0.314 | 0.410 | 0.067 | -0.038 |
| Einstellung\_UGC | 0.367 | 0.314 | 1.000 | 0.486 | 0.060 | -0.011 |
| Kaufabsicht | 0.379 | 0.410 | 0.486 | 1.000 | 0.076 | -0.060 |
| Alter | -0.035 | 0.067 | 0.060 | 0.076 | 1.000 | -0.195 |
| Instagram\_Nutzung | 0.046 | -0.038 | -0.011 | -0.060 | -0.195 | 1.000 |

### 6 Korrelationsmatrix (erweitert) - p-Werte

|  | Vertrauenswuerdigkeit | Informationsqualitaet | Einstellung\_UGC | Kaufabsicht | Alter | Instagram\_Nutzung |
| --- | --- | --- | --- | --- | --- | --- |
| Vertrauenswuerdigkeit | 0.0000 | 0.0000 | 0.0001 | 0.0001 | 0.6906 | 0.5923 |
| Informationsqualitaet | 0.0000 | 0.0000 | 0.0023 | 0.0000 | 0.4425 | 0.6656 |
| Einstellung\_UGC | 0.0001 | 0.0023 | 0.0000 | 0.0000 | 0.4941 | 0.9016 |
| Kaufabsicht | 0.0001 | 0.0000 | 0.0000 | 0.0000 | 0.3827 | 0.4897 |
| Alter | 0.6906 | 0.4425 | 0.4941 | 0.3827 | 0.0000 | 0.0243 |
| Instagram\_Nutzung | 0.5923 | 0.6656 | 0.9016 | 0.4897 | 0.0243 | 0.0000 |

## 7-8 Multikollinearitaet (Toleranzwerte)

| Praediktor | R2 | Toleranz | Problematisch |
| --- | --- | --- | --- |
| Vertrauenswuerdigkeit | 0.299 | 0.701 | FALSE |
| Informationsqualitaet | 0.264 | 0.736 | FALSE |
| Einstellung_UGC | 0.173 | 0.827 | FALSE |

## 9-13 Diagnostik
- Normalverteilung (Shapiro-Wilk): W = 0.960, p = 0.0006
- Homoskedastizitaet (Breusch-Pagan): BP = 7.34, df = 3, p = 0.0619
- Linearitaet: residuals_vs_fitted.svg
- Unabhaengigkeit (Durbin-Watson): DW = 1.706, p = 0.0430
- Mahalanobis-Ausreisser: Cutoff = 16.266, Ausreisser = 0

## Kurzfazit
- Modell erklaert 31.8% der Varianz (R2 = 0.318, adj. R2 = 0.302).
- Signifikante Praediktoren: Informationsqualitaet, Einstellung_UGC.
- Nicht signifikant: Vertrauenswuerdigkeit.
- Alter x Instagram_Nutzung zeigt einen negativen Zusammenhang.

## Sachzusammenhang (Einordnung)
- Kaufabsicht haengt am staerksten mit Einstellung_UGC (r = 0.486) und Informationsqualitaet (r = 0.410) zusammen.
- Alter zeigt keinen signifikanten Zusammenhang mit Einstellung_UGC oder Kaufabsicht.
- Instagram-Nutzung ist negativ mit Alter korreliert (juengere nutzen Instagram staerker).
- Multikollinearitaet und Ausreisser sind unproblematisch; Residuen zeigen Hinweise auf Abweichung von Normalverteilung und moegliche Autokorrelation.

## Dokumentation Tasks 1-13
1. Alter x Einstellung: Zusammenhang Alter und Einstellung_UGC.
2. Alter x Kaufabsicht: Zusammenhang Alter und Kaufabsicht.
3. Alter x Instagram: Zusammenhang Alter und Instagram-Nutzung.
4. Geschlecht x Kaufabsicht: Zusammenhang Geschlecht und Kaufabsicht.
5. Geschlecht x Instagram: Zusammenhang Geschlecht und Instagram-Nutzung.
6. Korrelationsmatrix: Uebersicht aller paarweisen Zusammenhaenge.
7. Multikollinearitaet: Regression jedes Praediktors auf die anderen.
8. Toleranz (1 - R2): Indikator fuer problematische Ueberschneidungen.
9. Shapiro-Wilk: Normalverteilung der Residuen.
10. Homoskedastizitaet: konstante Residuenstreuung.
11. Linearitaet: Residuen vs. Fitted Plot.
12. Durbin-Watson: Unabhaengigkeit der Residuen.
13. Mahalanobis: multivariate Ausreisser.

## Visualisierungen (Dateien)
- correlation_heatmap.svg
- correlation_pvalues_heatmap.svg
- correlation_extended_heatmap.svg (falls vorhanden)
- correlation_extended_pvalues_heatmap.svg (falls vorhanden)
- descriptives_means_sd.svg
- reliability_alpha.svg
- regression_coefficients_standardized.svg
- age_correlations.svg
- gender_correlations.svg
- multicollinearity_tolerance.svg
- residuals_vs_fitted.svg
- mahalanobis_distances.svg
- sample_sizes.svg
- scale_means_by_gender.svg (falls vorhanden)
