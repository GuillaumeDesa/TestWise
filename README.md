# TestWise

**TestWise** is a Shiny app that helps linguistics students run χ² and Fisher's exact tests of independence on contingency tables without writing a single line of R code.

It automatically selects the appropriate test based on the structure of the data, generates publication-quality visualisations, computes effect size, and exports results as a formatted HTML report or a high-resolution PNG. An in-app glossary explains every statistical concept along the way.

TestWise was developed by [Guillaume Desagulier](https://github.com/GuillaumeDesa) (Université Bordeaux Montaigne) for use in linguistics courses where students have limited statistical and programming backgrounds. The source code is fully commented and intended to be read alongside the app.

The previous version is available at [chisq-fisher-viz](https://github.com/GuillaumeDesa/chisq-fisher-viz).

## What the app does

TestWise takes a contingency table as input and:

1. **Selects the right test automatically.** It computes expected cell frequencies and applies the χ² test of independence if all are ≥ 5, or Fisher's exact test if any fall below 5. The reason for the choice is displayed in plain English.
2. **Visualises the result.** An association plot (Cohen-Friendly, via the `vcd` package) is shown for the χ² test; a mosaic plot for Fisher's exact test.
3. **Reports the p-value with an interpretation** calibrated to the chosen significance level α (configurable, default 0.05).
4. **Computes Cramér's V** (χ² test only) with a plain-English label (weak / moderate / strong), because a p-value alone does not tell you how strong an association is.
5. **Exports results** as a formatted HTML report and/or a 300 DPI PNG of the plot.

## How to run it

### Option 1 — RStudio (recommended)

1. Open `testwise.R` in RStudio.
2. Click the **Run App** button at the top right of the editor pane.
3. The app opens in a browser window or in RStudio's viewer pane.

### Option 2 — R console

```r
shiny::runApp("path/to/testwise.R")
```

Replace `"path/to/"` with the folder where you saved the file.

**First run only:** missing packages are installed automatically. This may take a minute or two.

## Input format

TestWise accepts contingency tables in the following formats:

| Format | Extension | Notes |
|--------|-----------|-------|
| Excel  | `.xlsx`, `.xls` | First sheet is used |
| CSV    | `.csv` | First column must be row labels |
| Tab-separated | `.txt` | First column must be row labels |

In all cases, the **first row** must contain column headers and the **first column** must contain row labels. All data cells must be non-negative integer counts.

## Dependencies

All packages are installed automatically on first run if not already present.

| Package | Purpose |
|---------|---------|
| `shiny` | App framework |
| `openxlsx` | Reading `.xlsx`/`.xls` files |
| `vcd` | Association and mosaic plots |
| `shinycssloaders` | Spinner while plots render |
| `shinyBS` | Tooltip help on sidebar options |
| `rmarkdown` | HTML report generation |
| `knitr` | Table rendering in reports |
| `kableExtra` | Styled HTML tables in reports |
| `ggplot2` | Loaded for potential extensions |

R version 4.1 or later is recommended.

## Features at a glance

- **Data preview tab** — inspect your uploaded table before running the analysis
- **Automatic test selection** — χ² or Fisher's exact test, chosen transparently
- **Configurable α** — set your own significance threshold (default: 0.05)
- **Monte Carlo simulation** — optional for Fisher's exact test on larger tables; activates automatically if the exact computation exceeds available workspace
- **Cramér's V** — effect size with interpretation, for χ² results
- **Colour-coded result boxes** — green for significant, amber for non-significant
- **Hover tooltips** — plain-language explanations of every option
- **About tab** — in-app glossary covering null hypothesis, p-value, effect size, residuals, and how to read each plot type
- **HTML report export** — styled, table-formatted, includes effect size
- **PNG plot export** — 300 DPI, user-controlled dimensions

## A note on the mosaic plot

The mosaic plot displayed when Fisher's exact test is used is a general-purpose visualisation tool, not a statistical companion specifically designed for Fisher's test. The association plot, by contrast, was designed to display Pearson residuals from the χ² test and has a tighter statistical justification. The mosaic plot is used here because it gives a clear visual impression of cell frequencies and is immediately readable. A heatmap or a bar chart of proportions would be equally valid alternatives. The key results to focus on are always the **p-value** and the **frequency tables**.

## Licence

This work © 2025 Guillaume Desagulier is licensed under [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/).

You are free to use, share, and adapt this app for non-commercial purposes, provided you give appropriate credit.

---

## Citation

If you use TestWise in your teaching or research, please cite it as:

> Desagulier, G. (*2026*). *TestWise: a Shiny app for χ² and Fisher's exact tests of independence*. Université Bordeaux Montaigne. https://github.com/GuillaumeDesa/TestWise

## Related resources

- [Original blog post](https://corpling.hypotheses.org/5113) introducing the first version of the app
- [First version of the app](https://github.com/GuillaumeDesa/chisq-fisher-viz) (superseded)
- Desagulier, G. (2017). *Corpus Linguistics and Statistics with R*. Springer. — Chapter 8 covers hypothesis testing in detail.
