# =============================================================================
# TestWise — χ² / Fisher's Exact Test of Independence
# Author: Guillaume Desagulier, Université Bordeaux Montaigne
# GitHub: https://github.com/GuillaumeDesa/TestWise
# License: CC BY-NC 4.0
# =============================================================================

# --- Required libraries ------------------------------------------------------
packages <- c("shiny", "openxlsx", "ggplot2", "shinycssloaders",
              "vcd", "rmarkdown", "shinyBS", "knitr", "kableExtra")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Dynamic copyright year
current_year <- format(Sys.Date(), "%Y")

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(

  # --- Global CSS ------------------------------------------------------------
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,300;0,400;0,500;0,700;1,400&family=Roboto+Slab:wght@400;600;700&display=swap"),
    tags$style(HTML("

      /* ── Typography & colour variables ── */
      body { font-family: 'Roboto', sans-serif; color: #2c2c2c; background-color: #fafafa; }
      h2, h3, h4, h5 { font-family: 'Roboto Slab', serif; }
      code, pre { font-family: 'Roboto Mono', monospace; }
      .navbar { background-color: #2c3e50 !important; }

      /* ── Title bar ── */
      .title-bar {
        background-color: #2c3e50;
        color: white;
        padding: 18px 24px 14px 24px;
        margin-bottom: 20px;
        border-radius: 4px;
      }
      .title-bar h2 { margin: 0; font-size: 1.5em; font-weight: bold; }
      .title-bar p  { margin: 4px 0 0 0; font-size: 0.9em; color: #ccd6e0; }

      /* ── Sidebar ── */
      .well { background-color: #f0f4f8; border: 1px solid #d0dbe8; border-radius: 6px; }

      /* ── Step labels ── */
      .step-label {
        font-weight: bold;
        font-size: 0.85em;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #2c3e50;
        margin-top: 14px;
        margin-bottom: 4px;
        border-left: 3px solid #2c3e50;
        padding-left: 7px;
      }

      /* ── Result boxes ── */
      .result-box {
        padding: 12px 16px;
        border-radius: 6px;
        margin-top: 14px;
        font-size: 0.95em;
        line-height: 1.6;
      }
      .result-significant   { background-color: #eafaf1; border-left: 5px solid #27ae60; }
      .result-nonsignificant{ background-color: #fef9e7; border-left: 5px solid #f39c12; }
      .result-info          { background-color: #eaf4fb; border-left: 5px solid #2980b9; }

      /* ── Effect size box ── */
      .effect-box {
        padding: 10px 14px;
        background-color: #f4f6f7;
        border: 1px solid #bdc3c7;
        border-radius: 6px;
        margin-top: 10px;
        font-size: 0.9em;
      }

      /* ── Tables ── */
      .table-section { margin-top: 20px; }
      table.shiny-table { font-size: 0.88em; width: 100%; }
      table.shiny-table th { background-color: #2c3e50; color: white; padding: 6px 10px; }
      table.shiny-table td { padding: 5px 10px; }
      table.shiny-table tr:nth-child(even) { background-color: #f0f4f8; }

      /* ── Interpretation bullets ── */
      .interp-box {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 14px 18px;
        margin-top: 16px;
        font-size: 0.9em;
        line-height: 1.7;
      }
      .interp-box ul { margin-bottom: 0; }

      /* ── Download section ── */
      .download-section {
        margin-top: 14px;
        padding: 12px;
        background-color: #eaf4fb;
        border-radius: 6px;
      }

      /* ── About tab ── */
      .about-section { max-width: 800px; line-height: 1.8; font-size: 0.95em; }
      .about-section h4 { color: #2c3e50; margin-top: 24px; }
      .about-section .concept-box {
        background-color: #f0f4f8;
        border-radius: 6px;
        padding: 12px 16px;
        margin: 10px 0;
        border-left: 4px solid #2980b9;
      }

      /* ── Data preview ── */
      .preview-box {
        background-color: #fff;
        border: 1px solid #d0dbe8;
        border-radius: 6px;
        padding: 10px;
        overflow-x: auto;
        font-size: 0.85em;
      }

      /* ── Footer ── */
      .app-footer {
        font-size: 0.8em;
        color: #7f8c8d;
        margin-top: 20px;
        padding-top: 10px;
        border-top: 1px solid #dee2e6;
        overflow: hidden;
        position: relative;
      }
      .app-footer a { color: #2980b9; }

      /* ── Tab content padding ── */
      .tab-content { padding-top: 16px; }

    "))
  ),

  # --- Title bar -------------------------------------------------------------
  div(class = "title-bar",
    tags$h2(HTML("TestWise")),
    tags$p(HTML("&chi;&sup2; and Fisher's Exact Test of Independence &mdash; a guided tool for corpus linguistics students &mdash; Université Bordeaux Montaigne"))
  ),

  # --- Main layout -----------------------------------------------------------
  sidebarLayout(

    # ── Sidebar ──────────────────────────────────────────────────────────────
    sidebarPanel(width = 3,

      # Step 1 — Upload
      div(class = "step-label", "Step 1 — Upload your table"),
      fileInput("file", label = NULL,
                accept  = c(".xlsx", ".xls", ".csv", ".txt"),
                placeholder = "No file selected"),
      helpText(HTML(
        "Accepted formats: <code>.xlsx</code>, <code>.xls</code>, <code>.csv</code>, <code>.txt</code>.<br>
         The <strong>first row</strong> must contain column headers.<br>
         The <strong>first column</strong> must contain row labels."
      )),

      # Step 2 — Options
      div(class = "step-label", "Step 2 — Options"),
      checkboxInput("monte_carlo",
        HTML("Use Monte Carlo simulation <small>(recommended for tables larger than 2×2 when Fisher's test is selected)</small>"),
        value = FALSE),
      bsTooltip("monte_carlo",
        "When a table is larger than 2×2 and expected frequencies are low, Fisher's exact test can be very slow or fail entirely. Monte Carlo simulation approximates the p-value quickly and reliably using 100,000 random samples.",
        placement = "right", trigger = "hover"),

      numericInput("alpha", HTML("Significance level (&alpha;):"),
                   value = 0.05, min = 0.001, max = 0.10, step = 0.005),
      bsTooltip("alpha",
        "The conventional threshold is α = 0.05. This means you accept a 5% risk of incorrectly rejecting the null hypothesis. Some fields use α = 0.01 for more stringent tests.",
        placement = "right", trigger = "hover"),

      # Step 3 — Run
      div(class = "step-label", "Step 3 — Run the analysis"),
      actionButton("submit", "Run analysis", icon = icon("play"),
                   style = "background-color:#2c3e50; color:white; width:100%; margin-top:6px;"),

      # Results (appear after analysis)
      uiOutput("sidebar_results"),

      # Downloads (appear after analysis)
      uiOutput("download_section"),

      # Footer
      div(class = "app-footer",
        p(HTML(paste0("© ", current_year, " Guillaume Desagulier — ",
          "<a href='https://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>CC BY-NC 4.0</a>"
        ))),
        p(HTML("<a href='https://github.com/GuillaumeDesa/chisq-fisher-viz' target='_blank'>GitHub repository</a>"))
      )
    ), # end sidebarPanel

    # ── Main panel ───────────────────────────────────────────────────────────
    mainPanel(width = 9,

      tabsetPanel(id = "main_tabs",

        # ── Tab 1: Data preview ─────────────────────────────────────────────
        tabPanel("Data preview",
          uiOutput("data_preview_ui")
        ),

        # ── Tab 2: Results ──────────────────────────────────────────────────
        tabPanel("Results",
          uiOutput("results_ui")
        ),

        # ── Tab 3: About ────────────────────────────────────────────────────
        tabPanel("About this app",
          div(class = "about-section",

            h4("What does this app do?"),
            p("This app helps you determine whether two categorical variables are
               statistically independent — that is, whether knowing the value of one
               variable tells you anything useful about the other. It does so by
               automatically selecting between two classical tests:
               the χ² (chi-squared) test and Fisher's exact test."),

            h4("Which test should be used, and why?"),
            div(class = "concept-box",
              tags$b("χ² test of independence"),
              tags$ul(
                tags$li("Used when all expected cell frequencies are ≥ 5."),
                tags$li("Based on an approximation of the χ² distribution."),
                tags$li("Works well for large, well-populated contingency tables."),
                tags$li("Produces a χ² statistic and a p-value.")
              )
            ),
            div(class = "concept-box",
              tags$b("Fisher's exact test"),
              tags$ul(
                tags$li("Used when one or more expected cell frequencies fall below 5."),
                tags$li("Calculates the exact probability rather than relying on an approximation."),
                tags$li("More reliable for small samples or sparse tables."),
                tags$li("For tables larger than 2×2, Monte Carlo simulation may be needed.")
              )
            ),

            h4("Key statistical concepts"),
            div(class = "concept-box",
              tags$b("The null hypothesis (H₀)"),
              p("The null hypothesis states that the two variables are independent —
                 that is, knowing the category of one variable gives you no information
                 about the other. The test asks: how likely is it to observe data at
                 least as extreme as ours if H₀ were true?")
            ),
            div(class = "concept-box",
              tags$b("The p-value"),
              p("The p-value is the probability of obtaining results at least as extreme
                 as those observed, under the assumption that the null hypothesis is true.
                 A small p-value (typically below 0.05) suggests that the data are
                 unlikely under H₀, which leads us to reject it.
                 A large p-value means we do not have enough evidence to reject H₀."),
              p(tags$em("Important: the p-value does not measure the size or importance
                         of an effect — only its statistical detectability given the
                         sample size. This is why we also report effect size (Cramér's V)."))
            ),
            div(class = "concept-box",
              tags$b("Effect size: Cramér's V (χ² test only)"),
              p("Cramér's V ranges from 0 (no association) to 1 (perfect association).
                 It answers a different question than the p-value: not 'is there an
                 association?' but 'how strong is that association?'"),
              tags$ul(
                tags$li("V ≈ 0.10 → weak association"),
                tags$li("V ≈ 0.30 → moderate association"),
                tags$li("V ≈ 0.50 or above → strong association")
              ),
              p(tags$em("Note: these thresholds are conventional, not universal.
                          Always interpret effect size in the context of your field."))
            ),
            div(class = "concept-box",
              tags$b("Pearson residuals (χ² test only)"),
              p("A residual measures the discrepancy between an observed and an expected
                 frequency in a given cell, normalised by the expected frequency.
                 Large positive residuals (roughly > 2) indicate that a combination
                 occurs more often than expected under independence.
                 Large negative residuals (roughly < −2) indicate that a combination
                 is rarer than expected.
                 Residuals help you identify which specific cells are driving
                 the overall test result.")
            ),

            h4("How to read the association plot"),
            p("The association plot (Cohen-Friendly, produced by the ",
              tags$code("vcd"), " package) displays Pearson residuals visually.
               Each rectangle represents a cell in the contingency table.
               Rectangles that extend above the baseline have positive residuals
               (more observations than expected); those below the baseline have
               negative residuals (fewer than expected).
               The shading indicates statistical significance:
               darker colours signal stronger deviations from independence."),

            h4("How to read the mosaic plot"),
            p("The mosaic plot is displayed when Fisher's exact test is used.
               Each tile's area is proportional to the observed frequency in
               that cell. Shading works the same way as in the association plot:
               it highlights cells where the deviation from expected frequency
               is statistically meaningful."),
            div(class = "concept-box", style = "border-left-color: #e67e22;",
              tags$b("Important caveat about this visualisation"),
              p("The mosaic plot is not the canonical or natural companion to Fisher's exact test —
                 it is a general-purpose tool for visualising contingency tables that happens to work
                 with either test. The association plot, by contrast, is specifically designed to
                 display Pearson residuals from the χ² test, so it has a tighter statistical
                 justification. The mosaic plot is used here for pragmatic reasons: it gives a
                 clear visual impression of the cell frequencies. A heatmap or a bar chart of
                 proportions would be equally valid. Do not over-interpret the choice of plot type —
                 the p-value and frequency tables are the results that matter.")
            ),

            h4("About this app"),
            p(tags$b("TestWise"), " was developed by Guillaume Desagulier (Université Bordeaux Montaigne)
               to give linguistics students access to principled, automatic statistical
               testing without requiring prior R knowledge. The source code is fully
               commented and available on ",
              tags$a("GitHub", href = "https://github.com/GuillaumeDesa/TestWise",
                     target = "_blank"), " under a CC BY-NC 4.0 licence."),
            p("If you are curious about the statistics behind these tests, a good
               starting point is Chapter 8 of ",
              tags$em("Corpus Linguistics and Statistics with R"),
              " (Desagulier, 2017, Springer).")
          )
        ) # end About tab

      ) # end tabsetPanel
    ) # end mainPanel
  ) # end sidebarLayout
) # end fluidPage


# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Reactive store for all computed results
  results <- reactiveVal(NULL)

  # Show the Results tab automatically after running the analysis
  observeEvent(results(), {
    req(results())
    updateTabsetPanel(session, "main_tabs", selected = "Results")
  })

  # Switch to Data preview tab when a file is uploaded
  observeEvent(input$file, {
    updateTabsetPanel(session, "main_tabs", selected = "Preview")
  })

  # ── Data preview ────────────────────────────────────────────────────────────
  output$data_preview_ui <- renderUI({
    if (is.null(input$file)) {
      return(div(style = "color:#7f8c8d; font-style:italic; margin-top:20px;",
        icon("arrow-left"), " Upload a file to preview it here."))
    }

    tryCatch({
      ext  <- tools::file_ext(input$file$name)
      data <- load_data(input$file$datapath, ext)

      tagList(
        h4(paste("Preview:", input$file$name)),
        p(paste(nrow(data), "rows ×", ncol(data), "columns detected.")),
        div(class = "preview-box",
          renderTable(head(data, 10), rownames = TRUE)
        ),
        if (nrow(data) > 10)
          p(em(paste("(Showing first 10 rows of", nrow(data), ")")))
        ,
        br(),
        p("If the table looks correct, go to ",
          tags$b("Step 2"), " and click ", tags$b("Run analysis"), ".")
      )
    }, error = function(e) {
      div(class = "result-box result-nonsignificant",
        icon("triangle-exclamation"),
        paste(" Could not read the file:", e$message))
    })
  })

  # ── Helper: load data ───────────────────────────────────────────────────────
  load_data <- function(path, ext) {
    if (ext %in% c("xlsx", "xls")) {
      data <- read.xlsx(path, sheet = 1, rowNames = TRUE)
    } else if (ext == "csv") {
      data <- read.csv(path, row.names = 1, check.names = FALSE)
    } else if (ext == "txt") {
      data <- read.table(path, sep = "\t", header = TRUE, row.names = 1,
                         check.names = FALSE)
    } else {
      stop("Unsupported file format. Please upload .xlsx, .xls, .csv, or .txt.")
    }

    # Validate: numeric values only
    if (!all(sapply(data, is.numeric))) {
      stop("All cells must contain numeric values (counts). Please check your file.")
    }
    data
  }

  # ── Main analysis ────────────────────────────────────────────────────────────
  observeEvent(input$submit, {
    req(input$file)

    tryCatch({
      ext  <- tools::file_ext(input$file$name)
      data <- load_data(input$file$datapath, ext)

      freq_table <- as.matrix(data)
      storage.mode(freq_table) <- "double"

      # Validate: no empty rows/columns
      if (any(rowSums(freq_table) == 0) || any(colSums(freq_table) == 0)) {
        stop("Your table contains at least one row or column with a total of 0.
              Please remove empty rows/columns and try again.")
      }

      # Validate: no negative values
      if (any(freq_table < 0)) {
        stop("Your table contains negative values. Frequency counts must be zero or positive.")
      }

      alpha       <- input$alpha
      num_rows    <- nrow(freq_table)
      num_cols    <- ncol(freq_table)
      is_2x2     <- (num_rows == 2 && num_cols == 2)
      use_mc      <- input$monte_carlo && !is_2x2

      # χ² is always computed for expected frequencies
      chi_result  <- chisq.test(freq_table, correct = FALSE)
      observed    <- chi_result$observed
      expected    <- chi_result$expected
      residuals   <- chi_result$residuals

      # ── Dynamic plot dimensions ──────────────────────────────────────────
      base_h          <- 350
      plot_height_px  <- max(base_h, base_h + 55 * (num_rows - 1) + 35 * (num_cols - 1))
      plot_width_px   <- max(500,    500  + 60 * (num_cols - 1))
      plot_height_in  <- plot_height_px / 96
      plot_width_in   <- plot_width_px  / 96

      current <- list(
        freq_table     = freq_table,
        observed       = observed,
        expected       = expected,
        residuals      = residuals,
        alpha          = alpha,
        num_rows       = num_rows,
        num_cols       = num_cols,
        plot_height_px = plot_height_px,
        plot_width_px  = plot_width_px,
        plot_height_in = plot_height_in,
        plot_width_in  = plot_width_in,
        test_type      = "",
        p_value        = NA,
        test_label     = "",
        test_message   = "",
        p_interpretation = "",
        effect_size_html = "",
        plot_interpretation = "",
        chi_statistic  = NA,
        cramers_v      = NA
      )

      # ── Decide which test to use ─────────────────────────────────────────
      if (any(expected < 5)) {

        # Fisher's exact test
        fisher_result <- tryCatch({
          fisher.test(freq_table, simulate.p.value = use_mc, B = 100000)
        }, error = function(e) {
          if (grepl("workspace", e$message, ignore.case = TRUE)) {
            # Automatically fall back to Monte Carlo if workspace exceeded
            fisher.test(freq_table, simulate.p.value = TRUE, B = 100000)
          } else stop(e)
        })

        p_value   <- fisher_result$p.value
        mc_note   <- if (use_mc) " (Monte Carlo simulation, B = 100,000)" else ""
        test_label   <- paste0("Fisher's Exact Test", mc_note)
        test_message <- paste0(
          "One or more expected cell frequencies are below 5 ",
          "(minimum: ", round(min(expected), 2), "). ",
          "Fisher's Exact Test was therefore used instead of the χ² test."
        )

        current$test_type    <- "Fisher"
        current$test_label   <- test_label
        current$test_message <- test_message
        current$p_value      <- p_value

        # p-value interpretation
        if (p_value < alpha) {
          current$p_interpretation <- paste0(
            "<strong>p-value (", test_label, "):</strong> ", round(p_value, 4),
            " — below α = ", alpha, "<br><br>",
            "We <strong>reject the null hypothesis of independence</strong>. ",
            "There is a statistically significant association between the two variables. ",
            "In other words, the distribution of one variable is not the same across ",
            "all levels of the other variable."
          )
          current$result_class <- "result-significant"
        } else {
          current$p_interpretation <- paste0(
            "<strong>p-value (", test_label, "):</strong> ", round(p_value, 4),
            " — above α = ", alpha, "<br><br>",
            "We <strong>fail to reject the null hypothesis of independence</strong>. ",
            "There is no statistically significant association between the two variables ",
            "at the chosen significance level (α = ", alpha, "). ",
            "This does not necessarily mean the variables are unrelated — ",
            "a larger sample might reveal an effect."
          )
          current$result_class <- "result-nonsignificant"
        }

        # No Cramér's V for Fisher
        current$effect_size_html <- paste0(
          "<em>Effect size (Cramér's V) is not computed for Fisher's Exact Test. ",
          "To quantify the strength of association, consider computing the odds ratio ",
          "manually for 2×2 tables.</em>"
        )

        current$plot_interpretation <- paste0(
          "<strong>How to read this mosaic plot:</strong>",
          "<ul>",
          "<li>Each tile's <strong>area</strong> is proportional to the observed count in that cell.</li>",
          "<li><strong>Blue shading</strong> means the observed count is higher than expected under independence — ",
          "this combination occurs more often than chance would predict.</li>",
          "<li><strong>Red shading</strong> means the observed count is lower than expected — ",
          "this combination is rarer than chance would predict.</li>",
          "<li>Unshaded tiles fall within the expected range — no meaningful deviation.</li>",
          "</ul>",
          "<div class='result-box result-info' style='margin-top:10px; font-size:0.88em;'>",
          "<strong>&#9432; A note on this visualisation:</strong> ",
          "The mosaic plot is shown here as a convenient way to read your contingency table — ",
          "not because it is the natural or default companion to Fisher's exact test. ",
          "Unlike the association plot, which is specifically designed to display Pearson residuals ",
          "from the χ² test, the mosaic plot is a general-purpose tool that works with any contingency table. ",
          "A heatmap or a simple bar chart of proportions would be equally valid alternatives. ",
          "The key results to focus on are the <strong>p-value</strong> and the <strong>frequency tables</strong> below.",
          "</div>"
        )

      } else {

        # χ² test
        p_value      <- chi_result$p.value
        chi_stat     <- chi_result$statistic
        cramers_v    <- sqrt(chi_stat / (sum(freq_table) * (min(num_rows, num_cols) - 1)))

        test_label   <- "χ² Test of Independence"
        test_message <- paste0(
          "All expected cell frequencies are ≥ 5 ",
          "(minimum: ", round(min(expected), 2), "). ",
          "The χ² test of independence was therefore used."
        )

        current$test_type     <- "ChiSq"
        current$test_label    <- test_label
        current$test_message  <- test_message
        current$p_value       <- p_value
        current$chi_statistic <- chi_stat
        current$cramers_v     <- cramers_v

        if (p_value < alpha) {
          current$p_interpretation <- paste0(
            "<strong>p-value (χ² test):</strong> ", round(p_value, 4),
            " — below α = ", alpha, "<br>",
            "<strong>χ² statistic:</strong> ", round(chi_stat, 3), "<br><br>",
            "We <strong>reject the null hypothesis of independence</strong>. ",
            "There is a statistically significant association between the two variables. ",
            "In other words, the distribution of one variable is not the same across ",
            "all levels of the other variable."
          )
          current$result_class <- "result-significant"
        } else {
          current$p_interpretation <- paste0(
            "<strong>p-value (χ² test):</strong> ", round(p_value, 4),
            " — above α = ", alpha, "<br>",
            "<strong>χ² statistic:</strong> ", round(chi_stat, 3), "<br><br>",
            "We <strong>fail to reject the null hypothesis of independence</strong>. ",
            "There is no statistically significant association between the two variables ",
            "at the chosen significance level (α = ", alpha, "). ",
            "This does not necessarily mean the variables are unrelated — ",
            "a larger sample might reveal an effect."
          )
          current$result_class <- "result-nonsignificant"
        }

        # Effect size label
        v_label <- if (cramers_v >= 0.5) "strong"
                   else if (cramers_v >= 0.3) "moderate"
                   else "weak"

        current$effect_size_html <- paste0(
          "<strong>Effect size — Cramér's V:</strong> ", round(cramers_v, 3),
          " (", v_label, " association)<br>",
          "<em>Interpretation: V ranges from 0 (no association) to 1 (perfect association). ",
          "A statistically significant result with a weak effect size may have limited ",
          "practical relevance. Always interpret effect size alongside the p-value.</em>"
        )

        current$plot_interpretation <- paste0(
          "<strong>How to read this association plot:</strong>",
          "<ul>",
          "<li>Each rectangle represents a cell in your contingency table.</li>",
          "<li>Rectangles <strong>above</strong> the baseline have <strong>positive residuals</strong>: ",
          "the observed count is higher than expected under independence.</li>",
          "<li>Rectangles <strong>below</strong> the baseline have <strong>negative residuals</strong>: ",
          "the observed count is lower than expected.</li>",
          "<li>Residuals greater than roughly ±2 (shaded cells) indicate ",
          "statistically meaningful deviations from independence.</li>",
          "<li>The width of each rectangle reflects the expected frequency for that cell.</li>",
          "</ul>"
        )
      }

      results(current)

    }, error = function(e) {
      showModal(modalDialog(
        title = "Something went wrong",
        div(
          p("The analysis could not be completed. Here is the error message:"),
          div(style = "font-family: monospace; background: #f8f9fa; padding: 10px;",
              e$message),
          br(),
          p("Common causes:"),
          tags$ul(
            tags$li("The file has non-numeric values in the data cells."),
            tags$li("The first row is not a header row, or the first column is not a label column."),
            tags$li("The table has empty rows or columns."),
            tags$li("For large, sparse tables: try enabling Monte Carlo simulation.")
          )
        ),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
      results(NULL)
    })
  })

  # ── Sidebar results (appear after analysis) ──────────────────────────────────
  output$sidebar_results <- renderUI({
    req(results())
    r <- results()
    tagList(
      hr(),
      div(class = paste("result-box", r$result_class),
        HTML(r$p_interpretation)
      ),
      div(class = "effect-box",
        HTML(r$effect_size_html)
      ),
      div(class = "result-box result-info", style = "margin-top:10px;",
        icon("circle-info"), " ",
        HTML(r$test_message)
      )
    )
  })

  # ── Download section (appears after analysis) ────────────────────────────────
  output$download_section <- renderUI({
    req(results())
    div(class = "download-section",
      div(class = "step-label", "Download results"),
      downloadButton("downloadReport",  "Full report (.html)", style = "width:100%; margin-bottom:6px;"),
      downloadButton("downloadPlotPNG", "Plot (.png)", style = "width:100%; margin-bottom:6px;"),
      br(),
      uiOutput("png_width_input"),
      uiOutput("png_height_input")
    )
  })

  output$png_width_input <- renderUI({
    req(results())
    numericInput("png_width", "Plot width (inches):",
                 value = round(results()$plot_width_in, 1), min = 2, max = 30, step = 0.5)
  })
  output$png_height_input <- renderUI({
    req(results())
    numericInput("png_height", "Plot height (inches):",
                 value = round(results()$plot_height_in, 1), min = 2, max = 30, step = 0.5)
  })

  # ── Results tab UI ───────────────────────────────────────────────────────────
  output$results_ui <- renderUI({
    if (is.null(results())) {
      return(div(style = "color:#7f8c8d; font-style:italic;",
        icon("arrow-left"), " Run the analysis to see results here."))
    }

    r <- results()
    tagList(

      # Plot
      h4(if (r$test_type == "Fisher") "Mosaic plot" else "Association plot"),
      div(id = "plot-container",
        style = paste0("height:", r$plot_height_px, "px; margin-bottom: 24px;"),
        withSpinner(plotOutput("plot", height = paste0(r$plot_height_px, "px")))
      ),
      div(class = "interp-box", HTML(r$plot_interpretation)),

      # Tables
      div(class = "table-section",
        h4("Frequency tables"),
        fluidRow(
          column(6,
            h5("Observed frequencies"),
            div(class = "preview-box", tableOutput("observed_table"))
          ),
          column(6,
            h5("Expected frequencies"),
            div(class = "preview-box", tableOutput("expected_table"))
          )
        ),
        if (r$test_type == "ChiSq") {
          tagList(
            br(),
            h5("Pearson residuals"),
            helpText(HTML(
              "Residuals above <strong>+2</strong> or below <strong>−2</strong> ",
              "(shaded in the plot) indicate cells that deviate meaningfully from independence."
            )),
            div(class = "preview-box", tableOutput("residuals_table"))
          )
        }
      )
    )
  })

  # ── Plot ─────────────────────────────────────────────────────────────────────
  output$plot <- renderPlot({
    req(results())
    r <- results()
    if (r$test_type == "Fisher") {
      mosaic(~ ., data = as.table(r$freq_table),
             shade = TRUE,
             main  = "Mosaic Plot (Fisher's Exact Test)",
             labeling_args = list(gp_labels = gpar(fontsize = 10)))
    } else {
      assoc(r$freq_table,
            shade    = TRUE,
            labeling = labeling_values,
            main     = "Association Plot (χ² Test)")
    }
  })

  # ── Frequency tables ─────────────────────────────────────────────────────────
  output$observed_table <- renderTable({
    req(results())
    results()$observed
  }, rownames = TRUE, digits = 0)

  output$expected_table <- renderTable({
    req(results())
    round(results()$expected, 2)
  }, rownames = TRUE)

  output$residuals_table <- renderTable({
    req(results())
    round(results()$residuals, 3)
  }, rownames = TRUE)

  # ── HTML report download ─────────────────────────────────────────────────────
  output$downloadReport <- downloadHandler(
    filename = function() paste0("Statistical_Report_", Sys.Date(), ".html"),
    content  = function(file) {
      req(results())
      r <- results()

      strip_html <- function(s) {
        s <- gsub("<[^>]+>", " ", s)
        s <- gsub("&chi;",   "χ",  s)
        s <- gsub("&sup2;",  "²",  s)
        s <- gsub("&alpha;", "α",  s)
        s <- gsub("&nbsp;",  " ",  s)
        s <- trimws(gsub("\\s+", " ", s))
        s
      }

      # Format tables as kable-ready data frames
      obs_df  <- as.data.frame(r$observed)
      exp_df  <- as.data.frame(round(r$expected, 2))

      temp_rmd <- file.path(tempdir(), "report.Rmd")

      rmd_lines <- c(
        "---",
        "title: 'Test of Independence — Statistical Report'",
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output:",
        "  html_document:",
        "    theme: flatly",
        "    toc: true",
        "    toc_float: true",
        "    highlight: tango",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
        "```",
        "",
        "## Test performed",
        "",
        paste0("**Test:** ", r$test_label),
        "",
        paste0("**Reason:** ", strip_html(r$test_message)),
        "",
        "## Result",
        "",
        paste0("**p-value:** ", round(r$p_value, 4)),
        "",
        if (!is.na(r$chi_statistic))
          paste0("**χ² statistic:** ", round(r$chi_statistic, 3)),
        "",
        paste0("**Significance level (α):** ", r$alpha),
        "",
        paste0("**Conclusion:** ", strip_html(r$p_interpretation)),
        "",
        if (!is.null(r$cramers_v) && !is.na(r$cramers_v))
          c("## Effect size",
            "",
            paste0("**Cramér's V:** ", round(r$cramers_v, 3)),
            "",
            strip_html(r$effect_size_html),
            ""),
        "## Frequency tables",
        "",
        "### Observed frequencies",
        "",
        "```{r obs-table}",
        "obs_mat <- readRDS(file.path(tempdir(), 'obs_mat.rds'))",
        "knitr::kable(obs_mat, format = 'html') |>",
        "  kableExtra::kable_styling(bootstrap_options = c('striped','hover','condensed'))",
        "```",
        "",
        "### Expected frequencies",
        "",
        "```{r exp-table}",
        "exp_mat <- readRDS(file.path(tempdir(), 'exp_mat.rds'))",
        "knitr::kable(exp_mat, format = 'html') |>",
        "  kableExtra::kable_styling(bootstrap_options = c('striped','hover','condensed'))",
        "```",
        ""
      )

      if (r$test_type == "ChiSq") {
        res_df <- as.data.frame(round(r$residuals, 3))
        rmd_lines <- c(rmd_lines,
          "### Pearson residuals",
          "",
          "> Residuals with an absolute value above 2 indicate cells that deviate",
          "> meaningfully from independence.",
          "",
          "```{r res-table}",
          "res_mat <- readRDS(file.path(tempdir(), 'res_mat.rds'))",
          "knitr::kable(res_mat, format = 'html') |>",
          "  kableExtra::kable_styling(bootstrap_options = c('striped','hover','condensed'))",
          "```",
          ""
        )
        saveRDS(res_df, file.path(tempdir(), "res_mat.rds"))
      }

      rmd_lines <- c(rmd_lines,
        "---",
        "",
        paste0("*Report generated on ", format(Sys.Date(), "%d %B %Y"),
               " using TestWise (χ²/Fisher Shiny app) by Guillaume Desagulier — ",
               "[CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/)*")
      )

      saveRDS(obs_df, file.path(tempdir(), "obs_mat.rds"))
      saveRDS(exp_df, file.path(tempdir(), "exp_mat.rds"))
      writeLines(rmd_lines, temp_rmd)

      rmarkdown::render(temp_rmd, output_file = file,
                        envir = new.env(parent = globalenv()),
                        quiet = TRUE)
    }
  )

  # ── PNG plot download ────────────────────────────────────────────────────────
  output$downloadPlotPNG <- downloadHandler(
    filename = function() {
      test_tag <- if (!is.null(results()) && results()$test_type == "Fisher")
                    "MosaicPlot" else "AssociationPlot"
      paste0(test_tag, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(results(), input$png_width, input$png_height)
      r <- results()
      png(file,
          width  = input$png_width,
          height = input$png_height,
          units  = "in",
          res    = 300)
      if (r$test_type == "Fisher") {
        mosaic(~ ., data = as.table(r$freq_table),
               shade = TRUE, main = "Mosaic Plot (Fisher's Exact Test)",
               labeling_args = list(gp_labels = gpar(fontsize = 10)))
      } else {
        assoc(r$freq_table, shade = TRUE,
              labeling = labeling_values,
              main     = "Association Plot (χ² Test)")
      }
      dev.off()
    }
  )

} # end server

# =============================================================================
shinyApp(ui = ui, server = server)
