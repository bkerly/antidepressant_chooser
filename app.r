library(shiny)
library(bslib)

# ─── Data ─────────────────────────────────────────────────────────────────────
# Considerations live in www/considerations.csv, one row per (domain, id).
# Icons are stored as space-separated Unicode code points so the CSV stays
# ASCII and does not depend on the reader's locale.

decode_icon <- function(x) {
  vapply(strsplit(x, " ", fixed = TRUE), function(cp) {
    intToUtf8(strtoi(cp, 16L))
  }, character(1))
}

all_considerations <- read.csv("www/considerations.csv", stringsAsFactors = FALSE)
all_considerations$label <- gsub("|", "\n", all_considerations$label, fixed = TRUE)
all_considerations$icon <- decode_icon(all_considerations$icon)

conditions <- list(
  depression = list(
    nav      = "Depression",
    title    = "Find Your Antidepressant",
    blurb    = paste(
      "Everyone\u2019s needs are different. Select what matters most to you,",
      "and we\u2019ll show you which medications may be the best fit.",
      "This tool is for informational purposes\u2014always discuss options with your doctor."
    ),
    file     = "www/medications.csv",
    subtypes = NULL
  ),
  anxiety = list(
    nav      = "Anxiety",
    title    = "Find Your Anxiety Medication",
    blurb    = paste(
      "Panic disorder and generalized anxiety are treated with overlapping drugs",
      "but different evidence. Pick the condition first, then choose what matters most to you."
    ),
    file     = "www/medications_anxiety.csv",
    subtypes = c("Panic disorder" = "panic", "Generalized anxiety" = "gad"),
    subtype_label = "Which are you treating?",
    subtype_default = "gad"
  ),
  lipids = list(
    nav      = "Cholesterol",
    title    = "Find Your Cholesterol Medication",
    blurb    = paste(
      "These are options for lowering LDL cholesterol in someone who has not yet",
      "had a heart attack or stroke. Select what matters most to you to see how the",
      "options compare. Always discuss the choice with your doctor."
    ),
    file     = "www/medications_lipids.csv",
    subtypes = c("Low intensity" = "low",
                 "Moderate intensity" = "moderate",
                 "High intensity" = "high"),
    subtype_label = "How much LDL reduction do you need?",
    subtype_default = "moderate"
  ),
  hypertension = list(
    nav      = "Blood Pressure",
    title    = "Find Your Blood Pressure Medication",
    blurb    = paste(
      "Stage 1 is usually treated with one drug; stage 2 usually starts with two,",
      "preferably in a single pill. Pick the stage first, then choose what matters most to you."
    ),
    file     = "www/medications_hypertension.csv",
    subtypes = c("Stage 1 (130-139 / 80-89)" = "stage1",
                 "Stage 2 (140-180 / 90-120)" = "stage2"),
    subtype_label = "What is the starting blood pressure?",
    subtype_default = "stage2"
  ),
  contraception = list(
    nav      = "Birth Control",
    title    = "Find Your Birth Control",
    blurb    = paste(
      "There is no best method, only the one that fits what you care about.",
      "Start with everything, or narrow to daily pills if that is the shape you want."
    ),
    file     = "www/medications_contraception.csv",
    subtypes = c("All methods" = "all", "Daily pills only" = "pills"),
    subtype_label = "What are you comparing?",
    subtype_default = "all"
  )
)

load_meds <- function(spec, score_ids) {
  df <- read.csv(spec$file, stringsAsFactors = FALSE)

  absent <- setdiff(score_ids, names(df))
  if (length(absent)) {
    message(sprintf("%s: no column for %s; shown as unrated.",
                    spec$file, paste(absent, collapse = ", ")))
    for (a in absent) df[[a]] <- NA_real_
  }

  for (col in score_ids) {
    v <- suppressWarnings(as.numeric(df[[col]]))
    v[!is.na(v) & (v < 1 | v > 5)] <- NA_real_
    df[[col]] <- v
  }

  # generic_score is the legacy name for the no-selection sort order.
  if (!"default_score" %in% names(df)) {
    df$default_score <- if ("generic_score" %in% names(df)) {
      suppressWarnings(as.numeric(df$generic_score))
    } else {
      NA_real_
    }
  } else {
    df$default_score <- suppressWarnings(as.numeric(df$default_score))
  }

  if (!"subtype" %in% names(df)) df$subtype <- "all"
  for (col in c("brand_name", "class", "cost_estimate", "summary", "dose", "add_on")) {
    if (!col %in% names(df)) df[[col]] <- ""
  }
  df
}

condition_data <- lapply(names(conditions), function(dom) {
  cons <- all_considerations[all_considerations$domain == dom, ]
  list(spec = conditions[[dom]], cons = cons,
       meds = load_meds(conditions[[dom]], cons$id))
})
names(condition_data) <- names(conditions)

# ─── Helpers ──────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (is.null(a)) b else a

is_addon <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  isTRUE(trimws(tolower(as.character(x)[1])) %in% c("1", "yes", "y", "true", "t"))
}

flat_label <- function(x) gsub("\n", " ", x, fixed = TRUE)

score_row <- function(label, score) {
  fill <- if (is.na(score)) "fill-na" else paste0("fill-", score)
  reading <- if (is.na(score)) "not rated" else paste0(score, " out of 5")
  div(class = "score-row",
    span(class = "score-label", label),
    div(class = "score-bar-bg", role = "img",
        `aria-label` = paste0(label, ": ", reading),
        div(class = paste0("score-bar-fill ", fill))
    )
  )
}

# ─── Module ───────────────────────────────────────────────────────────────────

conditionUI <- function(id, spec, cons) {
  ns <- NS(id)

  tagList(
    div(class = "app-header",
      tags$h1(spec$title),
      tags$p(spec$blurb)
    ),

    if (!is.null(spec$subtypes)) {
      div(class = "subtype-section",
        radioButtons(ns("subtype"),
                     spec$subtype_label %||% "Which are you treating?",
                     choices = spec$subtypes,
                     selected = spec$subtype_default %||% spec$subtypes[[1]],
                     inline = TRUE)
      )
    },

    div(class = "considerations-section",
      tags$h3("Pick what\u2019s important to you:"),
      div(class = "consideration-grid", role = "group",
          `aria-label` = "Considerations",
        lapply(seq_len(nrow(cons)), function(i) {
          cid <- cons$id[i]
          tags$button(
            type = "button",
            id = ns(paste0("btn_", cid)),
            class = "consideration-btn",
            `aria-pressed` = "false",
            title = cons$description[i],
            onclick = sprintf("Shiny.setInputValue('%s', Math.random())",
                              ns(paste0("toggle_", cid))),
            span(class = "btn-icon", `aria-hidden` = "true", cons$icon[i]),
            span(class = "btn-label", cons$label[i])
          )
        })
      )
    ),

    uiOutput(ns("results_header")),
    uiOutput(ns("medication_cards"))
  )
}

conditionServer <- function(id, spec, cons, meds) {
  force(spec); force(cons); force(meds)

  moduleServer(id, function(input, output, session) {

    active <- reactiveValues()
    for (cid in cons$id) active[[cid]] <- FALSE

    lapply(cons$id, function(cid) {
      observeEvent(input[[paste0("toggle_", cid)]], {
        active[[cid]] <- !isTRUE(active[[cid]])
        session$sendCustomMessage("setToggle", list(
          id = session$ns(paste0("btn_", cid)),
          on = isTRUE(active[[cid]])
        ))
      }, ignoreInit = TRUE)
    })

    active_ids <- reactive({
      cons$id[vapply(cons$id, function(cid) isTRUE(active[[cid]]), logical(1))]
    })

    current_meds <- reactive({
      if (is.null(spec$subtypes)) return(meds)
      req(input$subtype)
      meds[meds$subtype == input$subtype, , drop = FALSE]
    })

    ranked <- reactive({
      df <- current_meds()
      sel <- active_ids()

      if (nrow(df) == 0) {
        return(cbind(df, rank_score = numeric(0),
                     match_pct = numeric(0), n_rated = integer(0)))
      }

      if (length(sel) == 0) {
        df$rank_score <- ifelse(is.na(df$default_score), 0, df$default_score)
        df$match_pct <- NA_real_
        df$n_rated <- NA_integer_
      } else {
        sub <- as.matrix(df[, sel, drop = FALSE])
        n_rated <- rowSums(!is.na(sub))
        avg <- ifelse(n_rated > 0, rowSums(sub, na.rm = TRUE) / n_rated, NA_real_)
        df$rank_score <- ifelse(is.na(avg), -Inf, avg)
        df$match_pct <- round(avg / 5 * 100)
        df$n_rated <- n_rated
      }

      df[order(-df$rank_score, df$name), , drop = FALSE]
    })

    output$results_header <- renderUI({
      sel <- active_ids()
      df <- ranked()
      unrated <- if (length(sel) == 0) 0 else sum(df$n_rated < length(sel))

      div(class = "results-header",
        div(class = "results-count",
          if (length(sel) == 0) {
            paste0("Showing all ", nrow(df), " medications")
          } else {
            paste0("Ranked by your ", length(sel), " selection",
                   if (length(sel) > 1) "s" else "")
          }
        ),
        if (length(sel) > 0) {
          div(class = "active-filters",
            lapply(flat_label(cons$label[match(sel, cons$id)]), function(lbl) {
              span(class = "active-filter-tag", lbl)
            })
          )
        },
        if (unrated > 0) {
          div(class = "unrated-note",
              sprintf("%d medication%s not rated on every selection; scored on what is known.",
                      unrated, if (unrated > 1) "s are" else " is"))
        }
      )
    })

    output$medication_cards <- renderUI({
      df <- ranked()
      sel <- active_ids()
      if (nrow(df) == 0) {
        return(div(class = "med-grid",
                   div(class = "empty-note", "No medications match this selection.")))
      }
      top <- if (nrow(df) > 0 && length(sel) > 0) max(df$match_pct, na.rm = TRUE) else NA_real_
      rest <- setdiff(cons$id, sel)

      div(class = "med-grid",
        lapply(seq_len(nrow(df)), function(i) {
          row <- df[i, ]
          is_top <- !is.na(top) && !is.na(row$match_pct) && row$match_pct == top

          div(class = "med-card", onclick = "toggleCard(this)",
            div(class = paste0("med-card-header", if (is_top) " top-match" else ""),
              div(class = "badge-row",
                span(class = "class-badge", row$class),
                if (!is.na(row$match_pct)) {
                  span(class = paste0("match-badge", if (is_top) " top" else ""),
                       paste0(row$match_pct, "% match"))
                }
              ),
              tags$h4(row$name),
              div(class = "brand-name", row$brand_name),
              if (is_addon(row$add_on)) {
                div(class = "addon-note",
                    "Usually added to a statin rather than used alone")
              }
            ),
            div(class = "med-card-body",
              div(class = "med-meta",
                span(class = "meta-item", paste0("\U0001F4B2 ", row$cost_estimate, "/mo")),
                if (nzchar(row$dose)) span(class = "meta-item dose-item", row$dose)
              ),
              div(class = "med-summary", row$summary),
              if (length(sel) > 0) {
                div(class = "score-bars",
                  lapply(sel, function(cid) {
                    score_row(flat_label(cons$label[cons$id == cid]), row[[cid]])
                  })
                )
              },
              tags$button(type = "button", class = "expand-hint",
                `aria-expanded` = "false",
                onclick = "event.stopPropagation(); toggleCard(this.closest('.med-card'))",
                span(class = "hint-text", "Show all scores"),
                span(class = "chevron", `aria-hidden` = "true", "\u25BC")
              )
            ),
            div(class = "all-scores",
              div(class = "all-scores-title",
                  if (length(sel) > 0) "All other scores" else "All scores"),
              div(class = "score-bars",
                lapply(rest, function(cid) {
                  score_row(flat_label(cons$label[cons$id == cid]), row[[cid]])
                })
              )
            )
          )
        })
      )
    })
  })
}

# ─── Styles ───────────────────────────────────────────────────────────────────

app_css <- HTML('
  :root {
    --pp-navy: #1B3A4B;
    --pp-teal: #2B6777;
    --pp-teal-light: #52AB98;
    --pp-tint: #E8F4F0;
    --pp-white: #FFFFFF;
    --pp-accent: #C8D8E4;
    --pp-gold: #D4A574;
    --pp-text: #2D3436;
    --pp-muted: #636E72;
  }

  /* One accent pair per condition, same family so the app reads as one product. */
  .cond-anxiety {
    --pp-navy: #2C3557;
    --pp-teal: #4A5A8A;
    --pp-teal-light: #7B8CC4;
    --pp-tint: #ECEFF8;
  }
  .cond-lipids {
    --pp-navy: #4A2B34;
    --pp-teal: #7A4A56;
    --pp-teal-light: #B07C86;
    --pp-tint: #F6ECEE;
  }
  .cond-hypertension {
    --pp-navy: #24402F;
    --pp-teal: #3E6B52;
    --pp-teal-light: #7FAE90;
    --pp-tint: #EAF2ED;
  }
  .cond-contraception {
    --pp-navy: #3A2A4A;
    --pp-teal: #6B4A7A;
    --pp-teal-light: #A98BB8;
    --pp-tint: #F1ECF5;
  }

  body {
    background: linear-gradient(135deg, #f5f7fa 0%, #e8ecf1 100%);
    color: var(--pp-text);
  }

  /* The navbar and the header band should meet. bslib puts padding on the
     tab container; zero it here and let .app-header own the top edge.
     If a sliver survives on your bslib version, add a negative top margin
     to .app-header below. */
  .navbar { margin-bottom: 0 !important; }
  .bslib-page-navbar,
  .bslib-page-navbar > .container-fluid,
  .bslib-page-navbar > .container-fluid > .tab-content,
  .tab-content,
  .tab-pane { padding-top: 0; margin-top: 0; row-gap: 0; gap: 0; }

  .addon-note {
    font-size: 0.7rem;
    font-weight: 600;
    color: var(--pp-teal);
    margin-top: 5px;
  }

  .empty-note {
    grid-column: 1 / -1;
    text-align: center;
    color: var(--pp-muted);
    padding: 2rem 0;
  }

  .app-header {
    background: linear-gradient(135deg, var(--pp-navy) 0%, var(--pp-teal) 100%);
    color: white;
    padding: 2rem 2rem 1.5rem;
    margin: 0 -1rem;
    border-radius: 0 0 24px 24px;
    text-align: center;
    box-shadow: 0 4px 20px rgba(27, 58, 75, 0.15);
  }
  .app-header h1 {
    font-family: "Playfair Display", serif;
    font-size: 2rem;
    font-weight: 700;
    margin-bottom: 0.5rem;
    letter-spacing: -0.5px;
  }
  .app-header p {
    font-size: 1rem;
    opacity: 0.9;
    max-width: 62ch;
    margin: 0 auto;
    line-height: 1.5;
  }

  .subtype-section {
    max-width: 1050px;
    margin: 1.25rem auto 0;
    text-align: center;
  }
  .subtype-section .control-label {
    font-weight: 600;
    color: var(--pp-navy);
    margin-bottom: 0.5rem;
  }
  .subtype-section .shiny-options-group {
    display: inline-flex;
    gap: 4px;
    background: var(--pp-white);
    border: 2px solid var(--pp-accent);
    border-radius: 999px;
    padding: 4px;
  }
  .subtype-section .radio-inline {
    margin: 0;
    padding: 6px 16px 6px 34px;
    border-radius: 999px;
    font-size: 0.85rem;
    font-weight: 600;
    cursor: pointer;
  }
  .subtype-section .radio-inline:has(input:checked) {
    background: var(--pp-teal);
    color: white;
  }

  .considerations-section {
    text-align: center;
    padding: 1.5rem 1rem 0.5rem;
  }
  .considerations-section h3 {
    font-family: "Playfair Display", serif;
    font-size: 1.25rem;
    font-weight: 600;
    color: var(--pp-navy);
    margin-bottom: 1rem;
  }

  .consideration-grid {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
    gap: 10px;
    max-width: 1050px;
    margin: 0 auto;
  }

  .consideration-btn {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    width: 100px;
    padding: 12px 4px;
    background: var(--pp-white);
    color: inherit;
    border: 2px solid var(--pp-accent);
    border-radius: 16px;
    cursor: pointer;
    transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
    user-select: none;
    text-align: center;
    box-shadow: 0 2px 8px rgba(0,0,0,0.04);
  }
  .consideration-btn:hover {
    border-color: var(--pp-teal);
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(43, 103, 119, 0.15);
  }
  .consideration-btn:focus-visible,
  .med-card:focus-visible,
  .expand-hint:focus-visible {
    outline: 3px solid var(--pp-navy);
    outline-offset: 2px;
  }
  .consideration-btn.active {
    background: linear-gradient(135deg, var(--pp-teal) 0%, var(--pp-teal-light) 100%);
    border-color: var(--pp-teal);
    color: white;
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(43, 103, 119, 0.25);
  }
  .consideration-btn .btn-icon {
    font-size: 1.5rem;
    margin-bottom: 5px;
    line-height: 1;
  }
  .consideration-btn .btn-label {
    font-size: 0.67rem;
    font-weight: 600;
    line-height: 1.25;
    white-space: pre-line;
    letter-spacing: 0.2px;
  }

  .results-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    flex-wrap: wrap;
    gap: 8px;
    padding: 1rem 1rem 0.5rem;
    max-width: 1100px;
    margin: 0 auto;
  }
  .results-count {
    font-size: 0.95rem;
    color: var(--pp-muted);
    font-weight: 500;
  }
  .active-filters {
    display: flex;
    gap: 6px;
    flex-wrap: wrap;
    align-items: center;
  }
  .active-filter-tag {
    display: inline-block;
    background: var(--pp-teal);
    color: white;
    font-size: 0.7rem;
    font-weight: 600;
    padding: 4px 10px;
    border-radius: 20px;
  }
  .unrated-note {
    flex-basis: 100%;
    font-size: 0.75rem;
    color: var(--pp-muted);
  }

  .med-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(290px, 1fr));
    gap: 16px;
    padding: 0.5rem 1rem 2rem;
    max-width: 1100px;
    margin: 0 auto;
  }

  .med-card {
    background: var(--pp-white);
    border-radius: 16px;
    overflow: hidden;
    box-shadow: 0 2px 12px rgba(0,0,0,0.06);
    transition: box-shadow 0.25s cubic-bezier(0.4, 0, 0.2, 1);
    display: flex;
    flex-direction: column;
    cursor: pointer;
  }
  .med-card:hover { box-shadow: 0 8px 30px rgba(0,0,0,0.1); }

  .med-card-header {
    padding: 1rem 1.1rem 0.8rem;
    border-bottom: 1px solid #eef2f5;
  }
  .med-card-header.top-match {
    background: linear-gradient(135deg, var(--pp-tint) 0%, var(--pp-white) 100%);
    border-bottom-color: var(--pp-teal-light);
  }
  .badge-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 8px;
    margin-bottom: 0.5rem;
  }
  .class-badge {
    font-size: 0.68rem;
    font-weight: 700;
    color: var(--pp-teal);
    background: var(--pp-tint);
    padding: 3px 10px;
    border-radius: 8px;
  }
  .match-badge {
    font-size: 0.7rem;
    font-weight: 700;
    color: var(--pp-muted);
    background: #f1f4f7;
    padding: 3px 10px;
    border-radius: 8px;
    white-space: nowrap;
  }
  .match-badge.top {
    color: white;
    background: var(--pp-teal);
  }
  .med-card-header h4 {
    font-family: "Playfair Display", serif;
    font-size: 1.15rem;
    font-weight: 700;
    margin: 0;
    color: var(--pp-navy);
  }
  .brand-name {
    font-size: 0.78rem;
    color: var(--pp-muted);
  }

  .med-card-body {
    padding: 0.9rem 1.1rem 0.6rem;
    display: flex;
    flex-direction: column;
    flex: 1;
  }
  .med-meta {
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
    margin-bottom: 0.6rem;
  }
  .meta-item {
    font-size: 0.75rem;
    color: var(--pp-teal);
    font-weight: 700;
    background: var(--pp-tint);
    padding: 3px 10px;
    border-radius: 8px;
  }
  .dose-item {
    color: var(--pp-muted);
    background: #f1f4f7;
    font-weight: 600;
  }

  .med-summary {
    font-size: 0.82rem;
    color: var(--pp-muted);
    line-height: 1.5;
    margin-bottom: 0.7rem;
    flex: 1;
  }

  .score-bars {
    display: flex;
    flex-direction: column;
    gap: 5px;
    margin-top: auto;
  }
  .score-row {
    display: flex;
    align-items: center;
    gap: 8px;
  }
  .score-label {
    font-size: 0.68rem;
    font-weight: 600;
    color: var(--pp-muted);
    width: 118px;
    text-align: right;
    flex-shrink: 0;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .score-bar-bg {
    flex: 1;
    height: 7px;
    background: #edf2f7;
    border-radius: 4px;
    overflow: hidden;
  }
  .score-bar-fill {
    height: 100%;
    border-radius: 4px;
    transition: width 0.4s cubic-bezier(0.4, 0, 0.2, 1);
  }
  .fill-5 { background: var(--pp-teal-light); width: 100%; }
  .fill-4 { background: var(--pp-teal); width: 80%; }
  .fill-3 { background: var(--pp-gold); width: 60%; }
  .fill-2 { background: #e0a886; width: 40%; }
  .fill-1 { background: #d9838a; width: 20%; }
  .fill-na {
    width: 100%;
    background: repeating-linear-gradient(45deg, #dfe6ec 0 4px, #eef2f5 4px 8px);
  }

  .expand-hint {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 4px;
    width: 100%;
    padding: 8px 0 4px;
    margin-top: 0.5rem;
    background: none;
    border: none;
    font-size: 0.7rem;
    font-weight: 600;
    color: var(--pp-teal);
    opacity: 0.75;
    cursor: pointer;
  }
  .med-card:hover .expand-hint { opacity: 1; }
  .expand-hint .chevron {
    display: inline-block;
    transition: transform 0.3s ease;
    font-size: 0.8rem;
  }
  .med-card.expanded .expand-hint .chevron { transform: rotate(180deg); }

  .all-scores {
    max-height: 0;
    overflow: hidden;
    transition: max-height 0.35s cubic-bezier(0.4, 0, 0.2, 1);
    background: #fafbfc;
  }
  .med-card.expanded .all-scores {
    max-height: 700px;
    border-top: 1px solid #eef2f5;
  }
  .all-scores-title {
    font-size: 0.7rem;
    font-weight: 700;
    color: var(--pp-navy);
    padding: 0.8rem 1.1rem 0.4rem;
  }
  .all-scores .score-bars { padding: 0 1.1rem 1rem; }

  .disclaimer {
    max-width: 800px;
    margin: 0 auto 2rem;
    padding: 1.2rem 1.5rem;
    background: #fff8f0;
    border: 1px solid #f0d9c0;
    border-radius: 12px;
    font-size: 0.82rem;
    color: #8b6914;
    line-height: 1.6;
    text-align: center;
  }
  .disclaimer strong {
    display: block;
    margin-bottom: 4px;
    font-size: 0.88rem;
  }

  @media (prefers-reduced-motion: reduce) {
    * { transition: none !important; }
  }
')

app_js <- HTML('
  Shiny.addCustomMessageHandler("setToggle", function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    el.classList.toggle("active", msg.on);
    el.setAttribute("aria-pressed", msg.on ? "true" : "false");
  });

  function toggleCard(card) {
    var open = card.classList.toggle("expanded");
    var btn = card.querySelector(".expand-hint");
    if (!btn) return;
    btn.setAttribute("aria-expanded", open ? "true" : "false");
    btn.querySelector(".hint-text").textContent =
      open ? "Hide other scores" : "Show all scores";
  }
')

# ─── App ──────────────────────────────────────────────────────────────────────

nav_panels <- lapply(names(conditions), function(dom) {
  cd <- condition_data[[dom]]
  nav_panel(
    title = cd$spec$nav,
    div(class = paste0("cond-", dom), conditionUI(dom, cd$spec, cd$cons))
  )
})

ui <- do.call(page_navbar, c(nav_panels, list(
  title = "Medication Chooser",
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Playfair Display"),
    primary = "#2B6777",
    "border-radius" = "12px"
  ),
  footer = tagList(
    tags$head(tags$style(app_css)),
    div(class = "disclaimer",
      tags$strong("\u26A0\uFE0F This tool is for educational purposes only"),
      paste(
        "It does not replace professional medical advice. Medication selection should always",
        "be discussed with a healthcare provider who knows your full medical history.",
        "Scores are based on published clinical evidence from UpToDate and peer-reviewed",
        "literature. Cost estimates are approximate with a GoodRx coupon and may vary by",
        "pharmacy and location."
      )
    ),
    tags$script(app_js)
  )
)))

server <- function(input, output, session) {
  lapply(names(conditions), function(dom) {
    cd <- condition_data[[dom]]
    conditionServer(dom, cd$spec, cd$cons, cd$meds)
  })
}

shinyApp(ui, server)
