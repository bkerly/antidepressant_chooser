library(shiny)
library(bslib)

# ─── Load medication data from CSV ────────────────────────────────────────────
medications <- read.csv("www/medications.csv", stringsAsFactors = FALSE)

# Convert score columns to numeric
score_cols <- c("generic_score", "sexual", "sleep", "weight", "anxiety", "pain",
                "energy", "pregnancy", "gi", "discontinuation", "interactions",
                "pediatric", "older_adults", "breastfeeding")
for (col in score_cols) {
  medications[[col]] <- as.numeric(medications[[col]])
}

# ── Consideration metadata ────────────────────────────────────────────────────
considerations <- data.frame(
  id = c("sexual", "sleep", "weight", "anxiety", "pain",
         "energy", "pregnancy", "breastfeeding", "gi", "discontinuation",
         "interactions", "pediatric", "older_adults"),
  label = c(
    "Low Sexual\nSide Effects",
    "Helpful\nfor Sleep",
    "Low Weight\nGain",
    "Helps with\nAnxiety",
    "Helps with\nPain",
    "More\nEnergy",
    "Safer in\nPregnancy",
    "Safer for\nBreastfeeding",
    "Low GI\nSide Effects",
    "Easier\nto Stop",
    "Fewer Drug\nInteractions",
    "Better for\nPediatrics",
    "Better for\nOlder Adults"
  ),
  icon = c(
    "\U0001F496",
    "\U0001F31B",
    "\u2696\uFE0F",
    "\U0001F9D8",
    "\U0001F4AA",
    "\u26A1",
    "\U0001F930",
    "\U0001F931",
    "\U0001F34E",
    "\U0001F513",
    "\U0001F48A",
    "\U0001F9D2",
    "\U0001F9D3"
  ),
  description = c(
    "Minimizes risk of decreased libido, difficulty with orgasm, or erectile dysfunction",
    "Provides sedation that can help with insomnia and sleep difficulties",
    "Minimizes risk of weight gain during treatment",
    "Effective for co-occurring anxiety symptoms and anxiety disorders",
    "Can help with chronic pain conditions, especially neuropathic pain",
    "Provides activating effects for fatigue, low energy, or hypersomnia",
    "Has the most reassuring safety data for use during pregnancy",
    "Has the most reassuring safety data for use while breastfeeding",
    "Minimizes nausea, diarrhea, and other gastrointestinal side effects",
    "Lower risk of withdrawal symptoms when discontinuing the medication",
    "Less likely to interfere with other medications you may be taking",
    "Has evidence of efficacy and safety in children and adolescents",
    "Well-suited for adults 65 and older, considering tolerability and safety"
  ),
  stringsAsFactors = FALSE
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Playfair Display"),
    primary = "#2B6777",
    "border-radius" = "12px"
  ),
  tags$head(
    tags$style(HTML('
      :root {
        --pp-navy: #1B3A4B;
        --pp-teal: #2B6777;
        --pp-teal-light: #52AB98;
        --pp-cream: #F2F2F2;
        --pp-white: #FFFFFF;
        --pp-accent: #C8D8E4;
        --pp-gold: #D4A574;
        --pp-text: #2D3436;
        --pp-muted: #636E72;
      }

      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #e8ecf1 100%);
        color: var(--pp-text);
      }

      .app-header {
        background: linear-gradient(135deg, var(--pp-navy) 0%, var(--pp-teal) 100%);
        color: white;
        padding: 2rem 2rem 1.5rem;
        margin: -1rem -1rem 0;
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
        max-width: 680px;
        margin: 0 auto;
        line-height: 1.5;
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
        transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
        display: flex;
        flex-direction: column;
        cursor: pointer;
      }

      .med-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.1);
      }

      .expand-hint {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 4px;
        padding: 6px 0 2px;
        font-size: 0.7rem;
        font-weight: 600;
        color: var(--pp-teal);
        opacity: 0.7;
        transition: opacity 0.2s;
      }

      .med-card:hover .expand-hint {
        opacity: 1;
      }

      .expand-hint .chevron {
        display: inline-block;
        transition: transform 0.3s ease;
        font-size: 0.8rem;
      }

      .med-card.expanded .expand-hint .chevron {
        transform: rotate(180deg);
      }

      .med-card.expanded .expand-hint .hint-text {
        display: none;
      }

      .all-scores {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.4s cubic-bezier(0.4, 0, 0.2, 1),
                    padding 0.3s ease;
        padding: 0 1.2rem;
      }

      .med-card.expanded .all-scores {
        max-height: 600px;
        padding: 0.5rem 1.2rem 1rem;
      }

      .all-scores-title {
        font-size: 0.72rem;
        font-weight: 700;
        color: var(--pp-navy);
        text-transform: uppercase;
        letter-spacing: 0.8px;
        margin-bottom: 8px;
        padding-top: 8px;
        border-top: 1px solid #edf2f7;
      }

      .med-card-header {
        background: linear-gradient(135deg, var(--pp-navy) 0%, var(--pp-teal) 100%);
        color: white;
        padding: 1rem 1.2rem 0.8rem;
        position: relative;
      }

      .med-card-header.top-match {
        background: linear-gradient(135deg, #1a5c3a 0%, #2d8659 100%);
      }

      .med-card-header .class-badge {
        display: inline-block;
        background: rgba(255,255,255,0.2);
        font-size: 0.65rem;
        font-weight: 700;
        padding: 2px 8px;
        border-radius: 20px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        margin-bottom: 4px;
      }

      .med-card-header h4 {
        font-family: "Playfair Display", serif;
        font-size: 1.3rem;
        font-weight: 700;
        margin: 0;
        line-height: 1.2;
      }

      .med-card-header .brand-name {
        font-size: 0.82rem;
        opacity: 0.8;
        font-style: italic;
        margin-top: 2px;
      }

      .match-badge {
        position: absolute;
        top: 10px;
        right: 10px;
        background: var(--pp-gold);
        color: var(--pp-navy);
        font-size: 0.7rem;
        font-weight: 800;
        padding: 4px 10px;
        border-radius: 20px;
        letter-spacing: 0.3px;
      }

      .match-badge.top {
        background: #f0c040;
      }

      .med-card-body {
        padding: 0.9rem 1.2rem;
        flex: 1;
        display: flex;
        flex-direction: column;
      }

      .med-meta {
        display: flex;
        gap: 12px;
        margin-bottom: 0.6rem;
      }

      .meta-item {
        font-size: 0.75rem;
        color: var(--pp-teal);
        font-weight: 700;
        background: #e8f4f0;
        padding: 3px 10px;
        border-radius: 8px;
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
        width: 110px;
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
        transition: width 0.6s cubic-bezier(0.4, 0, 0.2, 1);
      }

      .fill-5 { background: var(--pp-teal-light); width: 100%; }
      .fill-4 { background: var(--pp-teal); width: 80%; }
      .fill-3 { background: var(--pp-gold); width: 60%; }
      .fill-2 { background: #e0a886; width: 40%; }
      .fill-1 { background: #d9838a; width: 20%; }

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
    '))
  ),

  div(class = "app-header",
    tags$h1("Find Your Antidepressant"),
    tags$p("Everyone\u2019s needs are different. Select what matters most to you, and we\u2019ll show you which medications may be the best fit. This tool is for informational purposes\u2014always discuss options with your doctor.")
  ),

  div(class = "considerations-section",
    tags$h3("Pick what\u2019s important to you:"),
    div(class = "consideration-grid",
      lapply(1:nrow(considerations), function(i) {
        div(
          id = paste0("btn_", considerations$id[i]),
          class = "consideration-btn",
          onclick = sprintf("Shiny.setInputValue('toggle_%s', Math.random())", considerations$id[i]),
          title = considerations$description[i],
          div(class = "btn-icon", considerations$icon[i]),
          div(class = "btn-label", considerations$label[i])
        )
      })
    )
  ),

  uiOutput("results_header"),
  uiOutput("medication_cards"),

  div(class = "disclaimer",
    tags$strong("\u26A0\uFE0F This tool is for educational purposes only"),
    "It does not replace professional medical advice. Antidepressant selection should always be discussed with a healthcare provider who knows your full medical history. Scores are based on published clinical evidence from UpToDate and peer-reviewed literature. Cost estimates are approximate with a GoodRx coupon and may vary by pharmacy and location."
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  active <- reactiveValues()
  for (cid in considerations$id) {
    active[[cid]] <- FALSE
  }

  lapply(considerations$id, function(cid) {
    observeEvent(input[[paste0("toggle_", cid)]], {
      active[[cid]] <- !active[[cid]]
      if (active[[cid]]) {
        session$sendCustomMessage("addClass", list(id = paste0("btn_", cid), cls = "active"))
      } else {
        session$sendCustomMessage("removeClass", list(id = paste0("btn_", cid), cls = "active"))
      }
    }, ignoreInit = TRUE)
  })

  active_ids <- reactive({
    ids <- c()
    for (cid in considerations$id) {
      if (isTRUE(active[[cid]])) ids <- c(ids, cid)
    }
    ids
  })

  ranked_meds <- reactive({
    sel <- active_ids()
    df <- medications

    if (length(sel) == 0) {
      df$total_score <- df$generic_score
      df$match_pct <- NA
    } else {
      df$total_score <- rowSums(df[, sel, drop = FALSE])
      max_possible <- length(sel) * 5
      df$match_pct <- round(df$total_score / max_possible * 100)
    }

    df <- df[order(-df$total_score), ]
    df
  })

  output$results_header <- renderUI({
    sel <- active_ids()
    sel_labels <- considerations$label[match(sel, considerations$id)]
    sel_labels <- gsub("\n", " ", sel_labels)

    div(class = "results-header",
      div(class = "results-count",
        if (length(sel) == 0) {
          paste0("Showing all ", nrow(medications), " medications")
        } else {
          paste0("Ranked by your ", length(sel), " selection", ifelse(length(sel) > 1, "s", ""))
        }
      ),
      if (length(sel) > 0) {
        div(class = "active-filters",
          lapply(sel_labels, function(lbl) {
            span(class = "active-filter-tag", lbl)
          })
        )
      }
    )
  })

  output$medication_cards <- renderUI({
    df <- ranked_meds()
    sel <- active_ids()
    top_score <- if (nrow(df) > 0 && length(sel) > 0) df$total_score[1] else NA
    all_cids <- considerations$id

    div(class = "med-grid",
      lapply(1:nrow(df), function(i) {
        row <- df[i, ]
        is_top <- !is.na(top_score) && row$total_score == top_score
        card_id <- paste0("card_", gsub("[^a-zA-Z]", "", row$name))

        # Determine which scores to show in the expanded section
        # (all scores minus the ones already visible from active filters)
        expanded_cids <- setdiff(all_cids, sel)

        div(class = "med-card", id = card_id,
          onclick = sprintf("this.classList.toggle('expanded')"),
          div(class = paste0("med-card-header", ifelse(is_top, " top-match", "")),
            span(class = "class-badge", row$class),
            if (!is.na(row$match_pct)) {
              span(class = paste0("match-badge", ifelse(is_top, " top", "")),
                if (is_top) "\u2B50 " else "",
                paste0(row$match_pct, "% Match")
              )
            },
            tags$h4(row$name),
            div(class = "brand-name", row$brand_name)
          ),
          div(class = "med-card-body",
            div(class = "med-meta",
              span(class = "meta-item", paste0("\U0001F4B2 ", row$cost_estimate, "/mo"))
            ),
            div(class = "med-summary", row$summary),
            if (length(sel) > 0) {
              div(class = "score-bars",
                lapply(sel, function(cid) {
                  score <- row[[cid]]
                  label <- gsub("\n", " ", considerations$label[considerations$id == cid])
                  div(class = "score-row",
                    span(class = "score-label", label),
                    div(class = "score-bar-bg",
                      div(class = paste0("score-bar-fill fill-", score))
                    )
                  )
                })
              )
            },
            div(class = "expand-hint",
              span(class = "hint-text", "Tap to see all scores"),
              span(class = "chevron", "\u25BC")
            )
          ),
          # Expandable section with remaining scores
          div(class = "all-scores",
            div(class = "all-scores-title",
              if (length(sel) > 0) "All Other Scores" else "All Scores"
            ),
            div(class = "score-bars",
              lapply(expanded_cids, function(cid) {
                score <- row[[cid]]
                label <- gsub("\n", " ", considerations$label[considerations$id == cid])
                div(class = "score-row",
                  span(class = "score-label", label),
                  div(class = "score-bar-bg",
                    div(class = paste0("score-bar-fill fill-", score))
                  )
                )
              })
            )
          )
        )
      })
    )
  })
}

ui <- tagList(
  ui,
  tags$script(HTML('
    Shiny.addCustomMessageHandler("addClass", function(msg) {
      document.getElementById(msg.id).classList.add(msg.cls);
    });
    Shiny.addCustomMessageHandler("removeClass", function(msg) {
      document.getElementById(msg.id).classList.remove(msg.cls);
    });
  '))
)

shinyApp(ui, server)