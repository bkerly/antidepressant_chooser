library(shiny)
library(bslib)

# ─── Evidence-based scoring data ──────────────────────────────────────────────
# Each medication scored 1-5 per consideration (5 = best fit)
# Sources: UpToDate Side Effects Table, MDD Initial Treatment, Antenatal SSRI Safety,
#          Late-Life Depression, Pediatric Pharmacotherapy articles

medications <- data.frame(
  name = c(
    "Sertraline", "Escitalopram", "Fluoxetine", "Citalopram",
    "Paroxetine", "Bupropion", "Mirtazapine", "Duloxetine",
    "Venlafaxine", "Vortioxetine", "Vilazodone", "Desvenlafaxine"
  ),
  class = c(
    "SSRI", "SSRI", "SSRI", "SSRI",
    "SSRI", "Atypical", "Atypical", "SNRI",
    "SNRI", "Serotonin Modulator", "Serotonin Modulator", "SNRI"
  ),
  generic_score = c(
    5, 5, 5, 4,
    3, 4, 3, 4,
    4, 4, 3, 3
  ),
  # ── Low Sexual Side Effects ──
  # Inverse of UpToDate sexual dysfunction rating
  # Bupropion (0) → 5, Mirtazapine (1+) → 4, Vortioxetine (1+) → 4
  # SNRIs (1+) → 4, SSRIs (3+) → 2, Paroxetine (4+) → 1
  sexual = c(
    2,  # Sertraline: 3+ sexual dysfunction
    2,  # Escitalopram: 3+
    2,  # Fluoxetine: 3+
    2,  # Citalopram: 3+
    1,  # Paroxetine: 4+
    5,  # Bupropion: 0
    4,  # Mirtazapine: 1+
    4,  # Duloxetine: 1+
    2,  # Venlafaxine: 3+
    4,  # Vortioxetine: 1+
    4,  # Vilazodone: 1+
    4   # Desvenlafaxine: 1+
  ),
  # ── Helpful for Sleep ──
  # Based on drowsiness/sedation rating from UpToDate table
  # Mirtazapine (4+) → 5, Paroxetine (2+) → 3, most SSRIs (1+) → 2
  # Bupropion (0) → 1, Fluoxetine (0) → 1
  sleep = c(
    2,  # Sertraline: 1+ drowsiness
    2,  # Escitalopram: 1+ drowsiness
    1,  # Fluoxetine: 0 drowsiness, 2+ insomnia
    2,  # Citalopram: 1+
    3,  # Paroxetine: 2+
    1,  # Bupropion: 0 drowsiness, 2+ insomnia
    5,  # Mirtazapine: 4+ drowsiness
    1,  # Duloxetine: 0, 1+ insomnia
    1,  # Venlafaxine: 1+, 2+ insomnia
    1,  # Vortioxetine: 0
    1,  # Vilazodone: 0, 2+ insomnia
    1   # Desvenlafaxine: 0, 2+ insomnia
  ),
  # ── Low Weight Gain ──
  # Inverse of UpToDate weight gain rating
  # Bupropion (0, may lose weight) → 5, Fluoxetine (0) → 5
  # Mirtazapine (4+) → 1, Paroxetine (2+) → 2
  weight = c(
    4,  # Sertraline: 1+
    4,  # Escitalopram: 1+
    5,  # Fluoxetine: 0
    4,  # Citalopram: 1+
    2,  # Paroxetine: 2+
    5,  # Bupropion: 0 (may cause weight loss)
    1,  # Mirtazapine: 4+
    4,  # Duloxetine: 0 to 1+
    4,  # Venlafaxine: 0 to 1+
    5,  # Vortioxetine: 0
    5,  # Vilazodone: 0
    3   # Desvenlafaxine: unknown
  ),
  # ── Helps with Anxiety ──
  # SSRIs are first-line for anxiety disorders; bupropion generally avoided
  anxiety = c(
    5,  # Sertraline: preferred for anxiety
    5,  # Escitalopram: preferred for anxiety
    4,  # Fluoxetine: good for anxiety
    4,  # Citalopram: good for anxiety
    4,  # Paroxetine: good for anxiety
    1,  # Bupropion: avoid in significant anxiety
    3,  # Mirtazapine: some anxiolytic effect
    4,  # Duloxetine: good for GAD
    4,  # Venlafaxine: good for GAD
    3,  # Vortioxetine: modest data
    3,  # Vilazodone: modest data
    3   # Desvenlafaxine: some data
  ),
  # ── Helps with Pain ──
  # SNRIs (duloxetine especially) have evidence for neuropathic pain
  pain = c(
    2,  # Sertraline: minimal pain benefit
    2,  # Escitalopram: minimal
    2,  # Fluoxetine: minimal
    2,  # Citalopram: minimal
    2,  # Paroxetine: minimal
    1,  # Bupropion: no pain benefit
    2,  # Mirtazapine: some data
    5,  # Duloxetine: FDA-approved for neuropathic pain
    4,  # Venlafaxine: evidence for pain
    1,  # Vortioxetine: no data
    1,  # Vilazodone: no data
    3   # Desvenlafaxine: some SNRI class benefit
  ),
  # ── More Energy / Less Sedation ──
  # Inverse of drowsiness + consideration of activating properties
  energy = c(
    3,  # Sertraline: 1+ drowsiness, 2+ activation
    3,  # Escitalopram: 1+ drowsiness, 1+ activation
    4,  # Fluoxetine: 0 drowsiness, 2+ activation
    3,  # Citalopram: 1+ drowsiness
    2,  # Paroxetine: 2+ drowsiness
    5,  # Bupropion: 0 drowsiness, activating
    1,  # Mirtazapine: 4+ drowsiness
    3,  # Duloxetine: 0 drowsiness
    3,  # Venlafaxine: 1+ drowsiness
    4,  # Vortioxetine: 0 drowsiness
    3,  # Vilazodone: 0 drowsiness
    3   # Desvenlafaxine: 0 drowsiness
  ),
  # ── Safer in Pregnancy ──
  # Based on UpToDate antenatal SSRI/antidepressant review
  # SSRIs as a class: not major teratogens; sertraline has most reassuring data
  # Paroxetine: avoid (possible cardiac defect risk)
  # Non-SSRIs: less data
  pregnancy = c(
    5,  # Sertraline: most data, commonly used in pregnancy
    4,  # Escitalopram: reasonable safety data
    4,  # Fluoxetine: extensive data, some concern re: long half-life
    4,  # Citalopram: reasonable data
    2,  # Paroxetine: avoid—possible cardiac defect risk
    2,  # Bupropion: limited pregnancy data
    2,  # Mirtazapine: limited data
    2,  # Duloxetine: limited data
    3,  # Venlafaxine: some data, no major teratogenicity signal
    1,  # Vortioxetine: very limited data
    1,  # Vilazodone: very limited data
    1   # Desvenlafaxine: very limited data
  ),
  # ── Low GI Side Effects ──
  # Inverse of GI toxicity rating from UpToDate table
  gi = c(
    2,  # Sertraline: 2+ GI (diarrhea common)
    4,  # Escitalopram: 1+
    4,  # Fluoxetine: 1+
    4,  # Citalopram: 1+
    4,  # Paroxetine: 1+
    4,  # Bupropion: 1+
    5,  # Mirtazapine: 0
    3,  # Duloxetine: 2+
    3,  # Venlafaxine: 2+
    1,  # Vortioxetine: 3+
    1,  # Vilazodone: 4+
    3   # Desvenlafaxine: 2+
  ),
  # ── Easier to Stop (low discontinuation risk) ──
  # Fluoxetine: longest half-life, easiest to stop
  # Paroxetine & venlafaxine: worst discontinuation syndrome
  discontinuation = c(
    3,  # Sertraline: moderate
    3,  # Escitalopram: moderate
    5,  # Fluoxetine: long half-life, easiest
    3,  # Citalopram: moderate
    1,  # Paroxetine: worst discontinuation
    4,  # Bupropion: relatively easy
    4,  # Mirtazapine: relatively easy
    2,  # Duloxetine: can be difficult
    1,  # Venlafaxine: severe discontinuation
    3,  # Vortioxetine: moderate
    3,  # Vilazodone: moderate
    1   # Desvenlafaxine: can be difficult
  ),
  stringsAsFactors = FALSE
)

# ── Consideration metadata ────────────────────────────────────────────────────
considerations <- data.frame(
  id = c("sexual", "sleep", "weight", "anxiety", "pain",
         "energy", "pregnancy", "gi", "discontinuation"),
  label = c(
    "Low Sexual\nSide Effects",
    "Helpful\nfor Sleep",
    "Low Weight\nGain",
    "Helps with\nAnxiety",
    "Helps with\nPain",
    "More\nEnergy",
    "Safer in\nPregnancy",
    "Low GI\nSide Effects",
    "Easier\nto Stop"
  ),
  icon = c(
    "\U0001F496",   # heart
    "\U0001F31B",   # moon
    "\u2696\uFE0F", # scale
    "\U0001F9D8",   # meditation
    "\U0001F4AA",   # flexed bicep
    "\u26A1",       # lightning bolt
    "\U0001F930",   # pregnant
    "\U0001F34E",   # apple (stomach)
    "\U0001F513"    # unlocked
  ),
  description = c(
    "Minimizes risk of decreased libido, difficulty with orgasm, or erectile dysfunction",
    "Provides sedation that can help with insomnia and sleep difficulties",
    "Minimizes risk of weight gain during treatment",
    "Effective for co-occurring anxiety symptoms and anxiety disorders",
    "Can help with chronic pain conditions, especially neuropathic pain",
    "Provides activating effects for fatigue, low energy, or hypersomnia",
    "Has the most reassuring safety data for use during pregnancy",
    "Minimizes nausea, diarrhea, and other gastrointestinal side effects",
    "Lower risk of withdrawal symptoms when discontinuing the medication"
  ),
  stringsAsFactors = FALSE
)

# ── Brief medication summaries ────────────────────────────────────────────────
med_summaries <- c(
  "Sertraline" = "A well-studied SSRI often considered first-line. Generally well-tolerated with the most pregnancy safety data among antidepressants.",
  "Escitalopram" = "An SSRI known for good tolerability. Effective for both depression and anxiety with a relatively clean side-effect profile.",
  "Fluoxetine" = "The longest-acting SSRI, making it easiest to stop. Also first-line for children/adolescents. Energizing and weight-neutral.",
  "Citalopram" = "An SSRI similar to escitalopram. Note: doses limited to 20 mg/day in adults over 60 due to heart rhythm concerns.",
  "Paroxetine" = "An SSRI that is mildly sedating but has higher rates of sexual side effects, weight gain, and discontinuation symptoms. Avoid in pregnancy.",
  "Bupropion" = "An atypical antidepressant that is energizing, weight-neutral, and has no sexual side effects. Not helpful for anxiety or pain.",
  "Mirtazapine" = "A sedating antidepressant that helps with sleep and appetite. Good for patients who need to gain weight. Causes the most weight gain.",
  "Duloxetine" = "An SNRI with FDA approval for several pain conditions. Good choice when depression co-occurs with chronic pain.",
  "Venlafaxine" = "An SNRI effective for depression, anxiety, and pain. Can raise blood pressure at higher doses. Difficult to discontinue.",
  "Vortioxetine" = "A newer serotonin modulator with low sexual side effects. May improve cognitive symptoms of depression. Can cause significant nausea.",
  "Vilazodone" = "A newer serotonin modulator with lower sexual side effects. Must be taken with food. Can cause significant GI side effects.",
  "Desvenlafaxine" = "An SNRI (active metabolite of venlafaxine). Fewer drug interactions than venlafaxine but similar discontinuation concerns."
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

      /* ── Header ── */
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

      /* ── Consideration pills ── */
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
        gap: 12px;
        max-width: 900px;
        margin: 0 auto;
      }

      .consideration-btn {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        width: 110px;
        padding: 14px 8px;
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
        font-size: 1.6rem;
        margin-bottom: 6px;
        line-height: 1;
      }

      .consideration-btn .btn-label {
        font-size: 0.72rem;
        font-weight: 600;
        line-height: 1.3;
        white-space: pre-line;
        letter-spacing: 0.2px;
      }

      /* ── Results area ── */
      .results-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
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
        font-size: 0.72rem;
        font-weight: 600;
        padding: 4px 10px;
        border-radius: 20px;
      }

      .med-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
        gap: 16px;
        padding: 0.5rem 1rem 2rem;
        max-width: 1100px;
        margin: 0 auto;
      }

      /* ── Medication cards ── */
      .med-card {
        background: var(--pp-white);
        border-radius: 16px;
        overflow: hidden;
        box-shadow: 0 2px 12px rgba(0,0,0,0.06);
        transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
        display: flex;
        flex-direction: column;
      }

      .med-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.1);
      }

      .med-card-header {
        background: linear-gradient(135deg, var(--pp-navy) 0%, var(--pp-teal) 100%);
        color: white;
        padding: 1.1rem 1.2rem 0.8rem;
        position: relative;
      }

      .med-card-header .class-badge {
        display: inline-block;
        background: rgba(255,255,255,0.2);
        font-size: 0.68rem;
        font-weight: 700;
        padding: 3px 10px;
        border-radius: 20px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        margin-bottom: 6px;
      }

      .med-card-header h4 {
        font-family: "Playfair Display", serif;
        font-size: 1.35rem;
        font-weight: 700;
        margin: 0;
      }

      .match-badge {
        position: absolute;
        top: 12px;
        right: 12px;
        background: var(--pp-gold);
        color: var(--pp-navy);
        font-size: 0.72rem;
        font-weight: 800;
        padding: 4px 10px;
        border-radius: 20px;
        letter-spacing: 0.3px;
      }

      .med-card-body {
        padding: 1rem 1.2rem;
        flex: 1;
        display: flex;
        flex-direction: column;
      }

      .med-summary {
        font-size: 0.85rem;
        color: var(--pp-muted);
        line-height: 1.55;
        margin-bottom: 0.8rem;
        flex: 1;
      }

      .score-bars {
        display: flex;
        flex-direction: column;
        gap: 6px;
        margin-top: auto;
      }

      .score-row {
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .score-label {
        font-size: 0.7rem;
        font-weight: 600;
        color: var(--pp-muted);
        width: 100px;
        text-align: right;
        flex-shrink: 0;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .score-bar-bg {
        flex: 1;
        height: 8px;
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

      /* ── Disclaimer ── */
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

      /* ── No-selection state ── */
      .empty-state {
        text-align: center;
        padding: 3rem 1rem;
        color: var(--pp-muted);
      }

      .empty-state .icon {
        font-size: 3rem;
        margin-bottom: 1rem;
        opacity: 0.5;
      }
    '))
  ),

  # ── Header ──
  div(class = "app-header",
    tags$h1("Find Your Antidepressant"),
    tags$p("Everyone\u2019s needs are different. Select what matters most to you, and we\u2019ll show you which medications may be the best fit. This tool is for informational purposes\u2014always discuss options with your doctor.")
  ),

  # ── Consideration buttons ──
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

  # ── Results ──
  uiOutput("results_header"),
  uiOutput("medication_cards"),

  # ── Disclaimer ──
  div(class = "disclaimer",
    tags$strong("\u26A0\uFE0F This tool is for educational purposes only"),
    "It does not replace professional medical advice. Antidepressant selection should always be discussed with a healthcare provider who knows your full medical history. Scores are based on published clinical evidence from UpToDate and peer-reviewed literature."
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Track which considerations are active
  active <- reactiveValues()
  for (cid in considerations$id) {
    active[[cid]] <- FALSE
  }

  # Toggle handlers
  lapply(considerations$id, function(cid) {
    observeEvent(input[[paste0("toggle_", cid)]], {
      active[[cid]] <- !active[[cid]]
      # Update CSS class
      if (active[[cid]]) {
        session$sendCustomMessage("addClass", list(id = paste0("btn_", cid), cls = "active"))
      } else {
        session$sendCustomMessage("removeClass", list(id = paste0("btn_", cid), cls = "active"))
      }
    }, ignoreInit = TRUE)
  })

  # Compute active consideration IDs
  active_ids <- reactive({
    ids <- c()
    for (cid in considerations$id) {
      if (isTRUE(active[[cid]])) ids <- c(ids, cid)
    }
    ids
  })

  # Compute ranked medications
  ranked_meds <- reactive({
    sel <- active_ids()
    df <- medications

    if (length(sel) == 0) {
      df$total_score <- df$generic_score
      df$match_pct <- NA
    } else {
      score_cols <- sel
      df$total_score <- rowSums(df[, score_cols, drop = FALSE])
      max_possible <- length(sel) * 5
      df$match_pct <- round(df$total_score / max_possible * 100)
    }

    df <- df[order(-df$total_score), ]
    df
  })

  # Results header
  output$results_header <- renderUI({
    sel <- active_ids()
    sel_labels <- considerations$label[match(sel, considerations$id)]
    sel_labels <- gsub("\n", " ", sel_labels)

    div(class = "results-header",
      div(class = "results-count",
        if (length(sel) == 0) {
          "Showing all 12 medications"
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

  # Medication cards
  output$medication_cards <- renderUI({
    df <- ranked_meds()
    sel <- active_ids()

    div(class = "med-grid",
      lapply(1:nrow(df), function(i) {
        row <- df[i, ]
        summary_text <- med_summaries[row$name]

        div(class = "med-card",
          div(class = "med-card-header",
            span(class = "class-badge", row$class),
            if (!is.na(row$match_pct)) {
              span(class = "match-badge", paste0(row$match_pct, "% Match"))
            },
            tags$h4(row$name)
          ),
          div(class = "med-card-body",
            div(class = "med-summary", summary_text),
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
            }
          )
        )
      })
    )
  })
}

# ── JavaScript for class toggling ─────────────────────────────────────────────
# We inject this via the UI since Shiny needs it available at page load
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