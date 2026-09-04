# Medication Chooser

A Shiny app that ranks medications against what a particular patient cares about.
Each drug is scored 1 to 5 on every consideration that plausibly drives selection.
The user taps the considerations that matter to them and the ranking recomputes.

Three conditions are covered: depression, anxiety, and LDL cholesterol lowering for
primary prevention.

Live at <https://salishresearchgroup.shinyapps.io/antidepressant_chooser/>

## What it is for

Antidepressants are close to equivalent on efficacy, and so are SSRIs for anxiety,
and so are statins at equal intensity. The choice between them is made on side
effects, interactions, cost, and circumstance. That is a preference-sensitive
decision, which makes it a bad fit for a linear guideline and a good fit for
letting the patient say what they weigh most.

The app is a conversation aid, not a prescribing tool. It ranks; it does not
recommend, and it does not know anything about the person using it.

## Repository layout

    app.R                          the whole application
    www/considerations.csv         what can be selected, per condition
    www/medications.csv            depression
    www/medications_anxiety.csv    anxiety
    www/medications_lipids.csv     LDL lowering

## Data model

`considerations.csv` has one row per (domain, id). It defines the buttons and,
by doing so, the score columns the app looks for in that domain's medication file.

| column | meaning |
| --- | --- |
| `domain` | `depression`, `anxiety`, or `lipids` |
| `id` | matches a column name in the medication file |
| `label` | button text; `\|` marks the line break |
| `icon` | space-separated Unicode code points, e.g. `2696 FE0F` |
| `description` | tooltip text |

Icons are stored as code points rather than literal emoji so the CSV stays ASCII
and does not depend on the reader's locale.

Each medication file has one row per drug, or per (drug, subtype) where a domain
has a toggle, plus:

| column | meaning |
| --- | --- |
| `name`, `brand_name`, `class` | display |
| `subtype` | which toggle position this row belongs to; omit if the domain has none |
| `dose`, `cost_estimate` | display; both optional |
| `summary` | one or two sentences shown on the card |
| `default_score` | sort order when nothing is selected |
| `add_on` | optional; `1` marks a drug usually added to another rather than used alone |
| one column per consideration `id` | 1 to 5, higher is better on that consideration |

Scores are directional, not absolute: 5 on `weight` means least weight gain, 5 on
`sexual` means fewest sexual side effects.

### Subtypes

Two domains split their medication list:

- **anxiety** toggles between panic disorder (`panic`) and generalized anxiety
  (`gad`), defaulting to GAD. Every drug appears under both. What changes is
  mostly the `evidence` score, because the difference between the two conditions
  is not which drugs exist but which have trial support. Buspirone scores 4 for
  GAD and 1 for panic; duloxetine 5 and 2; quetiapine 3 and 1.
- **lipids** toggles between `low`, `moderate`, and `high` intensity, defaulting
  to moderate, using the standard statin intensity bands. Statins appear in more
  than one tier at different doses, with the dose-dependent scores (muscle,
  glucose, cognition) differing between them. Non-statins sit in the tier matching
  the LDL reduction they achieve on their own.

## Scoring

With nothing selected, drugs are ordered by `default_score`.

With one or more considerations selected, each drug's score is the mean of its
values on those considerations, and `match_pct` is that mean over 5. Weights are
equal and the combination is additive; the mean rather than the sum means a drug
missing a score on one selection is ranked on what is known rather than dropped
silently to the bottom. Cards show which scores are unrated with a hatched bar,
and the results header counts them.

Consequences worth knowing:

- A drug scoring 5, 5, 1 ties one scoring 4, 4, 3. For comfort considerations
  that is reasonable. For anything closer to a hard constraint (pregnancy) it is
  not, and the ranking should be read alongside the individual bars.
- Selecting many considerations pulls the ranking toward drugs that are
  unremarkable everywhere.

## Adding a condition

1. Add rows to `www/considerations.csv` under a new `domain`.
2. Add `www/medications_<domain>.csv` with a column per consideration `id`.
3. Add an entry to the `conditions` list in `app.R` with `nav`, `title`, `blurb`,
   `file`, and optionally `subtypes`, `subtype_label`, `subtype_default`.

No other code changes. A missing score column logs at startup and renders as
unrated rather than failing.

## Sources

Scores are drawn from UpToDate topic reviews and the primary literature they
cite, principally:

- Unipolar major depression: choosing initial treatment
- Generalized anxiety disorder in adults: management
- Panic disorder in adults: treatment overview
- LDL-cholesterol-lowering therapy in the primary prevention of ASCVD
- The 2026 ACC/AHA dyslipidemia guideline

Pregnancy and breastfeeding scores come from general practice rather than those
reviews and are the least well sourced values in the tables.

Cost estimates are approximate cash prices with a discount coupon and vary by
pharmacy and location.

## Running it

    shiny::runApp()

Requires `shiny` and `bslib`. Deployed with `rsconnect::deployApp()`.

## Disclaimer

Educational use only. This does not replace professional medical advice.
Medication selection should be discussed with a clinician who knows the patient's
full history.
