# Murdoch IMPS Data Analyst Project

Analysis of Australian political finance disclosures across major parties, selected minor parties, and independents, with hypothesis-driven visual storytelling.

## Project Purpose

This repository evaluates patterns in political donations and receipts using legally disclosed funding data and aggregate returns. The project focuses on:

- Comparing funding composition across actor types (major parties, minor parties, independents)
- Testing donor-source and diversification hypotheses
- Evaluating temporal dynamics tied to political leverage and legislative tenure

## Political Context (Australia)

- Australia is a bicameral parliamentary system.
- The House of Representatives forms government.
- The Senate is constitutionally powerful; balance-of-power dynamics often involve minor parties and independents.

## Entities Included

### Major Parties
- ALP (Australian Labor Party)
- LPA (Liberal Party of Australia)
- Nationals (treated as a major actor for this study)

### Selected Minor Parties
- Australian Greens
- One Nation
- Animal Justice Party (AJP)
- Australian Citizens Party (ACP)
- Katter's Australian Party (KAP)
- Shooters

### Independents / Independent Cases
- Andrew Wilkie
- McGowan/Haines (treated as a single analytical entity)
- Additional independent cases may appear in source files depending on cycle updates

## Data Notes

- Two funding views are used:
  - Aggregate receipts
  - Legally disclosed donations
- These are often not equal due to disclosure thresholds and rule changes over time.
- Parties report both "donations" and "other donations"; both are treated as funding inputs in this analysis.
- Independent reporting formats differ from party reporting, especially for election-return years.

## Hypotheses

- **H1**: Non-major actors have a higher average share of individual and public-subvention funding than major parties.
  - **H1a**: Minor parties derive a higher individual-donor share than independents.
  - **H1b**: Minor parties derive a higher share of small individual donations than independents.
- **H2**: Minor-party funding is less diversified than major parties, but more diversified than independents.
- **H3**: Receipts increase in years when an actor is politically pivotal.
- **H3a**: Longer continuous legislative tenure predicts lower year-to-year receipt volatility.

## Visual Storytelling Approach

Recommended chart strategy implemented across scripts:

- **H1a**: 100% stacked composition bars
- **H1b**: 100% stacked bars by donation-size bands
- **H2**: Stacked composition (and/or treemap for diversification narrative)
- **H3**: Multi-line temporal chart with pivotal-event annotations
- **H3a**: Scatter (tenure vs volatility), optionally with labels and fit line

To support comparability across actor scales, transformations/normalization are applied where needed (including log treatment for skewed dollar values in relevant analyses).

## Repository Structure

- `imps_party_data/NORMALISED_HEADER_FILES/`
  - Core hypothesis scripts (`hypothesis H1a.R`, `hypothesis H1b.R`, `hypothesis H2.R`, `hypothesis H3.R`)
  - Source workbooks (`*.xlsx`)
  - Exported tables and figures (`*.csv`, `*.png`, `*.pdf`)
- `imps_party_data/NORMALISED_HEADER_FILES/GrayScale_visualisations/`
  - Grayscale/patterned variants for publication-safe output

## How to Run

1. Open R/RStudio in the repository root.
2. Ensure required packages are available (`readxl`, `dplyr`, `tidyr`, `ggplot2`, `writexl`, and script-specific extras such as `ggpattern`).
3. Run each hypothesis script from `imps_party_data/NORMALISED_HEADER_FILES/`.
4. Review generated outputs in `imps_party_data/` and `imps_party_data/NORMALISED_HEADER_FILES/`.

## Output Artifacts

Typical outputs include:

- Hypothesis figures for H1a, H1b, H2, H3, H3a
- Volatility and diversification tables in CSV format
- Publication-oriented grayscale/patterned chart variants

## Limitations and Interpretation

- Disclosure thresholds and reporting-rule changes can introduce time-varying observability bias.
- Minor-party and independent histories are uneven in length.
- Aggregate and disclosed streams should be interpreted as complementary, not interchangeable.

## Author

See `ABOUT_ME.md` for author profile and project role.
