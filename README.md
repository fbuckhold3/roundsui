# roundsui

Shared Shiny UI components and design tokens for the [rounds](https://github.com/fbuckhold3) ecosystem of internal medicine GME applications — the "Ward Notes" visual system.

`roundsui` is deliberately independent of any app's REDCap data-loading layer (that's `gmed`'s job). It exists so the ~13 apps in the rounds ecosystem can share one visual system without every app also inheriting `gmed`'s data-loading dependency.

## Status

Early — first component family only. See [Component roadmap](#component-roadmap) below.

## Install

```r
# from GitHub, once pushed
renv::install("fbuckhold3/roundsui")
```

## Usage

```r
library(shiny)
library(bslib)
library(roundsui)

ui <- page_fluid(
  theme = create_roundsui_theme(),
  load_roundsui_styles(),
  # ... your app UI
)
```

`load_roundsui_styles()` can be loaded alongside `gmed::load_gmed_styles()` during migration — roundsui's CSS custom properties are prefixed `--roundsui-*`, so they don't collide with gmed's `--gmed-*`/`--ssm-*` tokens.

## Try it locally

A standalone gallery demos every Shared Feedback/State component:

```r
devtools::load_all(".")
shiny::runApp("inst/examples/state_gallery")
```

## Component roadmap

Priority order, per the shape brief this package was planned from (see `rounds/PRODUCT.md`):

1. **Shared Feedback/State** (loading, empty, error) — ✅ shipped
   - `roundsui_loading_state()`, `roundsui_loading_overlay()`
   - `roundsui_empty_state()`, `roundsui_empty_table()`, `roundsui_empty_chart_annotation()`
   - `roundsui_inline_error()`, `roundsui_error_modal()`, `roundsui_notify()`
2. **Shell & Navigation** — ✅ shipped
   - `roundsui_page()` (generalizes `gmed_page()`)
   - `roundsui_nav_blocks()` (generalizes `gmed_nav_blocks()`; genuinely responsive grid, Font Awesome instead of Bootstrap Icons, no colored border-left, keyboard-operable)
   - `gmed_app_header()` deliberately not ported — no real consumer uses it anymore
3. **Data-Visualization wrappers** — ✅ shipped
   - `roundsui_status_badge()` (pill/dot variants; consolidates `gmed_status_badge()` + `create_status_indicator()`)
   - `roundsui_datatable()` (consolidates 4 overlapping DT constructors, including a dead-alias bug)
   - `roundsui_plotly_layout()` (new — consistent typography/gridline/hover chrome over existing chart logic)
4. **Data-Entry patterns** — the edit/view/compare 3-mode pattern (`gmed::mod_career_goals`) and overwrite/additive submission split, generalized

## Design tokens ("Ward Notes")

Restrained color strategy — neutrals plus one accent, conventional status colors. See `roundsui_colors()` or `inst/www/css/roundsui-tokens.css` for the full token set. Confirmed via `/impeccable new-work` on 2026-08-11; approved direction mock: <https://claude.ai/code/artifact/af9c7f48-607c-4982-94a4-de30485d7d86>.

| Token | Light | Dark |
|---|---|---|
| `ground` | `#F6F7FA` | `#0D1015` |
| `surface` | `#FFFFFF` | `#151920` |
| `ink` | `#13161F` | `#ECEEF3` |
| `accent` | `#3355D8` | `#6E8AFF` |
| `success` | `#16A34A` | `#3FCB74` |
| `warning` | `#C2760A` | `#E7A23D` |
| `danger` | `#D33A2C` | `#F0685C` |
