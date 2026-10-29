# Talk Style Guide

Conventions for revealjs decks in `talk_*/`, checked against `talk_dds_reinsrecover_202610`, `talk_rr_reinsrecover_202607`, `talk_dds_survivalbayes_202603`, `talk_tcd_hiltswaltl_202609` and `talk_sonia_hiltswaltl_202606`. Where talks differ it says "varies". Does not apply to `ws_*` workshop material. Copy the newest full talk (`talk_dds_reinsrecover_202610`) as the template.

## Slide density

- One idea per slide. Median is 3-4 words per slide (crude count, text outside code chunks), only 0-2 slides per deck over 30 words.
- Long decks use many `---` breaks: 44 (dds_reinsrecover), 27 (rr_reinsrecover), 43 (dds_survivalbayes). Heading-only decks (tcd, sonia) use 2 and are about 8-10 slides: varies.
- A slide is often a single sentence, a single figure or a single table. Split rather than add bullets.
- Render to PDF and page through it to check density.

## YAML and slide structure

- Format block is identical across talks: `revealjs`, `embed-resources: true`, `theme: night`, `slide-level: 3`, `navigation-mode: vertical`, `css: styles.css`, `center-title-slide: true`, `slide-number: true`, `progress: true`, `controls: true`.
- `title`, `subtitle` (venue; absent in some talks) and `author: "Mick Cooney <email>"`: the email varies by talk.
- `#` headings are section title slides, `##` vertical-stack parents, `###` slides inside a stack. Short decks use `#` only.
- Most slides in the long decks have no heading: bare text or a figure separated by `---`.
- Closing slide is `# Thank You!` where present (tcd); varies elsewhere.

## The `\` spacer

- A lone `\` line between two statements adds vertical space on one slide (28 uses in dds_reinsrecover, 43 in dds_survivalbayes, 9 in rr_reinsrecover).
- Pattern: text, blank line, `\`, blank line, text. Keep the spacer lines surrounded by blank lines.
- Short heading-only decks barely use it (2-5): varies.

## CSS classes (`styles.css`)

- Base rules, present in all five: left-aligned lists, `* { text-align: center; }`, `.reveal table` at 80%, `.custom-small table` (.35em), `.small-code pre code` (.7em), `.left-align`.
- `.custom-medium table` (.65em): in dds_reinsrecover and rr_reinsrecover only.
- `.tiny-code` and `.photo-credit`: dds_reinsrecover only. `.tiny-code` is defined but unused in that qmd.
- Use via fenced divs: `::: {.custom-medium}` around a table chunk, `::: {.left-align}` around prose that must be left-aligned.
- Add a new class to the talk's `styles.css` only when needed; do not edit another talk's.

## Code and figures

- Code is not shown on slides: every chunk is `#| echo: false` (no `echo: true` in any of the five). Code lives in `.qmd` setup chunks and `lib_*.R` files.
- Figures and tables are drawn at render time from code chunks (`#| fig-width`, `#| fig-height`, `#| fig-align: center`), with fixed seeds. Do not commit rendered plot PNGs.
- Committed images are only external ones in `img/` (photos, QR codes, screenshots) plus the DAG PNGs under `demo_business/`. `img/` is absent in sonia.
- Insert images with `![](img/name.jpg){fig-align="center" height="600px"}`.
- Data is read from the talk's own directory (`demo_business/`, `data/`); the `data/` directories are read-only.

## Image credits

- Credit photos with a fenced div straight after the image: `::: {.photo-credit}` / `Photo: <source>` / `:::`.
- Only dds_reinsrecover has a credited photo (Hurricane Andrew); the class is in no other talk's CSS. Add it to `styles.css` when first needed.

## Per-talk directory

- Always present: `<name>.qmd`, `styles.css`, `Justfile`, `Dockerfile`, `lib_utils.R`, `.gitignore`, `build/`, an `.Rproj`.
- `README.md`: present in all except dds_survivalbayes (varies). Format: title, "Talk given at <venue> — <date>." (date format varies), then links to source and rendered slides (links checked in dds_reinsrecover only).
- Rendered `.html` is tracked in all five; `.pdf` is tracked in dds_reinsrecover, rr_reinsrecover and dds_survivalbayes only (tcd and sonia have none).
- `.gitignore` excludes `*_files/`, `*_cache/`, `*.log`, `tmp*`.

## Rendering

- All code runs in containers via `just` and podman; nothing is installed on the host. `OCI` falls back to docker only when podman is absent.
- `just podman-build` builds the image from the talk's `Dockerfile` (`rocker/tidyverse` base).
- `just podman-render-html` runs `quarto render ... --to revealjs` in that image with the talk directory mounted.
- `just podman-render-pdf` runs `astefanutti/decktape:3.12.0` at `--size 1920x1200` on the HTML. `just podman-render-all` does both.
- `just render-talk` / `pdf-talk` (host quarto) exist in each Justfile but are not the route to use.
- Render from the talk's own directory, not the repo root.

## Root README: adding a talk

- Add a row to the matching category table; the Date column is `Month Year` with U+00A0 (non-breaking space) between month and year.
- The Link cell is `[📁 View](./talk_dir/file.html)` with U+00A0 between the folder emoji and `View`, so it stays on one line. Grep for `\xc2\xa0` in existing rows to match.
- Category tables have a Type column; the Most Recent Talks table does not (varies by table).
- Most Recent Talks is the five newest talks actually given, newest first, hand-maintained. Update it after the talk is given, not before; drop the oldest row.
- Link target varies: older rows point to the talk's `README.md`, recent ones to the rendered `.html`. Use the `.html`.
