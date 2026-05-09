# WebHealth

An open-source website health auditor — works pre-launch or on a live site. Written in Elm + Lamdera.

Drop in a URL, get a single-page report covering meta tags, content structure, security headers, accessibility basics, social previews, and link health. Recent audits are kept in shared history.

## What it checks

- **Meta**: title, description, canonical, favicon, viewport, html lang
- **Content**: H1, heading hierarchy, image alt text
- **Technical**: HTTPS, HSTS, security headers, CSP quality, cookies, server-version disclosure, Cloudflare detection, perceived load time, render-blocking resources, compression, robots.txt, sitemap, crawl directives
- **Accessibility**: form labels, landmarks (header/nav/main/footer)
- **Social**: Open Graph basics, OG image, Twitter card, JSON-LD, PWA manifest, OG/Twitter quality
- **Links**: internal + external link health, link format

## Out of scope (for now)

Lighthouse-style runtime measurements (LCP, CLS, TBT, JS runtime errors) need a headless Chrome and aren't covered here — everything is server-side static analysis driven from Elm.

## Run it

```bash
lamdera live
```

Then open http://localhost:8000.

## Stack

- **Lamdera** (full-stack Elm) — frontend + backend in one binary
- `hecrj/html-parser` for parsing target HTML
- `elm/http` for fetching pages, robots, sitemap, favicon, and link probes

## Layout

```
src/
  Frontend.elm    — UI, router, history view
  Backend.elm     — orchestrates the audit, stores recent history
  Audit.elm       — checks (one function per check)
  HtmlQuery.elm   — small DOM query helpers
  FixPrompt.elm   — builds the LLM remediation prompt
  Types.elm       — shared model (Frontend / Backend / wire)
```

## License

MIT.
