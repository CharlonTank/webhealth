# WebHealth

An open-source website health auditor - works pre-launch or on a live site. Written in Elm + Lamdera.

Drop in a URL, get a single-page report covering meta tags, content structure, security headers, accessibility basics, social previews, and link health. Recent audits are kept in shared history.

## What it checks

- **Meta**: title, description, canonical, favicon, viewport, html lang
- **Content**: H1, heading hierarchy, image alt text
- **Technical**: HTTPS, HSTS, security headers, CSP quality, cookies, server-version disclosure, Cloudflare detection, perceived load time, render-blocking resources, compression, robots.txt, sitemap, crawl directives
- **Accessibility**: form labels, landmarks (header/nav/main/footer)
- **Social**: Open Graph basics, OG image, Twitter card, JSON-LD, PWA manifest, OG/Twitter quality
- **Links**: internal + external link health, link format

## Out of scope (for now)

Lighthouse-style runtime measurements (LCP, CLS, TBT, JS runtime errors) need a headless Chrome and aren't covered here - everything is server-side static analysis driven from Elm.

## Use it from CLI / Claude / agents

POST a URL, poll until ready. JSON response. No auth, no API key.

```bash
# Trigger the audit (first call returns "running")
curl -X POST https://webhealth.lamdera.app/_r/audit \
  -H "Content-Type: application/json" \
  -d '{"url":"https://your-site.com"}'
# {"status":"running","retry_in_seconds":8,"url":"https://your-site.com"}

# Wait ~10 seconds, repeat the same call to get the report
curl -X POST https://webhealth.lamdera.app/_r/audit \
  -H "Content-Type: application/json" \
  -d '{"url":"https://your-site.com"}'
# {"status":"ready","report":{ "score": 97, "passed": 33, "warnings": 1, "errors": 0,
#   "categories":[ {"name":"Rendering Architecture","checks":[…]},
#                  {"name":"Meta Information","checks":[…]}, … ] } }
```

One-shot polling helper:

```bash
while r=$(curl -s -X POST https://webhealth.lamdera.app/_r/audit \
  -H "Content-Type: application/json" \
  -d '{"url":"https://your-site.com"}') \
  && [ "$(echo "$r" | jq -r .status)" != "ready" ]
do sleep 8; done
echo "$r" | jq .report
```

Each individual check has the shape:

```json
{
  "id": "title-tag",
  "name": "Title Tag",
  "severity": "pass",
  "summary": "Found 44 characters. Length is optimal.",
  "affectedResources": [],
  "howToFix": null,
  "extra": []
}
```

`severity` is `"pass"`, `"warning"`, or `"error"`. Categories returned: Rendering Architecture, Meta Information, Content Structure, Technical Optimization, Accessibility Basics, Social & Rich Results, Links Analysis.

The audit fetches the page in parallel with a browser User-Agent and a Googlebot User-Agent so it can detect bot-cloaked SSR (a Cloudflare Worker that serves pre-rendered HTML to crawlers). Structural checks (h1, headings, landmarks) operate on whichever view actually has content.

> Tip: the first audit for a URL is cached. To force a fresh re-audit after deploying fixes, vary the URL with a query string like `?_t=42` and bump the number.

## Run it locally

```bash
lamdera live
```

Then open http://localhost:8000.

## Stack

- **Lamdera** (full-stack Elm) - frontend + backend in one binary
- `hecrj/html-parser` for parsing target HTML
- `elm/http` for fetching pages, robots, sitemap, favicon, and link probes

## Layout

```
src/
  Frontend.elm    - UI, router, history view
  Backend.elm     - orchestrates the audit, stores recent history
  Audit.elm       - checks (one function per check)
  HtmlQuery.elm   - small DOM query helpers
  FixPrompt.elm   - builds the LLM remediation prompt
  Types.elm       - shared model (Frontend / Backend / wire)
```

## License

MIT.
