#!/usr/bin/env python3
"""Render an accumulated osdp.trace log (JSON Lines, see Osdp_trace.record)
as a self-contained, filterable/sortable HTML report.

Schema-agnostic: the set of columns is whatever keys are present across
the rows of the input file(s), a union not a fixed layout -- any project
logging to the trace file can add its own columns (an example name, a
box, a confidence level, ...) alongside the common ones (solver, basis,
pad, ...) and they render as one extra column, no code change needed here.
A few well-known column names get extra treatment when present: "success"
(a sound/unsound badge), any column whose name contains "residual"
(magnitude-coloured, scientific notation), any column whose name contains
"time" (rendered in milliseconds when numeric).

Usage: gen_report.py <input.jsonl> [<input2.jsonl> ...] <output.html>
"""
import html
import json
import math
import sys
from datetime import datetime, timezone

PREFERRED_COLUMNS = [
    "timestamp", "suite", "solver", "basis", "pad",
    "success", "sdpret", "max_residual", "time_sec",
]


def read_rows(paths):
    rows = []
    for path in paths:
        with open(path) as f:
            for lineno, line in enumerate(f, 1):
                line = line.strip()
                if not line:
                    continue
                try:
                    obj = json.loads(line)
                except json.JSONDecodeError as e:
                    print(f"{path}:{lineno}: skipping malformed line: {e}", file=sys.stderr)
                    continue
                rows.append({k: "" if v is None else str(v) for k, v in obj.items()})
    return rows


def column_order(rows):
    all_keys = set()
    for r in rows:
        all_keys.update(r.keys())
    ordered = [c for c in PREFERRED_COLUMNS if c in all_keys]
    remaining = sorted(all_keys - set(ordered))
    return ordered + remaining


def as_float(s):
    try:
        v = float(s)
        return None if math.isnan(v) else v
    except (TypeError, ValueError):
        return None


def is_residual_col(col):
    return "residual" in col.lower()


def is_time_col(col):
    return "time" in col.lower()


def is_bool_str(s):
    return s.strip().lower() in ("true", "false")


def fmt_cell(col, value):
    if col == "success":
        ok = value.strip().lower() == "true"
        cls = "badge-ok" if ok else "badge-bad"
        label = "sound" if ok else "unsound"
        return f'<span class="badge {cls}">{label}</span>', ""
    if is_residual_col(col):
        v = as_float(value)
        if v is None:
            return "&mdash;" if value == "" else html.escape(value), ""
        cls = "res-good" if v <= 1e-9 else "res-ok" if v <= 1e-4 else "res-bad"
        return f"{v:.2e}", f"num {cls}"
    if is_time_col(col):
        v = as_float(value)
        if v is None:
            return html.escape(value), ""
        return f"{v * 1000:.1f} ms", "num"
    v = as_float(value)
    if v is not None and not is_bool_str(value):
        return html.escape(value), "num"
    return html.escape(value), ""


def row_html(row, columns):
    data_attrs = "".join(
        f' data-{c}="{html.escape(row.get(c, ""))}"'
        for c in ("suite", "solver", "basis", "pad") if c in columns
    )
    cells = []
    for c in columns:
        text, cls = fmt_cell(c, row.get(c, ""))
        cls_attr = f' class="{cls}"' if cls else ""
        cells.append(f"<td{cls_attr}>{text}</td>")
    return f"      <tr{data_attrs}>\n        " + "\n        ".join(cells) + "\n      </tr>"


def stat_tile(label, value, tone=""):
    tone_cls = f" stat-{tone}" if tone else ""
    return f"""
      <div class="stat{tone_cls}">
        <div class="stat-value">{value}</div>
        <div class="stat-label">{label}</div>
      </div>"""


def build_stats(rows):
    tiles = [stat_tile("Rows", len(rows))]
    if any("success" in r for r in rows):
        sound = sum(1 for r in rows if r.get("success", "").strip().lower() == "true")
        unsound = sum(1 for r in rows if "success" in r) - sound
        tiles.append(stat_tile("Sound", sound, tone="good"))
        tiles.append(stat_tile("Unsound", unsound, tone=("bad" if unsound else "")))
    residual_cols = set()
    for r in rows:
        residual_cols.update(c for c in r if is_residual_col(c))
    residuals = [v for v in (as_float(r.get(c)) for r in rows for c in residual_cols) if v is not None]
    if residuals:
        tiles.append(stat_tile("Worst residual", f"{max(residuals):.2e}", tone="accent"))
    for label, key in (("Suites", "suite"), ("Solvers", "solver"), ("Bases", "basis")):
        vals = {r[key] for r in rows if key in r}
        if vals:
            tiles.append(stat_tile(label, len(vals)))
    return "".join(tiles)


PAGE = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>OSDP trace report</title>
<style>
  :root {{
    color-scheme: light dark;
    --bg: #f6f7f5;
    --panel: #ffffff;
    --fg: #16201d;
    --muted: #5c6b64;
    --border: #dde2dd;
    --row-alt: #eef1ee;
    --accent: #0f6d66;
    --accent-fg: #eafbf8;
    --good: #1f7a4d;      --good-bg: #e3f5ea;
    --bad:  #ae2c22;      --bad-bg:  #fbe8e6;
    --warn: #96650a;      --warn-bg: #faf0d9;
    --shadow: 0 1px 2px rgba(20, 30, 27, 0.06), 0 8px 24px rgba(20, 30, 27, 0.05);
  }}
  @media (prefers-color-scheme: dark) {{
    :root {{
      --bg: #101513; --panel: #161d1a; --fg: #e6ece9; --muted: #93a39c;
      --border: #26302b; --row-alt: #1b2320;
      --accent: #35b3a6; --accent-fg: #06211e;
      --good: #4fd18b; --good-bg: #113325;
      --bad: #f2857a; --bad-bg: #3a1613;
      --warn: #e3b23c; --warn-bg: #382a0c;
      --shadow: 0 1px 2px rgba(0,0,0,0.3), 0 8px 24px rgba(0,0,0,0.35);
    }}
  }}
  :root[data-theme="dark"] {{
    --bg: #101513; --panel: #161d1a; --fg: #e6ece9; --muted: #93a39c;
    --border: #26302b; --row-alt: #1b2320;
    --accent: #35b3a6; --accent-fg: #06211e;
    --good: #4fd18b; --good-bg: #113325;
    --bad: #f2857a; --bad-bg: #3a1613;
    --warn: #e3b23c; --warn-bg: #382a0c;
    --shadow: 0 1px 2px rgba(0,0,0,0.3), 0 8px 24px rgba(0,0,0,0.35);
  }}
  :root[data-theme="light"] {{
    --bg: #f6f7f5; --panel: #ffffff; --fg: #16201d; --muted: #5c6b64;
    --border: #dde2dd; --row-alt: #eef1ee;
    --accent: #0f6d66; --accent-fg: #eafbf8;
    --good: #1f7a4d; --good-bg: #e3f5ea;
    --bad: #ae2c22; --bad-bg: #fbe8e6;
    --warn: #96650a; --warn-bg: #faf0d9;
    --shadow: 0 1px 2px rgba(20, 30, 27, 0.06), 0 8px 24px rgba(20, 30, 27, 0.05);
  }}
  * {{ box-sizing: border-box; }}
  html, body {{ overflow-x: hidden; }}
  body {{
    background: var(--bg); color: var(--fg);
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
    margin: 0; padding: 2.5rem 1.25rem 4rem;
    -webkit-font-smoothing: antialiased;
  }}
  .wrap {{ max-width: 1080px; margin: 0 auto; display: flex; flex-direction: column; gap: 1.5rem; }}
  .masthead {{ display: flex; flex-direction: column; gap: 0.5rem; }}
  .eyebrow {{
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase;
    color: var(--accent); font-weight: 600;
  }}
  h1 {{
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    font-size: 1.55rem; font-weight: 600; margin: 0; text-wrap: balance;
    letter-spacing: -0.01em;
  }}
  .subtitle {{ color: var(--muted); font-size: 0.92rem; max-width: 68ch; line-height: 1.5; }}
  .subtitle code {{
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    background: var(--row-alt); padding: 0.05rem 0.35rem; border-radius: 4px; font-size: 0.85em;
  }}

  .stats {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(9.5rem, 1fr)); gap: 0.75rem; }}
  .stat {{
    background: var(--panel); border: 1px solid var(--border); border-radius: 10px;
    padding: 0.9rem 1rem; box-shadow: var(--shadow);
  }}
  .stat-value {{
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
    font-variant-numeric: tabular-nums;
    font-size: 1.5rem; font-weight: 600; line-height: 1.1;
  }}
  .stat-label {{ color: var(--muted); font-size: 0.76rem; margin-top: 0.3rem; text-transform: uppercase; letter-spacing: 0.03em; }}
  .stat-good .stat-value {{ color: var(--good); }}
  .stat-bad .stat-value {{ color: var(--bad); }}
  .stat-accent .stat-value {{ color: var(--accent); }}

  .panel {{
    background: var(--panel); border: 1px solid var(--border); border-radius: 12px;
    box-shadow: var(--shadow); overflow: hidden;
  }}
  .controls {{
    display: flex; gap: 1.1rem; flex-wrap: wrap; align-items: center;
    padding: 0.85rem 1.1rem; border-bottom: 1px solid var(--border);
  }}
  .controls .field {{ display: flex; flex-direction: column; gap: 0.2rem; }}
  .controls label {{
    color: var(--muted); font-size: 0.68rem; text-transform: uppercase; letter-spacing: 0.05em;
  }}
  select {{
    background: var(--bg); color: var(--fg); border: 1px solid var(--border);
    border-radius: 6px; padding: 0.3rem 0.5rem; font-size: 0.85rem;
    font-family: inherit;
  }}
  select:focus-visible, th:focus-visible {{ outline: 2px solid var(--accent); outline-offset: 1px; }}

  .table-scroll {{ overflow-x: auto; }}
  table {{ width: 100%; border-collapse: collapse; font-size: 0.84rem; min-width: 720px; }}
  th, td {{ padding: 0.5rem 0.9rem; border-bottom: 1px solid var(--border); text-align: left; white-space: nowrap; }}
  th {{
    position: sticky; top: 0; background: var(--panel); cursor: pointer; user-select: none;
    color: var(--muted); font-weight: 600; font-size: 0.72rem; text-transform: uppercase;
    letter-spacing: 0.04em; border-bottom: 1px solid var(--border);
  }}
  th:hover {{ color: var(--accent); }}
  th::after {{ content: "\\2195"; opacity: 0.35; margin-left: 0.35rem; font-size: 0.7em; }}
  tbody tr:nth-child(even) {{ background: var(--row-alt); }}
  tbody tr:hover {{ background: var(--good-bg); }}
  td.num {{
    font-variant-numeric: tabular-nums;
    font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
  }}
  .res-good {{ color: var(--good); }}
  .res-ok {{ color: var(--warn); }}
  .res-bad {{ color: var(--bad); font-weight: 700; }}
  .badge {{
    display: inline-block; padding: 0.15rem 0.55rem; border-radius: 999px;
    font-size: 0.72rem; font-weight: 700; letter-spacing: 0.01em;
  }}
  .badge-ok {{ color: var(--good); background: var(--good-bg); }}
  .badge-bad {{ color: var(--bad); background: var(--bad-bg); }}

  footer {{ color: var(--muted); font-size: 0.78rem; line-height: 1.6; max-width: 68ch; }}
  footer code {{ font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace; }}
</style>
</head>
<body>
<div class="wrap">
  <div class="masthead">
    <div class="eyebrow">osdp.trace &middot; accumulated log</div>
    <h1>osdp trace report</h1>
    <div class="subtitle">
      {nb_sources} source log(s), {nb_rows} accumulated rows. Generated {generated_at}.
      Columns are whatever fields the logging project chose to record
      (see <code>Osdp_trace.record</code>) &mdash; this report doesn't assume a
      fixed schema, so a downstream project's own context columns show up
      automatically alongside osdp's.
    </div>
  </div>

  <div class="stats">{stats}
  </div>

  <div class="panel">
    <div class="controls">{filters}
    </div>
    <div class="table-scroll">
      <table id="t">
        <thead>
          <tr>{header_cells}
          </tr>
        </thead>
        <tbody>
{rows}
        </tbody>
      </table>
    </div>
  </div>

  <footer>
    Rows accumulate across runs and across whichever projects log to this
    file; nothing is ever rewritten in place, so this report reflects the
    full history in the source log(s), not just the latest run.
  </footer>
</div>
<script>
(function () {{
  var table = document.getElementById('t');
  var tbody = table.tBodies[0];
  var rows = Array.prototype.slice.call(tbody.rows);

  ['suite', 'solver', 'basis', 'pad'].forEach(function (key) {{
    var sel = document.getElementById('f-' + key);
    if (!sel) return;
    var vals = new Set();
    rows.forEach(function (r) {{ if (r.dataset[key] !== undefined) vals.add(r.dataset[key]); }});
    Array.from(vals).sort().forEach(function (v) {{
      var o = document.createElement('option');
      o.value = v; o.textContent = v;
      sel.appendChild(o);
    }});
    sel.addEventListener('change', applyFilter);
  }});

  function applyFilter() {{
    var filters = {{}};
    ['suite', 'solver', 'basis', 'pad'].forEach(function (key) {{
      var sel = document.getElementById('f-' + key);
      if (sel) filters[key] = sel.value;
    }});
    rows.forEach(function (r) {{
      var show = Object.keys(filters).every(function (k) {{
        return !filters[k] || r.dataset[k] === filters[k];
      }});
      r.style.display = show ? '' : 'none';
    }});
  }}

  var sortDir = {{}};
  table.tHead.querySelectorAll('th').forEach(function (th, idx) {{
    th.setAttribute('tabindex', '0');
    function doSort() {{
      var k = th.dataset.k;
      var dir = sortDir[k] = -(sortDir[k] || -1);
      var sorted = rows.slice().sort(function (a, b) {{
        var av = a.cells[idx].textContent.trim();
        var bv = b.cells[idx].textContent.trim();
        var an = parseFloat(av), bn = parseFloat(bv);
        var cmp = (!isNaN(an) && !isNaN(bn)) ? (an - bn) : av.localeCompare(bv);
        return cmp * dir;
      }});
      sorted.forEach(function (r) {{ tbody.appendChild(r); }});
    }}
    th.addEventListener('click', doSort);
    th.addEventListener('keydown', function (e) {{ if (e.key === 'Enter') doSort(); }});
  }});
}})();
</script>
</body>
</html>
"""


def main():
    if len(sys.argv) < 3:
        print(__doc__, file=sys.stderr)
        sys.exit(1)
    *in_paths, out_html = sys.argv[1:]
    rows = read_rows(in_paths)
    columns = column_order(rows)

    filters = "".join(
        f"""
      <div class="field"><label for="f-{c}">{c.capitalize()}</label>
        <select id="f-{c}"><option value="">all</option></select></div>"""
        for c in ("suite", "solver", "basis", "pad") if c in columns
    )
    header_cells = "".join(f'\n            <th data-k="{c}">{html.escape(c)}</th>' for c in columns)
    rows_html = "\n".join(row_html(r, columns) for r in rows)
    stats = build_stats(rows)
    generated_at = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    page = PAGE.format(
        rows=rows_html, stats=stats, filters=filters, header_cells=header_cells,
        generated_at=generated_at, nb_rows=len(rows), nb_sources=len(in_paths),
    )
    with open(out_html, "w") as f:
        f.write(page)
    print(f"wrote {out_html} ({len(rows)} rows from {len(in_paths)} source(s), {len(columns)} columns)")


if __name__ == "__main__":
    main()
