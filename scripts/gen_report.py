#!/usr/bin/env python3
"""Generate report/full_test_comparison.html from a full_matrix.csv
produced by scripts/full_test_comparison.sh (osdp's test/matrix_runner.exe
and saimcop's residual_probe.native, concatenated).

Usage: gen_report.py <input.csv> <output.html>
"""
import csv
import html
import math
import sys
from datetime import datetime, timezone


def read_rows(path):
    with open(path, newline="") as f:
        reader = csv.DictReader(f, delimiter=";")
        return list(reader)


def as_float(s):
    try:
        return float(s)
    except (TypeError, ValueError):
        return None


def fmt_residual(s):
    v = as_float(s)
    if v is None or math.isnan(v):
        return "&mdash;"
    return f"{v:.2e}"


def fmt_time(s):
    v = as_float(s)
    if v is None:
        return "&mdash;"
    return f"{v * 1000:.1f} ms"


def residual_class(s):
    v = as_float(s)
    if v is None or math.isnan(v):
        return ""
    if v <= 1e-9:
        return "res-good"
    if v <= 1e-4:
        return "res-ok"
    return "res-bad"


def row_html(r):
    success = r["success"].strip().lower() == "true"
    badge = (
        '<span class="badge badge-ok">sound</span>'
        if success
        else '<span class="badge badge-bad">unsound</span>'
    )
    rescls = residual_class(r["max_residual"])
    return f"""
      <tr data-suite="{html.escape(r['suite'])}" data-solver="{html.escape(r['solver'])}"
          data-basis="{html.escape(r['basis'])}" data-pad="{html.escape(r['pad'])}">
        <td>{html.escape(r['suite'])}</td>
        <td>{html.escape(r['solver'])}</td>
        <td>{html.escape(r['basis'])}</td>
        <td class="num">{html.escape(r['pad'])}</td>
        <td>{badge}</td>
        <td class="muted">{html.escape(r['sdpret'])}</td>
        <td class="num {rescls}">{fmt_residual(r['max_residual'])}</td>
        <td class="num">{fmt_time(r['time_sec'])}</td>
      </tr>"""


def stat_tile(label, value, tone=""):
    tone_cls = f" stat-{tone}" if tone else ""
    return f"""
      <div class="stat{tone_cls}">
        <div class="stat-value">{value}</div>
        <div class="stat-label">{label}</div>
      </div>"""


PAGE = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>OSDP solver &times; basis &times; padding matrix</title>
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
      --bg: #101513;
      --panel: #161d1a;
      --fg: #e6ece9;
      --muted: #93a39c;
      --border: #26302b;
      --row-alt: #1b2320;
      --accent: #35b3a6;
      --accent-fg: #06211e;
      --good: #4fd18b;      --good-bg: #113325;
      --bad:  #f2857a;      --bad-bg:  #3a1613;
      --warn: #e3b23c;      --warn-bg: #382a0c;
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
  .wrap {{ max-width: 1040px; margin: 0 auto; display: flex; flex-direction: column; gap: 1.5rem; }}
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
  .subtitle {{ color: var(--muted); font-size: 0.92rem; max-width: 62ch; line-height: 1.5; }}
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
  td.muted {{ color: var(--muted); }}
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
    <div class="eyebrow">osdp &middot; regression matrix</div>
    <h1>solver &times; basis &times; padding</h1>
    <div class="subtitle">
      Every combination of SDP backend, polynomial basis and padding setting,
      run against osdp's own test suite and the downstream <code>saimcop</code>
      project. Generated {generated_at} by
      <code>scripts/full_test_comparison.sh</code> (<code>make fulltest_comparison</code>).
      &ldquo;Sound&rdquo; means the solver reported success (or partial success)
      <em>and</em> the returned witness's exact coefficient residual is below 1e-4.
    </div>
  </div>

  <div class="stats">{stats}
  </div>

  <div class="panel">
    <div class="controls">
      <div class="field"><label for="f-suite">Suite</label>
        <select id="f-suite"><option value="">all</option></select></div>
      <div class="field"><label for="f-solver">Solver</label>
        <select id="f-solver"><option value="">all</option></select></div>
      <div class="field"><label for="f-basis">Basis</label>
        <select id="f-basis"><option value="">all</option></select></div>
      <div class="field"><label for="f-pad">Padding</label>
        <select id="f-pad"><option value="">all</option></select></div>
    </div>
    <div class="table-scroll">
      <table id="t">
        <thead>
          <tr>
            <th data-k="suite">Suite</th>
            <th data-k="solver">Solver</th>
            <th data-k="basis">Basis</th>
            <th data-k="pad">Pad</th>
            <th data-k="sound">Sound</th>
            <th data-k="sdpret">SDP status</th>
            <th data-k="residual">Max residual</th>
            <th data-k="time">Time</th>
          </tr>
        </thead>
        <tbody>{rows}
        </tbody>
      </table>
    </div>
  </div>

  <footer>
    Residual is an exact reconstruction: v(x)<sup>T</sup>Qv(x) is expanded in the
    classical monomial basis via each basis's rational <code>prod_to_classical</code>
    and compared coefficient-by-coefficient against the actual solved constraint
    polynomial &mdash; not a floating-point sampling estimate. Time is wall-clock
    (<code>Unix.gettimeofday</code>), so it is comparable across in-process
    (Csdp, Mosek) and subprocess-based (Sdpa, Sdpa-Gmp, Sdpa-Dd) backends alike.
  </footer>
</div>
<script>
(function () {{
  var table = document.getElementById('t');
  var tbody = table.tBodies[0];
  var rows = Array.prototype.slice.call(tbody.rows);

  function uniq(key) {{
    var s = new Set();
    rows.forEach(function (r) {{ s.add(r.dataset[key]); }});
    return Array.from(s).sort();
  }}
  function fillSelect(id, key) {{
    var sel = document.getElementById(id);
    uniq(key).forEach(function (v) {{
      var o = document.createElement('option');
      o.value = v; o.textContent = v;
      sel.appendChild(o);
    }});
  }}
  fillSelect('f-suite', 'suite');
  fillSelect('f-solver', 'solver');
  fillSelect('f-basis', 'basis');
  fillSelect('f-pad', 'pad');

  function applyFilter() {{
    var fs = document.getElementById('f-suite').value;
    var fo = document.getElementById('f-solver').value;
    var fb = document.getElementById('f-basis').value;
    var fp = document.getElementById('f-pad').value;
    rows.forEach(function (r) {{
      var show = (!fs || r.dataset.suite === fs)
        && (!fo || r.dataset.solver === fo)
        && (!fb || r.dataset.basis === fb)
        && (!fp || r.dataset.pad === fp);
      r.style.display = show ? '' : 'none';
    }});
  }}
  ['f-suite', 'f-solver', 'f-basis', 'f-pad'].forEach(function (id) {{
    document.getElementById(id).addEventListener('change', applyFilter);
  }});

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
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        sys.exit(1)
    in_csv, out_html = sys.argv[1], sys.argv[2]
    rows = read_rows(in_csv)

    total = len(rows)
    sound = sum(1 for r in rows if r["success"].strip().lower() == "true")
    unsound = total - sound
    residuals = [v for v in (as_float(r["max_residual"]) for r in rows)
                 if v is not None and not math.isnan(v)]
    worst = max(residuals) if residuals else float("nan")
    solvers = sorted({r["solver"] for r in rows})
    bases = sorted({r["basis"] for r in rows})

    stats = "".join([
        stat_tile("Configurations", total),
        stat_tile("Sound", sound, tone="good"),
        stat_tile("Unsound", unsound, tone=("bad" if unsound else "")),
        stat_tile("Worst residual", fmt_residual(str(worst)), tone="accent"),
        stat_tile("Solvers", len(solvers)),
        stat_tile("Bases", len(bases)),
    ])

    rows_html = "".join(row_html(r) for r in rows)
    generated_at = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    page = PAGE.format(rows=rows_html, stats=stats, generated_at=generated_at)
    with open(out_html, "w") as f:
        f.write(page)
    print(f"wrote {out_html} ({total} rows, {sound} sound, {unsound} unsound)")


if __name__ == "__main__":
    main()
