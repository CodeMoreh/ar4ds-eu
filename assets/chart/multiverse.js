/*
  Interactive specification curve for the multiverse page of a case.

  mvChart(id, D) draws one curve into the element with that id, which carries
  the markup of assets/chart/multiverse.template.html. D is the payload built
  by mv_payload() in the chart script of the case (R/<slug>/_chart_multiverse.R,
  for example R/housing/_chart_multiverse.R): one entry per specification,
  sorted by its value on the vertical axis, with the option of every
  decision coded as an index into its list of labels, and the reference marks
  with the lines of their pop-ups. Several charts can share a page; each keeps
  its own state.

  The vertical value is D.r, whatever its scale. Optional fields of the payload
  adapt the chart to a curve on another scale or of another size, and each
  falls back to the behaviour of the EU-frames curve (a partial correlation,
  hundreds of specifications) when it is absent:
    yLabel    the label of the vertical axis
    yDec      decimals of the tick labels (1)
    vfmt      {pre, suf, d}: how a value is printed in a pop-up ("r = " and 3 decimals)
    sl        one line of statistics per specification (t, df and bootstrap p)
    det       per specification, the [label, value, class] lines of its pop-up
              (the lines of the EU-frames decisions)
    tagGap    the height of a reference tag above its mark, in units of D.r (0.12)
    xStep     the spacing of the rank ticks (200; doubled on a narrow screen)
    xPad      an inset in pixels between the axes and the first and last marks (0)
    size      [narrow, wide] size of a mark ([4, 5])
    sw        stroke width of a mark ("0.7")
    op        outline opacities {sig, ns, ghostSig, ghostNs} (OP below)
    csvHead   the first six column names of the downloaded CSV
    up        true when values above zero lie in the direction of the claim
              (a coefficient that the claim expects to be positive): the
              shaded band then lies below zero, and the notes on the chart and
              the status line under "Show only" count from above zero. The
              payload sorts the specifications from the largest value down,
              so that the strongest support still sits on the left (false)

  Conventions set by the author of the course: one ranking for all
  specifications; two significance classes at the 5% level (an open circle for
  p < 0.05, a cross for p >= 0.05, at the same footprint); translucent thin
  outlines; reference marks in ink with a thin line to a tag that holds the
  pop-up open when clicked; the mark under the pointer redrawn in burgundy.
*/
(function () {
  "use strict";

  var NS = "http://www.w3.org/2000/svg";
  var MINUS = "−";
  // HILITE marks the specification under the pointer: a burgundy that matches
  // no colour of the categorical palette, so it cannot be read as an option
  var PLAIN = "#1f5fa8", INK = "#1d2733", GHOST = "#b8bfc6", HILITE = "#8b1538";
  var CAT = ["#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9"];
  // outline opacity: p < 0.05, p >= 0.05, and the same two when "Show only"
  // hides a mark; overlapping outlines add up, so even the strongest stays
  // well short of opaque
  var OP = { sig: "0.25", ns: "0.12", ghostSig: "0.1", ghostNs: "0.05" };

  function el(name, attrs, parent) {
    var e = document.createElementNS(NS, name);
    for (var k in attrs) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return (x < 0 ? MINUS : "") + Math.abs(x).toFixed(d); }
  function fmtP(p) { return p < 0.001 ? "p < 0.001" : "p = " + p.toFixed(3); }
  function pct(x) { return Math.round(100 * x) + "%"; }
  function num(x) { return x.toLocaleString("en-GB"); }
  function circlePath(x, y, r) {
    return "M" + (x - r) + "," + y + "a" + r + "," + r + " 0 1,0 " + (2 * r) + ",0a" + r + "," + r + " 0 1,0 " + (-2 * r) + ",0Z";
  }
  function crossPath(x, y, h) {
    return "M" + (x - h) + "," + (y - h) + "L" + (x + h) + "," + (y + h) + "M" + (x + h) + "," + (y - h) + "L" + (x - h) + "," + (y + h);
  }
  // an open circle for p < 0.05 and a cross for p >= 0.05, both 8 pixels
  // across at desktop width, so that only the shape differs
  function markPath(cls, x, y, s) {
    return cls === 1 ? circlePath(x, y, s * 0.8) : crossPath(x, y, s * 0.8);
  }
  function paint(path, col, op) {
    path.setAttribute("stroke", col);
    path.setAttribute("fill", "none");
    path.setAttribute("stroke-opacity", op);
  }
  function option(parent, value, text) {
    var o = document.createElement("option");
    o.value = String(value);
    o.textContent = text;
    parent.appendChild(o);
  }

  window.mvChart = function (id, D) {
    var root = document.getElementById(id);
    if (!root || root.getAttribute("data-mv-ready")) return;
    root.setAttribute("data-mv-ready", "1");

    var N = D.r.length;
    // what a value on the supporting side of zero supports: the claim, or the
    // hypothesis on the negative scales; below zero unless the payload sets up
    var target = D.target || "claim";
    var up = D.up === true;
    function supports(v) { return up ? v > 0 : v < 0; }
    var op = D.op || OP;
    var size = D.size || [4.0, 5.0];
    var yDec = D.yDec == null ? 1 : D.yDec;
    function tick(v) { return Math.abs(v) < 1e-9 ? "0" : fmt(v, yDec); }
    function val(x) {
      return D.vfmt ? (D.vfmt.pre || "") + fmt(x, D.vfmt.d) + (D.vfmt.suf || "") : "r = " + fmt(x, 3);
    }
    var marks = D.an || [];
    var plot = root.querySelector(".mv-plot");
    var tip = root.querySelector(".mv-tip");
    var legend = root.querySelector(".mv-legend");
    var status = root.querySelector(".mv-status");
    var sel = root.querySelector(".mv-sel");
    var fax = root.querySelector(".mv-fax");
    var flv = root.querySelector(".mv-flv");
    var dl = root.querySelector(".mv-dl");

    var svg = document.createElementNS(NS, "svg");
    svg.setAttribute("tabindex", "0");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", D.summary);
    plot.insertBefore(svg, tip);

    D.axes.forEach(function (a, i) {
      option(sel, i, a.name);
      option(fax, i, a.key ? a.name : "All specifications");
    });
    flv.style.display = "none";

    // axis: the decision that colours the marks; faxis/flevel: the decision
    // and option kept by "Show only" (faxis 0 = everything); cur: the
    // specification under the keyboard cursor; pinned: a reference mark whose
    // details are held open
    var state = { axis: 0, faxis: 0, flevel: 0, cur: -1, pinned: null, W: 0, H: 0, s: 4, px: [], py: [], marks: [], ax: [], ay: [], tags: [] };

    function axisOf() { return D.axes[state.axis]; }
    function colourOf(i) {
      var a = axisOf();
      return a.key ? CAT[D.code[a.key][i] % CAT.length] : PLAIN;
    }
    function shown(i) {
      var a = D.axes[state.faxis];
      return !a.key || D.code[a.key][i] === state.flevel;
    }

    function draw() {
      var W = Math.max(280, Math.round(plot.clientWidth || root.clientWidth || 640));
      var narrow = W < 560;
      var axis = axisOf();
      var hasStrip = !!axis.key;
      var rowH = 20, stripTop = (narrow ? 300 : 390) + 6;
      var H = stripTop + (hasStrip ? 18 + axis.levels.length * rowH + 30 : 24);
      state.W = W; state.H = H;
      var m = { l: narrow ? 46 : 54, r: 10, t: 12, b: H - stripTop + 28 };
      var Y_MIN = D.y.min, Y_MAX = D.y.max;
      var pad = D.xPad || 0;
      var xs = function (i) { return m.l + pad + (N > 1 ? i / (N - 1) : 0.5) * (W - m.l - m.r - 2 * pad); };
      var ys = function (v) { return m.t + (Y_MAX - v) / (Y_MAX - Y_MIN) * (H - m.t - m.b); };

      while (svg.firstChild) svg.removeChild(svg.firstChild);
      svg.setAttribute("viewBox", "0 0 " + W + " " + H);
      svg.setAttribute("height", String(H));

      // the band of the specifications that run against the claim: above
      // zero, or below it when the payload sets up
      var bandTop = up ? ys(0) : ys(Y_MAX), bandBottom = up ? ys(Y_MIN) : ys(0);
      el("rect", { x: m.l, y: bandTop, width: W - m.l - m.r, height: Math.max(0, bandBottom - bandTop), fill: "#a10000", "fill-opacity": "0.045" }, svg);
      var g = el("g", { "font-size": "11", fill: "#6b7682" }, svg);
      D.y.ticks.forEach(function (v) {
        var zero = Math.abs(v) < 1e-9;
        el("line", { x1: m.l, x2: W - m.r, y1: ys(v), y2: ys(v), stroke: zero ? "#8a929a" : "#e6e9ed", "stroke-width": zero ? "1.2" : "1" }, g);
        var t = el("text", { x: m.l - 6, y: ys(v) + 3.5, "text-anchor": "end" }, g);
        t.textContent = tick(v);
      });
      var xStep = D.xStep || 200;
      var step = narrow ? 2 * xStep : xStep, xt = [1];
      for (var v = step; v <= N - step / 3; v += step) xt.push(v);
      xt.push(N);
      xt.forEach(function (v) {
        var t = el("text", { x: xs(v - 1), y: H - m.b + 15, "text-anchor": v === 1 ? "start" : (v === N ? "end" : "middle") }, g);
        t.textContent = num(v);
      });
      var xl = el("text", { x: m.l + (W - m.l - m.r) / 2, y: H - 6, "text-anchor": "middle", "font-size": "12", fill: "#4f5b67" }, svg);
      xl.textContent = narrow ? "Specifications, ranked by result" : "Specifications, ranked from the strongest support for the " + target + " (left) to the weakest (right)";
      var yl = el("text", { transform: "translate(13," + (m.t + (H - m.t - m.b) / 2) + ") rotate(-90)", "text-anchor": "middle", "font-size": "12", fill: "#4f5b67" }, svg);
      yl.textContent = D.yLabel || "Partial correlation, claim-aligned";

      var ann = el("g", { "font-size": "11.5", fill: "#4f5b67" }, svg);
      var a1 = el("text", { x: m.l + 8, y: ys(Y_MAX) + 14 }, ann);
      a1.textContent = up
        ? "above zero: in the direction of the " + target
        : (narrow ? "above zero: against the " + target : "above zero: runs against the " + target);
      var a2 = el("text", { x: W - m.r - 8, y: ys(Y_MIN) - 6, "text-anchor": "end" }, ann);
      a2.textContent = up ? "below zero: runs against the " + target : "below zero: in the direction of the " + target;

      var pts = el("g", { "stroke-width": D.sw || "0.7" }, svg);
      var s = narrow ? size[0] : size[1];
      state.s = s;
      state.xs = xs; state.ys = ys;
      state.px = new Array(N); state.py = new Array(N); state.marks = new Array(N);
      for (var i = 0; i < N; i++) {
        var x = xs(i), y = ys(D.r[i]);
        state.px[i] = x; state.py[i] = y;
        state.marks[i] = el("path", { d: markPath(D.sg[i], x, y, s) }, pts);
      }

      // beneath the curve, one row per option of the decision chosen in
      // "Colour by", with a tick at the rank of every specification that uses
      // it: where the ticks of one row bunch, that option pushes results to
      // that part of the curve
      if (hasStrip) {
        var strip = el("g", {}, svg);
        var head = el("text", { x: m.l, y: stripTop + 11, "font-size": "11.5", "font-weight": "650", fill: "#4f5b67" }, strip);
        head.textContent = axis.name;
        axis.levels.forEach(function (lab, j) {
          var top = stripTop + 18 + j * rowH;
          el("rect", { x: m.l, y: top + 9, width: W - m.l - m.r, height: 9, fill: "#f3f5f7" }, strip);
          var dpath = "";
          for (var i = 0; i < N; i++) if (D.code[axis.key][i] === j) dpath += "M" + xs(i).toFixed(1) + "," + (top + 9) + "v9";
          el("path", { d: dpath, stroke: CAT[j % CAT.length], "stroke-width": "1", "stroke-opacity": "0.55" }, strip);
          var t = el("text", { x: m.l + 2, y: top + 7, "font-size": "10.5", fill: "#4f5b67" }, strip);
          t.textContent = lab;
        });
      }

      // the reference marks, each an ordinary mark in ink at its place on the
      // curve, with a thin line up to a clickable tag; tags are placed in rank
      // order, and one that would overlap a neighbour is pushed up until it
      // clears it
      var refBase = el("g", {}, svg);
      var refTags = el("g", {}, svg);
      state.ax = []; state.ay = []; state.tags = [];
      var order = marks.map(function (a, k) { return k; }).sort(function (p, q) { return marks[p].rank - marks[q].rank; });
      var placed = [];
      order.forEach(function (k) {
        var a = marks[k];
        var x = xs(Math.min(N - 1, a.rank)), y = ys(a.r);
        state.ax[k] = x; state.ay[k] = y;
        var tg = el("g", { "class": "mv-tag", "font-size": "11", "font-weight": "500", tabindex: "0", role: "button" }, refTags);
        tg.setAttribute("aria-label", "Show the details of " + a.title);
        var tx = el("text", { x: x, y: 0, "text-anchor": "middle", fill: INK }, tg);
        tx.textContent = a.label;
        var bw = (tx.getComputedTextLength ? tx.getComputedTextLength() : 0) || a.label.length * 6.4;
        var left = Math.max(m.l + 2, Math.min(x - bw / 2 - 5, W - m.r - bw - 12));
        var right = left + bw + 10;
        var ty = ys(a.r + (D.tagGap || 0.12)), moved = true, guard = 0;
        while (moved && guard++ < 10) {
          moved = false;
          placed.forEach(function (p) {
            if (left < p.right + 6 && right > p.left - 6 && Math.abs(ty - p.ty) < 21) { ty = p.ty - 21; moved = true; }
          });
        }
        placed.push({ left: left, right: right, ty: ty });
        el("line", { x1: x, x2: x, y1: y - s - 1, y2: ty + 9, stroke: "#9aa3ab", "stroke-width": "0.7" }, refBase);
        paint(el("path", { d: markPath(a.sg, x, y, s), "stroke-width": "1.2" }, refBase), INK, "1");
        tx.setAttribute("x", String(left + 5 + bw / 2));
        tx.setAttribute("y", String(ty + 4));
        var bg = el("rect", { x: left, y: ty - 8, width: bw + 10, height: 17, rx: 4, fill: "#ffffff", "fill-opacity": "0.9", stroke: "#b9c0c8", "stroke-width": "0.8" });
        tg.insertBefore(bg, tx);
        state.tags[k] = tg;
        tg.addEventListener("click", function (evt) { evt.stopPropagation(); togglePin(k); });
        tg.addEventListener("keydown", function (evt) {
          if (evt.key === "Enter" || evt.key === " ") { evt.preventDefault(); togglePin(k); }
        });
      });

      // the mark under the pointer, redrawn above the curve in burgundy over a
      // white halo, with a ring around it
      state.halo = el("path", { fill: "none", stroke: "#ffffff", "stroke-width": "4", "stroke-opacity": "0.9", "stroke-linecap": "round", visibility: "hidden" }, svg);
      state.hl = el("path", { fill: "none", stroke: HILITE, "stroke-width": "1.8", "stroke-linecap": "round", visibility: "hidden" }, svg);
      state.ring = el("circle", { r: 9, fill: "none", stroke: INK, "stroke-width": "1.6", visibility: "hidden" }, svg);
      if (state.pinned !== null) state.tags[state.pinned].classList.add("on");
      recolour();
      if (state.pinned !== null) show("an", state.pinned); else hide();
    }

    function recolour() {
      var i, nShown = 0, below = 0, belowSig = 0;
      for (i = 0; i < N; i++) {
        if (shown(i)) {
          paint(state.marks[i], colourOf(i), D.sg[i] ? op.sig : op.ns);
          nShown++;
          if (supports(D.r[i])) { below++; if (D.sg[i]) belowSig++; }
        } else {
          // faint enough that the hidden specifications only trace the curve
          paint(state.marks[i], GHOST, D.sg[i] ? op.ghostSig : op.ghostNs);
        }
      }
      var f = D.axes[state.faxis];
      status.textContent = f.key
        ? num(nShown) + " of " + num(N) + " specifications shown (" + f.name.toLowerCase() + ": " + f.levels[state.flevel] + "): " +
          pct(below / nShown) + (up ? " lie above zero" : " fall below zero") + ", in the direction of the " + target + ", and " + pct(belowSig / nShown) + " do so with p < 0.05."
        : "";
      var a = axisOf();
      while (legend.firstChild) legend.removeChild(legend.firstChild);
      if (a.key) a.levels.forEach(function (lab, j) { legend.appendChild(key("swatch", CAT[j % CAT.length], lab)); });
      legend.appendChild(key(1, "#4f5b67", "p < 0.05"));
      legend.appendChild(key(0, "#4f5b67", "p ≥ 0.05"));
      if (f.key) legend.appendChild(key("ghost", GHOST, "not shown"));
    }

    // the colour key is a small square, so that it cannot be read as a shape
    function key(kind, col, lab) {
      var span = document.createElement("span");
      span.className = "mv-key";
      var k = document.createElementNS(NS, "svg");
      k.setAttribute("viewBox", "0 0 15 15");
      k.setAttribute("aria-hidden", "true");
      if (kind === "swatch") el("rect", { x: 3, y: 3, width: 9, height: 9, rx: 2, fill: col }, k);
      else if (kind === "ghost") paint(el("path", { d: markPath(1, 7.5, 7.5, 5.6), "stroke-width": "1.2" }, k), col, "0.4");
      else paint(el("path", { d: markPath(kind, 7.5, 7.5, 5.6), "stroke-width": "1.2" }, k), col, kind === 0 ? "0.55" : "1");
      span.appendChild(k);
      span.appendChild(document.createTextNode(lab));
      return span;
    }

    function line(k, v, cls) {
      var d = document.createElement("div");
      if (cls) d.className = cls;
      if (k) {
        var s = document.createElement("span");
        s.className = "k";
        s.textContent = k + ": ";
        d.appendChild(s);
      }
      d.appendChild(document.createTextNode(v));
      tip.appendChild(d);
    }

    function lab(k, i) { return D.lv[k][D.code[k][i]]; }

    function fillTip(kind, i) {
      while (tip.firstChild) tip.removeChild(tip.firstChild);
      if (kind === "an") {
        var a = marks[i];
        line("", val(a.r), "v");
        line("", a.stat, "s");
        line("", a.title, "t");
        a.lines.forEach(function (p) { line(p[0], p[1]); });
        a.notes.forEach(function (n) { line("", n, "n"); });
        line("", state.pinned === i ? "Click the tag again, or press Escape, to release." : "Click the tag to hold this open.", "h");
        return;
      }
      line("", val(D.r[i]), "v");
      line("", D.sl ? D.sl[i] : "t = " + fmt(D.st[i], 2) + " on " + D.df[i] + " df · bootstrap " + fmtP(D.p[i]), "s");
      if (i === D.ref) line("", D.refText, "t");
      if (D.det) {
        D.det[i].forEach(function (p) { line(p[0], p[1], p[2]); });
        line("Rank", num(i + 1) + " of " + num(N));
        return;
      }
      line("Outcome", lab("o", i) + (D.reversed ? " (reverse-coded)" : ""));
      var pred = lab("x", i);
      if (D.code.x[i] === 0) pred += ", " + lab("pf", i).toLowerCase();
      line("Predictor", pred);
      line("Series", lab("ms", i));
      line("Adjustment", D.code.c[i] === 0 ? lab("c", i) : D.adjOther[D.code.x[i]]);
      line("Estimator", lab("e", i));
      line("", D.gloss[D.code.e[i]], "n");
      line("No-opinion rule", lab("nr", i));
      line("Item set", lab("it", i));
      line("Cell weighting", lab("cw", i));
      line("Rank", num(i + 1) + " of " + num(N));
    }

    function place(x, y) {
      tip.classList.add("on");
      var tw = tip.offsetWidth, th = tip.offsetHeight;
      var left = x + 14 + tw > state.W ? x - tw - 14 : x + 14;
      left = Math.max(0, Math.min(left, state.W - tw));
      var top = y - th - 12;
      if (top < 0) top = Math.min(y + 14, state.H - th);
      tip.style.left = left + "px";
      tip.style.top = Math.max(0, top) + "px";
      state.ring.setAttribute("cx", x);
      state.ring.setAttribute("cy", y);
      state.ring.setAttribute("visibility", "visible");
    }

    function show(kind, i) {
      fillTip(kind, i);
      var x = kind === "an" ? state.ax[i] : state.px[i];
      var y = kind === "an" ? state.ay[i] : state.py[i];
      var d = markPath(kind === "an" ? marks[i].sg : D.sg[i], x, y, state.s);
      state.halo.setAttribute("d", d);
      state.hl.setAttribute("d", d);
      state.halo.setAttribute("visibility", "visible");
      state.hl.setAttribute("visibility", "visible");
      place(x, y);
    }

    function hide() {
      tip.classList.remove("on");
      if (state.ring) state.ring.setAttribute("visibility", "hidden");
      if (state.hl) { state.hl.setAttribute("visibility", "hidden"); state.halo.setAttribute("visibility", "hidden"); }
    }

    // a clicked tag holds the details of its reference mark open until the
    // tag is clicked again, Escape is pressed, or the chart is clicked elsewhere
    function togglePin(k) {
      var was = state.pinned;
      if (was !== null && state.tags[was]) state.tags[was].classList.remove("on");
      if (was === k) { state.pinned = null; hide(); return; }
      state.pinned = k;
      state.tags[k].classList.add("on");
      show("an", k);
    }
    function unpin() {
      if (state.pinned === null) return;
      state.tags[state.pinned].classList.remove("on");
      state.pinned = null;
      hide();
    }

    // the nearest mark to the pointer among the specifications on show and
    // the reference marks, on equal terms; the hit radius is far larger than
    // the marks, so the pointer only has to be closest, not on target
    function nearest(evt) {
      var b = svg.getBoundingClientRect();
      var sx = state.W / (b.width || state.W);
      var mx = (evt.clientX - b.left) * sx, my = (evt.clientY - b.top) * sx;
      var best = null, bd = 256, k, dx, dy, d;
      for (k = 0; k < N; k++) {
        if (!shown(k)) continue;
        dx = state.px[k] - mx; dy = state.py[k] - my; d = dx * dx + dy * dy;
        if (d < bd) { bd = d; best = { kind: "pt", i: k }; }
      }
      for (k = 0; k < marks.length; k++) {
        dx = state.ax[k] - mx; dy = state.ay[k] - my; d = dx * dx + dy * dy;
        if (d < bd) { bd = d; best = { kind: "an", i: k }; }
      }
      return best;
    }

    function onPointer(evt) {
      if (evt.type === "pointermove" && state.pinned !== null) return;
      if (evt.type === "pointerdown") {
        if (evt.target.closest && evt.target.closest(".mv-tag")) return;
        unpin();
      }
      var h = nearest(evt);
      if (!h) { hide(); return; }
      if (h.kind === "pt") state.cur = h.i;
      show(h.kind, h.i);
    }
    svg.addEventListener("pointermove", onPointer);
    svg.addEventListener("pointerdown", onPointer);
    svg.addEventListener("pointerleave", function (evt) { if (evt.pointerType === "mouse" && state.pinned === null) hide(); });
    svg.addEventListener("blur", function () { if (state.pinned === null) hide(); });

    // keyboard stepping moves through the specifications on show, skipping
    // any that "Show only" has faded
    function snap(i, dir) {
      while (i >= 0 && i < N && !shown(i)) i += dir;
      return i;
    }
    function stepFrom(from, by) {
      var dir = by > 0 ? 1 : -1, i = from, n = Math.abs(by);
      while (n-- > 0) {
        var j = snap(i + dir, dir);
        if (j < 0 || j >= N) break;
        i = j;
      }
      return i;
    }
    svg.addEventListener("keydown", function (evt) {
      var by = { ArrowRight: 1, ArrowLeft: -1, ArrowUp: 1, ArrowDown: -1, PageUp: 50, PageDown: -50 }[evt.key];
      if (evt.key === "Escape") { unpin(); hide(); return; }
      if (evt.key === "Home") state.cur = snap(0, 1);
      else if (evt.key === "End") state.cur = snap(N - 1, -1);
      else if (by) state.cur = state.cur < 0 ? (by > 0 ? snap(0, 1) : snap(N - 1, -1)) : stepFrom(shown(state.cur) ? state.cur : snap(state.cur, by > 0 ? 1 : -1), by);
      else return;
      evt.preventDefault();
      unpin();
      if (state.cur >= 0 && state.cur < N) show("pt", state.cur);
    });

    sel.addEventListener("change", function () {
      state.axis = +sel.value;
      draw();
    });
    fax.addEventListener("change", function () {
      state.faxis = +fax.value;
      state.flevel = 0;
      while (flv.firstChild) flv.removeChild(flv.firstChild);
      var a = D.axes[state.faxis];
      if (a.key) {
        a.levels.forEach(function (l, j) { option(flv, j, l); });
        flv.style.display = "";
      } else {
        flv.style.display = "none";
      }
      recolour();
      if (state.pinned === null) hide();
    });
    flv.addEventListener("change", function () {
      state.flevel = +flv.value;
      recolour();
      if (state.pinned === null) hide();
    });

    // the data behind the chart, with the option codes that the exercise
    // helpers take as arguments
    dl.addEventListener("click", function () {
      // a missing value (a logit has no residual df) is written as an empty cell
      var q = function (v) {
        v = v === null || v === undefined ? "" : String(v);
        return /[",\n]/.test(v) ? '"' + v.replace(/"/g, '""') + '"' : v;
      };
      var keys = D.csvAxes.map(function (c) { return c[0]; });
      var head = D.csvHead || ["rank", "r_ca", "t_ca", "df", "p_wcr", "significant_5pc"];
      var rows = [head.concat(D.csvAxes.map(function (c) { return c[1]; })).join(",")];
      for (var i = 0; i < N; i++) {
        rows.push([i + 1, D.r[i], D.st[i], D.df[i], D.p[i], D.sg[i] ? "yes" : "no"]
          .concat(keys.map(function (k) { return D.cd[k][D.code[k][i]]; })).map(q).join(","));
      }
      var blob = new Blob(["﻿" + rows.join("\r\n")], { type: "text/csv;charset=utf-8" });
      var a = document.createElement("a");
      a.href = URL.createObjectURL(blob);
      a.download = D.file;
      document.body.appendChild(a);
      a.click();
      setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 500);
    });

    var pending = false, lastW = 0;
    function onResize() {
      if (pending) return;
      pending = true;
      requestAnimationFrame(function () {
        pending = false;
        var w = Math.round(plot.clientWidth);
        if (w && w !== lastW) { lastW = w; draw(); }
      });
    }
    if (window.ResizeObserver) new ResizeObserver(onResize).observe(plot);
    else window.addEventListener("resize", onResize);
    lastW = Math.round(plot.clientWidth);
    draw();
  };
})();
