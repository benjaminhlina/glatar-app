glatar_diagram_ui <- function() {
  htmltools::tags$div(
    style = "width:100%; padding: 10px 0;",
    htmltools::tags$style(HTML("
      /* ── GLATAR Bubble Diagram ── */
      .glatar-diagram-wrap {
        font-family: 'Georgia', 'Times New Roman', serif;
        background: linear-gradient(160deg, #0a1628 0%, #0d2340 50%, #0a1e35 100%);
        border-radius: 12px;
        padding: 32px 24px 36px;
        position: relative;
        overflow: hidden;
        box-shadow: 0 8px 32px rgba(0,0,0,0.4);
      }
      .glatar-diagram-wrap::before {
        content: '';
        position: absolute;
        inset: 0;
        background:
          radial-gradient(ellipse 60% 40% at 20% 80%, rgba(0,120,180,0.08) 0%, transparent 60%),
          radial-gradient(ellipse 40% 50% at 80% 20%, rgba(0,180,160,0.07) 0%, transparent 60%);
        pointer-events: none;
      }
      .glatar-diagram-title {
        text-align: center;
        color: #a8d4f0;
        font-size: 13px;
        letter-spacing: 3px;
        text-transform: uppercase;
        margin-bottom: 28px;
        opacity: 0.75;
      }

      /* ── SVG canvas ── */
      #glatar-svg {
        display: block;
        margin: 0 auto;
        max-width: 700px;
        width: 100%;
        overflow: visible;
      }

      /* ── Connector lines ── */
      .glatar-spoke {
        stroke: rgba(100,180,220,0.18);
        stroke-width: 1.5;
        stroke-dasharray: 4 4;
        transition: stroke 0.3s, stroke-opacity 0.3s;
      }

      /* ── Bubbles ── */
      .glatar-bubble-group {
        cursor: pointer;
      }
      .glatar-bubble-bg {
        transition: r 0.25s ease, filter 0.25s ease;
        filter: drop-shadow(0 4px 10px rgba(0,0,0,0.5));
      }
      .glatar-bubble-group:hover .glatar-bubble-bg,
      .glatar-bubble-group.active .glatar-bubble-bg {
        filter: drop-shadow(0 0 18px var(--glow)) drop-shadow(0 4px 10px rgba(0,0,0,0.5));
      }
      .glatar-bubble-ring {
        fill: none;
        stroke-width: 1.5;
        opacity: 0.5;
        transition: opacity 0.25s, stroke-width 0.25s;
      }
      .glatar-bubble-group:hover .glatar-bubble-ring,
      .glatar-bubble-group.active .glatar-bubble-ring {
        opacity: 1;
        stroke-width: 2.5;
      }
      .glatar-bubble-icon {
        font-size: 20px;
        dominant-baseline: central;
        text-anchor: middle;
        pointer-events: none;
        transition: font-size 0.25s;
      }
      .glatar-bubble-label {
        fill: #cee8f8;
        font-family: 'Georgia', serif;
        font-size: 9.5px;
        text-anchor: middle;
        dominant-baseline: central;
        pointer-events: none;
        letter-spacing: 0.3px;
      }

      /* ── Centre hub ── */
      .glatar-hub-outer { opacity: 0.25; }
      .glatar-hub-inner { opacity: 0.55; }
      .glatar-hub-label {
        fill: #e0f2ff;
        font-family: 'Georgia', serif;
        font-size: 11px;
        font-weight: bold;
        text-anchor: middle;
        dominant-baseline: central;
        letter-spacing: 1.5px;
      }

      /* ── Info card ── */
      .glatar-info-card {
        max-width: 660px;
        margin: 24px auto 0;
        background: rgba(255,255,255,0.04);
        border: 1px solid rgba(100,180,220,0.2);
        border-radius: 10px;
        padding: 18px 22px;
        min-height: 80px;
        display: flex;
        align-items: flex-start;
        gap: 16px;
        transition: opacity 0.3s, transform 0.3s;
        opacity: 0;
        transform: translateY(8px);
        pointer-events: none;
      }
      .glatar-info-card.visible {
        opacity: 1;
        transform: translateY(0);
        pointer-events: auto;
      }
      .glatar-card-icon {
        font-size: 26px;
        flex-shrink: 0;
        line-height: 1;
        padding-top: 2px;
      }
      .glatar-card-body {}
      .glatar-card-name {
        color: #6dcfff;
        font-family: 'Georgia', serif;
        font-size: 14px;
        font-weight: bold;
        margin-bottom: 5px;
        letter-spacing: 0.5px;
      }
      .glatar-card-desc {
        color: #9cc8e0;
        font-family: 'Georgia', serif;
        font-size: 12px;
        line-height: 1.6;
        margin-bottom: 6px;
      }
      .glatar-card-tags {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin-top: 6px;
      }
      .glatar-card-tag {
        background: rgba(100,200,240,0.1);
        border: 1px solid rgba(100,200,240,0.25);
        border-radius: 20px;
        padding: 2px 9px;
        font-size: 10px;
        color: #7ec8e8;
        font-family: 'Georgia', serif;
      }
      .glatar-hint {
        text-align: center;
        color: rgba(150,190,220,0.4);
        font-size: 11px;
        font-family: 'Georgia', serif;
        margin-top: 10px;
        letter-spacing: 0.5px;
        font-style: italic;
      }
      .glatar-card-doc {
        display: none;
        margin-top: 10px;
      }
      .glatar-card-doc.has-doc {
        display: block;
      }
      .glatar-doc-link {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 5px 13px;
        border-radius: 20px;
        border: 1px solid rgba(100,200,240,0.35);
        background: rgba(100,200,240,0.07);
        color: #7ec8e8;
        font-family: 'Georgia', serif;
        font-size: 11px;
        text-decoration: none;
        letter-spacing: 0.4px;
        transition: background 0.2s, border-color 0.2s, color 0.2s;
      }
      .glatar-doc-link:hover {
        background: rgba(100,200,240,0.18);
        border-color: rgba(100,200,240,0.65);
        color: #b8e8ff;
        text-decoration: none;
      }
      .glatar-doc-link svg {
        flex-shrink: 0;
        opacity: 0.75;
      }
    ")),

    htmltools::tags$div(
      class = "glatar-diagram-wrap",

      htmltools::tags$div(class = "glatar-diagram-title", "Database Components"),
      htmltools::tags$div(class = "glatar-hint", "Click a bubble to explore each data type"),

      # SVG bubble diagram
      htmltools::tags$svg(
        id = "glatar-svg",
        viewBox = "0 0 500 260",
        xmlns = "http://www.w3.org/2000/svg",
        htmltools::tags$defs(
          htmltools::tags$radialGradient(
            id = "hub-grad", cx = "50%", cy = "35%", r = "65%",
            htmltools::tags$stop(offset = "0%",   `stop-color` = "#1a4a6e"),
            htmltools::tags$stop(offset = "100%", `stop-color` = "#0a2440")
          ),
          # individual bubble gradients generated in JS
        )
      ),

      # Info card
      htmltools::tags$div(
        class = "glatar-info-card",
        id = "glatar-card",
        htmltools::tags$div(class = "glatar-card-icon", id = "card-icon", ""),
        htmltools::tags$div(
          class = "glatar-card-body",
          htmltools::tags$div(class = "glatar-card-name", id = "card-name", ""),
          htmltools::tags$div(class = "glatar-card-desc", id = "card-desc", ""),
          htmltools::tags$div(class = "glatar-card-tags", id = "card-tags"),
          htmltools::tags$div(class = "glatar-card-doc", id = "card-doc")
        )
      ),

      #  htmltools::tags$div(class = "glatar-hint", "Click a bubble to explore each data type")
    ),

    # Build the SVG with JS
    htmltools::tags$script(HTML('
(function() {

  var components = [
    {
      id: "ed",
      label: ["Energy","Density"],
      icon: "⚡",
      color: "#f0a500",
      glow: "#f0a500",
      grad: ["#bc7a00ff","#7f5900ff"],
      desc: "Measures the caloric content of tissue samples (Joules/g wet or dry weight), providing a key index of organismal condition and energy reserves. Widely used in bioenergetics models.",
      tags: ["Joules/g wet weight","Joules/g dry weight","Bomb calorimetry","Proximate estimation"]
    },
    {
      id: "pc",
      label: ["Proximate","Composition"],
      icon: null,
      pieChart: true,
      color: "#5ec45e",
      glow: "#5ec45e",
      grad: ["#247824ff","#114611ff"],
      desc: "Quantifies the proportion of water, lipid, protein, and ash in tissue, revealing nutritional quality and body condition of fish and invertebrates.",
      tags: ["% water","% lipid","% protein","% ash","Soxhlet extraction"]
    },
    {
      id: "si",
      label: ["Stable","Isotopes"],
      icon: "⚛",
      color: "#64c8f5",
      glow: "#64c8f5",
      grad: ["#2a84c8ff","#155c96ff"],
      desc: "Ratios of δ¹³C and δ¹⁵N (and others) in tissue are used to infer trophic position, dietary sources, and migration patterns across food webs.",
      tags: ["δ¹³C","δ¹⁵N","δ³⁴S","Trophic position","Food web tracing"],
      doc: "documentation/stable_isotope_documentation.pdf"
    },
    {
      id: "th",
      label: ["Thiamine"],
      icon: "💊",
      color: "#e06dff",
      glow: "#e06dff",
      grad: ["#8900c0ff","#50006fff"],
      desc: "Thiamine (Vitamin B₁) concentrations in eggs and tissue are critical indicators of early mortality syndrome (EMS/TAMS) in salmonids and other Great Lakes fishes.",
      tags: ["nmol/g","Egg thiamine","EMS / TAMS","Salmonid health"],
      doc: "documentation/thiamine_and_thiaminase_documentation.pdf"
    },
    {
      id: "fa",
      label: ["Fatty","Acids"],
      icon: "🫧",
      color: "#ff9055",
      glow: "#ff9055",
      grad: ["#973500ff","#602000ff"],
      desc: "Fatty acid profiles (e.g., EPA, DHA, DPA) serve as dietary tracers and indicators of nutritional quality, linking primary producers to top predators.",
      tags: ["EPA","DHA","DPA","% total FA","Dietary tracers"],
      doc: "documentation/fatty_acids_documentation.pdf"
    },
    {
      id: "hg",
      label: ["Mercury"],
      icon: "⚗️",
      color: "#c8e840",
      glow: "#c8e840",
      grad: ["#84a100ff","#566900ff"],
      desc: "Total mercury and methylmercury concentrations in fish tissue are essential for contaminant monitoring, consumption advisories, and food web biomagnification studies.",
      tags: ["Total Hg","Methylmercury","μg/g wet wt","Bioaccumulation"]
    },
    {
      id: "pcb",
      label: ["PCBs"],
      icon: "☣",
      color: "#ff5f72",
      glow: "#ff5f72",
      grad: ["#be0020ff","#6a0013ff"],
      desc: "Polychlorinated biphenyl concentrations (individual congeners and totals) in fish and invertebrate tissue reflect legacy contaminant loads and are used in ecological risk assessments.",
      tags: ["Total PCBs","Congener profiles","ng/g wet wt","Risk assessment"],
      doc: "documentation/polychlorinated_biphenyls_documentation.pdf"
    }
  ];

  // Layout: arc arrangement
  var cx = 250, cy = 128;
  var r  = 105; // orbit radius
  var n  = components.length;

  // positions evenly spaced on an ellipse
  var positions = components.map(function(c, i) {
    var angle = (2 * Math.PI * i / n) - Math.PI / 2;
    return {
      x: cx + r * 1.18 * Math.cos(angle),
      y: cy + r * 0.78 * Math.sin(angle)
    };
  });

  var svg = document.getElementById("glatar-svg");
  var NS  = "http://www.w3.org/2000/svg";

  function el(tag, attrs, parent) {
    var e = document.createElementNS(NS, tag);
    Object.keys(attrs || {}).forEach(function(k) { e.setAttribute(k, attrs[k]); });
    if (parent) parent.appendChild(e);
    return e;
  }

  // defs
  var defs = el("defs", {}, svg);
  el("radialGradient", {id:"hub-grad", cx:"50%", cy:"35%", r:"65%"}, defs);
  var hg = svg.querySelector("#hub-grad");
  var s1 = el("stop", {offset:"0%","stop-color":"#1e5070"}, hg);
  var s2 = el("stop", {offset:"100%","stop-color":"#071828"}, hg);

  components.forEach(function(c) {
    var g = el("radialGradient", {id:"grad-"+c.id, cx:"40%", cy:"30%", r:"65%"}, defs);
    el("stop", {offset:"0%","stop-color":c.grad[0]}, g);
    el("stop", {offset:"100%","stop-color":c.grad[1]}, g);
  });

  // spokes
  positions.forEach(function(p) {
    el("line", {
      class:"glatar-spoke",
      x1: cx, y1: cy,
      x2: p.x, y2: p.y
    }, svg);
  });

  // hub
  var hubGroup = el("g", {class:"glatar-hub", style:"cursor:pointer;"}, svg);
  el("circle", {cx:cx, cy:cy, r:38, fill:"url(#hub-grad)", class:"glatar-hub-outer",
    stroke:"rgba(100,180,220,0.15)","stroke-width":"1"}, hubGroup);
  el("circle", {cx:cx, cy:cy, r:28, fill:"url(#hub-grad)", class:"glatar-hub-inner",
    stroke:"rgba(100,200,240,0.3)","stroke-width":"1.5"}, hubGroup);
  var hubText = el("text", {class:"glatar-hub-label", x:cx, y:cy-5}, hubGroup);
  hubText.textContent = "GLATAR";
  var hubSub = el("text", {
    fill:"rgba(160,210,240,0.5)", "font-size":"7",
    "text-anchor":"middle", "dominant-baseline":"central",
    "font-family":"Georgia,serif", "letter-spacing":"1.5px",
    x:cx, y:cy+9
  }, hubGroup);
  hubSub.textContent = "DATABASE";

  var hubActive = false;
  hubGroup.addEventListener("click", function() {
    // deactivate any open bubble card
    components.forEach(function(cc) {
      var prev = document.getElementById("bubble-"+cc.id);
      if (prev) prev.classList.remove("active");
    });
    activeIdx = null;

    if (hubActive) {
      hubActive = false;
      hubGroup.classList.remove("active");
      document.getElementById("glatar-card").classList.remove("visible");
      return;
    }
    hubActive = true;
    hubGroup.classList.add("active");

    document.getElementById("card-icon").textContent = "🌊";
    document.getElementById("card-name").textContent = "GLATAR — Great Lakes Aquatic Tissue Analysis Repository";
    document.getElementById("card-name").style.color = "#64c8f5";
    document.getElementById("card-desc").textContent = "GLATAR brings together energy density, proximate composition, stable isotopes, thiamine, fatty acids, mercury, and PCB data into a single, unified repository. By integrating these complementary data, researchers and managers can explore bioenergetics, contaminant dynamics, nutritional quality, and food web structure across species, locations, and time, all through a single interactive platform.";

    var tagsEl = document.getElementById("card-tags");
    tagsEl.innerHTML = "";
    ["Multi-metric integration","Great Lakes & North America","Researchers & managers","Bioenergetics","Food web dynamics","Contaminant monitoring"].forEach(function(t) {
      var span = document.createElement("span");
      span.className = "glatar-card-tag";
      span.textContent = t;
      tagsEl.appendChild(span);
    });

    var docEl = document.getElementById("card-doc");
    docEl.innerHTML = "";
    docEl.classList.add("has-doc");
    var a = document.createElement("a");
    a.className = "glatar-doc-link";
    a.href = "documentation/rationale_for_GLATAR.pdf";
    a.target = "_blank";
    a.rel = "noopener noreferrer";
    a.innerHTML = \'<svg xmlns="http://www.w3.org/2000/svg" width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/></svg>View GLATAR Purpose Document\';
    docEl.appendChild(a);

    document.getElementById("glatar-card").classList.add("visible");
  });

  // bubbles
  var activeIdx = null;

  // helper: draw a mini pie chart in SVG at (px, py) with given radius
  function drawPie(parent, px, py, pr) {
    var slices = [
      { pct: 0.70, fill: "#4ab8e8" },   // water – blue
      { pct: 0.15, fill: "#f0d060" },   // lipid – yellow
      { pct: 0.12, fill: "#f5b638ff" },   // protein – green
      { pct: 0.03, fill: "#979797ff" }    // ash – grey
    ];
    var startAngle = -Math.PI / 2; // start at top
    slices.forEach(function(s) {
      var endAngle = startAngle + s.pct * 2 * Math.PI;
      var x1 = px + pr * Math.cos(startAngle);
      var y1 = py + pr * Math.sin(startAngle);
      var x2 = px + pr * Math.cos(endAngle);
      var y2 = py + pr * Math.sin(endAngle);
      var large = s.pct > 0.5 ? 1 : 0;
      var d = "M "+px+" "+py+" L "+x1+" "+y1+" A "+pr+" "+pr+" 0 "+large+" 1 "+x2+" "+y2+" Z";
      el("path", {d:d, fill:s.fill, stroke:"rgba(0,0,0,0.3)", "stroke-width":"0.5"}, parent);
      startAngle = endAngle;
    });
    // subtle rim
    el("circle", {cx:px, cy:py, r:pr, fill:"none", stroke:"rgba(255,255,255,0.15)", "stroke-width":"0.75"}, parent);
  }

  components.forEach(function(c, i) {
    var p = positions[i];
    var bRad = 30;

    var g = el("g", {class:"glatar-bubble-group", id:"bubble-"+c.id,
      style:"--glow: "+c.glow}, svg);
    g.setAttribute("tabindex","0");

    el("circle", {
      class:"glatar-bubble-bg",
      cx:p.x, cy:p.y, r:bRad,
      fill:"url(#grad-"+c.id+")",
      stroke:c.color, "stroke-width":"1.5",
      "stroke-opacity":"0.5"
    }, g);
    el("circle", {
      class:"glatar-bubble-ring",
      cx:p.x, cy:p.y, r:bRad+4,
      stroke:c.color
    }, g);

    // icon — mini pie chart for proximate composition, emoji for all others
    if (c.pieChart) {
      drawPie(g, p.x, p.y - 8, 7);
    } else {
      var icon = el("text", {
        class:"glatar-bubble-icon",
        x:p.x, y:p.y - 8
      }, g);
      icon.textContent = c.icon;
    }

    // label (two lines)
    c.label.forEach(function(line, li) {
      var t = el("text", {
        class:"glatar-bubble-label",
        x:p.x, y:p.y + 10 + li*11
      }, g);
      t.textContent = line;
    });

    function activate() {
      // deactivate previous
      components.forEach(function(cc) {
        var prev = document.getElementById("bubble-"+cc.id);
        if (prev) prev.classList.remove("active");
      });
      hubActive = false;
      hubGroup.classList.remove("active");

      if (activeIdx === i) {
        activeIdx = null;
        document.getElementById("glatar-card").classList.remove("visible");
        return;
      }
      activeIdx = i;
      g.classList.add("active");

      // fill card
      document.getElementById("card-icon").textContent = c.cardIcon || c.icon;
      document.getElementById("card-name").textContent = c.label.join(" ");
      document.getElementById("card-desc").textContent = c.desc;
      var tagsEl = document.getElementById("card-tags");
      tagsEl.innerHTML = "";
      c.tags.forEach(function(t) {
        var span = document.createElement("span");
        span.className = "glatar-card-tag";
        span.textContent = t;
        tagsEl.appendChild(span);
      });
      document.getElementById("card-name").style.color = c.color;
      document.getElementById("glatar-card").classList.add("visible");

      // doc link
      var docEl = document.getElementById("card-doc");
      docEl.innerHTML = "";
      if (c.doc) {
        docEl.classList.add("has-doc");
        var a = document.createElement("a");
        a.className = "glatar-doc-link";
        a.href = c.doc;
        a.target = "_blank";
        a.rel = "noopener noreferrer";
        a.innerHTML = \'<svg xmlns="http://www.w3.org/2000/svg" width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/></svg>\' + "View Documentation";
        docEl.appendChild(a);
      } else {
        docEl.classList.remove("has-doc");
      }
    }

    g.addEventListener("click", activate);
    g.addEventListener("keydown", function(e) { if (e.key==="Enter"||e.key===" ") activate(); });
  });

})();
    '))
  )
}