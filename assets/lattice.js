/* Landing hero: a lattice of dots that carries the site palette
   left to right, brightens near the cursor and leans toward it.
   Mounts into #hero-field. Replaces particles.js. */
(function () {
  var STOPS = ["#b68235", "#96434a", "#6f5192", "#3f6493", "#2f7a7d", "#4b7a55"];
  var PITCH = 27;      // distance between dots, px
  var RADIUS = 330;    // how far the cursor reaches
  var LEAN = 0.07;     // how far dots slide toward the cursor

  function lut() {
    var rgb = STOPS.map(function (h) {
      return [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)];
    });
    var out = [];
    for (var i = 0; i < 128; i++) {
      var p = (i / 127) * (rgb.length - 1);
      var k = Math.min(rgb.length - 2, Math.floor(p)), f = p - k;
      var c = [0, 1, 2].map(function (j) { return Math.round(rgb[k][j] + (rgb[k + 1][j] - rgb[k][j]) * f); });
      out.push("rgb(" + c[0] + "," + c[1] + "," + c[2] + ")");
    }
    return out;
  }

  function start() {
    var host = document.getElementById("hero-field");
    if (!host) return;
    var reduce = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    var cv = document.createElement("canvas");
    host.appendChild(cv);
    var ctx = cv.getContext("2d");
    var COLORS = lut(), N = COLORS.length - 1;
    var W = 0, H = 0, dpr = Math.min(2, window.devicePixelRatio || 1);
    var cx = 0, cy = 0, tx = 0, ty = 0, idle = false;

    function size() {
      var r = host.getBoundingClientRect();
      W = Math.max(1, r.width); H = Math.max(1, r.height);
      cv.width = W * dpr; cv.height = H * dpr;
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      if (!cx) { cx = tx = W * 0.62; cy = ty = H * 0.5; }
      draw();
    }

    function draw() {
      ctx.clearRect(0, 0, W, H);
      for (var y = PITCH * 0.6; y < H; y += PITCH) {
        for (var x = PITCH * 0.6; x < W; x += PITCH) {
          var dx = cx - x, dy = cy - y;
          var w = Math.max(0, 1 - Math.sqrt(dx * dx + dy * dy) / RADIUS);
          var t = Math.min(1, Math.max(0, (x / W) * 0.78 + (y / H) * 0.22));
          ctx.fillStyle = COLORS[Math.round(t * N)];
          ctx.globalAlpha = 0.2 + 0.62 * w * w;
          ctx.beginPath();
          ctx.arc(x + dx * LEAN * w, y + dy * LEAN * w, 0.8 + 2.2 * w * w, 0, Math.PI * 2);
          ctx.fill();
        }
      }
      ctx.globalAlpha = 1;
    }

    function tick() {
      var dx = tx - cx, dy = ty - cy;
      var busy = Math.abs(dx) + Math.abs(dy) > 0.4;
      if (busy || !idle) {
        idle = !busy;
        cx += dx * 0.18; cy += dy * 0.18;
        draw();
      }
      requestAnimationFrame(tick);
    }

    host.parentElement.addEventListener("mousemove", function (e) {
      var r = host.getBoundingClientRect();
      tx = e.clientX - r.left; ty = e.clientY - r.top;
    });

    window.addEventListener("resize", size);
    size();
    if (!reduce) tick();
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
