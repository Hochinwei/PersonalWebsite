/* Site chrome: page accent + the wordmark that pours colour.
   Loaded on every page via _quarto.yml -> include-after-body. */
(function () {
  function pageKey() {
    var p = location.pathname.toLowerCase();
    if (p.indexOf("/projects") > -1) return "projects";
    if (p.indexOf("/blog") > -1) return "blog";
    if (p.indexOf("/about") > -1) return "about";
    if (p.indexOf("/cv") > -1) return "cv";
    if (p.indexOf("/contact") > -1) return "contact";
    return "index";
  }

  function start() {
    var page = pageKey();
    document.body.setAttribute("data-page", page);

    // Tag each nav item so it can hover in its own colour
    var map = { about: "about", blog: "blog", projects: "projects", cv: "cv", contact: "contact" };
    document.querySelectorAll(".navbar-nav .nav-link").forEach(function (a) {
      var label = (a.textContent || "").trim().toLowerCase();
      var key = map[label];
      if (!key) return;
      a.setAttribute("data-accent", key);
      if (key === page) a.classList.add("active"); else a.classList.remove("active");
    });

    // The wordmark: brand text becomes a hollow outline with a
    // gradient-filled twin stacked exactly on top of it.
    var brand = document.querySelector(".navbar-brand");
    if (brand && !brand.classList.contains("wordmark")) {
      var text = (brand.textContent || "Ho Chin Wei").trim() || "Ho Chin Wei";
      brand.classList.add("wordmark");
      brand.textContent = text;
      var fill = document.createElement("span");
      fill.className = "wordmark-fill";
      fill.setAttribute("aria-hidden", "true");
      fill.textContent = text;
      brand.appendChild(fill);
      brand.setAttribute("title", page === "index" ? "You are here \u2014 home" : "Back to home");
      if (page === "index") brand.classList.add("is-here");
    }
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
