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

  // Quarto renders a listing's category filter into the right margin sidebar.
  // This design wants it in the body column, under the lede and above the post
  // list. Move the nodes rather than rebuilding them: quarto-listing.js has
  // already bound onclick handlers to each chip, and every selector it uses
  // afterwards is document-wide, so a move keeps the filtering working.
  function relocateCategories() {
    var sidebar = document.getElementById("quarto-margin-sidebar");
    var listing = document.querySelector("main.content .quarto-listing");
    if (!sidebar || !listing) return;

    var chips = sidebar.querySelector(".quarto-listing-category");
    if (!chips) return;

    var rack = document.createElement("div");
    rack.className = "category-rack";
    var title = sidebar.querySelector(".quarto-listing-category-title");
    if (title) rack.appendChild(title);
    rack.appendChild(chips);
    listing.parentNode.insertBefore(rack, listing);

    // Only retire the sidebar if the filter was all it held - on pages with
    // toc-location: left the table of contents lives here too.
    if (!sidebar.children.length) sidebar.remove();
  }

  function start() {
    var page = pageKey();
    document.body.setAttribute("data-page", page);

    relocateCategories();

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
