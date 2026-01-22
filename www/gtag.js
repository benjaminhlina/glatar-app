var gtagScript = document.createElement('script');
gtagScript.async = true;
gtagScript.src = "https://www.googletagmanager.com/gtag/js?id=G-KP6R7HNSDB";
document.head.appendChild(gtagScript);

window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'G-KP6R7HNSDB');
// ---- Shiny page tracking ----
$(document).on('shiny:connected', function() {
  gtag('event', 'page_view', { page_path: location.pathname });
});

// Track tab / value changes as navigation
$(document).on('shiny:value', function() {
  gtag('event', 'page_view', { page_path: location.pathname });
});
