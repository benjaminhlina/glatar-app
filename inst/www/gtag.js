// var gtagScript = document.createElement('script');
// gtagScript.async = true;
// gtagScript.src = "https://www.googletagmanager.com/gtag/js?id=G-KP6R7HNSDB";
// document.head.appendChild(gtagScript);

window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'G-KP6R7HNSDB');

// ---- Shiny page tracking ----

// ---- Session tracking state ----
var currentSection = null;
var sectionEntryTime = null;

function sendTimeOnSection(section, entryTime) {
  if (!section || !entryTime) return;
  var seconds = Math.round((Date.now() - entryTime) / 1000);
  if (seconds < 1) return; // ignore noise
  gtag('event', 'time_on_section', {
    section_name: section,
    seconds_spent: seconds,
    page_path: location.pathname
  });
}

function trackNavigation(newSection) {
  // Send time for the section the user is LEAVING
  sendTimeOnSection(currentSection, sectionEntryTime);
  // Start tracking the new section
  currentSection = newSection;
  sectionEntryTime = Date.now();
  // Standard page_view event
  gtag('event', 'page_view', { page_path: location.pathname });
}

// ---- Shiny event hooks ----
$(document).on('shiny:connected', function() {
  trackNavigation(location.pathname);
});

$(document).on('shiny:value', function(e) {
  // e.name is the output ID that changed — useful as a section label
  trackNavigation(e.name || location.pathname);
});

// Send time when user closes/leaves the tab
window.addEventListener('beforeunload', function() {
  sendTimeOnSection(currentSection, sectionEntryTime);
});

// Also send on visibility change (tab switch, minimize) — more reliable than beforeunload
document.addEventListener('visibilitychange', function() {
  if (document.visibilityState === 'hidden') {
    sendTimeOnSection(currentSection, sectionEntryTime);
    // Reset so we don't double-count if they come back
    sectionEntryTime = Date.now();
  }
});