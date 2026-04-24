// app.js — IMSLU Resident Dashboard client-side helpers

// Submit login form when Enter is pressed in the access code field.
// Using event delegation so the listener works even if the element is
// inserted after DOMContentLoaded (Shiny renders login asynchronously).
document.addEventListener("keypress", function (e) {
  if (e.key === "Enter" && e.target && e.target.id === "auth-access_code") {
    Shiny.setInputValue("auth-access_code_btn", Math.random(), { priority: "event" });
  }
});
