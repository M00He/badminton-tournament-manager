// Persistenz-Bridge: Turnierstand im Browser-localStorage halten + Auto-Resume.
// Wird in app.R INLINE in die Seite eingebettet (kein separater /persist.js-Request -> kein 404).
(function () {
  var KEY = "badminton_tournament_state";

  // Erst loslegen, wenn Shiny und jQuery wirklich geladen sind (Ladereihenfolge-sicher).
  function init() {
    if (typeof Shiny === "undefined" || typeof $ === "undefined" ||
        typeof Shiny.addCustomMessageHandler !== "function") {
      setTimeout(init, 50);
      return;
    }

    // Stand bei jeder Änderung schreiben
    Shiny.addCustomMessageHandler("persist_state", function (json) {
      // Defensiv: nur Strings speichern. Käme versehentlich ein Objekt (class-"json"
      // ohne as.character auf R-Seite), würde sonst "[object Object]" landen.
      var payload = (typeof json === "string") ? json : JSON.stringify(json);
      try { localStorage.setItem(KEY, payload); } catch (e) { console.error("persist failed", e); }
    });

    // Stand löschen (neues Turnier)
    Shiny.addCustomMessageHandler("clear_persisted", function (_msg) {
      try { localStorage.removeItem(KEY); } catch (e) {}
    });

    // Auto-Resume: gespeicherten Stand an den Server schicken
    function sendRestore() {
      // setInputValue wird erst beim Initialisieren von Shiny angehängt — defensiv prüfen,
      // sonst "Shiny.setInputValue is not a function", falls wir vor dem Connect feuern.
      if (typeof Shiny.setInputValue !== "function") return;
      var saved = "";
      try { saved = localStorage.getItem(KEY) || ""; } catch (e) {}
      // priority:"event" erzwingt das Auslösen auch bei unverändertem Wert (Resume nach Reload)
      Shiny.setInputValue("restored_state", saved, { priority: "event" });
    }
    // Auf künftige Verbindungen hören (zuverlässiger Zeitpunkt: setInputValue existiert dann sicher) ...
    $(document).on("shiny:connected", sendRestore);
    // ... UND sofort versuchen, falls shiny:connected schon gefeuert hat (localhost-Race).
    // sendRestore() ist no-op, solange setInputValue noch nicht bereit ist.
    sendRestore();
  }

  init();
})();
