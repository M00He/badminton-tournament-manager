// Persistenz-Bridge: Turnierstand im Browser-localStorage halten + Auto-Resume.
// (Der Backup-Download läuft über Shinys nativen downloadHandler, nicht über dieses Shim.)
(function () {
  var KEY = "badminton_tournament_state";

  // Erst loslegen, wenn Shiny und jQuery wirklich geladen sind (Ladereihenfolge-sicher).
  function init() {
    if (typeof Shiny === "undefined" || typeof $ === "undefined") {
      setTimeout(init, 50);
      return;
    }

    // Stand bei jeder Änderung schreiben
    Shiny.addCustomMessageHandler("persist_state", function (json) {
      try { localStorage.setItem(KEY, json); } catch (e) { console.error("persist failed", e); }
    });

    // Stand löschen (neues Turnier)
    Shiny.addCustomMessageHandler("clear_persisted", function (_msg) {
      try { localStorage.removeItem(KEY); } catch (e) {}
    });

    // Auto-Resume: gespeicherten Stand an den Server schicken
    function sendRestore() {
      var saved = "";
      try { saved = localStorage.getItem(KEY) || ""; } catch (e) {}
      // priority:"event" erzwingt das Auslösen auch bei unverändertem Wert (Resume nach Reload)
      Shiny.setInputValue("restored_state", saved, { priority: "event" });
    }
    $(document).on("shiny:connected", sendRestore);
  }

  init();
})();
