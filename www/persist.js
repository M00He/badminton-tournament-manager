(function () {
  var KEY = "badminton_tournament_state";

  Shiny.addCustomMessageHandler("persist_state", function (json) {
    try { localStorage.setItem(KEY, json); } catch (e) { console.error("persist failed", e); }
  });

  Shiny.addCustomMessageHandler("clear_persisted", function (_msg) {
    try { localStorage.removeItem(KEY); } catch (e) {}
  });

  Shiny.addCustomMessageHandler("download_backup", function (msg) {
    var blob = new Blob([msg.json], { type: "application/json" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = msg.filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  });

  $(document).on("shiny:connected", function () {
    var saved = "";
    try { saved = localStorage.getItem(KEY) || ""; } catch (e) {}
    Shiny.setInputValue("restored_state", saved, { priority: "event" });
  });
})();
