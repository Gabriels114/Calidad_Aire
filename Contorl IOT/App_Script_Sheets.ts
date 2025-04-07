function doGet(e) {
  return handleRequest(e);
}

function doPost(e) {
  return handleRequest(e);
}

function handleRequest(e) {
  // Si e no está definido (por ejemplo, al probar desde el editor), se crea un objeto vacío.
  e = e || { parameter: {} };

  // Registra los parámetros recibidos para depuración
  Logger.log("Parámetros recibidos: " + JSON.stringify(e.parameter));

  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  var params = e.parameter;
  var date = new Date();

  // Inserta la fila con los 8 valores esperados
  var newRow = [
    Utilities.formatDate(date, "GMT-6", "yyyy-MM-dd"),
    Utilities.formatDate(date, "GMT-6", "HH:mm:ss"),
    params.temp  || "",
    params.hum   || "",
    params.co2   || "",
    params.tvoc  || "",
    params.airQ  || "",
    params.co    || ""
  ];

  Logger.log("Nueva fila a insertar: " + newRow.join(", "));

  sheet.appendRow(newRow);

  return ContentService.createTextOutput("Datos guardados");
}
