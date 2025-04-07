#include <WiFi.h>
#include <HTTPClient.h>
#include <DHTesp.h>
#include <MQ135.h>
#include <MQUnifiedsensor.h>
#include <Adafruit_CCS811.h>

// --------------------
// Definición de pines para los sensores
#define PIN_DHT      19    // DHT11 (dato)
#define PIN_MQ135    34    // MQ-135 (salida analógica)
#define PIN_MQ7      35    // MQ-7   (salida analógica)
// El CCS811 utiliza I²C: SDA en GPIO21 y SCL en GPIO22 (por defecto)

// --------------------
// Crear objeto para el CCS811
Adafruit_CCS811 ccs;  // Sensor CJMCU-811

// --------------------
// Credenciales WiFi
const char* ssid     = "Alumnos";
const char* password = "@@1umN05@@";

// --------------------
// URL del Apps Script de Google Sheets (nueva URL)
const char* googleScriptURL = "https://script.google.com/macros/s/AKfycbwOps98I6UUA1Z5CF6cbVxBcXUna2LCRsax67ncxcRKKptV3e2CHoPVQj5ng_DIuEJ_/exec";

// --------------------
// Crear objetos para los sensores
DHTesp dht;                     // Sensor DHT11
MQ135 mq135_sensor(PIN_MQ135);  // Sensor MQ-135

// Configuración del MQ-7 utilizando la librería MQUnifiedsensor
#define Board "ESP32"
#define Voltage_Resolution 3.3
#define ADC_Bit_Resolution 12
#define Type "MQ-7"
MQUnifiedsensor mq7(Board, Voltage_Resolution, ADC_Bit_Resolution, PIN_MQ7, Type);

// Valor de calibración para MQ-7 (ajusta según tus mediciones)
float R0_MQ7 = 7.17;

// --------------------
// Estructura para almacenar los datos de los sensores
struct DatosSensores {
  float temp;      // DHT11: Temperatura
  float hum;       // DHT11: Humedad
  float ccs_co2;   // CCS811: CO₂ (ppm)
  float ccs_tvoc;  // CCS811: TVOC (ppb)
  float airQ;      // MQ-135: Calidad del aire (ppm)
  float co;        // MQ-7: CO (ppm)
};

// Intervalo de lectura (1 minuto)
unsigned long tiempoAnterior = 0;
const unsigned long INTERVALO_LECTURA = 60000;  // 60,000 ms

// -------------------------------------------------
// Función para conectar a WiFi
void conectarWiFi() {
  Serial.print("Conectando a WiFi");
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("\nWiFi conectado!");
  Serial.print("IP local: ");
  Serial.println(WiFi.localIP());
}

// -------------------------------------------------
// Función para leer los sensores
DatosSensores LeerSensores() {
  DatosSensores ds;

  // --- Lectura del DHT11 ---
  TempAndHumidity dht_data = dht.getTempAndHumidity();
  ds.temp = dht_data.temperature;
  ds.hum  = dht_data.humidity;

  // --- Lectura del CCS811 ---
  if (ccs.available()) {
    if (!ccs.readData()) {
      ds.ccs_co2  = ccs.geteCO2();  // Valor en ppm
      ds.ccs_tvoc = ccs.getTVOC();  // Valor en ppb
    } else {
      Serial.println("Error al leer CCS811");
      ds.ccs_co2  = -1;
      ds.ccs_tvoc = -1;
    }
  } else {
    ds.ccs_co2  = -1;
    ds.ccs_tvoc = -1;
  }

  // --- Lectura del MQ-135 (calidad del aire) ---
  ds.airQ = mq135_sensor.getPPM();

  // --- Lectura del MQ-7 (CO) ---
  mq7.update();
  ds.co = mq7.readSensor();

  return ds;
}

// -------------------------------------------------
// Función para imprimir en el Monitor Serial
void ImprimirEnSerial(const DatosSensores &ds) {
  Serial.println("===== Lectura de Sensores =====");
  Serial.print("Temp (DHT11): ");
  Serial.print(ds.temp, 1);
  Serial.println(" °C");

  Serial.print("Humedad (DHT11): ");
  Serial.print(ds.hum, 0);
  Serial.println(" %");

  Serial.print("CCS811 CO2: ");
  Serial.print(ds.ccs_co2);
  Serial.println(" ppm");

  Serial.print("CCS811 TVOC: ");
  Serial.print(ds.ccs_tvoc);
  Serial.println(" ppb");

  Serial.print("MQ-135 (AirQ): ");
  Serial.print(ds.airQ, 2);
  Serial.println(" ppm");

  Serial.print("MQ-7 (CO): ");
  Serial.print(ds.co, 2);
  Serial.println(" ppm");

  Serial.println("--------------------------------\n");
}

// -------------------------------------------------
// Función para enviar los datos a Google Sheets vía HTTP GET
void enviarDatosGoogleSheets(const DatosSensores &ds) {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    // Construir la URL con los parámetros
    String url = String(googleScriptURL)
                 + "?temp=" + String(ds.temp)
                 + "&hum="  + String(ds.hum)
                 + "&co2="  + String(ds.ccs_co2)
                 + "&tvoc=" + String(ds.ccs_tvoc)
                 + "&airQ=" + String(ds.airQ)
                 + "&co="   + String(ds.co);

    // Imprimir la URL para depuración
    Serial.print("URL enviada: ");
    Serial.println(url);

    http.begin(url);
    int httpResponseCode = http.GET();
    if (httpResponseCode > 0) {
      Serial.println("Datos enviados a Google Sheets");
    } else {
      Serial.print("Error al enviar datos: ");
      Serial.println(httpResponseCode);
    }
    http.end();
  } else {
    Serial.println("Sin conexión WiFi!");
  }
}

// -------------------------------------------------
// Función para verificar la conexión WiFi y reconectar si es necesario
void verificarConexionWiFi() {
  if (WiFi.status() != WL_CONNECTED) {
    Serial.println("WiFi desconectado! Intentando reconectar...");
    WiFi.disconnect();
    WiFi.reconnect();
    unsigned long inicio = millis();
    while (WiFi.status() != WL_CONNECTED && millis() - inicio < 10000) {
      delay(500);
      Serial.print(".");
    }
    if (WiFi.status() == WL_CONNECTED) {
      Serial.println("\nWiFi reconectado!");
      Serial.print("IP local: ");
      Serial.println(WiFi.localIP());
    } else {
      Serial.println("\nNo se pudo reconectar a WiFi.");
    }
  }
}

// -------------------------------------------------
// Función setup()
void setup() {
  Serial.begin(115200);
  Serial.println("Inicializando...");

  // Conectar a WiFi
  conectarWiFi();

  // Inicializar DHT11 en el PIN definido
  dht.setup(PIN_DHT, DHTesp::DHT11);

  // Inicializar MQ-7
  mq7.setRegressionMethod(1);
  mq7.setA(99.042);
  mq7.setB(-1.518);
  mq7.init();
  mq7.setR0(R0_MQ7);

  // Inicializar el CCS811
  if (!ccs.begin()) {
    Serial.println("No se encontró CCS811. Verifica cableado I2C.");
    while (1) {
      delay(100);
    }
  }
  // Nota: Para el MQ-135 no se utiliza setR0, ya que la librería no lo implementa.
}

// -------------------------------------------------
// Función loop()
void loop() {
  verificarConexionWiFi();
  unsigned long tiempoActual = millis();

  if (tiempoActual - tiempoAnterior >= INTERVALO_LECTURA) {
    tiempoAnterior = tiempoActual;

    DatosSensores ds = LeerSensores();
    ImprimirEnSerial(ds);
    enviarDatosGoogleSheets(ds);
  }

  // Aquí se pueden agregar otras tareas sin bloqueo
}