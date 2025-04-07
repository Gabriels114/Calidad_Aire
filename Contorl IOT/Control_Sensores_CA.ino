#include <LiquidCrystal.h>
#include "DHTesp.h"
#include <MQ135.h>
#include <MQUnifiedsensor.h>

// 📟 LCD 1602A – Pines RS, E, D4, D5, D6, D7
LiquidCrystal lcd(33, 32, 14, 27, 26, 25);

// 🔹 Pines de sensores
#define PIN_DHT     21     // DHT11
#define PIN_MQ135   34     // MQ135 A0
#define PIN_MQ7     35     // MQ7 A0

// 🔹 Instancias
DHTesp dht;
MQ135 mq135_sensor(PIN_MQ135);

// MQ-7 usando la librería oficial
#define Board "ESP32"
#define Voltage_Resolution 3.3
#define ADC_Bit_Resolution 12
#define Type "MQ-7"
MQUnifiedsensor mq7(Board, Voltage_Resolution, ADC_Bit_Resolution, PIN_MQ7, Type);

// 🔹 R0 calibrados (si tienes nuevos, cámbialos aquí)
float R0_MQ135 = 9.87;
float R0_MQ7 = 9.17;

void setup() {
  Serial.begin(115200);

  lcd.begin(16, 2);
  lcd.print("Inicializando...");
  delay(2000);
  lcd.clear();

  // Inicializar sensores
  dht.setup(PIN_DHT, DHTesp::DHT11);
  mq7.setRegressionMethod(1);
  mq7.setA(99.042);
  mq7.setB(-1.518);
  mq7.init();
  mq7.setR0(R0_MQ7);  // Establecer R0 calibrado
}

void loop() {
  // ✅ DHT11
  TempAndHumidity dht_data = dht.getTempAndHumidity();
  float temp = dht_data.temperature;
  float hum = dht_data.humidity;

  if (!isnan(temp) && !isnan(hum)) {
    Serial.println("🌡 Temp: " + String(temp, 1) + "°C");
    Serial.println("💧 Humedad: " + String(hum, 0) + "%");
  } else {
    Serial.println("⚠️ Error al leer DHT11.");
  }

  // ✅ MQ-135
  float sensor_mq135 = analogRead(PIN_MQ135);
  float ppm_mq135 = mq135_sensor.getPPM();  // << Usa método de librería directamente
  Serial.println("🌫️ MQ-135 ADC: " + String(sensor_mq135));
  Serial.println("🛑 Calidad del aire (CO2): " + String(ppm_mq135, 2) + " ppm");

  // ✅ MQ-7
  float sensor_mq7 = analogRead(PIN_MQ7);
  mq7.update();
  float ppm_mq7 = mq7.readSensor();  // << Usa método correcto
  Serial.println("⚠️ MQ-7 ADC: " + String(sensor_mq7));
  Serial.println("🔥 Monóxido de carbono (MQ-7): " + String(ppm_mq7, 2) + " ppm");

  Serial.println("------------------------------------------------");

  // ✅ LCD
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("T:");
  lcd.print(temp, 1);
  lcd.print("C H:");
  lcd.print(hum, 0);
  lcd.print("%");

  lcd.setCursor(0, 1);
  lcd.print("CO2:");
  lcd.print(ppm_mq135, 0);
  lcd.print("ppm CO:");
  lcd.print(ppm_mq7, 0);
  lcd.print("ppm");

  delay(2000);
}
