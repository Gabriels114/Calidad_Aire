#include <MQ135.h>

#define PIN_MQ135 26  // Cambia si usas otro pin

MQ135 mq135_sensor(PIN_MQ135);

void setup() {
    Serial.begin(115200);
    Serial.println("📢 Iniciando calibración del MQ-135...");
    delay(5000);  // Esperar estabilización
}

void loop() {
    float R0 = mq135_sensor.getRZero();  // Calcular R0 en aire limpio
    Serial.println("🔥 Valor R0 calculado: " + String(R0));
    delay(2000);  // Leer cada 2 segundos
}
