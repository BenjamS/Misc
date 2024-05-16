// Simple I2C test for ebay 128x64 oled.
#include <Wire.h>
#include "SSD1306Ascii.h"
#include "SSD1306AsciiWire.h"
#include <Adafruit_INA219.h>
#include <SdFat.h>

//declare timer trigger flag and counter value
volatile boolean triggered = false;

// 0X3C+SA0 - 0x3C or 0x3D
#define I2C_ADDRESS 0x3C

// Define proper RST_PIN if required.
#define RST_PIN -1

Adafruit_INA219 ina219;
SSD1306AsciiWire oled;
float current_mA = 0.0, oldcurr = 0.0;
float busvoltage = 0.0, oldbusvoltage = 0.0;
float shuntvoltage = 0.0, oldshuntvoltage = 0.0;
float loadvoltage = 0.0, oldvolt = 0.0;
float power_mW = 0.0, oldpow = 0.0;
float energy_mWh = 0.0, oldegy = 0.0;
unsigned long elapsed = 0;

// Keep track of total time and milliamp measurements for milliamp-hour computation.
//uint32_t total_sec = 0;
//float total_mA = 0.0;
//uint8_t counter = 0;

//declare microSD variables
#define CHIPSELECT 10
#define ENABLE_DEDICATED_SPI 1
#define SD_CONFIG SdSpiConfig(SD_CS_PIN, DEDICATED_SPI, SPI_CLOCK)
#define SPI_DRIVER_SELECT 0
uint8_t cycles = 0;
SdFat32 sd;
File32 measurFile;


//------------------------------------------------------------------------------
void setup() {
    // Disable ADC
  ADCSRA = 0;
  ACSR = 0x80;

  Serial.begin(115200);
//  Wire.begin();
//  Wire.setClock(400000L);

  // Set up the INA219
  ina219.begin();
//  if (! ina219.begin()) {
//    Serial.println("Failed to find INA219 chip");
 //   while (1) { delay(10); }
 // }
  // By default the INA219 will be calibrated with a range of 32V, 2A.
  // However uncomment one of the below to change the range.  A smaller
  // range can't measure as large of values but will measure with slightly
  // better precision.
  //ina219.setCalibration_32V_1A();
  ina219.setCalibration_16V_400mA();

  //setup the SDcard reader
  sd.begin(CHIPSELECT);
  measurFile.open("MEAS.csv", O_WRITE | O_CREAT | O_TRUNC);
  measurFile.print("Time,Voltage,Current\n");
  measurFile.sync();

#if RST_PIN >= 0
  oled.begin(&Adafruit128x64, I2C_ADDRESS, RST_PIN);
#else   // RST_PIN >= 0
  oled.begin(&Adafruit128x64, I2C_ADDRESS);
#endif  // RST_PIN >= 0
  oled.setFont(Adafruit5x7);
  oled.clear();

  // whatever this is
  // stop interrupts
  cli();

  // TIMER 1 for interrupt frequency 1 Hz:

  //initialise the CCR register and the counter
  TCCR1A = 0;
  TCCR1B = 0;
  TCNT1  = 0;
  
  // set compare match register for 10 Hz increments
  OCR1A = 12499; // = 8000000 / (64 * 10) - 1 (must be <65536)
  
  // turn on CTC mode
  TCCR1B |= (1 << WGM12);
  
  // Set CS12, CS11 and CS10 bits for 64 prescaler
  TCCR1B |= (0 << CS12) | (1 << CS11) | (1 << CS10);
  
  // enable timer compare interrupt
  TIMSK1 |= (1 << OCIE1A);

  // allow interrupts
  sei();

//  uint32_t m = micros();
//  oled.clear();
//  oled.println("Current:");
//  oled.println("A long line may be truncated");
//  oled.println();
//  oled.set2X();
//  oled.println("2X demo");
//  oled.set1X();
//  oled.print("\nmicros: ");
//  oled.print(micros() - m);
}
//------------------------------------------------------------------------------
//void loop(void) 
void loop() 
{

      //get the values measured by the INA219
    ina219values();

//  float shuntvoltage = 0;
//  float busvoltage = 0;
//  float current_mA = 0;
//  float loadvoltage = 0;
//  float power_mW = 0;

	//	Display update procedure in main loop to avoid
	//		wasting clock time in function call
	//
    //update the voltage line on the SSD1306 display
	if(loadvoltage != oldvolt){
		displayline(loadvoltage, 0, " V");
		oldvolt = loadvoltage;
	}
	
    //update the current line on the SSD1306 display
	if(current_mA != oldcurr){
		displayline(current_mA, 2, " mA");
		oldcurr = current_mA;
	}
	
    //update the power line on the SSD1306 display
	if(power_mW != oldpow){
		displayline(power_mW, 4, " mW");
		oldpow = power_mW;
	}
	
    //update the energy line on the SSD1306 display
	if(energy_mWh != oldegy){
		displayline(energy_mWh, 6, " mWh");
		oldegy = energy_mWh;
	}


//  shuntvoltage = ina219.getShuntVoltage_mV();
//  busvoltage = ina219.getBusVoltage_V();
//  current_mA = ina219.getCurrent_mA();
//  power_mW = ina219.getPower_mW();
//  loadvoltage = busvoltage + (shuntvoltage / 1000);
  
//  oled.clear();
//  oled.print("Bus Voltage:  "); oled.print(busvoltage); oled.println(" V");
//  oled.print("Shunt Voltage:"); oled.print(shuntvoltage); oled.println(" mV");
//  oled.print("Load Voltage: "); oled.print(loadvoltage); oled.println(" V");
//  oled.print("Current:      "); oled.print(current_mA); oled.println(" mA");
//  oled.print("Power:        "); oled.print(power_mW); oled.println(" mW");
//  oled.println("");

  shuntvoltage = ina219.getShuntVoltage_mV();
  busvoltage = ina219.getBusVoltage_V();
  current_mA = ina219.getCurrent_mA();
  power_mW = ina219.getPower_mW();
  loadvoltage = busvoltage + (shuntvoltage / 1000);
  
  Serial.print("Bus Voltage:   "); Serial.print(busvoltage); Serial.println(" V");
  Serial.print("Shunt Voltage: "); Serial.print(shuntvoltage); Serial.println(" mV");
  Serial.print("Load Voltage:  "); Serial.print(loadvoltage); Serial.println(" V");
  Serial.print("Current:       "); Serial.print(current_mA); Serial.println(" mA");
  Serial.print("Power:         "); Serial.print(power_mW); Serial.println(" mW");
  Serial.println("");


  delay(250);
}


void displayline(const float measurment, const uint8_t line_num, const char line_end[]) {
  char floatbuf[16]={0};
  
  //format the line ([-]xxxxx.xxx [unit])
  dtostrf(measurment, 10, 3, floatbuf);
  strcat(floatbuf, line_end);
  
  //place the cursor and write the line
  oled.setCursor(0, line_num);
  oled.print(floatbuf);
}

void ina219values() {
  float shuntvoltage = 0.0;
  float busvoltage = 0.0;
  //float current_mA = 0.0;
  
  //turn the INA219 on
  ina219.powerSave(false);
  
  //get the shunt voltage, bus voltage, current and power consumed from the INA219
  shuntvoltage = ina219.getShuntVoltage_mV();
  busvoltage = ina219.getBusVoltage_V();
  current_mA = ina219.getCurrent_mA();
  elapsed = millis();

  //turn the INA219 off
  ina219.powerSave(true);

  //compute the load voltage
  loadvoltage = busvoltage + (shuntvoltage / 1000.0);

  //compute the power consumed
  power_mW = loadvoltage*current_mA;
  
  //compute the energy consumed (t = elapsed[ms] / 3600[s/h] * 1000[ms/s])
  energy_mWh += power_mW * ( elapsed / 3600000.0);
}


