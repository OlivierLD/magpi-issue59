hat = DeviceOpen["SenseHAT"]
temp = DeviceRead[hat, "Temperature"]
hum = DeviceRead[hat, "Humidity"]
accel = DeviceRead[hat, "Acceleration"]
accelSI = UnitConvert[accel, "Meters"/"Seconds"^2]
DeviceWrite[hat, "Hello, world!"]
DeviceWrite[hat, {"Now in color!", "ScrollSpeed" -> 0.25, "Color" -> {255, 0, 128}}]

list = {{{255, 0, 0}, {255, 0, 0}, {255, 0, 0}, {255, 0, 0}, {255, 0, 0}, {255, 0, 0}, {255, 0, 0}, {255, 0, 0}}, 
        {{0, 255, 0}, {0, 255, 0}, {0, 255, 0}, {0, 255, 0}, {0, 255, 0}, {0, 255, 0}, {0, 255, 0}, {0, 255, 0}}, 
		{{0, 0, 255}, {0, 0, 255}, {0, 0, 255}, {0, 0, 255}, {0, 0, 255}, {0, 0, 255}, {0, 0, 255}, {0, 0, 255}}, 
		{{255, 255, 0}, {255, 255, 0}, {255, 255, 0}, {255, 255, 0}, {255, 255, 0}, {255, 255, 0}, {255, 255, 0}, {255, 255, 0}}, 
		{{255, 0, 255}, {255, 0, 255}, {255, 0, 255}, {255, 0, 255}, {255, 0, 255}, {255, 0, 255}, {255, 0, 255}, {255, 0, 255}}, 
		{{0, 255, 255}, {0, 255, 255}, {0, 255, 255}, {0, 255, 255}, {0, 255, 255}, {0, 255, 255}, {0, 255, 255}, {0, 255, 255}}, 
		{{255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}}, 
		{{0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}}};
DeviceWrite[hat, list];

db = CreateDatabin[]
SaveReadingToDataDrop[
   bin_Databin] := (Module[{dev, hum, temp, pres}, 
    dev = DeviceOpen["SenseHAT"];
    temp = DeviceRead[dev, "Temperature"];
    pres = DeviceRead[dev, "Pressure"];
    hum = DeviceRead[dev, "Humidity"];
    DatabinAdd[
     bin, <|"temperature" -> temp, "humidity" -> hum, 
      "air pressure" -> pres|>];
    DeviceClose[dev];]);
cronTask = RunScheduledTask[SaveReadingToDataDrop[db], 300]

DateListPlot[Databin["gwSkMvMW"]]["air pressure"]

StockTickerPi[
   dev_DeviceObject] := (Module[{len, price, str, stock, stockList}, 
    stockList = FinancialData["NYSE:*", "Lookup"];
    Do[stock = RandomChoice[stockList];
     price = FinancialData[stock];
     If[Head[price] === Real, 
      str = StringDrop[ToString[stock], 5] <> "$" <> ToString[price];
      DeviceWrite[
       dev, {str, "ScrollSpeed" \[RightArrow] 0.05, 
        "Color" -> {200, 0, 0}}]], 100];]);
StockTickerPi[hat]

GameOfLifePi[sh_DeviceObject, rounds_Integer, pause_Real, 
   color_List] := 
  Module[{GameOfLife, StartingImage, images}, 
   GameOfLife = {224, {2, {{2, 2, 2}, {2, 1, 2}, {2, 2, 2}}}, {1, 1}};
   StartingImage = RandomInteger[{0, 1}, {8, 8}];
   images = # & /@ 
     CellularAutomaton[GameOfLife, StartingImage, rounds];
   (DeviceWrite[sh, {#, "Color" \[RightArrow] color}];
      Pause[pause]) & /@ images;];
GameOfLifePi[hat, 25, 0.5, {128, 128, 128}];