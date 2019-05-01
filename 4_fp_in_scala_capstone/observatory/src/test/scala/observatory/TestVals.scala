package observatory

object TestVals {

  /*  Range of temperatures and colors - for testing  */
  val TEMPERATURE_COLORS_TEST: Seq[(Temperature, Color)] = Seq(
    (-60, Color(0, 0, 0)),
    (-50, Color(33, 0, 107)),
    (-27, Color(255, 0, 255)),
    (-15, Color(0, 0, 255)),
    (0, Color(0, 255, 255)),
    (12, Color(255, 255, 0)),
    (32, Color(255, 0, 0)),
    (60, Color(255, 255, 255)))


  val knownTemperatures: Iterable[(Location, Temperature)] = Iterable(
    // In Greenwich meridian: long: 0.0
    (Location(0.0, 0.0), 40.0),
    (Location(30.0, 0.0), 30.0),
    (Location(45.0, 0.0), 20.0),
    (Location(60.0, 0.0), 0.0),

    // long: 90.0
    (Location(0.0, 90.0), 40.0),
    (Location(30.0, 90.0), 30.0),
    (Location(45.0, 90.0), 20.0),
    (Location(60.0, 90.0), 0.0),

    // long: -90.0
    (Location(0.0, -90.0), 40.0),
    (Location(30.0, -90.0), 30.0),
    (Location(45.0, -90.0), 20.0),
    (Location(60.0, -90.0), 0.0),

    // long: 180.0
    (Location(0.0, 180.0), 40.0),
    (Location(30.0, 180.0), 30.0),
    (Location(45.0, 180.0), 20.0),
    (Location(60.0, 180.0), 0.0)
  )

  val knownTemperatures2: Iterable[(Location, Temperature)] = Iterable(
    // In Greenwich meridian: long: 0.0
    (Location(0.0, 0.0), 40.0),
    (Location(30.0, 0.0), 30.0),
    (Location(45.0, 0.0), 20.0),
    (Location(60.0, 0.0), 0.0),

    // long: 90.0
    (Location(0.0, 90.0), 40.0),
    (Location(30.0, 90.0), 30.0),
    (Location(45.0, 90.0), 20.0),
    (Location(60.0, 90.0), 0.0),

    // long: -90.0
    (Location(0.0, -90.0), 40.0),
    (Location(30.0, -90.0), 30.0),
    (Location(45.0, -90.0), 20.0),
    (Location(60.0, -90.0), 0.0),

    // long: 180.0
    (Location(0.0, 180.0), 40.0),
    (Location(30.0, 180.0), 30.0),
    (Location(45.0, 180.0), 20.0),
    (Location(60.0, 180.0), 0.0),

    // Random locations
    (Location(0.0, -100.0), 10.0),
    (Location(10.0, -100.0), 10.0),
    (Location(20.0, -100.0), 12.0),
    (Location(30.0, -100.0), 10.0),
    (Location(30.0, -100.0), 10.0),
    (Location(0.0, 100.0), 40.0),
    (Location(10.0, 100.0), 30.0),
    (Location(20.0, 100.0), 30.0),
    (Location(30.0, 100.0), 30.0),
    (Location(30.0, 800.0), 30.0),
    (Location(-45.0, 100.0), -20.0),
    (Location(-45.0, 100.0), -20.0),
    (Location(-45.0, 100.0), -20.0),
    (Location(-75.0, 100.0), -20.0),
    (Location(75.0, 100.0), -10.0)
  )

}
