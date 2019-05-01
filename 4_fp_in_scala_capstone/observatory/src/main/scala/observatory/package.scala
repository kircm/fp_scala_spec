package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  // mean Earth radius = 6371km
  // https://en.wikipedia.org/wiki/Great-circle_distance#Radius_for_spherical_Earth
  val MEDIAN_EARTH_RADIUS_KM = 6371
}
