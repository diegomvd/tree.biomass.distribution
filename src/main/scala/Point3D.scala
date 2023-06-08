case class Point3D(
                    r: Double,
                    phi: Double,
                    z: Double
                  ):

  def distance2D(point3D: Point3D): Double =
    Point3D.distance2D( this.r, this.phi, point3D.r, point3D.phi)

  def distance3D(point3D: Point3D): Double =
    Point3D.distance3D( this.r, this.phi, this.z, point3D.r, point3D.phi, point3D.z)

object Point3D:

  private def distance2D(
                  r1: Double,
                  phi1: Double,
                  r2: Double,
                  phi2: Double
                ):
  Double =
    math.sqrt(  r1 * r1 + r2 * r2 + 2.0 * r1 * r2 * math.cos(phi1-phi2)  )

  private def distance3D(
                  r1: Double,
                  phi1: Double,
                  z1: Double,
                  r2: Double,
                  phi2: Double,
                  z2: Double
                ):
  Double =
    math.sqrt( r1 * r1 + r2 * r2 + 2.0 * r1 * r2 * math.cos(phi1 - phi2) + math.pow(z1-z2, 2.0) )

end Point3D

