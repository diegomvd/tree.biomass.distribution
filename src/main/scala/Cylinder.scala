case class Cylinder(
                     o: Point3D,
                     d: Double // In cm.
                   ):

  def overlapBinary(cylinder: Cylinder): Boolean =
    Cylinder.overlapBinary(this.o,this.d,cylinder.o,cylinder.d)

  def overlapN(cylinders: List[Cylinder]): Boolean =
    cylinders.forall(c => this.overlapBinary(c))

object Cylinder:

  def overlapBinary(
                     o1: Point3D,
                     d1: Double,
                     o2: Point3D,
                     d2: Double
                   ):
  Boolean =
    o1.distance2D(o2) < 0.5*(d1/100.0+d2/100.0)

end Cylinder
