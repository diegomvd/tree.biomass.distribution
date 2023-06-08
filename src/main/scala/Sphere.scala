case class Sphere(
                   o: Point3D,
                   r: Double // In m.
                 ):

  def overlapBinary(sphere: Sphere): Boolean =
    Sphere.overlapBinary(this.o,this.r,sphere.o,sphere.r)

  def overlapCylinder(cylinder: Cylinder): Boolean =
    Sphere.overlapCylinder(this.o, this.r, cylinder.o, cylinder.d)

  def overlapN(spheres: List[Sphere]): Boolean =
    spheres.forall(s => this.overlapBinary(s))

  def overlapNCylinders(cylinders: List[Cylinder]): Boolean =
    cylinders.forall(c => this.overlapCylinder(c))


object Sphere:

  private def overlapBinary(
               o1: Point3D,
               r1: Double,
               o2: Point3D,
               r2: Double
             ):
  Boolean =
    o1.distance3D(o2) < r1 + r2

  private def overlapCylinder(
                             o1: Point3D,
                             r1: Double,
                             o2: Point3D,
                             d2: Double
                           ):
  Boolean =
    (o1.distance3D(o2) < r1 + d2/100.0 * 0.5) || (o2.z + r1 < o1.z)


end Sphere


