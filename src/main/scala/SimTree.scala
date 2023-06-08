import scala.annotation.tailrec
import scala.util.Random

case class SimTree(
                    location: Point3D,
                    stem: Cylinder,
                    crown: Sphere
                  ):

  def biomass(
               a: Double,
               b: Double
             ):
  Double =
    SimTree.biomass( this.crown.o.z + this.crown.r, this.crown.r, a, b )


object SimTree:

  def apply(
             dMin: Double,
             dMax: Double,
             m: Double,
             lR: Double,
             ah: Double,
             bh: Double,
             ac: Double,
             bc: Double,
             rnd: Random
           ):
  SimTree =

    val d: Double = SimTree.acceptedTreeDiameter(m, dMin, dMax, rnd)
    val h: Double = SimTree.allometry(d,ah,bh)
    val cr: Double = SimTree.allometry(d,ac,bc)

    val loc: Point3D = Point3D(rnd.between(0.0,lR), rnd.between(0.0,2.0*math.Pi), h)
    val origin: Point3D = Point3D( loc.r, loc.phi, h-cr )

    val stem: Cylinder = Cylinder(origin,d)
    val crown: Sphere = Sphere(origin,cr)

    new SimTree(loc,stem,crown)
  
  /**
   * From Taubert et al. 2015*/
  private def survivalProbability(
                                   d: Double,
                                   m: Double
                                 ):
  Double =
    math.exp( -m*d )

  private def acceptedTreeDiameter(
                                    m: Double,
                                    dMin: Double,
                                    dMax: Double,
                                    rnd: Random
                                  ):
  Double =

    @tailrec
    def rec(
             d: Option[Double],
             m: Double,
             rnd: Random
           ):
    Option[Double] =
      if d.isDefined
      then d
      else {
        val dTry: Double = rnd.between(dMin,dMax)
        if rnd.nextDouble() < survivalProbability(dTry,m) then rec(Some(dTry),m,rnd) else rec(None,m,rnd)
      }

    rec(None,m,rnd).getOrElse(0.0)


  private def allometry(
                         v: Double,
                         a: Double,
                         b: Double
                       ):
  Double =
    a * math.pow(v,b)

  private def biomass(
                       h: Double,
                       cr: Double,
                       a: Double,
                       b: Double
                     ):
  Double =
    println("Height and crown radius:")
    print(h);println(cr)
    allometry(h*cr,a,b)


end SimTree
