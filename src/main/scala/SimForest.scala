import scala.annotation.tailrec
import scala.util.Random


case class SimForest(
                      trees: List[SimTree]
                    ):

  def biomassDistribution(
                           a: Double,
                           b: Double
                         ):
  List[Double] =
    this.trees.map(_.biomass(a,b))

object SimForest:

  //val bdist: Unit = biomassDistribution(radius = 10.0,density = 1.0,dMin = 1.0, dMax = 10.0, m=1.0, a = 1.0,b = 1.0,seed = 1L)

  @main def biomassDistribution(
                                 radius: Double,
                                 density: Double,
                                 dMin: Double,
                                 dMax: Double,
                                 m: Double,
                                 ah: Double,
                                 bh: Double,
                                 ac: Double,
                                 bc: Double,
                                 a: Double,
                                 b: Double,
                                 seed: Long
                               ): Unit =
    val rnd: Random = Random(seed)
    val agbDist: List[Double] = SimForest(radius, density, dMin, dMax, m, ah, bh, ac, bc, rnd).biomassDistribution(a, b)
    println(agbDist)

  def apply(
             radius: Double,
             density: Double,
             dMin: Double,
             dMax: Double,
             m: Double,
             ah: Double,
             bh: Double,
             ac: Double,
             bc: Double,
             rnd: Random
           ):
  SimForest =

    def stopForestGrowth(
                          composition: List[SimTree],
                          density: Double,
                          radius: Double
                        ):
    Boolean =
      math.Pi * math.pow(radius,2) * density < composition.size.toDouble

    @tailrec
    def rec(
             composition: List[SimTree],
           ):
    List[SimTree] =
      if stopForestGrowth(composition, density, radius)
      then composition
      else {
        val newTree: SimTree = SimForest.createLegalTree(radius, dMin, dMax, m, ah, bh, ac, bc, composition, rnd)
        val newComposition = newTree :: composition
        println("Current tree density:")
        println(newComposition.size/(math.Pi*math.pow(radius,2)))
        rec(newComposition)
      }

    val firstTree: SimTree = SimTree(dMin,dMax,m,radius,ah,bh,ac,bc,rnd)
    val trees: List[SimTree] = rec( List(firstTree) )
    new SimForest(trees)

  private def createLegalTree(
                               radius: Double,
                               dMin: Double,
                               dMax: Double,
                               m: Double,
                               ah: Double,
                               bh: Double,
                               ac: Double,
                               bc: Double,
                               trees: List[SimTree],
                               rnd: Random
                             ):
  SimTree =

    @tailrec
    def rec(
             tree: Option[SimTree]
           ):
    Option[SimTree] =
      if tree.isDefined
      then tree
      else {
        val newTree: SimTree = SimTree(dMin,dMax,m,radius,ah,bh,ac,bc,rnd)
        val (stems, crowns): (List[Cylinder], List[Sphere]) = trees.map( t => (t.stem,t.crown) ).unzip

        if newTree.stem.overlapN(stems) && newTree.crown.overlapN(crowns) && newTree.crown.overlapNCylinders(stems)
        then rec(None)
        else rec(Some(newTree))
      }

    rec(None).getOrElse( SimTree( Point3D(0.0,0.0,0.0), Cylinder(Point3D(0.0,0.0,0.0),0.0), Sphere(Point3D(0.0,0.0,0.0),0.0) ) )

end SimForest

