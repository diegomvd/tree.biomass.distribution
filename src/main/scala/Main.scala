import scala.math.pow
import scala.util.Random

object Main extends App:


  val result: Unit = time{

    val agb = main(
      radius = 400.0, // In meters.
      density = 0.5, // In trees per squared meter.
      dMin = 1.0, // In centimeters.
      dMax = 100.0, // In centimeters.
      m = 0.044, // Ratio between mortality and growth rates: average extracted from Taubert et al. 2015
      ah = 2.78, // Pre-factor of HD allometry.
      bh = 0.65, // Scaling exponent of HD allometry.
      ac = 0.4, // Pre-factor of CD allometry.
      bc = 0.65, // Scaling exponent of CD allometry.
      a = 0.016 * math.exp(0.204*0.204*0.5), // Pre-factor of AGB allometry.
      b = 2.013, // Scaling exponent of AGB allometry.
      seed = 1L
    )
    println(agb)
    println(agb.size)
    println(agb.max)
    println(agb.min)

  }

  private def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * pow(10.0, -9.0) + " s")
    result
  }

  def main(
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
          ):
  List[Double] =
    val rnd: Random = Random(seed)
    SimForest(radius, density, dMin, dMax, m, ah, bh, ac, bc, rnd).biomassDistribution(a, b)

end Main

