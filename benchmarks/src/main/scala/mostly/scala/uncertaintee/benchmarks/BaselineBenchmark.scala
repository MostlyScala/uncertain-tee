package mostly.scala.uncertaintee.benchmarks

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.point
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class BaselineBenchmark extends BenchmarkSettings {

  private val point = Uncertain.point(0)

  @Benchmark
  def baseline(): Int = point.sample()
}
