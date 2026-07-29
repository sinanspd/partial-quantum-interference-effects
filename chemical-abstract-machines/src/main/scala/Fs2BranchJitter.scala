package com.sinanspd

import java.util.concurrent.ThreadLocalRandom
import scala.util.Random

final case class Fs2BranchJitterSchedule(seed: Long, delaysMillis: Vector[Int]) {
  val minimumMillis: Int = if (delaysMillis.isEmpty) 0 else delaysMillis.min
  val maximumMillis: Int = if (delaysMillis.isEmpty) 0 else delaysMillis.max
  val meanMillis: Double =
    if (delaysMillis.isEmpty) 0d
    else delaysMillis.map(_.toLong).sum.toDouble / delaysMillis.length.toDouble
}

object Fs2BranchJitter {
  def resolveSeed(seedOverride: Option[Long]): Long =
    seedOverride.getOrElse(
      ThreadLocalRandom.current().nextLong() ^
        System.nanoTime() ^
        System.currentTimeMillis()
    )

  def schedule(
      branchCount: Int,
      maximumDelayMillis: Int,
      seed: Long
  ): Fs2BranchJitterSchedule = {
    require(branchCount >= 0, "FS2 branch count cannot be negative")
    require(
      maximumDelayMillis >= 0 && maximumDelayMillis < Int.MaxValue,
      "FS2 branch jitter must be between zero and Int.MaxValue - 1 milliseconds"
    )
    val random = new Random(seed)
    val delays =
      if (maximumDelayMillis == 0) Vector.fill(branchCount)(0)
      else Vector.fill(branchCount)(random.nextInt(maximumDelayMillis + 1))
    Fs2BranchJitterSchedule(seed, delays)
  }
}
