package com.sinanspd

object Fs2BranchJitterVerification extends App {
  val first = Fs2BranchJitter.schedule(
    branchCount = 128,
    maximumDelayMillis = 5,
    seed = 260601922L
  )
  val repeated = Fs2BranchJitter.schedule(
    branchCount = 128,
    maximumDelayMillis = 5,
    seed = 260601922L
  )

  assert(first == repeated)
  assert(first.delaysMillis.length == 128)
  assert(first.delaysMillis.forall(delay => delay >= 0 && delay <= 5))
  assert(first.delaysMillis.distinct.length > 1)
  assert(first.minimumMillis >= 0)
  assert(first.maximumMillis <= 5)
  assert(first.meanMillis >= first.minimumMillis.toDouble)
  assert(first.meanMillis <= first.maximumMillis.toDouble)

  val disabled = Fs2BranchJitter.schedule(
    branchCount = 10,
    maximumDelayMillis = 0,
    seed = 1L
  )
  assert(disabled.delaysMillis == Vector.fill(10)(0))

  println("FS2 branch-jitter verification passed.")
}
