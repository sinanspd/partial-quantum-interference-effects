package com.sinanspd

import com.sinanspd.qure.circuit.QVec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/** Records exactly one sampled state, even when several reaction workers cross
  * the sampling threshold concurrently.
  *
  * Set `-Dcham.sample.file=/path/to/result.txt` or `CHAM_SAMPLE_FILE` to use a
  * separate result file for each experiment.
  */
final class SampledStateRecorder(
    configuredOutputPath: Option[Path] = None,
    context: Vector[(String, String)] = Vector.empty
) {
  private val firstSample = new AtomicReference[QVec]()
  private val sampleReady = new CountDownLatch(1)

  val outputPath: Path = configuredOutputPath
    .getOrElse(
      Paths.get(
        sys.props
          .get("cham.sample.file")
          .orElse(sys.env.get("CHAM_SAMPLE_FILE"))
          .getOrElse("target/sampled-state.txt")
      )
    )
    .toAbsolutePath
    .normalize

  /** Returns true only for the worker that selected the first sample. */
  def tryRecord(sample: QVec): Boolean = {
    val selected = firstSample.compareAndSet(null, sample)
    if (selected) {
      try {
        val bits = sample.v.map(if (_) '1' else '0').mkString
        val contextLines =
          if (context.isEmpty) ""
          else context.map { case (key, value) => s"$key=$value\n" }.mkString
        val contents =
          contextLines +
            s"bits=$bits\n" +
            s"amplitude.real=${sample.prop.real}\n" +
            s"amplitude.imag=${sample.prop.imag}\n" +
            s"molecule=$sample\n"

        val fileStatus = writeResult(contents)
        Console.out.synchronized {
          println(
            s"""
               |==================== SAMPLED STATE ====================
               |bits:      $bits
               |amplitude: ${sample.prop}
               |molecule:  $sample
               |${context.map { case (key, value) => s"$key: $value" }.mkString(", ")}
               |$fileStatus
               |=======================================================""".stripMargin
          )
        }
      } finally {
        // Release the application thread only after the selected state has been
        // fully reported. This prevents an asynchronous CHAM run from ending
        // merely because its main method reached the end.
        sampleReady.countDown()
      }
    }
    selected
  }

  def awaitSample(): QVec = {
    sampleReady.await()
    firstSample.get()
  }

  def appendMetadata(metadata: Vector[(String, String)]): Unit = {
    if (metadata.nonEmpty) {
      try {
        val contents = metadata.map { case (key, value) => s"$key=$value\n" }.mkString
        Files.write(
          outputPath,
          contents.getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND,
          StandardOpenOption.WRITE
        )
      } catch {
        case NonFatal(error) =>
          Console.err.println(
            s"Could not append run metadata to $outputPath: " +
              s"${error.getClass.getSimpleName}: ${error.getMessage}"
          )
      }
    }
  }

  private def writeResult(contents: String): String = {
    try {
      Option(outputPath.getParent).foreach(Files.createDirectories(_))
      Files.write(
        outputPath,
        contents.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE
      )
      s"saved to:   $outputPath"
    } catch {
      case NonFatal(error) =>
        s"not saved: ${error.getClass.getSimpleName}: ${error.getMessage}"
    }
  }
}
