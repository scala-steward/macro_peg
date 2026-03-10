package com.github.kmizu.macro_peg.ruby

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

object GeneratedRubyCorpusRunner {
  private val defaultRoots: List[String] = List(
    "third_party/ruby3/upstream/ruby/test/ruby",
    "third_party/ruby3/upstream/ruby/bootstraptest",
    "third_party/ruby3/upstream/ruby/test/prism"
  )

  private final case class FailureInfo(path: Path, reason: String, message: String)

  private def parseWithTimeout(input: String, timeoutMs: Int): Either[String, Any] = {
    @volatile var result: Option[(Any, Int)] = null
    @volatile var thrown: Throwable = null
    val worker = new Thread(
      () => {
        try {
          result = GeneratedRubyParser.parse(input)
        } catch {
          case t: Throwable => thrown = t
        }
      },
      "gen-ruby-corpus-worker"
    )
    worker.setDaemon(true)
    worker.start()
    worker.join(timeoutMs.toLong)

    if (worker.isAlive) {
      worker.interrupt()
      worker.join(10L)
      if (worker.isAlive) {
        try { worker.stop() } catch { case _: UnsupportedOperationException => () }
      }
      Left(s"timeout after ${timeoutMs}ms")
    } else if (thrown != null) {
      Left(s"exception: ${thrown.getClass.getSimpleName}: ${Option(thrown.getMessage).getOrElse("")}")
    } else if (result == null) {
      Left("exception: worker exited without result")
    } else {
      result match {
        case Some((v, pos)) => Right(v)
        case None           => Left("parse failed: no result")
      }
    }
  }

  private def collectRubyFiles(roots: List[Path]): List[Path] =
    roots.flatMap { root =>
      if (!Files.exists(root)) Nil
      else {
        val stream = Files.walk(root)
        try stream.iterator().asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".rb")).toList
        finally stream.close()
      }
    }.sorted

  private val codingPattern      = """(?i)\bcoding\s*[:=]\s*([A-Za-z0-9._-]+)""".r
  private val fileEncodingPattern = """(?i)\bfileencoding=([A-Za-z0-9._-]+)""".r

  private def detectRubyEncoding(bytes: Array[Byte]): Option[Charset] = {
    val header = new String(bytes.take(512), StandardCharsets.ISO_8859_1)
    val firstTwoLines = header.linesIterator.take(2).mkString("\n")
    val encodingName =
      codingPattern.findFirstMatchIn(firstTwoLines).map(_.group(1))
        .orElse(fileEncodingPattern.findFirstMatchIn(firstTwoLines).map(_.group(1)))
    encodingName.flatMap { name =>
      try Some(Charset.forName(name)) catch { case _: Exception => None }
    }
  }

  private def readRubySource(path: Path): String = {
    val bytes   = Files.readAllBytes(path)
    val charset = detectRubyEncoding(bytes).getOrElse(StandardCharsets.UTF_8)
    new String(bytes, charset)
  }

  def main(args: Array[String]): Unit = {
    val roots     = if (args.nonEmpty) args.toList.map(Paths.get(_)) else defaultRoots.map(Paths.get(_))
    val allFiles  = collectRubyFiles(roots)
    if (allFiles.isEmpty) {
      println("No .rb files found.")
      return
    }
    val timeoutMs   = sys.env.get("RUBY_CORPUS_TIMEOUT_MS").flatMap(_.toIntOption).getOrElse(5000)
    val sampleLimit = sys.env.get("RUBY_CORPUS_FAIL_SAMPLES").flatMap(_.toIntOption).getOrElse(20)
    val maxFiles    = sys.env.get("RUBY_CORPUS_MAX_FILES").flatMap(_.toIntOption).filter(_ > 0)
    val files       = maxFiles.map(allFiles.take).getOrElse(allFiles)
    maxFiles.foreach(lim => println(s"RUBY_CORPUS_MAX_FILES active: using first ${files.size}/${allFiles.size} files"))

    val started = System.nanoTime()
    var success = 0
    val failures = ArrayBuffer.empty[FailureInfo]

    files.foreach { path =>
      try {
        val source = readRubySource(path)
        parseWithTimeout(source, timeoutMs) match {
          case Right(_)    => success += 1
          case Left(error) =>
            val reason = if (error.startsWith("timeout")) "timeout"
                         else if (error.startsWith("exception")) "exception"
                         else "parse_error"
            failures += FailureInfo(path, reason, error.linesIterator.nextOption().getOrElse(error))
        }
      } catch {
        case NonFatal(e) =>
          failures += FailureInfo(path, "read_error", s"${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("")}")
      }
    }

    val total     = files.size
    val failed    = total - success
    val elapsedMs = (System.nanoTime() - started) / 1_000_000
    val rate      = if (total == 0) 0.0 else success.toDouble / total * 100.0

    println(f"GeneratedRubyParser corpus: total=$total success=$success failed=$failed rate=$rate%.2f%% elapsed_ms=$elapsedMs timeout_ms=$timeoutMs")
    if (failures.nonEmpty) {
      val samples = failures.take(sampleLimit)
      println(s"Failure samples (${samples.size}/${failures.size}):")
      samples.foreach(f => println(s"  - ${f.path}: ${f.reason}: ${f.message}"))
      // cluster
      failures.groupBy(f => s"${f.reason}: ${f.message.take(80)}")
        .toList.sortBy(-_._2.size).take(10)
        .foreach { case (k, vs) => println(s"  ${vs.size}x $k") }
    }
  }
}
