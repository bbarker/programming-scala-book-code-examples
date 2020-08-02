// src/main/scala/progscala3/typesystem/typepaths/TypePath.scala

class Service:                                                       // <1>
  class Logger:
    def log(message: String): Unit = println(s"log: $message")       // <2>

  val logger: Logger = new Logger

val s1 = new Service
val s2 = new Service:
	override val logger = s1.logger                                    // <3>
