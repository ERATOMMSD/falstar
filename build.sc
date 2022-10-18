import mill._
import mill.scalalib._

object falstar extends ScalaModule {
    def javaVersion = "1.8"
    def scalaVersion = "2.12.13"
    def mainClass = Some("falstar.Main")

    def ivyDeps = Agg(
        ivy"org.apache.commons:commons-csv:1.8")
}
