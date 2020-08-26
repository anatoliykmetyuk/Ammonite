package test.ammonite.ops

import ammonite.ops._
import utest._

object ShelloutTests extends TestSuite{
  val scriptFolder = pwd/Symbol("ops")/Symbol("src")/Symbol("test")/Symbol("resources")/Symbol("scripts")

  val tests = Tests {
    test("implicitWd"){
      import ammonite.ops.ImplicitWd._
      test("lines"){
        val res = %%(Symbol("ls"), "ops/src/test/resources/testdata")
        assert(res.out.lines == Seq("File.txt", "folder1", "folder2"))
      }
      test("string"){
        val res = %%(Symbol("ls"), "ops/src/test/resources/testdata")
        assert(res.out.string == "File.txt\nfolder1\nfolder2\n")
      }
      test("bytes"){
        if(Unix()){
          val res = %%(Symbol("echo"), "abc")
          val listed = res.out.bytes
          //        assert(listed == "File.txt\nfolder\nfolder2\nFile.txt".getBytes)
          listed.toSeq
        }
      }
      test("chained"){
        assert(%%(Symbol("git"), Symbol("init")).out.string.contains("Reinitialized existing Git repository"))
        assert(%%(Symbol("git"), "init").out.string.contains("Reinitialized existing Git repository"))
        assert(%%(Symbol("ls"), pwd).out.string.contains("readme.md"))
      }
      test("basicList"){
        val files = List("readme.md", "build.sbt")
        val output = %%(Symbol("ls"), files).out.string
        assert(files.forall(output.contains))
      }
      test("listMixAndMatch"){
        val stuff = List("I", "am", "bovine")
        val result = %%(Symbol("echo"), "Hello,", stuff, "hear me roar")
        assert(result.out.string.contains("Hello, " + stuff.mkString(" ") + " hear me roar"))
      }
      test("failures"){
        val ex = intercept[ShelloutException]{ %%(Symbol("ls"), "does-not-exist") }
        val res: CommandResult = ex.result
        assert(
          res.exitCode != 0,
          res.err.string.contains("No such file or directory")
        )
      }

      test("filebased"){
        if(Unix()){
          assert(%%(scriptFolder/Symbol("echo"), Symbol("HELLO")).out.lines.mkString == "HELLO")

          val res: CommandResult =
            %%(root/Symbol("bin")/Symbol("bash"), "-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)

          assert(res.out.string.trim == "Hello123")
        }
      }
      test("filebased2"){
        if(Unix()){
          val res = %%(Symbol("which"), Symbol("echo"))
          val echoRoot = Path(res.out.string.trim)
          assert(echoRoot == root/Symbol("bin")/Symbol("echo"))

          assert(%%(echoRoot, Symbol("HELLO")).out.lines == Seq("HELLO"))
        }
      }

      test("envArgs"){
        val res0 = %%(Symbol("bash"), "-c", "echo \"Hello$ENV_ARG\"", ENV_ARG=12)
        assert(res0.out.lines == Seq("Hello12"))

        val res1 = %%(Symbol("bash"), "-c", "echo \"Hello$ENV_ARG\"", ENV_ARG=12)
        assert(res1.out.lines == Seq("Hello12"))

        val res2 = %%(Symbol("bash"), "-c", "echo 'Hello$ENV_ARG'", ENV_ARG=12)
        assert(res2.out.lines == Seq("Hello$ENV_ARG"))

        val res3 = %%(Symbol("bash"), "-c", "echo 'Hello'$ENV_ARG", ENV_ARG=123)
        assert(res3.out.lines == Seq("Hello123"))
      }

    }
    test("workingDirectory"){
      implicit var wd = pwd
      val listed1 = %%(Symbol("ls"))

      wd /= up

      val listed2 = %%(Symbol("ls"))

      assert(listed2 != listed1)
    }
    test("customWorkingDir"){
      val res1 = %.ls()(pwd) // explicitly
      // or implicitly
      import ammonite.ops.ImplicitWd._
      val res2 = %ls
    }
    test("fileCustomWorkingDir"){
      if(Unix()){
        val output = %%.apply(scriptFolder/Symbol("echo_with_wd"), Symbol("HELLO"))(root/Symbol("usr"))
        assert(output.out.lines == Seq("HELLO /usr"))
      }
    }
  }
}
