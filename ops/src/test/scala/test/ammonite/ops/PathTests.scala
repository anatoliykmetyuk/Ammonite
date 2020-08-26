package test.ammonite.ops

import java.nio.file.Paths

import ammonite.ops._
import utest._
object PathTests extends TestSuite{
  val tests = Tests {
    test("Basic"){
      val rel = Symbol("src")/Symbol("main")/Symbol("scala")
      test("Transformers"){
        if(Unix()){
          assert(
            // ammonite.Path to java.nio.file.Path
            (root/Symbol("omg")).toNIO == Paths.get("/omg"),

            // java.nio.file.Path to ammonite.Path
            root/Symbol("omg") == Path(Paths.get("/omg")),
            empty/Symbol("omg") == RelPath(Paths.get("omg")),

            // ammonite.Path to String
            (root/Symbol("omg")).toString == "/omg",
            (empty/Symbol("omg")).toString == "omg",
            (up/Symbol("omg")).toString == "../omg",
            (up/up/Symbol("omg")).toString == "../../omg",

            // String to ammonite.Path
            root/Symbol("omg") == Path("/omg"),
            empty/Symbol("omg") == RelPath("omg")
          )
        }
      }

      test("RelPath"){
        test("Constructors"){
          test("Symbol"){
            if (Unix()){
              val rel1 = rel / Symbol("ammonite")
              assert(
                rel1.segments == Seq("src", "main", "scala", "ammonite"),
                rel1.toString == "src/main/scala/ammonite"
              )
            }
          }
          test("String"){
            if (Unix()){
              val rel1 = rel / "Path.scala"
              assert(
                rel1.segments == Seq("src", "main", "scala", "Path.scala"),
                rel1.toString == "src/main/scala/Path.scala"
              )
            }
          }
          test("Combos"){
            def check(rel1: RelPath) = assert(
              rel1.segments == Seq("src", "main", "scala", "sub1", "sub2"),
              rel1.toString == "src/main/scala/sub1/sub2"
            )
            test("ArrayString"){
              if (Unix()){
                val arr = Array("sub1", "sub2")
                check(rel / arr)
              }
            }
            test("ArraySymbol"){
              if (Unix()){
                val arr = Array(Symbol("sub1"), Symbol("sub2"))
                check(rel / arr)
              }
            }
            test("SeqString"){
              if (Unix()) check(rel / Seq("sub1", "sub2"))
            }
            test("SeqSymbol"){
              if (Unix()) check(rel / Seq(Symbol("sub1"), Symbol("sub2")))
            }
            test("SeqSeqSeqSymbol"){
              if (Unix()){
                check(
                  rel / Seq(Seq(Seq(Symbol("sub1")), Seq()), Seq(Seq(Symbol("sub2"))), Seq())
                )
              }
            }
          }
        }
        test("Relativize"){
          def eq[T](p: T, q: T) = assert(p == q)
          test - eq(Symbol("omg")/Symbol("bbq")/Symbol("wtf") relativeTo Symbol("omg")/Symbol("bbq")/Symbol("wtf"), empty)
          test - eq(Symbol("omg")/Symbol("bbq") relativeTo Symbol("omg")/Symbol("bbq")/Symbol("wtf"), up)
          test - eq(Symbol("omg")/Symbol("bbq")/Symbol("wtf") relativeTo Symbol("omg")/Symbol("bbq"), empty/Symbol("wtf"))
          test - eq(Symbol("omg")/Symbol("bbq") relativeTo Symbol("omg")/Symbol("bbq")/Symbol("wtf"), up)
          test - eq(up/Symbol("omg")/Symbol("bbq") relativeTo Symbol("omg")/Symbol("bbq"), up/up/up/Symbol("omg")/Symbol("bbq"))
          test - intercept[PathError.NoRelativePath](Symbol("omg")/Symbol("bbq") relativeTo up/Symbol("omg")/Symbol("bbq"))
        }
      }
      test("AbsPath"){
        val d = pwd
        val abs = d / rel
        test("Constructor"){
          if (Unix()) assert(
            abs.toString.drop(d.toString.length) == "/src/main/scala",
            abs.toString.length > d.toString.length
          )
        }
        test("Relativize"){
          def eq[T](p: T, q: T) = assert(p == q)
          test - eq(root/Symbol("omg")/Symbol("bbq")/Symbol("wtf") relativeTo root/Symbol("omg")/Symbol("bbq")/Symbol("wtf"), empty)
          test - eq(root/Symbol("omg")/Symbol("bbq") relativeTo root/Symbol("omg")/Symbol("bbq")/Symbol("wtf"), up)
          test - eq(root/Symbol("omg")/Symbol("bbq")/Symbol("wtf") relativeTo root/Symbol("omg")/Symbol("bbq"), empty/Symbol("wtf"))
          test - eq(root/Symbol("omg")/Symbol("bbq") relativeTo root/Symbol("omg")/Symbol("bbq")/Symbol("wtf"), up)
          test - intercept[PathError.NoRelativePath](Symbol("omg")/Symbol("bbq") relativeTo up/Symbol("omg")/Symbol("bbq"))
        }
      }
      test("Ups"){
        test("RelativeUps"){
          val rel2 = rel/up
          assert(
            rel2 == Symbol("src")/Symbol("main"),
            rel/up/up == empty/Symbol("src"),
            rel/up/up/up == empty,
            rel/up/up/up/up == up,
            rel/up/up/up/up/up == up/up,
            up/rel == up/Symbol("src")/Symbol("main")/Symbol("scala")
          )
        }
        test("AbsoluteUps"){
          // Keep applying `up` and verify that the path gets
          // shorter and shorter and eventually errors.
          var abs = pwd
          var i = abs.segmentCount
          while(i > 0){
            abs/=up
            i-=1
            assert(abs.segmentCount == i)
          }
          intercept[PathError.AbsolutePathOutsideRoot.type]{ abs/up }
        }
        test("RootUpBreak"){
          intercept[PathError.AbsolutePathOutsideRoot.type]{ root/up }
          val x = root/"omg"
          val y = x/up
          intercept[PathError.AbsolutePathOutsideRoot.type]{ y / up }
        }
      }
      test("Comparison"){
        test("Relative") - assert(
          Symbol("omg")/Symbol("wtf") == Symbol("omg")/Symbol("wtf"),
          Symbol("omg")/Symbol("wtf") != Symbol("omg")/Symbol("wtf")/Symbol("bbq"),
          Symbol("omg")/Symbol("wtf")/Symbol("bbq") startsWith Symbol("omg")/Symbol("wtf"),
          Symbol("omg")/Symbol("wtf") startsWith Symbol("omg")/Symbol("wtf"),
          up/Symbol("omg")/Symbol("wtf") startsWith up/Symbol("omg")/Symbol("wtf"),
          !(Symbol("omg")/Symbol("wtf") startsWith Symbol("omg")/Symbol("wtf")/Symbol("bbq")),
          !(up/Symbol("omg")/Symbol("wtf") startsWith Symbol("omg")/Symbol("wtf")),
          !(Symbol("omg")/Symbol("wtf") startsWith up/Symbol("omg")/Symbol("wtf"))
        )
        test("Absolute") - assert(
          root/Symbol("omg")/Symbol("wtf") == root/Symbol("omg")/Symbol("wtf"),
          root/Symbol("omg")/Symbol("wtf") != root/Symbol("omg")/Symbol("wtf")/Symbol("bbq"),
          root/Symbol("omg")/Symbol("wtf")/Symbol("bbq") startsWith root/Symbol("omg")/Symbol("wtf"),
          root/Symbol("omg")/Symbol("wtf") startsWith root/Symbol("omg")/Symbol("wtf"),
          !(root/Symbol("omg")/Symbol("wtf") startsWith root/Symbol("omg")/Symbol("wtf")/Symbol("bbq"))
        )
        test("Invalid"){
          compileError("""root/'omg/'wtf < 'omg/'wtf""")
          compileError("""root/'omg/'wtf > 'omg/'wtf""")
          compileError("""'omg/'wtf < root/'omg/'wtf""")
          compileError("""'omg/'wtf > root/'omg/'wtf""")
        }
      }
    }

    test("Errors"){
      test("InvalidChars"){
        val ex = intercept[PathError.InvalidSegment](Symbol("src")/"Main/.scala")

        val PathError.InvalidSegment("Main/.scala", msg1) = ex

        assert(msg1.contains("[/] is not a valid character to appear in a path segment"))

        val ex2 = intercept[PathError.InvalidSegment](root/"hello"/".."/"world")

        val PathError.InvalidSegment("..", msg2) = ex2

        assert(msg2.contains("use the `up` segment from `os.up`"))
      }
      test("InvalidSegments"){
        intercept[PathError.InvalidSegment]{root/ "core/src/test"}
        intercept[PathError.InvalidSegment]{root/ ""}
        intercept[PathError.InvalidSegment]{root/ "."}
        intercept[PathError.InvalidSegment]{root/ ".."}
      }
      test("EmptySegment"){
        intercept[PathError.InvalidSegment](Symbol("src") / "")
        intercept[PathError.InvalidSegment](Symbol("src") / ".")
        intercept[PathError.InvalidSegment](Symbol("src") / "..")
      }
      test("CannotRelativizeAbsAndRel"){
        val abs = pwd
        val rel = Symbol("omg")/Symbol("wtf")
        compileError("""
          abs relativeTo rel
        """).check(
          """
          abs relativeTo rel
                         ^
          """,
          "type mismatch"
        )
        compileError("""
          rel relativeTo abs
                     """).check(
            """
          rel relativeTo abs
                         ^
            """,
            "type mismatch"
          )
      }
      test("InvalidCasts"){
        if(Unix()){
          intercept[IllegalArgumentException](Path("omg/cow"))
          intercept[IllegalArgumentException](RelPath("/omg/cow"))
        }
      }
    }
    test("Extractors"){
      test("regex"){
        val r"omg$x" = "omgasd"
        assert(x == "asd")
        val r"${y}omg" = "asdomg"
        assert(y == "asd")
        val r"omg${z}bbq" = "omgasdbbq"
        assert(z == "asd")
        val r"omg${a}b${b}bq" = "omgasdbbq"
        assert(a == "asd", b == "")
      }
      test("paths"){
        val a/b/c/d/"omg" = pwd/Symbol("A")/Symbol("B")/Symbol("C")/Symbol("D")/"omg"
        assert(
          a == pwd/Symbol("A"),
          b == "B",
          c == "C",
          d == "D"
        )

        // If the paths aren't deep enough, it
        // just doesn't match but doesn't blow up
        root/Symbol("omg") match {
          case a3/b3/c3/d3/e3 => assert(false)
          case _ =>
        }
      }
    }
    test("sorting"){
      assert(
        Seq(root/Symbol("c"), root, root/Symbol("b"), root/Symbol("a")).sorted == Seq(root, root/Symbol("a"), root/Symbol("b"), root/Symbol("c")),
        Seq(up/Symbol("c"), up/up/Symbol("c"), Symbol("b")/Symbol("c"), Symbol("a")/Symbol("c"), Symbol("a")/Symbol("d")).sorted ==
          Seq(Symbol("a")/Symbol("c"), Symbol("a")/Symbol("d"), Symbol("b")/Symbol("c"), up/Symbol("c"), up/up/Symbol("c"))
      )
    }
    test("construction"){
      test("success"){
        if(Unix()){
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"

          assert(
            RelPath(relStr) == Symbol("hello")/Symbol("cow"),
            // Path(...) also allows paths starting with ~,
            // which is expanded to become your home directory
            Path(absStr) == root/Symbol("hello")/Symbol("world")
          )

          // You can also pass in java.io.File and java.nio.file.Path
          // objects instead of Strings when constructing paths
          val relIoFile = new java.io.File(relStr)
          val absNioFile = java.nio.file.Paths.get(absStr)

          assert(
            RelPath(relIoFile) == Symbol("hello")/Symbol("cow"),
            Path(absNioFile) == root/Symbol("hello")/Symbol("world"),
            Path(relIoFile, root/Symbol("base")) == root/Symbol("base")/Symbol("hello")/Symbol("cow")
          )
        }
      }
      test("basepath"){
        if(Unix()){
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"
          assert(
            FilePath(relStr) == Symbol("hello")/Symbol("cow"),
            FilePath(absStr) == root/Symbol("hello")/Symbol("world")
          )
        }
      }
      test("based"){
        if(Unix()){
          val relStr = "hello/cow/world/.."
          val absStr = "/hello/world"
          val basePath: FilePath = FilePath(relStr)
          assert(
            Path(relStr, root/Symbol("base")) == root/Symbol("base")/Symbol("hello")/Symbol("cow"),
            Path(absStr, root/Symbol("base")) == root/Symbol("hello")/Symbol("world"),
            Path(basePath, root/Symbol("base")) == root/Symbol("base")/Symbol("hello")/Symbol("cow"),
            Path(".", pwd).last != ""
          )
        }
      }
      test("failure"){
        if(Unix()){
          val relStr = "hello/.."
          intercept[java.lang.IllegalArgumentException]{
            Path(relStr)
          }

          val absStr = "/hello"
          intercept[java.lang.IllegalArgumentException]{
            RelPath(absStr)
          }

          val tooManyUpsStr = "/hello/../.."
          intercept[PathError.AbsolutePathOutsideRoot.type]{
            Path(tooManyUpsStr)
          }
        }
      }
      test("symlinks"){

        val names = Seq(Symbol("test123"), Symbol("test124"), Symbol("test125"), Symbol("test126"))
        val twd = tmp.dir()

        test("nestedSymlinks"){
          if(Unix()) {
            names.foreach(p => rm ! twd/p)
            mkdir ! twd/Symbol("test123")
            ln.s(twd/Symbol("test124"), twd/Symbol("test123"))
            ln.s(twd/Symbol("test125"), twd/Symbol("test124"))
            ln.s(twd/Symbol("test126"), twd/Symbol("test125"))
            assert(os.followLink(twd/Symbol("test126")).get == os.followLink(twd/Symbol("test123")).get)
            names.foreach(p => rm ! twd/p)
            names.foreach(p => assert(!exists(twd/p)))
          }
        }

        test("danglingSymlink"){
          if(Unix()) {
            names.foreach(p => rm ! twd/p)
            mkdir ! twd/Symbol("test123")
            ln.s(twd/Symbol("test124"), twd/Symbol("test123"))
            ln.s(twd/Symbol("test125"), twd/Symbol("test124"))
            ln.s(twd/Symbol("test126"), twd/Symbol("test125"))
            rm ! twd / Symbol("test123")
            assert( os.followLink(twd / Symbol("test126")).isEmpty)
            names.foreach(p => rm ! twd / p)
            names.foreach(p => assert(!exists(twd / p)))
            names.foreach(p => rm ! twd/p)
            names.foreach(p => assert(!exists(twd/p)))
          }
        }
      }
    }
  }
}
