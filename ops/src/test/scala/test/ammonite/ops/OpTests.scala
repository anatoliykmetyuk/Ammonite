package test.ammonite.ops

import java.nio.file.NoSuchFileException
import java.nio.{file => nio}

import ammonite.ops._

import utest._
object OpTests extends TestSuite{

  val tests = Tests {
    val res = pwd/Symbol("ops")/Symbol("src")/Symbol("test")/Symbol("resources")/Symbol("testdata")
    test("ls") - assert(
      ls(res).toSet == Set(res/Symbol("folder1"), res/Symbol("folder2"), res/"File.txt"),
      ls(res/Symbol("folder2")).toSet == Set(
        res/Symbol("folder2")/Symbol("folder2a"),
        res/Symbol("folder2")/Symbol("folder2b")
      )

//      ls(res/'folder2/'folder2b) == Seq()
    )
    test("lsR"){
      ls.rec(res).foreach(println)
      intercept[java.nio.file.NoSuchFileException](ls.rec(pwd/Symbol("target")/Symbol("nonexistent")))
      assert(
        ls.rec(res/Symbol("folder2")/Symbol("folder2b")) == Seq(res/Symbol("folder2")/Symbol("folder2b")/"b.txt"),
        ls.rec(res/Symbol("folder2")) == Seq(
          res/Symbol("folder2")/Symbol("folder2a"),
          res/Symbol("folder2")/Symbol("folder2b"),
          res/Symbol("folder2")/Symbol("folder2a")/"I am.txt",
          res/Symbol("folder2")/Symbol("folder2b")/"b.txt"
        ),
        ls.rec(res) == Seq(
          res/"File.txt",
          res/Symbol("folder1"),
          res/Symbol("folder2"),
          res/Symbol("folder1")/"Yoghurt Curds Cream Cheese.txt",
          res/Symbol("folder2")/Symbol("folder2a"),
          res/Symbol("folder2")/Symbol("folder2b"),
          res/Symbol("folder2")/Symbol("folder2a")/"I am.txt",
          res/Symbol("folder2")/Symbol("folder2b")/"b.txt"
        )
      )
    }
    test("lsRecPermissions"){
      if(Unix()){
        assert(ls.rec(root/Symbol("var")/Symbol("run")).nonEmpty)
      }
    }
    test("readResource"){
      test("positive"){
        test("absolute"){
          val contents = read(resource/Symbol("test")/Symbol("ammonite")/Symbol("ops")/Symbol("folder")/"file.txt")
          assert(contents.contains("file contents lols"))

          val cl = getClass.getClassLoader
          val contents2 = read(resource(cl)/Symbol("test")/Symbol("ammonite")/Symbol("ops")/Symbol("folder")/"file.txt")
          assert(contents2.contains("file contents lols"))
        }

        test("relative"){
          val cls = classOf[_root_.test.ammonite.ops.Testing]
          val contents = read! resource(cls)/Symbol("folder")/"file.txt"
          assert(contents.contains("file contents lols"))

          val contents2 = read! resource(getClass)/Symbol("folder")/"file.txt"
          assert(contents2.contains("file contents lols"))
        }
      }
      test("negative"){
        test - intercept[ResourceNotFoundException]{
          read(resource/Symbol("folder")/"file.txt")
        }

        test - intercept[ResourceNotFoundException]{
          val cls = classOf[_root_.test.ammonite.ops.Testing]
          read(
            resource(cls)/Symbol("test")/Symbol("ammonite")/Symbol("ops")/Symbol("folder")/"file.txt"
          )
        }
        test - intercept[ResourceNotFoundException]{
          read(resource(getClass)/Symbol("test")/Symbol("ammonite")/Symbol("ops")/Symbol("folder")/"file.txt")
        }
        test - intercept[ResourceNotFoundException]{
          read(resource(getClass.getClassLoader)/Symbol("folder")/"file.txt")
        }
      }
    }
    test("rm"){
      // shouldn't crash
      rm! pwd/Symbol("target")/Symbol("nonexistent")
    }
    test("Mutating"){
      val testPath = pwd/Symbol("target")/Symbol("test")
      rm! testPath
      mkdir! testPath
      test("cp"){
        val d = testPath/Symbol("copying")
        test("basic"){
          assert(
            !exists(d/Symbol("folder")),
            !exists(d/Symbol("file"))
          )
          mkdir! d/Symbol("folder")
          write(d/Symbol("file"), "omg")
          assert(
            exists(d/Symbol("folder")),
            exists(d/Symbol("file")),
            read(d/Symbol("file")) == "omg"
          )
          cp(d/Symbol("folder"), d/Symbol("folder2"))
          cp(d/Symbol("file"), d/Symbol("file2"))

          assert(
            exists(d/Symbol("folder")),
            exists(d/Symbol("file")),
            read(d/Symbol("file")) == "omg",
            exists(d/Symbol("folder2")),
            exists(d/Symbol("file2")),
            read(d/Symbol("file2")) == "omg"
          )
        }
        test("deep"){
          write(d/Symbol("folderA")/Symbol("folderB")/Symbol("file"), "Cow", createFolders = true)
          cp(d/Symbol("folderA"), d/Symbol("folderC"))
          assert(read(d/Symbol("folderC")/Symbol("folderB")/Symbol("file")) == "Cow")
        }
      }
      test("mv"){
        test("basic"){
          val d = testPath/Symbol("moving")
          mkdir! d/Symbol("folder")
          assert(ls(d) == Seq(d/Symbol("folder")))
          mv(d/Symbol("folder"), d/Symbol("folder2"))
          assert(ls(d) == Seq(d/Symbol("folder2")))
        }
        test("shallow"){
          val d = testPath/Symbol("moving2")
          mkdir(d)
          write(d/"A.scala", "AScala")
          write(d/"B.scala", "BScala")
          write(d/"A.py", "APy")
          write(d/"B.py", "BPy")
          def fileSet = ls(d).map(_.last).toSet
          assert(fileSet == Set("A.scala", "B.scala", "A.py", "B.py"))
          test("partialMoves"){
            ls! d | mv{case r"$x.scala" => s"$x.java"}
            assert(fileSet == Set("A.java", "B.java", "A.py", "B.py"))
            ls! d | mv{case r"A.$x" => s"C.$x"}
            assert(fileSet == Set("C.java", "B.java", "C.py", "B.py"))
          }
          test("fullMoves"){
            ls! d | mv.all{case r"$x.$y" => s"$y.$x"}
            assert(fileSet == Set("scala.A", "scala.B", "py.A", "py.B"))
            def die = ls! d | mv.all{case r"A.$x" => s"C.$x"}
            intercept[MatchError]{ die }
          }
        }
        test("deep"){
          val d = testPath/Symbol("moving2")
          mkdir(d)
          mkdir(d/Symbol("scala"))
          mkdir(d/Symbol("py"))
          write(d/Symbol("scala")/Symbol("A"), "AScala")
          write(d/Symbol("scala")/Symbol("B"), "BScala")
          write(d/Symbol("py")/Symbol("A"), "APy")
          write(d/Symbol("py")/Symbol("B"), "BPy")
          test("partialMoves"){
            ls.rec! d | mv*{case d/"py"/x => d/x }
            assert(
              ls.rec(d).toSet == Set(
                d/Symbol("py"),
                d/Symbol("scala"),
                d/Symbol("scala")/Symbol("A"),
                d/Symbol("scala")/Symbol("B"),
                d/Symbol("A"),
                d/Symbol("B")
              )
            )
          }
          test("fullMoves"){
            def die = ls.rec! d | mv.all*{case d/"py"/x => d/x }
            intercept[MatchError]{ die }

            ls.rec! d |? (_.isFile) | mv.all*{
              case d/"py"/x => d/Symbol("scala")/Symbol("py")/x
              case d/"scala"/x => d/Symbol("py")/Symbol("scala")/x
              case d => println("NOT FOUND " + d); d
            }

            assert(
              ls.rec(d).toSet == Set(
                d/Symbol("py"),
                d/Symbol("scala"),
                d/Symbol("py")/Symbol("scala"),
                d/Symbol("scala")/Symbol("py"),
                d/Symbol("scala")/Symbol("py")/Symbol("A"),
                d/Symbol("scala")/Symbol("py")/Symbol("B"),
                d/Symbol("py")/Symbol("scala")/Symbol("A"),
                d/Symbol("py")/Symbol("scala")/Symbol("B")
              )
            )
          }
        }
        //          ls! wd | mv*
      }
      test("mkdirRm"){
        test("singleFolder"){
          val single = testPath/Symbol("single")
          mkdir! single/Symbol("inner")
          assert(ls(single) == Seq(single/Symbol("inner")))
          rm! single/Symbol("inner")
          assert(ls(single) == Seq())
        }
        test("nestedFolders"){
          val nested = testPath/Symbol("nested")
          mkdir! nested/Symbol("inner")/Symbol("innerer")/Symbol("innerest")
          assert(
            ls(nested) == Seq(nested/Symbol("inner")),
            ls(nested/Symbol("inner")) == Seq(nested/Symbol("inner")/Symbol("innerer")),
            ls(nested/Symbol("inner")/Symbol("innerer")) == Seq(nested/Symbol("inner")/Symbol("innerer")/Symbol("innerest"))
          )
          rm! nested/Symbol("inner")
          assert(ls(nested) == Seq())
        }
      }
      test("readWrite"){
        val d = testPath/Symbol("readWrite")
        mkdir! d
        test("simple"){
          write(d/Symbol("file"), "i am a cow")
          assert(read(d/Symbol("file")) == "i am a cow")
        }
        test("autoMkdir"){
          write(d/Symbol("folder")/Symbol("folder")/Symbol("file"), "i am a cow", createFolders = true)
          assert(read(d/Symbol("folder")/Symbol("folder")/Symbol("file")) == "i am a cow")
        }
        test("binary"){
          write(d/Symbol("file"), Array[Byte](1, 2, 3, 4))
          assert(read(d/Symbol("file")).toSeq == Array[Byte](1, 2, 3, 4).toSeq)
        }
        test("concatenating"){
          write(d/Symbol("concat1"), Seq("a", "b", "c"))
          assert(read(d/Symbol("concat1")) == "abc")
          write(d/Symbol("concat2"), Seq(Array[Byte](1, 2), Array[Byte](3, 4)))
          assert(read.bytes(d/Symbol("concat2")).toSeq == Array[Byte](1, 2, 3, 4).toSeq)
        }
        test("writeAppend"){
          write.append(d/"append.txt", "Hello")
          assert(read(d/"append.txt") == "Hello")
          write.append(d/"append.txt", " World")
          assert(read(d/"append.txt") == "Hello World")
        }
        test("writeOver"){
          write.over(d/"append.txt", "Hello")
          assert(read(d/"append.txt") == "Hello")
          write.over(d/"append.txt", " Wor")
          assert(read(d/"append.txt") == " Wor")
        }
      }
      test("Failures"){
        val d = testPath/Symbol("failures")
        mkdir! d
        test("nonexistant"){
          test - intercept[nio.NoSuchFileException](ls! d/Symbol("nonexistent"))
          test - intercept[nio.NoSuchFileException](read! d/Symbol("nonexistent"))
          test - intercept[ResourceNotFoundException](read! resource/Symbol("failures")/Symbol("nonexistent"))
          test - intercept[nio.NoSuchFileException](cp(d/Symbol("nonexistent"), d/Symbol("yolo")))
          test - intercept[nio.NoSuchFileException](mv(d/Symbol("nonexistent"), d/Symbol("yolo")))
        }
        test("collisions"){
          mkdir! d/Symbol("folder")
          write(d/Symbol("file"), "lolol")
          test - intercept[nio.FileAlreadyExistsException](mv(d/Symbol("file"), d/Symbol("folder")))
          test - intercept[nio.FileAlreadyExistsException](cp(d/Symbol("file"), d/Symbol("folder")))
          test - intercept[nio.FileAlreadyExistsException](write(d/Symbol("file"), "lols"))
         }
      }
    }
  }
}
