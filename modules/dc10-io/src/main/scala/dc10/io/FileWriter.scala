package dc10.io

import cats.{FlatMap, Traverse}
import cats.effect.Concurrent
import cats.implicits.*
import dc10.compile.{Compiler, VirtualFile}
import dc10.compile.Compiler.toVirtualFile
import dc10.compile.Config
import dc10.ast.Definition.SourceFile
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}
// import dc10.ast.SourceFile

trait FileWriter[F[_]]:
  def writeFile(vf: VirtualFile): F[Path]

object FileWriter:
  def apply[F[_]: Files: Concurrent]: FileWriter[F] =
    new FileWriter[F]:
      def writeFile(vf: VirtualFile): F[Path] =
        for
          d <- Concurrent[F].pure(Path.fromNioPath(vf.path.getParent()))
          _ <- Files[F].createDirectories(d)
          p <- Concurrent[F].pure(Path.fromNioPath(vf.path))
          _ <- Stream(vf.contents)
              .through(text.utf8.encode)
              .through(Files[F].writeAll(p))
              .compile
              .drain
        yield p

extension [E[_]: FlatMap: Traverse, V](res: E[List[SourceFile]])
  def toFile[F[_]: Concurrent: Files](
    using
      C: Compiler[E, V, SourceFile],
      D: Config[V],
  ): F[E[List[Path]]] =
    res.toVirtualFile.traverse(vfs =>
      vfs.traverse(vf => FileWriter[F].writeFile(vf)))
