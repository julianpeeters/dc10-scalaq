package dc10.schema

import java.nio.file.Path

/**
 * The type A is, e.g., a single definition, or a list of defintions.
 */ 
case class FileDef[A](path: Path, contents: A)