package dc10.scalaq

import dc10.scala.predef.{Applications, Functions, Variables}
import dc10.scala.predef.datatype.{ComplexTypes, PrimitiveTypes, TemplateTypes}
import dc10.scala.predef.file.Files
import dc10.scala.predef.namespace.{Objects, Packages}
import dc10.scalaq.numbers.Nats

trait dsl

object dsl extends dsl
  // Lambda calculus
  with Applications.Mixins with Functions.Mixins with Variables.Mixins
  // Datatypes
  with ComplexTypes.Mixins with PrimitiveTypes.Mixins with TemplateTypes.Mixins
  // Namespaces
  with Objects.Mixins with Packages.Mixins
  // Source files
  with Files.Mixins
  // Linear abd Dependent types
  with LinearTypes.Mixins with Pi.Mixins with Vectors.Mixins with Nats.Mixins
  