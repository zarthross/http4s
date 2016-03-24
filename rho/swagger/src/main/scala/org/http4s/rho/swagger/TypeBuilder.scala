package org.http4s.rho.swagger

import java.util.Date
import java.time.Instant

import org.http4s.rho.bits.ResponseGenerator.EmptyRe
import org.http4s.rho.compat._

import org.log4s.getLogger

import scala.collection.mutable.LinkedHashMap
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

import scalaz._, Scalaz._

object TypeBuilder {
  import models._

  private[this] val logger = getLogger

  def collectModels(t: Type, alreadyKnown: Set[Model], sfs: SwaggerFormats): Set[Model] =
    try collectModels(dealias(t), alreadyKnown, Set.empty, sfs)
    catch { case NonFatal(e) => Set.empty }

  private def collectModels(t: Type, alreadyKnown: Set[Model], known: Set[Type], sfs: SwaggerFormats): Set[Model] = {

    def go(t: Type, alreadyKnown: Set[Model], known: Set[Type]): Set[Model] =
      dealias(t) match {

        case tpe if sfs.customSerializers.isDefinedAt(tpe) =>
          sfs.customSerializers(tpe)

        case tpe if tpe =:= weakTypeOf[EmptyRe] =>
          Set.empty

        case tpe if tpe.isEither || tpe.isMap =>
          go(typeArgs(tpe).head, alreadyKnown, typeArgs(tpe).toSet) ++
          go(typeArgs(tpe).last, alreadyKnown, typeArgs(tpe).toSet)

        case tpe if (tpe.isCollection || tpe.isOption) && typeArgs(tpe).nonEmpty =>
          val ntpe = typeArgs(tpe).head
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isProcess =>
          val ntpe = typeArgs(tpe).apply(1)
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isTask =>
          val ntpe = typeArgs(tpe).apply(0)
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if (alreadyKnown.map(_.id).contains(tpe.fullName) || (tpe.isPrimitive)) =>
          Set.empty

        case ExistentialType(_, _) =>
          Set.empty

        case tpe@TypeRef(_, sym: Symbol, tpeArgs: List[Type]) if isCaseClass(sym) =>
          val ctor = primaryConstructor(sym.asClass).asMethod
          val models = alreadyKnown ++ modelToSwagger(tpe, sfs)
          val generics = typeArgs(tpe).foldLeft(List[Model]()) { (acc, t) =>
            acc ++ go(t, alreadyKnown, typeArgs(tpe).toSet)
          }
          val children = paramLists(ctor).flatten.flatMap { paramsym =>
            val paramType =
              if (sym.isClass)
                paramsym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
              else
                sym.typeSignature
            go(paramType, alreadyKnown, known + tpe)
          }

          models ++ generics ++ children

        case e =>
          Set.empty
      }

    go(t, alreadyKnown, known)
  }

  private[this] val defaultExcluded =
    Set(typeOf[Nothing], typeOf[Null])

  private[this] def isCaseClass(sym: Symbol): Boolean =
    sym.isClass && sym.asClass.isCaseClass && primaryConstructor(sym.asClass).isMethod

  private[this] def isExcluded(t: Type, excludes: Seq[Type] = Nil) =
    (defaultExcluded ++ excludes).exists(_ =:= t)

  private def modelToSwagger(tpe: Type, sfs: SwaggerFormats): Option[Model] =
    try {
      val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
      val props: Map[String, Property] =
        paramLists(tpe
          .member(CONSTRUCTOR)
          .typeSignature.asInstanceOf[MethodSymbol])
          .flatten
          .map(paramSymToProp(sym, tpeArgs, sfs))
          .toMap

      ModelImpl(
        id          = tpe.fullName,
        id2         = tpe.simpleName,
        description = tpe.simpleName.some,
        properties  = props).some
    } catch {
      case NonFatal(t) => None
    }

  private def paramSymToProp
    (sym: Symbol, tpeArgs: List[Type], sfs: SwaggerFormats)(pSym: Symbol): (String, Property) = {
    val pType = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
    val required = !(pSym.asTerm.isParamWithDefault || pType.isOption)
    val prop = sfs.customFieldSerializers.applyOrElse(pType, { _: Type =>

      val TypeRef(_, ptSym: Symbol, ptTpeArgs: List[Type]) = pType

      if (pType.isCollection && !pType.isNothingOrNull)
        ArrayProperty(items = RefProperty(typeArgs(dealias(pType)).head.simpleName))
      else if (isCaseClass(ptSym))
        RefProperty(pType.simpleName)
      else
        DataType.fromType(pType) match {
          case DataType.ValueDataType(name, format, qName) =>
            AbstractProperty(`type` = name, description = qName, format = format)
          case DataType.ComplexDataType(name, qName) =>
            AbstractProperty(`type` = name, description = qName)
          case DataType.ContainerDataType(name, tpe, uniqueItems) =>
            AbstractProperty(`type` = name)
        }
    })
    (pSym.name.decodedName.toString, prop.withRequired(required))
  }

  sealed trait DataType {
    def name: String
  }

  object DataType {

    case class ValueDataType(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) extends DataType
    case class ContainerDataType(name: String, typeArg: Option[DataType] = None, uniqueItems: Boolean = false) extends DataType
    case class ComplexDataType(name: String, qualifiedName: Option[String] = None) extends DataType

    val Void = DataType("void")
    val String = DataType("string")
    val Byte = DataType("string", Some("byte"))
    val Int = DataType("integer", Some("int32"))
    val Long = DataType("integer", Some("int64"))
    val Float = DataType("number", Some("float"))
    val Double = DataType("number", Some("double"))
    val Boolean = DataType("boolean")
    val Date = DataType("string", Some("date"))
    val DateTime = DataType("string", Some("date-time"))

    object GenList {
      def apply(): DataType = ContainerDataType("List")
      def apply(v: DataType): DataType = new ContainerDataType("List", Some(v))
    }

    object GenSet {
      def apply(): DataType = ContainerDataType("Set", uniqueItems = true)
      def apply(v: DataType): DataType = new ContainerDataType("Set", Some(v), uniqueItems = true)
    }

    object GenArray {
      def apply(): DataType = ContainerDataType("Array")
      def apply(v: DataType): DataType = new ContainerDataType("Array", Some(v))
    }

    def apply(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) =
      new ValueDataType(name, format, qualifiedName)

    def apply(tag: TypeTag[_]): DataType = apply(tag.tpe)
    def apply(tag: Type): DataType = fromType(dealias(tag))

    private[this] val StringTypes = Set[Type](typeOf[String], typeOf[java.lang.String])
    private[this] def isString(t: Type) = StringTypes.exists(t =:= _)
    private[this] val BoolTypes = Set[Type](typeOf[Boolean], typeOf[java.lang.Boolean])
    private[this] def isBool(t: Type) = BoolTypes.exists(t =:= _)

    private[swagger] def fromType(t: Type): DataType = {
      val klass = if (t.isOption && typeArgs(t).size > 0) typeArgs(t).head else t
      if (klass <:< typeOf[Unit] || klass <:< typeOf[Void]) this.Void
      else if (t =:= weakTypeOf[EmptyRe]) this.Void
      else if (isString(klass)) this.String
      else if (klass <:< typeOf[Byte] || klass <:< typeOf[java.lang.Byte]) this.Byte
      else if (klass <:< typeOf[Long] || klass <:< typeOf[java.lang.Long]) this.Long
      else if (isInt(klass)) this.Int
      else if (klass <:< typeOf[Float] || klass <:< typeOf[java.lang.Float]) this.Float
      else if (isDecimal(klass)) this.Double
      else if (isDateTime(klass)) this.DateTime
      else if (isBool(klass)) this.Boolean
      else if (klass <:< typeOf[scala.collection.Set[_]] || klass <:< typeOf[java.util.Set[_]]) {
        if (typeArgs(t).nonEmpty) GenSet(fromType(typeArgs(t).head))
        else GenSet()
      } else if (klass <:< typeOf[collection.Seq[_]] || klass <:< typeOf[java.util.List[_]]) {
        if (typeArgs(t).nonEmpty) GenList(fromType(typeArgs(t).head))
        else GenList()
      } else if (t.isArray || isCollection(klass)) {
        if (typeArgs(t).nonEmpty) GenArray(fromType(typeArgs(t).head))
        else GenArray()
      } else if (t.isProcess) {
        if (typeArgs(t).nonEmpty) GenArray(fromType(typeArgs(t)(1)))
        else GenArray()
      } else {
        val stt = if (t.isOption) typeArgs(t).head else t
        ComplexDataType(stt.simpleName, qualifiedName = Option(stt.fullName))
      }
    }

    private[this] val IntTypes =
      Set[Type](
        typeOf[Int], typeOf[java.lang.Integer], typeOf[Short],
        typeOf[java.lang.Short], typeOf[BigInt], typeOf[java.math.BigInteger])

    private[this] def isInt(t: Type): Boolean = IntTypes.exists(t =:= _)

    private[this] val DecimalTypes =
      Set[Type](
        typeOf[Double], typeOf[java.lang.Double],
        typeOf[BigDecimal], typeOf[java.math.BigDecimal])

    private[this] def isDecimal(t: Type): Boolean =
      DecimalTypes.exists(t =:= _)

    private[this] val DateTimeTypes =
      Set[Type](typeOf[Date], typeOf[Instant])

    private[this] def isDateTime(t: Type): Boolean =
      DateTimeTypes.exists(t <:< _)

    private[this] def isCollection(t: Type): Boolean =
      t <:< typeOf[collection.Traversable[_]] || t <:< typeOf[java.util.Collection[_]]
  }
}
