package aoc


import aoc.util.Resources

object Day4 extends App {

  sealed trait PassportField extends Product with Serializable
  object PassportField {
    case object byr extends PassportField
    case object iyr extends PassportField
    case object eyr extends PassportField
    case object hgt extends PassportField
    case object hcl extends PassportField
    case object ecl extends PassportField
    case object pid extends PassportField
    case object cid extends PassportField

    val Required: List[PassportField] = List(byr, iyr, eyr, hgt, hcl, ecl, pid)
    val Optional: List[PassportField] = List(cid)
    val All: List[PassportField] = Required ++ Optional

    def fromStringOption(s: String): Option[PassportField] =
      All.find(_.toString.stripSuffix("$") == s)
  }

  case class ValidationCheck[A, B](run: A => Either[String, B]) {
    def >>[C](other: ValidationCheck[B, C]): ValidationCheck[A, C] =
      ValidationCheck(a => run(a).flatMap(other.run))

    def contramap[C](f: C => A): ValidationCheck[C, B] =
      ValidationCheck(c => run(f(c)))

    def void: ValidationCheck[A, Unit] =
      ValidationCheck(a => run(a).map(_ => ()))

    def isValid(value: A): Boolean =
      run(value).isRight
  }

  object ValidationCheck {
    trait Parse[A] {
      def apply(s: String): Option[A]
    }

    def constValid[A]: ValidationCheck[A, A] = ValidationCheck(Right(_))

    def matchesPattern(regex: String): ValidationCheck[String, String] =
      ValidationCheck(v => Either.cond(v.matches(regex), v, s"does not match $regex"))

    def atLeast(min: Int): ValidationCheck[Int, Int] =
      ValidationCheck(v => Either.cond(min <= v, v, s"less than $min"))

    def atMost(max: Int): ValidationCheck[Int, Int] =
      ValidationCheck(v => Either.cond(v <= max, v, s"greater than $max"))

    def bounded(min: Int, max: Int): ValidationCheck[Int, Int] =
      atLeast(min) >> atMost(max)

    def oneOf[A](values: Set[A]): ValidationCheck[A, A] =
      ValidationCheck(v => Either.cond(values.contains(v), v, s"not one of $values"))

    def toInt: ValidationCheck[String, Int] =
      ValidationCheck(_.toIntOption.toRight("not a valid integer"))

    def parseTo[A](implicit parse: Parse[A]): ValidationCheck[String, A] =
      ValidationCheck(a => parse(a).toRight("can not be parsed"))
  }

  case class Height(value: Int, unit: LengthUnit)
  object Height {
    implicit val parseHeight: ValidationCheck.Parse[Height] = s => {
      val reg = "(\\d+)(cm|in)".r
      s match {
        case reg(v, u) =>
          for {
            value <- v.toIntOption
            unit <- LengthUnit.fromStringOption(u)
          } yield Height(value, unit)
        case _         => None
      }
    }

    def isValid: ValidationCheck[Height, Height] =
      ValidationCheck(height => LengthUnit.validationRule(height.unit).run(height.value).map(_ => height))
  }

  sealed trait LengthUnit extends Product with Serializable
  object LengthUnit {
    case object Cm extends LengthUnit
    case object In extends LengthUnit

    def fromStringOption(value: String): Option[LengthUnit] =
      value match {
        case "cm" => Some(Cm)
        case "in" => Some(In)
        case _ => None
      }

    def validationRule: LengthUnit => ValidationCheck[Int, Int] = {
      case Cm => ValidationCheck.bounded(150, 193)
      case In => ValidationCheck.bounded(59, 76)
    }
  }

  def solvePart1(input: List[Map[PassportField, String]]): Int = {
    def isValidPassport(fields: Map[PassportField, String]) =
      PassportField.Required.forall(fields.contains)

    input.count(isValidPassport)
  }

  def solvePart2(input: List[Map[PassportField, String]]): Int = {

    def isValidPassport(fields: Map[PassportField, String]) =
      PassportField.Required
        .map(f => f -> validationRule(f))
        .forall { case (field, rule) =>
          fields.get(field).exists(rule.isValid)
        }

    input.count(isValidPassport)
  }

  private def validationRule(field: PassportField) = {
    import ValidationCheck._

    field match {
      case PassportField.byr => matchesPattern("\\d{4}") >> toInt >> bounded(1920, 2002).void
      case PassportField.iyr => matchesPattern("\\d{4}")  >> toInt >> bounded(2010, 2020).void
      case PassportField.eyr => matchesPattern("\\d{4}") >> toInt >> bounded(2020, 2030).void
      case PassportField.hgt => parseTo[Height] >> Height.isValid.void
      case PassportField.hcl => matchesPattern("#[0-9a-f]{6}").void
      case PassportField.ecl => oneOf(Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")).void
      case PassportField.pid => matchesPattern("\\d{9}").void
      case PassportField.cid => constValid[String].void
    }
  }

  private def parsePassportFields(s: String): Map[PassportField, String] =
    s.split("\\s+")
      .flatMap {
        case s"$key:$value" =>
        PassportField.fromStringOption(key).map(_ -> value)
      }
      .toMap

  private val input = Resources.string("day4.txt").split("\n\n")
    .map(parsePassportFields)
    .toList

  println(solvePart1(input)) // 190
  println(solvePart2(input)) // 121

}
