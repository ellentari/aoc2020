package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day21 extends App {

  case class Allergen(name: String) extends AnyVal
  case class Ingredient(name: String) extends AnyVal

  case class FoodEntry(ingredients: Set[Ingredient], allergens: Set[Allergen])

  case class AllergenLocation(allergen: Allergen, ingredient: Ingredient)

  def solvePart1(foods: List[FoodEntry]): Int = {
    val allergenicIngredients = possibleAllergenLocations(foods).values.flatten.toSet
    val allergenFreeIngredients = foods.flatMap(_.ingredients).toSet -- allergenicIngredients

    foods.flatMap(_.ingredients)
      .count(allergenFreeIngredients.contains)
  }

  def solvePart2(foods: List[FoodEntry]): Option[String] =
    findAllergenLocations(foods)
      .map(_
        .sortBy(_.allergen.name)
        .map(_.ingredient.name)
        .mkString(",")
      )

  private def possibleAllergenLocations(foods: List[FoodEntry]): Map[Allergen, Set[Ingredient]] = {
    def reducePossibleLocations(possibleLocations: Map[Allergen, Set[Ingredient]], entry: FoodEntry) = {
      val entryAllergenLocations = entry.allergens.flatMap(a => entry.ingredients.map(a -> _))
        .groupMap(_._1)(_._2)

      entryAllergenLocations.foldLeft(possibleLocations) { case (possible, (allergen, ingredients)) =>
        possible.updatedWith(allergen)(_.map(_.intersect(ingredients)))
      }
    }

    val initialPossibleLocations = foods
      .flatMap(food => food.ingredients.flatMap(i => food.allergens.map(i -> _)))
      .groupMap(_._2)(_._1)
      .view.mapValues(_.toSet).toMap

    foods.foldLeft(initialPossibleLocations)(reducePossibleLocations)
  }

  private def findAllergenLocations(input: List[FoodEntry]): Option[List[AllergenLocation]] = {
    @tailrec
    def loop(
      possibleLocations: Map[Allergen, Set[Ingredient]],
      result: List[AllergenLocation]
    ): Option[List[AllergenLocation]] =
      if (possibleLocations.isEmpty) Some(result)
      else {
        val uniqueLocationAllergen = possibleLocations
          .collectFirst {
            case (allergen, ingredients) if ingredients.size == 1 => AllergenLocation(allergen, ingredients.head)
          }

        uniqueLocationAllergen match {
          case None => None
          case Some(loc) =>
            val updMap = possibleLocations.view.mapValues(_ - loc.ingredient).toMap - loc.allergen

            loop(updMap, loc :: result)
        }
      }

    loop(possibleAllergenLocations(input), Nil)
  }

  private def parseFood(s: String): FoodEntry =
    s match {
      case s"$ingredients (contains $allergens)" =>
        val is = ingredients.split(" ").map(Ingredient).toSet
        val as = allergens.split(", ").map(Allergen).toSet
        FoodEntry(is, as)
    }

  private val input = Resources.lines("day21.txt").map(parseFood)

  println(solvePart1(input)) // 2614
  println(solvePart2(input)) // qhvz,kbcpn,fzsl,mjzrj,bmj,mksmf,gptv,kgkrhg
}
