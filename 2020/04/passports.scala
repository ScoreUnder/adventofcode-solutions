#!/usr/bin/env scala
//!#

val passports = io.Source.fromFile("input").getLines()
  .flatMap(_.split(" "))
  .foldLeft(List(Map.empty[String, String])) { (acc, next) =>
    if (next.isEmpty) Map.empty[String, String] :: acc
    else {
      val Array(key, value) = next.split(":", 2)
      acc.head + (key -> value) :: acc.tail
    }
  }
  .toVector

def numBetween(min: Int, max: Int)(value: String) =
  value.toIntOption.exists(n => n >= min && n <= max)

val eyeColors = "amb blu brn gry grn hzl oth".split(" ").toSet
val fieldValidators: Map[String, String => Boolean] = Map(
  "byr" -> numBetween(1920, 2002),
  "iyr" -> numBetween(2010, 2020),
  "eyr" -> numBetween(2020, 2030),
  "hgt" -> { h =>
    if (h.endsWith("in")) {
      h.dropRight(2).toIntOption.exists(n => n >= 59 && n <= 76)
    } else if (h.endsWith("cm")) {
      h.dropRight(2).toIntOption.exists(n => n >= 150 && n <= 193)
    } else false
  },
  "hcl" -> { c => c.startsWith("#") && c.drop(1).forall("0123456789abcdef".contains(_)) && c.length == 7 },
  "ecl" -> eyeColors.contains,
  "pid" -> { pid => pid.length == 9 && pid.forall(Character.isDigit) },
)

def part1 =
  passports.count(m => fieldValidators.keys.forall(r => m.contains(r)))

def part2 =
  passports.count(m => fieldValidators.forall { case (key, validator) => m.contains(key) && validator(m(key)) })

println(part1)
println(part2)
