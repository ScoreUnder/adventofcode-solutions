#!/usr/bin/env scala
import scala.annotation.nowarn

val PWD_RE = """(\d+)-(\d+) (.): (.+)""".r

@nowarn // spurious warning about a type test
final case class PasswordPolicy(p1: Int, p2: Int, chr: Char)

val passwords = io.Source.fromFile("input").getLines().collect {
  case PWD_RE(min, max, chr, pwd) =>
    PasswordPolicy(min.toInt, max.toInt, chr(0)) -> pwd
}.toVector

val matchesPolicy1 = { (policy: PasswordPolicy, input: String) =>
  val count = input.count(_ == policy.chr)
  count >= policy.p1 && count <= policy.p2
}

val matchesPolicy2 = { (policy: PasswordPolicy, input: String) =>
  List(input(policy.p1 - 1), input(policy.p2 - 1)).count(_ == policy.chr) == 1
}

def part1() =
  println(passwords.count(matchesPolicy1.tupled))

def part2() =
  println(passwords.count(matchesPolicy2.tupled))

part1()
part2()