package io.github.dataunitylab.jsonoid

import java.nio.file.Files
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.sys.process.Process
import scala.util.control.Breaks._

import io.github.michaelmior.automaton.{Automaton, SpecialOperations, State, Transition}

import discovery.Helpers._


object RegexDiscover {
  implicit class StateOps(s: State) {
    def removeTransition(t: Transition): Boolean = s.getTransitions.remove(t)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements",
                          "org.wartremover.warts.ToString"))
  implicit class AutomatonOps(a: Automaton) {
    def writePng(filename: String): Unit = {
      val file = Files.createTempFile("jsonoid", ".dot")
      Files.write(file, a.toDot.getBytes("UTF-8"))
      Process(Seq("dot", "-Tpng", file.toString, "-o", filename)).!
    }
  }

  implicit class TransitionOps(t: Transition) {
    def matchesRange(r: Range): Boolean = r.head === t.getMin && r.last === t.getMax
    def coveredBy(r: Range): Boolean = r.head >= t.getMin && r.last <= t.getMax
  }

  val lowercaseAlpha: Range = Range.inclusive('a', 'z')
  val uppercaseAlpha: Range = Range.inclusive('A', 'Z')
  val numeric: Range = Range.inclusive('0', '9')

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def automatonFromString(s: String): Automaton = {
    val automaton = new Automaton()
    var state = new State()
    automaton.setInitialState(state)

    for (char <- s) {
      val newState = new State()
      val transition = new Transition(char, newState)
      state.addTransition(transition)
      state = newState
    }
    state.setAccept(true)

    automaton
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var",
                          "org.wartremover.warts.While"))
  def groupChars[A <: Range: ClassTag](chars: Set[A]): Set[A] = {
    var finalChars = chars
    var toAdd = Set.empty[A]
    var toRemove = Set.empty[A]
    var changed = true
    // println("GROUPING")
    while (changed) {
      changed = false
      breakable { finalChars.toSeq.combinations(2).foreach { case Seq(a: A, b: A) =>
        // println(a, b)

        if (a.containsSlice(b)) {
          changed = true
          toRemove = Set(b)
        }
        if (b.containsSlice(a)) {
          changed = true
          toRemove = Set(a)
        }

        if (changed) {
          break()
        }

        Seq(lowercaseAlpha, uppercaseAlpha, numeric).foreach { g => {
          val group = g.asInstanceOf[A]
          if (group.containsSlice(a) && group.containsSlice(b)) {
            // println("YES")
            changed = true
            toRemove = Set(a, b)
            toAdd = Set(group)
          }
        }}
      }}

      finalChars = finalChars -- toRemove ++ toAdd
    }
    finalChars
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals",
                          "org.wartremover.warts.MutableDataStructures",
                          "org.wartremover.warts.NonUnitStatements",
                          "org.wartremover.warts.Var",
                          "org.wartremover.warts.While"))
  def simplify(a: Automaton): Boolean = {
    var changed = false
    var states = mutable.Queue(a.getInitialState)
    val explored = mutable.Set.empty[State]

    while (states.nonEmpty) {
      // Get the next state and enqueue its targets
      val s = states.dequeue()

      if (!explored.contains(s)) {
        explored.add(s)

        val transitions = s.getTransitions.asScala.toList
        transitions.foreach(t => states.enqueue(t.getDest))

        // Group the transition ranges together
        val chars = transitions.map(t => Range.inclusive(t.getMin, t.getMax)).toSet
        val groups = groupChars(chars)

        // Remap the transitions according to the groups
        chars.zipWithIndex.foreach { case (c, i) =>
          groups.foreach { g =>
            if (g != c && g.containsSlice(c)) {
              changed = true
              s.getTransitions.remove(transitions(i))
              s.getTransitions.add(new Transition(g.head.toChar, g.last.toChar, transitions(i).getDest))
            }
          }
        }
      }
    }
    a.setDeterministic(false)
    a.restoreInvariant()
    a.determinize()

    changed
  }

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures",
                          "org.wartremover.warts.NonUnitStatements",
                          "org.wartremover.warts.Var",
                          "org.wartremover.warts.While"))
  def flattenChains(a: Automaton): Boolean = {
    var changed = false
    var states = mutable.Queue(a.getInitialState)
    val explored = mutable.Set.empty[State]

    breakable { while (states.nonEmpty) {
      // Get the next state and enqueue its targets
      val s = states.dequeue()
      if (!explored.contains(s)) {
        explored.add(s)

        val transitions = s.getTransitions.asScala
        Seq(lowercaseAlpha, uppercaseAlpha, numeric).foreach { g => {
          val groupTransitions = transitions.filter(_.matchesRange(g))
          if (groupTransitions.nonEmpty) {
            changed = true
            flattenChain(a, s, g)
            break()
          }
        }}

        transitions.foreach(t => states.enqueue(t.getDest))
      }
    }}

    changed
  }

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures",
                          "org.wartremover.warts.NonUnitStatements",
                          "org.wartremover.warts.While"))
  def flattenChain(a: Automaton, s: State, r: Range): Unit = {
    val toMerge = mutable.Set.empty[State]
    val states = mutable.Queue(s)
    val explored = mutable.Set.empty[State]

    while (states.nonEmpty) {
      val s = states.dequeue()

      // Skip explored states
      if (!explored.contains(s)) {
        explored.add(s)

        val transitions = s.getTransitions.asScala
        transitions.foreach { t =>
          states.enqueue(t.getDest)
          // println("MATCH? ", t, r)
          // println(t.getMin, t.getMax)
          if (t.coveredBy(r)) {
            // println("YES")
            toMerge.add(t.getDest)
          }
        }
      }
    }

    // Merge the
    Automaton.setAllowMutate(true)
    SpecialOperations.mergeStates(a, toMerge.asJava)
    Automaton.setAllowMutate(false)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements",
                          "org.wartremover.warts.Var",
                          "org.wartremover.warts.While"))
  def main(args: Array[String]): Unit = {
    val a1 = automatonFromString("foobarbaz1")
    val a2 = automatonFromString("foobarcorge2")
    val a4 = automatonFromString("foobarquux3")
    val a5 = automatonFromString("foobargarply4")
    // println(a1.getShortestExample(true))
    // println(a2.getShortestExample(true))
    var a3 = a1.union(a2)
    // println(a3.getNumberOfStates)
    a3.minimize
    simplify(a3)
    a3 = a3.union(a4)
    var i = 0
    var changed = true
    // while (changed) {
    //   changed = simplify(a3) // && flattenChains(a3)
    //   if (changed) {
    //     a3.minimize
    //     val file = Files.createTempFile(s"jsonoid${i.toString}", ".dot")
    //     Files.write(file, a3.toDot.getBytes("UTF-8"))
    //     Process(Seq("dot", "-Tpng", file.toString, "-o", file.toString + ".png")).!
    //     println(file.toString)
    //   }
    //   i += 1
    // }
    a3.writePng("/tmp/jsonoid0.png")
    simplify(a3)
    a3.writePng("/tmp/jsonoid1.png")
    // println(a3.getNumberOfStates)
    // println(a3.getShortestExample(true))
    // println(groupChars(Set(numeric, lowercaseAlpha, uppercaseAlpha)))
    // println(groupChars(Set(Range.inclusive('a', 'b'), Range.inclusive('g', 'h'))))
    simplify(a3)
    a3.writePng("/tmp/jsonoid2.png")
    println(a3.getShortestExample(true))
    a3.minimize
    a3.writePng("/tmp/jsonoid3.png")
    simplify(a3)
    a3.writePng("/tmp/jsonoid4.png")
    flattenChains(a3)
    a3.writePng("/tmp/jsonoid5.png")
    simplify(a3)
    a3.writePng("/tmp/jsonoid6.png")
    flattenChains(a3)
    a3.writePng("/tmp/jsonoid7.png")
    println(a3.getShortestExample(true))
    println(a3.toDot)
  }
}

