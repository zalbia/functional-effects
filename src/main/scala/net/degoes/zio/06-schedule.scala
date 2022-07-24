package net.degoes.zio

import zio.Schedule.exponential
import zio._

import java.io.IOException

object Retry {
  // Schedule[-Env, -In, +Out]

  // Schedule A: |XXXXXXXXXX|             |XXXXXXXXXX|             |XXXXXXX|   |XXX|  |XX|
  // Schedule B:     |XXXXXX|  |XXXXXX|            |XXXXXXX|          |XXX|  |XX|
  // A isect  B:     |XXXXXX|                      |X|                |XXX|
  // A union  B: |XXXXXXXXXX|  |XXXXXX|   |XXXXXXXXXXXXXXXX|       |XXXXXXX| |XXXXX|  |XX|

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes: Schedule[Any, Any, Long] = Schedule.recurs(5L)

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1 = ZIO.replicate(5)(Console.printLine("Hello World"))

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  val everySecond = Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  val fiveTimesEverySecond = fiveTimes && everySecond

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action Console.printLine("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  val repeated2 = Console.printLine("Hi hi").repeat(fiveTimesEverySecond)

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  val fiveTimesThenEverySecond = fiveTimes andThen everySecond

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1   = ZIO.fail("Uh oh!")
  val retried5 = error1.retry(fiveTimes)

  /**
   * EXERCISE
   *
   * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats the minimum of five times and
   * every second.
   */
  val fiveTimesOrEverySecond = fiveTimes || everySecond

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  val exponentialSchedule = Schedule.exponential(10.millis)

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  val jitteredExponential = exponentialSchedule.jittered

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  val oneHundred = Schedule.forever.whileOutput(_ <= 100)

  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] = Schedule.identity

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, List[A]] = Schedule.identity[A].collectAll.map(_.toList)

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  val fiveTimesEverySecondR = fiveTimes *> everySecond

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import Schedule.{ collectAll, exponential, fixed, recurs }
  def mySchedule[A]: Schedule[Any, A, List[A]] =
    exponential(10.millis)
      .whileOutput(_ < 60.seconds)
      .andThen(fixed(60.seconds) && recurs(100))
      .jittered *> collectAll.map(_.toList)
}
