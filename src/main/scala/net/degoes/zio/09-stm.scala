package net.degoes.zio

import zio._

object StmSwap extends ZIOAppDefault {

  import zio.stm._

  /**
   * EXERCISE
   *
   * Demonstrate the following code does not reliably swap two values in the
   * presence of concurrency.
   */
  def exampleRef: UIO[Int] = {
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for {
        v1 <- ref1.get
        v2 <- ref2.get
        _  <- ref2.set(v1)
        _  <- ref1.set(v2)
      } yield ()

    for {
      ref1   <- Ref.make(100)
      ref2   <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeatN(100).fork
      fiber2 <- swap(ref2, ref1).repeatN(100).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _)
    } yield value
  }

  /**
   * EXERCISE
   *
   * Using `STM`, implement a safe version of the swap function.
   */
  def exampleStm: UIO[Int] = {
    def swap[A](tref1: TRef[A], tref2: TRef[A]) =
      for {
        v1 <- tref1.get
        v2 <- tref2.get
        _  <- tref2.set(v1)
        _  <- tref1.set(v2)
      } yield ()

    for {
      tref1  <- TRef.make(100).commit
      tref2  <- TRef.make(0).commit
      fiber1 <- swap(tref1, tref2).commit.repeatN(100).fork
      fiber2 <- swap(tref2, tref1).commit.repeatN(100).fork
      _      <- (fiber1 zip fiber2).join
      value  <- ((tref1.get zipWith tref2.get)(_ + _)).commit
    } yield value
  }

  val run =
    exampleRef.map(_.toString).flatMap(Console.printLine(_)) *>
      exampleStm.map(_.toString).flatMap(Console.printLine(_))
}

object StmLock extends ZIOAppDefault {

  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, implement a simple binary lock by implementing the creation,
   * acquisition, and release methods.
   */
  class Lock private (tref: TRef[Boolean]) {
    def acquire: UIO[Unit] =
      STM.atomically {
        for {
          locked <- tref.get
          _      <- if (locked) STM.retry else tref.set(true)
        } yield ()
      }

    def release: UIO[Unit] =
      tref.set(false).commit
  }

  object Lock {
    def make: UIO[Lock] = TRef.make(false).commit.map(new Lock(_))
  }

  val run =
    (for {
      lock <- Lock.make
      fiber1 <- ZIO
                 .acquireReleaseWith(lock.acquire)(_ => lock.release)(_ => Console.printLine("Bob  : I have the lock!"))
                 .repeat(Schedule.recurs(10))
                 .fork
      fiber2 <- ZIO
                 .acquireReleaseWith(lock.acquire)(_ => lock.release)(_ => Console.printLine("Sarah: I have the lock!"))
                 .repeat(Schedule.recurs(10))
                 .fork
      _ <- (fiber1 zip fiber2).join
    } yield ())
}

object StmQueue extends ZIOAppDefault {

  import zio.stm._

  import scala.collection.immutable.{ Queue => ScalaQueue }

  /**
   * EXERCISE
   *
   * Using STM, implement a async queue with double back-pressuring.
   */
  class Queue[A] private (capacity: Int, queue: TRef[ScalaQueue[A]]) {
    def take: UIO[A] =
      STM.atomically {
        for {
          scQueue <- queue.get
          a       <- if (scQueue.isEmpty) STM.retry else queue.modify(_.dequeue)
        } yield a
      }

    def offer(a: A): UIO[Unit] =
      STM.atomically {
        for {
          scQueue <- queue.get
          _       <- if (scQueue.size >= capacity) STM.retry else queue.set(scQueue.enqueue(a))
        } yield ()
      }
  }

  object Queue {
    def bounded[A](capacity: Int): UIO[Queue[A]] =
      TRef.make(ScalaQueue.empty[A]).map(new Queue(capacity, _)).commit
  }

  val run =
    (for {
      queue <- Queue.bounded[Int](10)
      _     <- ZIO.foreach(0 to 100)(i => queue.offer(i)).fork
      _     <- ZIO.foreachDiscard(0 to 100)(_ => queue.take.flatMap(i => Console.printLine(s"Got: ${i}")))
    } yield ())
}

object StmLunchTime extends ZIOAppDefault {

  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Attendee.
   */
  final case class Attendee(state: TRef[Attendee.State]) {
    import Attendee.State._

    def feed: STM[Nothing, Unit] =
      state.set(Full)
  }
  object Attendee {
    sealed trait State
    object State {
      case object Starving extends State
      case object Full     extends State
    }
  }

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Table.
   */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => true)

    def vacateSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => false)
  }

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds a single attendee.
   */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] =
    t.findEmptySeat.flatMap {
      case Some(index) => t.takeSeat(index) *> a.feed *> t.vacateSeat(index)
      case None        => STM.retry
    }

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds only the starving attendees.
   */
  def feedStarving(table: Table, attendees: Iterable[Attendee]): UIO[Unit] =
    STM.atomically {
      for {
        starvingAttendees <- STM.filter(attendees)(_.state.get.map(_ == Attendee.State.Starving))
        _                 <- STM.foreach(starvingAttendees)(feedAttendee(table, _))
      } yield ()
    }

  val run = {
    val Attendees = 100
    val TableSize = 5

    for {
      attendees <- ZIO.foreach(0 to Attendees)(_ =>
                    TRef
                      .make[Attendee.State](Attendee.State.Starving)
                      .map(Attendee(_))
                      .commit
                  )
      table <- TArray
                .fromIterable(List.fill(TableSize)(false))
                .map(Table(_))
                .commit
      _ <- feedStarving(table, attendees)
    } yield ExitCode.success
  }
}

object StmPriorityQueue extends ZIOAppDefault {

  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, design a priority queue, where smaller integers are assumed
   * to have higher priority than greater integers.
   */
  class PriorityQueue[A] private (
    minLevel: TRef[Option[Int]],
    map: TMap[Int, TQueue[A]]
  ) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] =
      for {
        _ <- minLevel.update {
              case Some(currentMin) => Some(currentMin.min(priority))
              case _                => Some(priority)
            }
        queue <- map.get(priority).flatMap {
                  case Some(queue) => STM.succeed(queue)
                  case None        => TQueue.unbounded[A]
                }
        _ <- map.putIfAbsent(priority, queue)
        _ <- queue.offer(a)
      } yield ()

    private def newMin: USTM[Option[Int]] =
      map.keys.map(_.foldLeft(Option.empty[Int]) { (a, i) =>
        a match {
          case Some(priority) => Some(priority.min(i))
          case None           => Some(i)
        }
      })

    def take: STM[Nothing, A] =
      minLevel.get.flatMap {
        case Some(min) =>
          map.get(min).flatMap {
            case Some(queue) =>
              queue.take <* STM.whenSTM(queue.isEmpty) {
                map.delete(min) *> newMin.flatMap(minLevel.set)
              }
            case None => STM.retry
          }
        case None => STM.retry
      }
  }
  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] =
      TRef
        .make(Option.empty[Int])
        .zipWith(TMap.empty[Int, TQueue[A]])(new PriorityQueue(_, _))
  }

  val run =
    (for {
      _     <- Console.printLine("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit
      lowPriority = ZIO.foreach(0 to 100) { i =>
        queue
          .offer(s"Offer: ${i} with priority 3", 3)
          .commit
          .delay(1.milli)
      }
      highPriority = ZIO.foreach(0 to 100) { i =>
        queue
          .offer(s"Offer: ${i} with priority 0", 0)
          .commit
          .delay(2.millis)
      }
      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
            .flatMap(Console.printLine(_))
            .forever
            .fork *>
            Console.readLine
    } yield 0)
}

object StmReentrantLock extends ZIOAppDefault {

  import zio.stm._

  private final case class WriteLock(
    writeCount: Int,
    readCount: Int,
    fiberId: FiberId
  )
  private final class ReadLock private (readers: Map[FiberId, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: FiberId): Boolean =
      readers.isEmpty || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: FiberId): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: FiberId, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
      )
    }
  }
  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: FiberId, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
   * EXERCISE
   *
   * Using STM, implement a reentrant read/write lock.
   */
  class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] =
      data.get.map {
        case Left(_)          => 0
        case Right(writeLock) => writeLock.writeCount
      }.commit

    def writeLocked: UIO[Boolean] =
      data.get.map {
        case Left(_)  => false
        case Right(_) => true
      }.commit

    def readLocks: UIO[Int] =
      data.get.map {
        case Left(readLock) => readLock.total
        case Right(_)       => 0
      }.commit

    def readLocked: UIO[Boolean] =
      writeLocked.map(!_)

    val read: ZIO[Scope, Nothing, Int] =
      ZIO.fiberId.flatMap { fiberId =>
        STM.atomically {
          for {
            readLock <- data.get.flatMap {
                         case Left(readLock) =>
                           val adjusted = readLock.adjust(fiberId, -1)
                           data.set(Left(adjusted)) *> STM.succeed(adjusted)
                         case Right(_) =>
                           STM.retry
                       }
          } yield readLock.total
        }
      }

    val write: ZIO[Scope, Nothing, Int] =
      ZIO.fiberId.flatMap { fiberId =>
        STM.atomically {
          for {
            writeLock <- data.get.flatMap {
                          case Left(_)          => STM.retry
                          case Right(writeLock) => ???
                        }
          } yield ???
        }
      }
  }
  object ReentrantReadWriteLock {
    def make: UIO[ReentrantReadWriteLock] = ???
  }

  val run = ???
}

object StmDiningPhilosophers extends ZIOAppDefault {

  import zio.stm._

  import java.io.IOException

  sealed trait Fork
  val Fork = new Fork {}

  final case class Placement(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  )

  final case class Roundtable(seats: Vector[Placement])

  /**
   * EXERCISE
   *
   * Using STM, implement the logic of a philosopher to take not one fork, but
   * both forks when they are both available.
   */
  def takeForks(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  ): STM[Nothing, (Fork, Fork)] =
    left.get.zip(right.get).flatMap {
      case (Some(leftFork), Some(rightFork)) =>
        STM.succeed((leftFork, rightFork))
      case _ =>
        STM.retry
    }

  /**
   * EXERCISE
   *
   * Using STM, implement the logic of a philosopher to release both forks.
   */
  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(
    tuple: (Fork, Fork)
  ): STM[Nothing, Unit] =
    left.set(Some(tuple._1)) *> right.set(Some(tuple._2))

  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] = {
    val makeFork = TRef.make[Option[Fork]](Some(Fork))

    (for {
      allForks0 <- STM.foreach(0 to size)(i => makeFork)
      allForks  = allForks0 ++ List(allForks0(0))
      placements = (allForks zip allForks.drop(1)).map {
        case (l, r) => Placement(l, r)
      }
    } yield Roundtable(placements.toVector)).commit
  }

  def eat(
    philosopher: Int,
    roundtable: Roundtable
  ): ZIO[Any, IOException, Unit] = {
    val placement = roundtable.seats(philosopher)

    val left  = placement.left
    val right = placement.right

    for {
      forks <- takeForks(left, right).commit
      _     <- Console.printLine(s"Philosopher ${philosopher} eating...")
      _     <- putForks(left, right)(forks).commit
      _     <- Console.printLine(s"Philosopher ${philosopher} is done eating")
    } yield ()
  }

  val run = {
    val count = 10

    def eaters(table: Roundtable): Iterable[ZIO[Any, IOException, Unit]] =
      (0 to count).map(index => eat(index, table))

    (for {
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table))
      _     <- fiber.join
      _     <- Console.printLine("All philosophers have eaten!")
    } yield ())
  }
}
