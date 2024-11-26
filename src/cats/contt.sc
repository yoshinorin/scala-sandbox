// https://typelevel.org/cats/datatypes/contt.html
//> using dep "org.typelevel::cats-core:2.12.0"

case class User(id: Int, name: String, age: Int)
sealed abstract class UserUpdateResult
case class Succeeded(updatedUserId: Int) extends UserUpdateResult
case object Failed extends UserUpdateResult

import cats.Eval
import cats.data.ContT

def updateUser(
    persistToDatabase: User => Eval[UserUpdateResult]
)(existingUser: User, newName: String, newAge: Int): Eval[UserUpdateResult] = {
  val trimmedName = newName.trim
  val cappedAge = newAge max 150
  val updatedUser = existingUser.copy(name = trimmedName, age = cappedAge)

  persistToDatabase(updatedUser)
}

def updateUserCont(
    existingUser: User,
    newName: String,
    newAge: Int
): ContT[Eval, UserUpdateResult, User] =
  ContT.apply[Eval, UserUpdateResult, User] {
    next => // next: User => Eval[UserUpdateResult]
      val trimmedName = newName.trim
      val cappedAge = newAge min 150
      val updatedUser = existingUser.copy(name = trimmedName, age = cappedAge)

      next(updatedUser)
  }

val existingUser = User(100, "Alice", 42)
// existingUser: User = User(id = 100, name = "Alice", age = 42)

val computation = updateUserCont(existingUser, "Bob", 200)
// computation: ContT[Eval, UserUpdateResult, User] = FromFn(
//   runAndThen = Single(f = <function1>, index = 0)
// )

val eval = computation.run { user =>
  Eval.later {
    println(s"Persisting updated user to the DB: $user")
    Succeeded(user.id)
  }
}
// eval: Eval[UserUpdateResult] = cats.Later@121d47a9

eval.value
// Persisting updated user to the DB: User(100,Bob,150)
// res0: UserUpdateResult = Succeeded(updatedUserId = 100)

/*
 * Composition
 *
 */
val anotherComputation = computation.map { user =>
  Map(
    "id" -> user.id.toString,
    "name" -> user.name,
    "age" -> user.age.toString
  )
}

// anotherComputation: ContT[Eval, UserUpdateResult, Map[String, String]] = FromFn(
//   runAndThen = Single(
//     f = cats.data.ContT$$Lambda$14246/0x00007f832a671428@74faa65b,
//     index = 0
//   )
// )

val anotherEval = anotherComputation.run { userFields => // Map[String, String]
  Eval.later {
    println(s"Persisting these fields to the DB: $userFields")
    Succeeded(userFields("id").toInt)
  }
}
// anotherEval: Eval[UserUpdateResult] = cats.Eval$$anon$5@38538d64

anotherEval.value
// Persisting these fields to the DB: Map(id -> 100, name -> Bob, age -> 150)
// res1: UserUpdateResult = Succeeded(updatedUserId = 100)

val updateUserModel: ContT[Eval, UserUpdateResult, User] =
  updateUserCont(existingUser, "Bob", 200).map { updatedUser =>
    println("Updated user model")
    updatedUser
  }
// updateUserModel: ContT[Eval, UserUpdateResult, User] = FromFn(
//   runAndThen = Single(
//     f = cats.data.ContT$$Lambda$14246/0x00007f832a671428@745a18f4,
//     index = 0
//   )
// )

val persistToDb: User => ContT[Eval, UserUpdateResult, UserUpdateResult] = {
  user =>
    ContT.apply[Eval, UserUpdateResult, UserUpdateResult] {
      next => // next: UserUpdateResult => Eval[UserUpdateResult]
        println(s"Persisting updated user to the DB: $user")
        next(Succeeded(user.id))
        // next(Failed)
    }
}

// persistToDb: User => ContT[Eval, UserUpdateResult, UserUpdateResult] = <function1>

val publishEvent
    : UserUpdateResult => ContT[Eval, UserUpdateResult, UserUpdateResult] = {
  userUpdateResult =>
    ContT.apply[Eval, UserUpdateResult, UserUpdateResult] { next =>
      userUpdateResult match {
        case Succeeded(userId) =>
          println(s"Publishing 'user updated' event for user ID $userId")
        case Failed =>
          println("Not publishing 'user updated' event because update failed")
      }

      next(userUpdateResult)
    }
}
// publishEvent: UserUpdateResult => ContT[Eval, UserUpdateResult, UserUpdateResult] = <function1>

val chainOfContinuations =
  updateUserModel flatMap persistToDb flatMap publishEvent
// chainOfContinuations: ContT[Eval, UserUpdateResult, UserUpdateResult] = FromFn(
//   runAndThen = Single(
//     f = cats.data.ContT$$Lambda$14250/0x00007f832a671db0@889bd78,
//     index = 0
//   )
// )

val evalComp = chainOfContinuations.run { finalResult => // UserUpdateResult
  Eval.later {
    println("Finished!")
    finalResult
  }
}
// evalComp: Eval[UserUpdateResult] = cats.Eval$$anon$5@6e43f11e

evalComp.value
// Updated user model
// Persisting updated user to the DB: User(100,Bob,150)
// Publishing 'user updated' event for user ID 100
// Finished!
// res2: UserUpdateResult = Succeeded(updatedUserId = 100)

println(evalComp.value)
// Succeeded(100)
