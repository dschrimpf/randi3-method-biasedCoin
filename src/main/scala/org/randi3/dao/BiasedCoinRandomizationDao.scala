package org.randi3.dao

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession

import org.scalaquery.ql.extended.ExtendedProfile


import org.randi3.randomization.BiasedCoinRandomization
import scalaz._
import org.randi3.schema.DatabaseSchema


class BiasedCoinRandomizationDao(database: Database, driver: ExtendedProfile) extends AbstractRandomizationMethodDao(database, driver) {


  import driver.Implicit._

  val schemaCore = new DatabaseSchema(driver)
  import schemaCore._


  def create(randomizationMethod: BiasedCoinRandomization, trialId: Int): Validation[String, Int] = {
    database withSession {
      threadLocalSession withTransaction {
        RandomizationMethods.noId insert (trialId, generateBlob(randomizationMethod.random), randomizationMethod.getClass.getName)
      }
      getId(trialId)
    }

  }

  def get(id: Int): Validation[String, Option[BiasedCoinRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromId(id).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._3 == classOf[BiasedCoinRandomization].getName) {
          Success(Some(new BiasedCoinRandomization(rm._1.get, 0)(deserializeRandomGenerator(rm._2.get))))
        } else {
          Failure("Wrong plugin")
        }

      } else Failure("More than one method with id=" + id + " found")
    }
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[BiasedCoinRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromTrialId(trialId).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[BiasedCoinRandomization].getName) {
          Success(Some(new BiasedCoinRandomization(rm._1.get, 0)(deserializeRandomGenerator(rm._3.get))))
        } else {
          Failure("Wrong plugin")
        }
      } else Failure("More than one method for trial with id=" + trialId + " found")
    }
  }

  def update(randomizationMethod: BiasedCoinRandomization): Validation[String, BiasedCoinRandomization] = {
    database withSession {
      queryRandomizationMethodFromId(randomizationMethod.id).mutate { r =>
        r.row = r.row.copy(_2 = generateBlob(randomizationMethod.random), _3 = randomizationMethod.getClass.getName)
      }
    }

    get(randomizationMethod.id).either match {
      case Left(x) => Failure(x)
      case Right(None) => Failure("Method not found")
      case Right(Some(randomizationMethodResult)) => Success(randomizationMethodResult)
    }
  }

  def delete(randomizationMethod: BiasedCoinRandomization) {
    database withSession {
      queryRandomizationMethodFromId(randomizationMethod.id).mutate { r =>
        r.delete()
      }
    }
  }

}
