package org.randi3.dao

import org.apache.commons.math3.random.MersenneTwister

import org.junit.runner.RunWith

import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession

import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import scala.Left
import org.randi3.randomization.BiasedCoinRandomization
import scala.Right
import scala.Some
import org.randi3.model.Trial


@RunWith(classOf[JUnitRunner])
class BiasedCoinRandomizationDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._

  import driver.Implicit._

  import schema._

  describe("BiasedCoinRandomizationDao create method") {

    it("should be able to create a new biased coin randomization method") {
      val truncatedRandomization: BiasedCoinRandomization = new BiasedCoinRandomization()(random = new MersenneTwister)
      val trialDB: Trial = trialDao.get(trialDao.create(createTrial.copy(randomizationMethod = None)).toOption.get).toOption.get.get

      val id = biasedCoinRandomizationDao.create(truncatedRandomization, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(id) => id
      }

      database withSession {
        val allRandomizationMethods = Query(RandomizationMethods).list
        allRandomizationMethods.size must be(1)
        allRandomizationMethods.head._4 must be(classOf[BiasedCoinRandomization].getName)

      }
    }


  }
}
