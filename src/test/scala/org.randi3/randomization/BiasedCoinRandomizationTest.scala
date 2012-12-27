package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import collection.mutable.ListBuffer
import org.randi3.model.{TrialSubject, TreatmentArm}
import org.apache.commons.math3.random.MersenneTwister
import java.util

@RunWith(classOf[JUnitRunner])
class BiasedCoinRandomizationTest extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironment._

  describe("A biased coin randomization method") {

    it("should be balanced after the end of the trial (two arms)") {

      val testArms = List((20, 20),(50, 100),(50, 150), (33, 100))

      val runs = 1000

      for(testCase <- testArms){

        val runResults = new ListBuffer[(Int, Int)]()

        for (run <- 1 to runs){


      val arms = new ListBuffer[TreatmentArm]()

      //create the arms
      arms.append(createTreatmentArm.copy(id = 1, plannedSize = testCase._1))
      arms.append(createTreatmentArm.copy(id = 2, plannedSize = testCase._2))

      val biasedCoinRandomizationMethod = new BiasedCoinRandomization()(random = new MersenneTwister())
      val trial = createTrial.copy(treatmentArms = arms.toList, randomizationMethod = Some(biasedCoinRandomizationMethod))

      for (i <- 1 to trial.plannedSubjects) {
        val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = Nil).toOption.get
        trial.randomize(subject).isSuccess must be(true)
        trial.getSubjects.size must be(i)
      }

        val sizeArm1 = trial.treatmentArms.find(arm => arm.id == 1).get.subjects.size
        val sizeArm2 = trial.treatmentArms.find(arm => arm.id == 2).get.subjects.size
          runResults.append((sizeArm1, sizeArm2))
    }
        runResults.size must be(runs)

        val meansTmp = runResults.toList.reduce((acc, act) => (acc._1 + act._1, acc._2 + act._2))
        val means = (meansTmp._1 / (runs * 1.0), meansTmp._2 / (runs*1.0))

        means._1 must be>=(testCase._1-0.5)
        means._1 must be<=(testCase._1+0.5)

        means._2 must be>=(testCase._2-0.5)
        means._2 must be<=(testCase._2+0.5)
      }

    }

    it("should be balanced after the end of the trial (three arms)") {

      val testArms = List((20, 20, 20),(50, 100, 50),(50, 150, 100), (33, 100, 123))

      val runs = 1000

      for(testCase <- testArms){

        val runResults = new ListBuffer[(Int, Int, Int)]()

        for (run <- 1 to runs){


          val arms = new ListBuffer[TreatmentArm]()

          //create the arms
          arms.append(createTreatmentArm.copy(id = 1, plannedSize = testCase._1))
          arms.append(createTreatmentArm.copy(id = 2, plannedSize = testCase._2))
          arms.append(createTreatmentArm.copy(id = 3, plannedSize = testCase._3))

          val biasedCoinRandomizationMethod = new BiasedCoinRandomization()(random = new MersenneTwister())
          val trial = createTrial.copy(treatmentArms = arms.toList, randomizationMethod = Some(biasedCoinRandomizationMethod))

          for (i <- 1 to trial.plannedSubjects) {
            val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = Nil).toOption.get
            trial.randomize(subject).isSuccess must be(true)
            trial.getSubjects.size must be(i)
          }

          val sizeArm1 = trial.treatmentArms.find(arm => arm.id == 1).get.subjects.size
          val sizeArm2 = trial.treatmentArms.find(arm => arm.id == 2).get.subjects.size
          val sizeArm3 = trial.treatmentArms.find(arm => arm.id == 3).get.subjects.size

          runResults.append((sizeArm1, sizeArm2, sizeArm3))
        }
        runResults.size must be(runs)

        val meansTmp = runResults.toList.reduce((acc, act) => (acc._1 + act._1, acc._2 + act._2, acc._3 + act._3))
        val means = (meansTmp._1 / (runs * 1.0), meansTmp._2 / (runs*1.0),  meansTmp._3 / (runs*1.0))

        means._1 must be>=(testCase._1-0.5)
        means._1 must be<=(testCase._1+0.5)

        means._2 must be>=(testCase._2-0.5)
        means._2 must be<=(testCase._2+0.5)

        means._3 must be>=(testCase._3-0.5)
        means._3 must be<=(testCase._3+0.5)
      }

    }

    it("should be balanced after the end of the trial (four arms)") {

      val testArms = List((20, 20, 20, 20),(50, 100, 50, 150),(50, 150, 100, 100), (33, 100, 123, 100))

      val runs = 1000

      for(testCase <- testArms){

        val runResults = new ListBuffer[(Int, Int, Int, Int)]()

        for (run <- 1 to runs){


          val arms = new ListBuffer[TreatmentArm]()

          //create the arms
          arms.append(createTreatmentArm.copy(id = 1, plannedSize = testCase._1))
          arms.append(createTreatmentArm.copy(id = 2, plannedSize = testCase._2))
          arms.append(createTreatmentArm.copy(id = 3, plannedSize = testCase._3))
          arms.append(createTreatmentArm.copy(id = 4, plannedSize = testCase._4))

          val biasedCoinRandomizationMethod = new BiasedCoinRandomization()(random = new MersenneTwister())
          val trial = createTrial.copy(treatmentArms = arms.toList, randomizationMethod = Some(biasedCoinRandomizationMethod))

          for (i <- 1 to trial.plannedSubjects) {
            val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = Nil).toOption.get
            trial.randomize(subject).isSuccess must be(true)
            trial.getSubjects.size must be(i)
          }

          val sizeArm1 = trial.treatmentArms.find(arm => arm.id == 1).get.subjects.size
          val sizeArm2 = trial.treatmentArms.find(arm => arm.id == 2).get.subjects.size
          val sizeArm3 = trial.treatmentArms.find(arm => arm.id == 3).get.subjects.size
          val sizeArm4 = trial.treatmentArms.find(arm => arm.id == 4).get.subjects.size

          runResults.append((sizeArm1, sizeArm2, sizeArm3, sizeArm4))
        }
        runResults.size must be(runs)

        val meansTmp = runResults.toList.reduce((acc, act) => (acc._1 + act._1, acc._2 + act._2, acc._3 + act._3,  acc._4 + act._4))
        val means = (meansTmp._1 / (runs * 1.0), meansTmp._2 / (runs*1.0),  meansTmp._3 / (runs*1.0), meansTmp._4 / (runs*1.0))

        means._1 must be>=(testCase._1-1.0)
        means._1 must be<=(testCase._1+1.0)

        means._2 must be>=(testCase._2-1.0)
        means._2 must be<=(testCase._2+1.0)

        means._3 must be>=(testCase._3-1.0)
        means._3 must be<=(testCase._3+1.0)

        means._4 must be>=(testCase._4-1.0)
        means._4 must be<=(testCase._4+1.0)
      }

    }
  }


}