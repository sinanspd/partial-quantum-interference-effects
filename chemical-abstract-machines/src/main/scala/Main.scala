package com.sinanspd 

import io.chymyst.jc._
import org.slf4j.LoggerFactory
import scala.util.Random
import scala.math._
import spire.math._
import spire.implicits._ 
import scala.util.control.NoStackTrace
import fs2._
import cats.effect.IO
import cats.Eval
import cats.effect.unsafe.implicits.global
import com.sinanspd.qure.circuit._
import com.sinanspd.qure.circuit.gates._
import com.sinanspd.qure.circuit.circuitError._
import com.sinanspd.qure.circuit.sampler._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/// This implementation uses streaming to concurrently evaluate the circuit, then falls back
// to CHAM to built interference. More efficient than the fully CHAM implementation 

class FakeSimonSampler(c: Circuit) extends Sampler {
        val s = getRandomStateSpace()

        private def getRandomStateSpace() = {
            val numberOfH = c.remainingGates.collect { case a : H => a }.length
            val s = gen(2, List()).filter(l => l.forall(_ == l.head))
            s(new Random().nextInt(s.length))
        }

        private def gen(n: Int, acc: List[Vector[Boolean]]) : List[Vector[Boolean]] = {
            if (n == 0) {
                acc
            } else {
                if (acc.length == 0) {
                gen(n - 1, List(Vector(false), Vector(true)))
                } else {
                gen(n - 1, (for (i <- acc) yield i :+ false) ++ (for (j <- acc) yield j :+ true))
                }
            }
        }

        def sample() = s
}

class FakeShorSampler() extends Sampler{ 
    def sample() = Vector(true, false, false, false) //doesn't matter what this returns 
}

case class ModGate(a: Int, n: Int) extends Gate 

object Main extends App {

    val correctAnswers = List()  
    val concurrentInstances = 4 // Fill this out before running 
    
    val logger = LoggerFactory.getLogger("example.Main")
    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val decider = new Random()
    val terminateEarly = true;
    val threshold = sys.props
      .get("cham.threshold")
      .orElse(sys.env.get("CHAM_THRESHOLD"))
      .fold(0.1)(_.toDouble)
    require(threshold > 0d, "cham.threshold must be greater than zero")

    val r = m[QVec]
    val tp = BlockingPool(32) 

    val sampledState = new SampledStateRecorder

    implicit class CircuitDeferral(c: Circuit){ 
        def deferMeasurements = {
            val rearrangedGates = c.remainingGates.foldLeft(List[Gate]())((a, b) => b match{
                case b : DeferredMeasurement if(a.isEmpty) => 
                    throw InvalidCircuitError("A DeferredMeasurement gate shouldn't appear at the beginning of a circuit")
                case b : DeferredMeasurement => {
                    val prevGate = a.takeRight(1).head
                    prevGate match{
                        case g: DeferredMeasurement => a.take(a.size - 1) :+ MultiDeferredMeasure(List(g.q, b.q))
                        case _ => a :+ b
                    }
                }
                case b : Measure => a :+ b
                case _ => 
                    if(a.isEmpty){
                        a :+ b 
                    }else{
                        val prevGate = a.takeRight(1).head
                         prevGate match{
                            case DeferredMeasurement(_) | MultiDeferredMeasure(_) => 
                                a.take(a.size - 1) ++ List(b, prevGate)
                            case _ => a :+ b
                        }
                    }
            })

            new Circuit(rearrangedGates)
        }
    }

    object Circuit{
        def apply(remainingGates: List[Gate]) = new Circuit(remainingGates).deferMeasurements
    }

    case class MultiDeferredMeasure(q: List[Int]) extends Gate 
    case class DeferredMeasurement(q: Int) extends Gate 

    site(tp) (
        go { case r(a) + r(b) ⇒ {
                logger.debug(s"Possible Reaction between $a and $b"); 
                if(a.v.sameElements(b.v)){ 
                    logger.debug("Molecules compatible. Reaction starting")
                    val newQ = QVec(Complex(a.prop.real + b.prop.real, a.prop.imag + b.prop.imag), a.v)
                    if(newQ.v(0) && newQ.v(1) && newQ.v(2) && newQ.v(3) && newQ.v(4)){
                        logger.debug(s"What I need $newQ")
                    }
                    if(newQ.prop.abs >= threshold){
                        if(sampledState.tryRecord(newQ)){
                            logger.info(s"Threshold $threshold passed: $newQ")
                            if(!terminateEarly){
                                scaleAndSample()
                            }
                        }
                        
                    }else if(newQ.prop.abs == 0.0){
                        logger.debug("Destrictive Interference")
                    }else{
                        logger.debug(s"Reaction finished, releasesing $newQ into the solution")
                        r(newQ)
                    }
                }else{
                    logger.debug("Incompatible molecules. Returning molecules to the pool")
                    r(a) + r(b)
                }
            }
        }
    )

    def build(c: Circuit, v: QVec)(implicit sampler: Sampler) : IO[Unit] = {
        c.remainingGates match{ 
            case Nil => 
               //for simon 
               // val midresult = sampler.sample()
                // if(v.v.takeRight(v.v.length / 2).sameElements(midresult)){
                //     val newMolecule = QVec(v.prop, v.v.take(v.v.length / 2))
                //     logger.debug("Releasing " + newMolecule)
                //     r(newMolecule)
                // }
                // val take = 4
                // val newMolecule = QVec(v.prop, v.v.take(take))
                //if(!v.v(0) & !v.v(1) & !v.v(2) & !v.v(3)){
                // logger.info("Releasing " + v)
                //IO.sleep(FiniteDuration(scala.util.Random.nextInt(40), scala.concurrent.duration.SECONDS)) *> 
                //IO.pure(r(v))
                //}

                //for the very large grover with ancillas 
                logger.debug("Releasing " + QVec(v.prop, v.v.take(5)))
                // This is needed for the giant grover instance otherwise the pool crashes 
                IO.sleep(FiniteDuration(scala.util.Random.nextInt(40), scala.concurrent.duration.SECONDS)) *>  IO.pure(r(QVec(v.prop, v.v.take(5))))
            case x :: xs => 
                x match {
                    case X(t) => 
                        val nv = QVec(v.prop, v.v.updated(t, !v.v(t)))
                        logger.debug(s"Applied  X: $nv")
                        build(Circuit(xs), nv)
                    case H(t) => 
                        val sign = if (v.v(t)) -1 else 1
                        val nc1 = Complex(sign * hScale * v.prop.real, sign * hScale * v.prop.imag)
                        val nc2 = Complex(hScale * v.prop.real, hScale * v.prop.imag) 
                        val m1 = if(v.v(t)){QVec(nc1, v.v)}else{QVec(nc1, v.v.updated(t, !v.v(t)))}
                        val m2 = if(v.v(t)){QVec(nc2, v.v.updated(t, !v.v(t)))}else{QVec(nc2, v.v)}
                        logger.debug(s"Done with H: m1: $m1, m2: $m2")
                        build(Circuit(xs), m1)
                        build(Circuit(xs), m2)
                    case CX(c, t) => 
                        if(v.v(c)){
                            logger.debug(s"Done after CX: with ${QVec(v.prop, v.v.updated(t, !(v.v(t))))}")
                            build(Circuit(xs), QVec(v.prop, v.v.updated(t, !(v.v(t)))))
                        }else{
                            logger.debug(s"Done after CX: with ${QVec(v.prop, v.v)}")
                            build(Circuit(xs), QVec(v.prop, v.v))
                        }
                    case CCX(c1, c2, t) => 
                        if(v.v(c1) && v.v(c2)){
                            val nc = QVec(v.prop, v.v.updated(t, !(v.v(t))))
                            logger.debug(s"Done after CCX: with $nc")
                            build(Circuit(xs), nc)
                        }else{
                            logger.debug(s"Done after CCX: with $v")
                            build(Circuit(xs), v)
                        }
                    case Swap(q1, q2) => 
                         val source = v.v(q1)
                         val target = v.v(q2)
                         val firstOverwrite = v.v.updated(q2, source)
                         val second = firstOverwrite.updated(q1, target)
                         build(Circuit(xs), QVec(v.prop, second))
                    case RZ(td, q) =>
                        val ph = RotationMath.applyRz(v.prop, td, v.v(q))
                        logger.debug(s"Done after RZ, with ${QVec(ph, v.v)}")
                        build(Circuit(xs), QVec(ph, v.v))
                    case Rotate(td, q) =>
                        val ph = RotationMath.applyPhase(v.prop, td, v.v(q))
                        logger.debug(s"Done after phase rotation, with ${QVec(ph, v.v)}")
                        build(Circuit(xs), QVec(ph, v.v))
                    case CRotate(c, td, q) =>
                        val ph =
                            RotationMath.applyControlledRotate(v.prop, td, v.v(c), v.v(q))
                        logger.debug(s"Done after controlled rotation, with ${QVec(ph, v.v)}")
                        build(Circuit(xs), QVec(ph, v.v))
                
                    case CZ(ctrl, target) =>  
                        if(v.v(ctrl) && v.v(target)){
                            build(Circuit(xs), QVec(v.prop * -1, v.v))
                        }else{
                            build(Circuit(xs), v)
                        }
                    case ModGate(a, n) =>
                        val rv: Vector[Boolean] = v.v.slice(0, 4)
                        val relevantbits = rv.map(a => if(a){1}else{0})
                        val rbstring = relevantbits.mkString("")
                        val i = Integer.parseInt(rbstring, 2)
                        val r = Math.floor(Math.pow(a, i) % n).toInt
                        val l = r.toBinaryString.toList.map(a => if(a == '1'){true}else{false})
                        build(Circuit(xs), QVec(v.prop, rv ++ l)) 
                    case Measure(q) => ??? 
                }
        }
    }

 def scaleAndSample() = {
        println("SCALING........")
        // terminate.volatileValue
        logger.debug(r.logSoup)
        println(r.volatileValue)
        val x = r.logSoup.split("Molecules:")(1)
        val each = x.split("terminate/P\\(QVec\\(")
        val pps = each.map(s => {
            val r = s.split(",\\)\\) * ")(0)
            val p = r.split("\\+")(0).drop(1)
            val v = r.split(",Vector").filter(u => u.contains("true") || u.contains("false"))
            (p, v.toVector)
        }).drop(1)

        val sqred = pps.map(x => (x._1.toDouble * x._1.toDouble, x._2)).toList
        val tot = sqred.foldLeft(0d)((a, b) => a + b._1)
        val scale = 1 / tot
        val scaled = pps.map(x => {
            val castToBool = x._2.map(b => b == "true")
            (x._1.toDouble * scale, castToBool)
        })
        println("pps+----")
        scaled.foreach(println)
        val res = new BasicSampler(scaled.toList).sample()
        println("------------- RESULT -----------")
        println(pps)
        println(res)
    }

    // Example Simon Circuit
    val fourbitsimon = Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1)))
    implicit val sampler = new FakeSimonSampler(fourbitsimon)
    val excs = QVec(1d, Vector(false, false, false, false))
    // Stream.eval(IO{build(fourbitsimon, excs)}).repeatN(concurrentInstances).compile.toVector.unsafeRunSync()


    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    // Example Shor Circuit
    //implicit val sampler = new FakeShorSampler()
    val _15bitshor = QVec(1d, Vector(false, false, false ,false ,false, false, false, false)) 
    val n15shor = Circuit(List(H(0), H(1), H(2), H(3), X(0), X(1), X(2), X(3), 
    ModGate(2, 15), 
    H(0), CRotate(0, -2, 1), Swap(0, 1), CRotate(1, -4, 2), Swap(1, 2),
    H(1), CRotate(2, -8, 3), Swap(2, 3), CRotate(1, -2, 2), Swap(1, 2), CRotate(2, -4, 3),
    Swap(2, 3), H(2), CRotate(2, -2, 3), Swap(2, 3), H(3)
    ))

    val _shor21 = QVec(1d, Vector(false, false, false, false, false, false, false, false))
    val n21shor = Circuit(List(H(0), H(1), H(2), CX(1, 5), H(3), CX(1, 7), CX(2, 5), CX(0, 7),
    CX(5, 3), RZ(-4, 3), CX(7, 3), RZ(4, 3), CX(5, 3), RZ(-4, 3), RZ(4, 5), CX(7, 3), CX(7, 5), RZ(4, 3), RZ(-4, 5), RZ(4, 7), 
    H(3), CX(7, 5), CX(0, 7), CX(1, 7), H(7), CX(3, 7), RZ(-4, 7), CX(0, 7), RZ(4,7), CX(3, 7), 
    RZ(4, 3), RZ(-4, 7), CX(0, 7), CX(0, 3), RZ(4, 7), RZ(4, 0), RZ(-4, 3), H(7), CX(0, 3), CX(3, 5), CX(7, 5), CX(0, 5), X(7), CX(2, 7), CX(1, 7), CX(0, 7),
    CX(0, 2), CX(2, 0), CX(0, 2), H(0), RZ(-4, 0), CX(0, 1), RZ(4, 1), CX(0, 1), RZ(-8, 0), RZ(-4, 1), CX(0, 2), H(1), RZ(8, 2), RZ(-4, 1), CX(0,2),
    RZ(-8, 2), CX(1, 2), RZ(4, 2), CX(1, 2), RZ(-4, 2), H(2)))

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    //Example Grover Circuit 
    val _g0000 = QVec(1d, Vector(false, false, false, false))
    val grover0000 = Circuit(List(
        H(0), H(1), H(2), H(3),
        X(0), X(1), X(2), X(3),
        CRotate(0, 2, 3), CX(0, 1), CRotate(1, -2, 3), CX(0, 1), CRotate(1, 2, 3), CX(1, 2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3), CX(1, 2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3),
        X(1), X(0), X(2), X(3), H(0), H(1), H(2), H(3), //r2
        X(1), X(0), X(2), X(3),
        CRotate(0, 2, 3), CX(0, 1), CRotate(1, -2, 3), CX(0, 1), CRotate(1, 2, 3), CX(1,2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3), CX(1, 2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3),
        X(1), X(0), X(2), X(3), H(0), H(1), H(2), H(3)))


    // Stream.eval(IO{build(grover0000, _g0000)}).compile.toVector.unsafeRunSync()

    val groverSAT011 = Circuit(List(H(0), H(1), H(2), X(2), CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1, 2),
    Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), Rotate(4, 2), CX(0,1), Rotate(4, 2), Rotate(4, 0), Rotate(-4, 1), X(2), CX(0,1),
    H(0), H(1), H(2), X(0), X(1), X(2),
    CX(1,2), Rotate(-4, 2), CX(0,2), Rotate(4, 2), CX(1, 2), Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), 
    Rotate(4, 0), Rotate(-4, 1), Rotate(4, 1), X(2), H(2), CX(0, 1), X(0), X(1), X(2), H(0), H(1),
    CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1,2), Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), 
    Rotate(4, 0), Rotate(-4, 1), Rotate(4, 2), X(0), CX(0, 1),
    H(0), H(1), H(2), X(0), X(1), X(2), H(0), H(1), H(2)))

    val _gSAT001 = QVec(1d, Vector(false, false, false))

    val gSAT111 = Circuit(List(H(0), H(1), H(2),
     H(2), CCX(0, 1, 2), H(2), 
     H(0), H(1), H(2), X(0), X(1), X(2), H(2), CCX(0, 1, 2), H(2), X(0), X(1), X(2), H(0), H(1), H(2), //r1
     //H(0), H(1), H(2), X(0), X(1), X(2), H(2), CCX(0, 1, 2), H(2), X(0), X(1), X(2), H(0), H(1), H(2), 
    ))

    val _gSAT111 = QVec(1d, Vector(false, false, false))
    // Stream.eval(build(gSAT111, _gSAT111)).compile.toVector.unsafeRunSync()


    //Requires 40GB+ memory to run!!! run at your own risk 
    val g511111 = Circuit(List(H(0), H(1), H(2), H(3), H(4),
        CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        H(0), H(1), H(2), H(3), H(4), X(0), X(1), X(2), X(3), X(4), 
        CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        X(0), X(1), X(2), X(3), X(4), H(0), H(1), H(2), H(3), H(4), //r1 
         CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        H(0), H(1), H(2), H(3), H(4), X(0), X(1), X(2), X(3), X(4), 
        CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        X(0), X(1), X(2), X(3), X(4), H(0), H(1), H(2), H(3), H(4), //r2
        // CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        // H(0), H(1), H(2), H(3), H(4), X(0), X(1), X(2), X(3), X(4), 
        // CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        // X(0), X(1), X(2), X(3), X(4), H(0), H(1), H(2), H(3), H(4) //r3
        //,CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        // H(0), H(1), H(2), H(3), H(4), X(0), X(1), X(2), X(3), X(4), 
        // CCX(0, 1, 5), CCX(1, 2, 6), CCX(5, 6, 7), CZ(7, 4), CCX(5, 6, 7), CCX(1, 2, 6), CCX(0, 1, 5),
        // X(0), X(1), X(2), X(3), X(4), H(0), H(1), H(2), H(3), H(4), //r4
    ))

     val _g511111 = QVec(1d, Vector(false, false, false, false, false, false, false ,false))
     Stream.eval(build(g511111, _g511111)).compile.toVector.unsafeRunSync()
     sampledState.awaitSample()
     if(terminateEarly){
         tp.shutdownNow()
     }
}
