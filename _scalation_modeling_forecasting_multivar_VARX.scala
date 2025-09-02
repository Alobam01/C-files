//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// VARX_Sim_Bank.scala
// Standalone Scalation Process Simulation (Scala 2.12)
// - A tiny bank with a time-varying arrival rate driven by an exogenous signal.
// - Customers arrive, seize a teller Resource, receive service, and depart.
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.math._
import scalation.random._
import scalation.simulation.process._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Customer actor: arrives -> request teller -> service -> release -> depart
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class Customer(idNum: Int, model: VarxBankSim) extends SimActor(s"Cust_$idNum", model) {

  override def act(): Unit = {
    val start = model.clock

    // request a teller (queue if all busy)
    model.teller.request()
    val qEnter = model.clock

    // sample service time (could also depend on exogenous signal if desired)
    val svc = model.serviceRV()
    hold(svc)

    // release teller
    model.teller.release()

    val finish = model.clock
    // record simple stats
    model.timeInSystem.tally(finish - start)
    model.waitInQueue.tally(qEnter - start)
    model.serviceTime.tally(svc)

    // send to sink (for counting/completion)
    model.sink.leave(this)
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// The simulation model
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class VarxBankSim(
                   name: String         = "VARX_Bank_Sim",
                   reps: Int            = 1,
                   animating: Boolean   = false,
                   aniRatio: Double     = 1.0,
                   nStop: Int           = 200,     // stop after this many arrivals
                   period: Double       = 60.0,    // period for exogenous sinusoid (sim time units)
                   alpha: Double        = 0.75,    // strength of exogenous effect on arrival rate
                   baseIAT: Double      = 3.0,     // base inter-arrival mean (time units)
                   svcMean: Double      = 2.0,     // service time mean
                   svcScv: Double       = 1.0,     // service SCV ~1 => exponential-ish
                   tellers: Int         = 2,       // number of teller servers
                   seedStream: Int      = 0
                 ) extends Model(name, reps, animating, aniRatio) {

  // ------------------------ Resources, Sink & Stats --------------------------
  val teller = Resource("Teller", tellers, this)
  val sink   = Sink("Sink")

  val timeInSystem = new Statistic("TimeInSystem")
  val waitInQueue  = new Statistic("WaitInQueue")
  val serviceTime  = new Statistic("ServiceTime")

  // ------------------------ Random Variates ----------------------------------
  // - Service time: Gamma(shape, scale) chosen from mean and SCV.
  //   For SCV=1, Gamma(shape=1) is exponential; SCV<1 → less variable; >1 more variable.
  //   shape k = 1/SCV, scale θ = mean * SCV
  private val shape = if (svcScv <= 0.0) 1.0 else 1.0 / svcScv
  private val scale = svcMean * (if (svcScv <= 0.0) 1.0 else svcScv)
  private val gammaSvc = Gamma(shape, scale, stream = seedStream)

  def serviceRV(): Double = max(1e-6, gammaSvc()) // avoid zero

  // ------------------------ Exogenous signal (X_t) ---------------------------
  // Example exogenous factor: sinusoid in [0, 1], mean 0.5
  // You can replace this with any time-varying function or external data.
  def exo(t: Double): Double = 0.5 + 0.5 * sin(2.0 * Pi * t / period)

  // Given time t, build a fresh inter-arrival draw that accounts for exogenous signal
  // We compress or stretch the mean IAT with (1 + alpha * (exo(t) - 0.5))
  // => when exo(t) > 0.5, arrivals faster; when < 0.5, slower.
  private def nextInterArrival(t: Double): Double = {
    val factor = 1.0 + alpha * (exo(t) - 0.5)     // typical range ~ [1 - alpha/2, 1 + alpha/2]
    val meanIAT = max(1e-6, baseIAT / factor)     // smaller mean => faster arrivals
    Exponential(meanIAT, stream = seedStream + 1)()
  }

  // ------------------------ Arrival process actor ----------------------------
  // We implement our own Source to allow time-varying IATs each cycle.
  private object ArrivalSource extends SimActor("ArrivalSource", this) {
    override def act(): Unit = {
      var i = 0
      while (i < nStop) {
        // wait next inter-arrival (depends on current simulation time via exo(clock))
        val iat = nextInterArrival(clock)
        hold(iat)

        // create & schedule new customer
        Customer(i, VarxBankSim.this).schedule()
        i += 1
      }
    }
  }

  // ------------------------ Build and run ------------------------------------
  def build(): Unit = {
    // nothing else to wire; customers handle their own flow
  }

  override def simulate(): Unit = {
    build()
    // start processes
    ArrivalSource.schedule(0.0)
    super.simulate()
  }

  override def report(): Unit = {
    super.report()
    println("--------------- Simulation Summary ---------------")
    println(f"Arrivals processed: ${sink.num}\n")
    println(f"Mean Time in System : ${timeInSystem.mean}%.4f")
    println(f"Mean Wait in Queue  : ${waitInQueue.mean}%.4f")
    println(f"Mean Service Time   : ${serviceTime.mean}%.4f")
    println("--------------------------------------------------")
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main: run the standalone simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def RunVarxBankSim(): Unit = {
  val sim = new VarxBankSim(
    nStop   = 200,   // total arrivals
    period  = 60.0,  // exogenous period
    alpha   = 0.75,  // exogenous effect strength
    baseIAT = 3.0,   // base inter-arrival mean
    svcMean = 2.0,   // mean service time
    tellers = 2
  )

  sim.simulate()
  sim.waitFinished()
  sim.report()
  Model.shutdown()
}
