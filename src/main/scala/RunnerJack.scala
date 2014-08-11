import edu.berkeley.path.beats.jaxb
import edu.berkeley.path.beats.simulator._
import java.io.{FileReader, File}
import java.util.Properties
import matlabcontrol.extensions.{MatlabTypeConverter, MatlabNumericArray}
import matlabcontrol.{MatlabProxyFactoryOptions, MatlabProxyFactory}
import scala.collection.JavaConversions._

/**
 * Created by jdr on 7/7/14.
 */

class JackObjectSomething(propsFn: String) extends JaxbObjectFactory {
  override def createScenario(): jaxb.Scenario = new JackScenario(propsFn)
}

class JackScenario(propsFn: String) extends Scenario {
  val props = new Properties()
  val fr = new FileReader(propsFn)
  props.load(fr)
  fr.close()
  val matlabRoot = props.getProperty("matlabRoot", "/Users/jdr/L0-boundary-flows")
  val beatsRoot = props.getProperty("beatsRoot", "/Users/jdr/runner/beats")
  val options = new MatlabProxyFactoryOptions.Builder()
  val hidden = false
  options.setHidden(hidden)
  options.setMatlabStartingDirectory(new File(matlabRoot))
  val factory = new MatlabProxyFactory(options.build())
  val proxy = factory.getProxy
  var sensor_ids: Array[Array[Double]] = null
  var sensor_link_ids: Array[Array[Double]] = null
  var demand_link_ids: Array[Array[Double]] = null
  var demandLinks: Seq[Link] = null
  var allLinks: Seq[Link] = null
  var allLinkIds: Array[Array[Double]] = null
  var processor: MatlabTypeConverter = new MatlabTypeConverter(proxy)
  var _sample_dt: Double = -1
  var _current_time: Double = -1
  var prev_time: Double = 0

  def saveArray(array: Array[Array[Double]], name: String) {
    processor.setNumericArray(name, new MatlabNumericArray(array, null))
  }

  override def populate() {
    super.populate()
    sensor_ids = Array(sensorSet.getSensor.toList.map{_.getId.toDouble}.toArray)
    demand_link_ids = Array(demandSet.getDemandProfile.map{_.getLinkIdOrg.toDouble}.toArray)
    sensor_link_ids = Array(sensorSet.getSensor.toList.map{_.getLinkId.toDouble}.toArray)
    val net = getNetworkSet.getNetwork.head.asInstanceOf[Network]
    allLinks = net.getListOfLinks.map{_.asInstanceOf[Link]}
    allLinkIds = Array(allLinks.map{_.getId.toDouble}.toArray)
    demandLinks = demand_link_ids.head.map{lid => net.getLinkWithId(lid.toLong)}
    saveArray(sensor_ids, "sensor_ids")
    saveArray(allLinkIds, "link_ids")
    saveArray(demand_link_ids, "link_ids_demand")
    proxy eval "setupBoundaryFlows;"
    //proxy eval "train_data_set;"
    proxy.eval("setup_est('%s', '%s')".format(propsFn, beatsRoot))
  }

  def matlabDensities = {
    val time_current = _current_time

    val sample_dt = _sample_dt
    val n_steps = math.max(1,math.min((time_current - prev_time) / sample_dt, 1000)).toInt
    val previous_time = time_current - n_steps * sample_dt
    println("current: " + time_current.toInt)
    println("previuos" + previous_time.toInt)

    // TODO(jackdreilly): This is obviously a stub
    val previous_demand_points = demand_link_ids.head.map{lid => {
      Array.fill(n_steps)(1.0)
    }}.toArray

    val previous_points = demandLinks.map{link => {
      link.getDemandProfile.predict_in_VPS(0, previous_time,sample_dt, n_steps)
    }}.toArray
    val currentTime = Array(Array(2001,1,10,0,0,time_current))
    saveArray(previous_points, "previous_points")
    saveArray(previous_demand_points, "previous_demand_points")
    saveArray(currentTime, "time_current")
    if (time_current > 0) {
      proxy.eval("update_demand_estimation(%s, %d, %s, %d)".format("link_ids_demand", previous_time.toInt, "previous_demand_points", sample_dt.toInt))
      proxy.eval("update_sensor_estimation(%s, %d, %s, %d)".format("sensor_ids", previous_time.toInt, "previous_points", sample_dt.toInt))
    }
    getCommandResult("give_estimate(%s, %d)".format("link_ids", time_current.toInt))
  }

  override def gather_current_densities(): InitialDensitySet = {
    val densities = matlabDensities
    val factory = new JaxbObjectFactory()
    val init_dens_set = factory.createInitialDensitySet().asInstanceOf[InitialDensitySet]
    allLinks.zip(densities).foreach{case (l,d) => {
      (0 until getNumVehicleTypes).foreach{v => {
        val den = factory.createDensity()
        den.setLinkId(l.getId)
        den.setVehicleTypeId(getVehicleTypeIdForIndex(v))
        den.setContent(d.toString)
        init_dens_set.getDensity.add(den)
      }}
    }}
    init_dens_set
  }

  def getCommandResult(command: String) = {
    proxy.eval("tmp = " + command)
    processor.getNumericArray("tmp").getRealArray2D
  }

  def getMatlabDemands(time_current: Double, sample_dt: Double, horizon_steps: Int) = {
    val previous_points = demandLinks.map{link => {
      val n_steps = math.max(1,math.min((time_current - prev_time) / sample_dt, 1000)).toInt
      val previous_time = time_current - n_steps * sample_dt
      link.getDemandProfile.predict_in_VPS(0, previous_time,sample_dt, n_steps)
    }}.toArray
    val currentTime = Array(Array(2001,1,10,0,0,time_current))
    saveArray(previous_points, "previous_points")
    saveArray(currentTime, "time_current")
    val stringForSensors = "link_ids_demand"
    proxy.eval("update_detectors(%s, %s, %s, %d)".format(stringForSensors, "time_current", "previous_points", sample_dt.toInt))
    getCommandResult("demand_for_beats(%s, %d, %d, %s)".format(stringForSensors, horizon_steps, sample_dt.toInt, "time_current"))
  }

  override def predict_demands(time_current: Double, sample_dt: Double, horizon_steps: Int): DemandSet = {
    println("beginning")
    println("time current: " + time_current)
    println("sample dt: " + sample_dt)
    println("horizon steps: " + horizon_steps)
    // huge hack, and potentially incorrect!
    _sample_dt = sample_dt
    prev_time = _current_time
    _current_time = time_current
    val demands = getMatlabDemands(time_current, sample_dt, horizon_steps)
    val factory = new JaxbObjectFactory()
    val demand_set = factory.createDemandSet().asInstanceOf[DemandSet]
    demandLinks.zip(demands).map{case (link, demand) => {
      val dp = factory.createDemandProfile().asInstanceOf[DemandProfile]
      val demand_profile = link.getDemandProfile
      demand_set.getDemandProfile.add(dp)
      dp.setLinkIdOrg(link.getId)
      dp.setDt(sample_dt)
      (0 until getNumVehicleTypes).foreach{i => {
        val dem = factory.createDemand()
        dp.getDemand.add(dem)
        dem.setVehicleTypeId(i)
        dem.setContent(BeatsFormatter.csv(demand, ","))
      }}
    }}
    demand_set
  }
}