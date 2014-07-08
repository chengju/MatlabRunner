import edu.berkeley.path.beats.jaxb
import edu.berkeley.path.beats.simulator._
import java.io.File
import matlabcontrol.extensions.{MatlabTypeConverter, MatlabNumericArray}
import matlabcontrol.{MatlabProxyFactoryOptions, MatlabProxyFactory}
import scala.collection.JavaConversions._

/**
 * Created by jdr on 7/7/14.
 */

class JackObjectSomething extends JaxbObjectFactory {
  override def createScenario(): jaxb.Scenario = new JackScenario
}

class JackScenario extends Scenario {
  val options = new MatlabProxyFactoryOptions.Builder()
  val hidden = false
  options.setHidden(hidden)
  options.setMatlabStartingDirectory(new File("/Users/jdr/L0-boundary-flows"))
  val factory = new MatlabProxyFactory(options.build())
  val proxy = factory.getProxy
  var sensor_ids: Array[Array[Double]] = null
  var link_ids: Array[Array[Double]] = null
  var demandLinks: Seq[Link] = null
  var processor: MatlabTypeConverter = new MatlabTypeConverter(proxy)

  def matlabDensities(nLinks: Int) = {
    val command = "ic_solution = test_ic(%d,1)" format nLinks
    proxy.eval(command)
    processor.getNumericArray("ic_solution").getRealArray2D.transpose.head
  }

  override def populate() {
    super.populate()
    sensor_ids = Array(sensorSet.getSensor.toList.map{_.getSensorIdOriginal.toDouble}.toArray)
    link_ids = Array(sensorSet.getSensor.toList.map{_.getLinkId.toDouble}.toArray)
    val net = getNetworkSet.getNetwork.head.asInstanceOf[Network]
    demandLinks = link_ids.head.map{lid => net.getLinkWithId(lid.toLong)}
    processor.setNumericArray("sensor_ids", new MatlabNumericArray(sensor_ids, null))
    proxy eval "setupBoundaryFlows;"
    proxy eval "train_data_set;"
  }

  override def gather_current_densities(): InitialDensitySet = {
    val network = this.networkSet.getNetwork.get(0).asInstanceOf[Network]
    val densities = matlabDensities(network.getListOfLinks.size())
    val factory = new JaxbObjectFactory()
    val init_dens_set = factory.createInitialDensitySet().asInstanceOf[InitialDensitySet]
    network.getListOfLinks.toList.zip(densities).foreach{case (l,d) => {
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


  override def predict_demands(time_current: Double, sample_dt: Double, horizon_steps: Int): DemandSet = {
    val network = getNetworkSet.getNetwork.get(0).asInstanceOf[Network]
    val previous_points = demandLinks.map{link => {
      val n_steps = math.max(1,math.min(time_current / sample_dt, 10)).toInt
        val previous_time = time_current - n_steps * sample_dt
        link.getDemandProfile.predict_in_VPS(0, previous_time,sample_dt, n_steps)
    }}.toArray
    proxy.eval("clear previous_points previous_points_real previous_points_imag")
    processor.setNumericArray("previous_points", new MatlabNumericArray(previous_points, null))
    proxy.eval("disp('hello after after')")
    val command = "demand_solution = demand_for_beats(%s, %d, %s, %d, %d, %d)".format("sensor_ids", horizon_steps.toInt, "previous_points", sample_dt.toInt, time_current.toInt, 0)
    proxy.eval(command)
    val demands = processor.getNumericArray("demand_solution").getRealArray2D
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

  override def finalize(): Unit = {
    proxy.disconnect()
    super.finalize()
  }
}