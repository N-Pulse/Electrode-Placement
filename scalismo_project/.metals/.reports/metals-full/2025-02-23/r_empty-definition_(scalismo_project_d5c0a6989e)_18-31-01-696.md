error id: `<none>`.
file:///C:/Users/willi/Desktop/N-pulse/scalismo_project/model_creation.scala
empty definition using pc, found symbol in pc: `<none>`.
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -

Document text:

```scala
//> using scala "3.3"
//> using dep "ch.unibas.cs.gravis::scalismo-ui:0.92.0"
// !!! if you are working on a Mac with M1 or M2 processor, use the following import instead !!!
// //> using dep "ch.unibas.cs.gravis::scalismo-ui:0.92.0,exclude=ch.unibas.cs.gravis%vtkjavanativesmacosimpl"

// Basic geometric primitives
import scalismo.geometry.*
import scalismo.common.PointId

// Geometric objects
import scalismo.mesh.*
import scalismo.image.{DiscreteImage, DiscreteImage3D}
import scalismo.statisticalmodel.PointDistributionModel

// IO Methods
import scalismo.io.*

// Visualization
import scalismo.ui.api.*

// File object from java
import java.io.File

// Choosing seeding mechanism for random number generator
import scalismo.utils.Random.FixedSeed.randBasis

import scalismo.io.LandmarkIO
import scalismo.common.Landmark

object ForearmElectrodes extends App
{
  scalismo.initialize()

  val ui = ScalismoUI()

  val forearmMesh: TriangleMesh[_3D] = MeshIO.readMesh(new File("datasets/forearm.ply")).get
  val forearmGroup = ui.createGroup("Forearm Model")
  val forearmView = ui.show(forearmGroup, forearmMesh, "Forearm")

  // Select electrode positions based on mesh points
  val electrodeIndices = Seq(100, 250, 400) // Example: selecting vertex indices
  val electrodePositions = electrodeIndices.map(i => forearmMesh.pointSet.point(PointId(i)))

  // Show electrodes on the mesh
  val electrodeViews = electrodePositions.map { p =>
    ui.show(forearmGroup, p, "Electrode")
  }

  // Convert electrodes to landmarks
  val electrodeLandmarks = projectedElectrodes.zipWithIndex.map {
    case (p, i) => Landmark(s"Electrode_$i", p)
  }

  // Save electrodes as landmarks
  LandmarkIO.writeLandmarksJson[_3D](electrodeLandmarks, new File("datasets/electrodes.json"))

}

```

#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.