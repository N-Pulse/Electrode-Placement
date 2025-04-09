//> using scala "3.3"
//> using dep "ch.unibas.cs.gravis::scalismo-ui:0.92.0"
// //> using dep "org.scalanlp:breeze_2.13:2.1.0"

import scalismo.ui.api.*
import scalismo.geometry.*
import scalismo.mesh.*
import scalismo.transformations.*
// import scalismo.registration.LandmarkRegistration
import scalismo.io.MeshIO
import scalismo.common.*

import scalismo.utils.Random.FixedSeed.randBasis
import breeze.linalg.{DenseMatrix, DenseVector, eigSym}
import scala.util.{Success, Failure}
import java.io.File
import java.awt.Color
import org.checkerframework.checker.units.qual.s

object OpeningBasedAlignment extends App:
  scalismo.initialize()

  val ui = ScalismoUI()

  // Load translated and scaled meshes
  val modelMesh = MeshIO.readMesh(File("datasets/scaled_stump_model.ply")).get
  val targetMesh = MeshIO.readMesh(File("datasets/translated_target_stump.ply")).get

  val modelGroup = ui.createGroup("Model")
  val targetGroup = ui.createGroup("Target")

  ui.show(modelGroup, modelMesh, "Model Mesh")
  ui.show(targetGroup, targetMesh, "Target Mesh")

  // Compute PCA (Eigenvectors & Eigenvalues) for a boundary loop
  def computeOpeningNormal(boundaryPoints: Seq[Point[_3D]]): EuclideanVector[_3D] =
    val mean = Point(
      boundaryPoints.map(_.x).sum / boundaryPoints.size,
      boundaryPoints.map(_.y).sum / boundaryPoints.size,
      boundaryPoints.map(_.z).sum / boundaryPoints.size
    )

    val centeredPoints = boundaryPoints.map(p => DenseVector(p.x - mean.x, p.y - mean.y, p.z - mean.z))
    val covarianceMatrix = centeredPoints.map(p => p * p.t).reduce(_ + _) / boundaryPoints.size.toDouble

    val eigDecomposition = eigSym(covarianceMatrix)
    val eigenvectors = eigDecomposition.eigenvectors

    val normalIdx = (0 until 3).minBy(i => eigDecomposition.eigenvalues(i)) // Smallest eigenvalue → normal direction
    val normalVector = eigenvectors(::, normalIdx)

    EuclideanVector(normalVector(0), normalVector(1), normalVector(2)).normalize

  // Function to compute center of mass of a mesh
  def computeCenterOfMass(mesh: TriangleMesh[_3D]): Point[_3D] =
    val points = mesh.pointSet.points.toIndexedSeq
    Point(
      points.map(_.x).sum / points.size,
      points.map(_.y).sum / points.size,
      points.map(_.z).sum / points.size
    )

  // Function to compute centroid of a loop
  def computeLoopCentroid(loop: Seq[Point[_3D]]): Point[_3D] =
    Point(
      loop.map(_.x).sum / loop.size,
      loop.map(_.y).sum / loop.size,
      loop.map(_.z).sum / loop.size
    )

  // // Detect boundary loops
  // def findBoundaryLoops(mesh: TriangleMesh[_3D]): Seq[Seq[Point[_3D]]] =
  //   val edgeCounts = collection.mutable.Map[(PointId, PointId), Int]()
  //   for (triangle <- mesh.triangulation.triangles) do
  //     val edges = Seq(
  //       (triangle.ptId1, triangle.ptId2),
  //       (triangle.ptId2, triangle.ptId3),
  //       (triangle.ptId3, triangle.ptId1)
  //     ).map { case (a, b) => if a.id < b.id then (a, b) else (b, a) }
  //     edges.foreach { edge => edgeCounts(edge) = edgeCounts.getOrElse(edge, 0) + 1 }

  //   val boundaryEdges = edgeCounts.collect {
  //     case ((p1, p2), count) if count == 1 => (mesh.pointSet.point(p1), mesh.pointSet.point(p2))
  //   }.toSeq

  //   val pointToEdges = boundaryEdges.flatMap(e => Seq(e._1 -> e, e._2 -> e)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  //   val loops = collection.mutable.ArrayBuffer[Seq[Point[_3D]]]()
  //   val visited = collection.mutable.Set[Point[_3D]]()

  //   def traceLoop(start: Point[_3D]): Seq[Point[_3D]] =
  //     val loop = collection.mutable.ArrayBuffer[Point[_3D]]()
  //     var current = start
  //     while (!visited.contains(current)) do
  //       visited.add(current)
  //       loop.append(current)
  //       val nextEdges = pointToEdges.getOrElse(current, Seq()).filterNot(e => visited.contains(e._2))
  //       if nextEdges.nonEmpty then current = nextEdges.head._2 else return loop.toSeq
  //     loop.toSeq

  //   for edge <- boundaryEdges if !visited.contains(edge._1) do loops.append(traceLoop(edge._1))

  //   loops.toSeq

  def findLargestBoundaryLoop(mesh: TriangleMesh[_3D]): Seq[Point[_3D]] =
    val edgeCounts = collection.mutable.Map[(PointId, PointId), Int]()

    for (triangle <- mesh.triangulation.triangles) do
      val edges = Seq(
        (triangle.ptId1, triangle.ptId2),
        (triangle.ptId2, triangle.ptId3),
        (triangle.ptId3, triangle.ptId1)
      ).map { case (a, b) => if a.id < b.id then (a, b) else (b, a) }
      edges.foreach { edge => edgeCounts(edge) = edgeCounts.getOrElse(edge, 0) + 1 }

    // Collect boundary edges
    val boundaryEdges = edgeCounts.collect {
      case ((p1, p2), count) if count == 1 => (mesh.pointSet.point(p1), mesh.pointSet.point(p2))
    }.toSeq

    val pointToEdges = boundaryEdges.flatMap(e => Seq(e._1 -> e, e._2 -> e)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    val loops = collection.mutable.ArrayBuffer[Seq[Point[_3D]]]()
    val visited = collection.mutable.Set[Point[_3D]]()

    def traceLoop(start: Point[_3D]): Seq[Point[_3D]] =
      val loop = collection.mutable.ArrayBuffer[Point[_3D]]()
      var current = start
      while (!visited.contains(current)) do
        visited.add(current)
        loop.append(current)
        val nextEdges = pointToEdges.getOrElse(current, Seq()).filterNot(e => visited.contains(e._2))
        if nextEdges.nonEmpty then current = nextEdges.head._2 else return loop.toSeq
      loop.toSeq

    for edge <- boundaryEdges if !visited.contains(edge._1) do loops.append(traceLoop(edge._1))
    printf("Found %d boundary loops\n", loops.size)

    // Return the **largest loop** by perimeter (assuming uniform spacing, length ≈ count of points)
    loops.minByOption(_.size).getOrElse(Seq.empty)

  val largestModelLoop = findLargestBoundaryLoop(modelMesh)
  val largestTargetLoop = findLargestBoundaryLoop(targetMesh)

  // val largestModelLoop = modelLoops.maxByOption(_.size).getOrElse(Seq.empty)
  // val largestTargetLoop = targetLoops.maxByOption(_.size).getOrElse(Seq.empty)

  // val modelCentroid = Point(
  //   largestModelLoop.map(_.x).sum / largestModelLoop.size,
  //   largestModelLoop.map(_.y).sum / largestModelLoop.size,
  //   largestModelLoop.map(_.z).sum / largestModelLoop.size
  // )

  // val targetCentroid = Point(
  //   largestTargetLoop.map(_.x).sum / largestTargetLoop.size,
  //   largestTargetLoop.map(_.y).sum / largestTargetLoop.size,
  //   largestTargetLoop.map(_.z).sum / largestTargetLoop.size
  // )

  val modelLoopCentroid = computeLoopCentroid(largestModelLoop)
  val targetLoopCentroid = computeLoopCentroid(largestTargetLoop)

  val modelCenterOfMass = computeCenterOfMass(modelMesh)
  val targetCenterOfMass = computeCenterOfMass(targetMesh)

  val ptsGroup = ui.createGroup("Points")
  ui.show(ptsGroup, Landmark("modelCentroid_pt", modelLoopCentroid), "Model centroid Point")
  ui.show(ptsGroup, Landmark("targetCentroid_pt", targetLoopCentroid), "Target centroid Point")

  ui.show(ptsGroup, Landmark("modelCenterOfMass_pt", modelCenterOfMass), "Model Center of Mass")
  ui.show(ptsGroup, Landmark("targetCenterOfMass_pt", targetCenterOfMass), "Target Center of Mass")

  // val modelNormal = computeOpeningNormal(largestModelLoop)
  // val targetNormal = computeOpeningNormal(largestTargetLoop)

  val modelVector = modelLoopCentroid - modelCenterOfMass
  val targetVector = targetLoopCentroid - targetCenterOfMass

  // println(s"Model opening centroid: $modelCentroid, normal: $modelNormal")
  // println(s"Target opening centroid: $targetCentroid, normal: $targetNormal")

  // Converts Axis-Angle representation to Euler Angles (Yaw, Pitch, Roll)
  def axisAngleToEuler(axis: EuclideanVector[_3D], theta: Double): (Double, Double, Double) =
    val ux = axis.x
    val uy = axis.y
    val uz = axis.z
    val cosTheta = Math.cos(theta)
    val sinTheta = Math.sin(theta)
    val oneMinusCosTheta = 1.0 - cosTheta

    // Compute rotation matrix from axis-angle
    val r00 = cosTheta + ux * ux * oneMinusCosTheta
    val r01 = ux * uy * oneMinusCosTheta - uz * sinTheta
    val r02 = ux * uz * oneMinusCosTheta + uy * sinTheta
    val r10 = uy * ux * oneMinusCosTheta + uz * sinTheta
    val r11 = cosTheta + uy * uy * oneMinusCosTheta
    val r12 = uy * uz * oneMinusCosTheta - ux * sinTheta
    val r20 = uz * ux * oneMinusCosTheta - uy * sinTheta
    val r21 = uz * uy * oneMinusCosTheta + ux * sinTheta
    val r22 = cosTheta + uz * uz * oneMinusCosTheta

    // Extract Euler angles from rotation matrix
    val yaw = Math.atan2(r21, r22)
    val pitch = Math.asin(-r20)
    val roll = Math.atan2(r10, r00)

    (yaw, pitch, roll)

  // Compute the optimal rotation using absolute normal alignment
  def computeRotation(from: EuclideanVector[_3D], to: EuclideanVector[_3D]): Rotation[_3D] =
    val axis = from.crossproduct(to).normalize
    val angle = Math.acos(from.dot(to) / (from.norm * to.norm))
    if angle.toDegrees == 180.0 then println("Vectors are opposite")

    println(s"Computed Rotation Axis: $axis, Angle: ${angle.toDegrees}")

    // Flip condition: If the dot product is negative, the normal is reversed
    val correctedAngle = if from.dot(to) < 0 then Math.PI + angle else angle

    println(s"Computed Rotation Axis: $axis, Angle: ${correctedAngle.toDegrees}")
  
    val rotationCenter = Point3D(0.0, 0.0, 0.0)
    if axis.norm == 0.0 then
      Rotation3D(0.0f, 0.0f, 0.0f, rotationCenter) // Identity rotation if vectors are the same
    else
      val (yaw, pitch, roll) = axisAngleToEuler(axis, correctedAngle)
      println(s"Computed Rotation Angles -> Yaw: ${yaw.toDegrees}, Pitch: ${pitch.toDegrees}, Roll: ${roll.toDegrees}")
      Rotation3D(roll, pitch, yaw, rotationCenter) // Apply rotation
    // val (yaw, pitch, roll) = axisAngleToEuler(axis, correctedAngle)
    // println(s"Computed Rotation Angles -> Yaw: ${yaw.toDegrees}, Pitch: ${pitch.toDegrees}, Roll: ${roll.toDegrees}")
    // Rotation3D(yaw, pitch, roll, rotationCenter) // Apply rotation


  // // Show vectors
  // val vectorGroup = ui.createGroup("Vectors")
  // val modelVectorField = VectorFieldView(modelCenterOfMass, modelVector)
  // val targetVectorField = VectorFieldView(targetCenterOfMass, targetVector)
  // ui.show(vectorGroup, modelVectorField, "Model Vector")
  // ui.show(vectorGroup, targetVectorField, "Target Vector")



  val normalRotation = computeRotation(modelVector, targetVector)

  // val model_pt: Point[_3D] = Point(15.0, 6.0, 0.0)
  // val target_pt : Point[_3D] = Point(0.0, 1.0, 7.0)

  // val exampleRotation = computeRotation(model_pt.toVector, target_pt.toVector)
  // println(s"Example Rotation: $exampleRotation")

  // val example_align : RigidTransformation[_3D] = RotationAfterTranslation3D(Translation3D(EuclideanVector(0.0, 0.0, 0.0)),exampleRotation)
  // val aligned_pt = example_align(model_pt)

  // val exampleGroup = ui.createGroup("Example")
  // ui.show(exampleGroup, Landmark("model_pt", model_pt), "Model Point")
  // ui.show(exampleGroup, Landmark("target_pt", target_pt), "Target Point")
  // ui.show(exampleGroup, Landmark("aligned_pt", aligned_pt), "Aligned Point")

  // Translate to align centroids
  val translationVector = targetCenterOfMass - modelCenterOfMass
  val normalTranslation = Translation3D(translationVector)

  // Apply rigid transformation
  val rigidTransform : RigidTransformation[_3D] = TranslationAfterRotation3D(normalTranslation, normalRotation)
  val alignedMesh = modelMesh.transform(rigidTransform)

  val alignedMeshView = ui.show(modelGroup, alignedMesh, "Aligned Model")
  alignedMeshView.color = java.awt.Color.GREEN

  println("Rigid alignment using opening completed.")

  // Save the aligned model
  val outputFile = new File("datasets/aligned_stump_model.ply")
  MeshIO.writeMesh(alignedMesh, outputFile) match
    case Failure(e) => println(s"Failed to write mesh: ${e.getMessage}")
    case Success(_) => println(s"Successfully saved aligned model to ${outputFile.getPath}")
