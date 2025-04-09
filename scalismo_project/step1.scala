//> using scala "3.3"
//> using dep "ch.unibas.cs.gravis::scalismo-ui:0.92.0"
//> using dep "org.scalanlp:breeze_2.13:2.1.0"

import scalismo.ui.api.*
import scalismo.geometry.*
import scalismo.mesh.*
import scalismo.io.MeshIO

import scalismo.utils.Random.FixedSeed.randBasis
import breeze.linalg.{DenseMatrix, DenseVector, eigSym}
import scala.util.{Success, Failure}
import java.io.File

object PCAScalingModel extends App:
  scalismo.initialize()

  val ui = ScalismoUI()

  // Load model and target stumps
  // val targetMesh = MeshIO.readMesh(File("datasets/stump.ply")).get                  // Use this for true size
  // val modelMesh = MeshIO.readMesh(File("datasets/stump_model_surface.ply")).get

  val modelMesh = MeshIO.readMesh(File("datasets/stump.ply")).get                  // Use this for bigger size
  val targetMesh = MeshIO.readMesh(File("datasets/stump_model_surface.ply")).get

  val targetGroup = ui.createGroup("Target")
  val modelGroup = ui.createGroup("Model")

  ui.show(targetGroup, targetMesh, "Original Target")
  ui.show(modelGroup, modelMesh, "Original Model")

  // Compute PCA (Eigenvectors & Eigenvalues) for a mesh
  def computePCA(mesh: TriangleMesh[_3D]): (DenseVector[Double], DenseMatrix[Double], Point[_3D]) =
    val points = mesh.pointSet.points.toIndexedSeq
    val mean = Point(
      points.map(_.x).sum / points.size,
      points.map(_.y).sum / points.size,
      points.map(_.z).sum / points.size
    )

    // Construct covariance matrix
    val centeredPoints = points.map(p => DenseVector(p.x - mean.x, p.y - mean.y, p.z - mean.z))
    val covarianceMatrix = centeredPoints.map(p => p * p.t).reduce(_ + _) / points.size.toDouble

    // Perform eigen decomposition
    val eigDecomposition = eigSym(covarianceMatrix)
    val eigenvalues = eigDecomposition.eigenvalues
    val eigenvectors = eigDecomposition.eigenvectors

    (eigenvalues, eigenvectors, mean)

  val (modelEigenvalues, modelEigenvectors, modelMean) = computePCA(modelMesh)
  val (targetEigenvalues, _, targetMean) = computePCA(targetMesh) // Only need eigenvalues for scaling

  // Translate both meshes to the origin
  def translateToOrigin(mesh: TriangleMesh[_3D], mean: Point[_3D]): TriangleMesh[_3D] =
    mesh.transform(p => Point(p.x - mean.x, p.y - mean.y, p.z - mean.z))

  val translatedModelMesh = translateToOrigin(modelMesh, modelMean)
  val translatedTargetMesh = translateToOrigin(targetMesh, targetMean)

  // Compute scaling factors along each principal axis
  val scaleFactors = DenseVector(
    Math.sqrt(targetEigenvalues(0) / modelEigenvalues(0)),
    Math.sqrt(targetEigenvalues(1) / modelEigenvalues(1)),
    Math.sqrt(targetEigenvalues(2) / modelEigenvalues(2))
  )
  println(s"Scaling factors along principal axes: $scaleFactors")

  // Apply scaling transformation in the principal component space
  def scalePointPCA(p: Point[_3D], eigenvectors: DenseMatrix[Double], scales: DenseVector[Double]): Point[_3D] =
    val transformed = eigenvectors * (scales *:* (eigenvectors.t * DenseVector(p.x, p.y, p.z))) // Apply scaling
    Point(transformed(0), transformed(1), transformed(2))

  val scaledMesh = translatedModelMesh.transform(p => scalePointPCA(p, modelEigenvectors, scaleFactors))

  val scaledModelView = ui.show(modelGroup, scaledMesh, "PCA Scaled Model")
  scaledModelView.color = java.awt.Color.GREEN

  val translatedTargetView = ui.show(targetGroup, translatedTargetMesh, "Translated Target")
  translatedTargetView.color = java.awt.Color.BLUE

  // Export both translated and scaled meshes
  def saveMesh(mesh: TriangleMesh[_3D], filename: String): Unit =
    MeshIO.writeMesh(mesh, File(filename)) match
      case Failure(e) => println(s"Failed to write mesh $filename: ${e.getMessage}")
      case Success(_) => println(s"Successfully saved $filename")

  saveMesh(translatedModelMesh, "datasets/scaled_translated_stump_model.ply")
  saveMesh(translatedTargetMesh, "datasets/translated_target_stump.ply")
  saveMesh(scaledMesh, "datasets/scaled_stump_model.ply")

  println("Translation & PCA-based Scaling complete. Ready for rigid alignment.")
