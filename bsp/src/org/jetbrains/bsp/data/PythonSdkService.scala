package org.jetbrains.bsp.data

import com.intellij.facet.FacetManager
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.project.IdeModifiableModelsProvider
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.libraries.Library
import com.jetbrains.python.configuration.PyConfigurableInterpreterList
import com.jetbrains.python.facet.PythonFacetType
import org.jetbrains.plugins.scala.project.external.ScalaAbstractProjectDataService

import scala.jdk.CollectionConverters._

class PythonSdkService extends ScalaAbstractProjectDataService[PythonSdkData, Library](PythonSdkData.Key) {

  override final def importData(
    toImport: java.util.Collection[_ <: DataNode[PythonSdkData]],
    projectData: ProjectData,
    project: Project,
    modelsProvider: IdeModifiableModelsProvider
  ): Unit = {
    toImport.forEach(doImport(_, project)(modelsProvider))
  }

  private def doImport(dataNode: DataNode[PythonSdkData], project: Project)
                      (implicit modelsProvider: IdeModifiableModelsProvider): Unit =
    for {
      module <- modelsProvider.getIdeModuleByNode(dataNode)
    } {
      val PythonSdkData(venv, _) = dataNode.getData
      configurePython(Option(venv.toString), module)(project, modelsProvider)
    }
  private def configurePython(pythonSdk: Option[String], module: Module)(implicit  project: Project,  modelsProvider: IdeModifiableModelsProvider): Unit = {
    val basePath = project.getBasePath
    val model = modelsProvider.getModifiableRootModel(module)
    pythonSdk.
      flatMap{
        psdk =>
          Option(PyConfigurableInterpreterList.getInstance(project))
            .map { pycil =>
              val pysdks = pycil.getAllPythonSdks(project)
              pysdks
            }
            .flatMap{a =>
              val res = a.asScala.find{a =>
                val res = a.getHomePath.toLowerCase
                val base = s"$basePath/$psdk".toLowerCase
                res == base
              }
              res
            }
      }
      .foreach{ psdk =>
        executeProjectChangeAction {
          model.setSdk(psdk)
          val facetManager = FacetManager.getInstance(module)
          val facetType = PythonFacetType.getInstance()
          val facet = facetManager.createFacet(facetType, facetType.getStringId, null)
          val modelFacet = modelsProvider.getModifiableFacetModel(module)
          if(!facetManager.getAllFacets.toList.exists(_.getName == facet.getName))
            modelFacet.addFacet(facet)
        }
      }
  }
}
