package org.jetbrains.bsp.data

import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.project.IdeModifiableModelsProvider
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.impl.{ProjectJdkImpl, SdkConfigurationUtil}
import com.intellij.openapi.projectRoots.{JavaSdk, ProjectJdkTable, Sdk}
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.roots.impl.LanguageLevelProjectExtensionImpl
import com.intellij.openapi.roots.ui.configuration.projectRoot.ProjectSdksModel
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vcs.roots.VcsRootDetector
import com.intellij.openapi.vcs.{ProjectLevelVcsManager, VcsDirectoryMapping, VcsRoot}
import com.intellij.openapi.vfs.{LocalFileSystem, VfsUtilCore}
import com.intellij.pom.java.LanguageLevel
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.project.external.{JdkByHome, PythonSdk, ScalaAbstractProjectDataService, SdkReference, SdkUtils}
import com.jetbrains.python.sdk.configuration.PyProjectVirtualEnvConfiguration
import com.jetbrains.python.sdk.{PySdkExtKt, PythonSdkType}
import org.jetbrains.plugins.scala.extensions.ObjectExt
import org.jetbrains.plugins.scala.extensions

import java.io.File
import java.nio.file.Path
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class BspProjectDataService extends ScalaAbstractProjectDataService[BspProjectData, Project](BspProjectData.Key) {

  override def importData(
    toImport: util.Collection[_ <: DataNode[BspProjectData]],
    projectData: ProjectData,
    project: Project,
    modelsProvider: IdeModifiableModelsProvider
  ): Unit = {
    toImport.forEach { node =>
      val jdk = Option(node.getData.jdk)
      configureJdk(jdk)(project)
      configureVcs(node.getData.vcsRootsCandidates.asScala, project)
      configurePythonVenv(Option(node.getData.pythonVenv.asInstanceOf[PythonSdk]))(project)
    }
  }

  private def configureVcs(vcsRootsCandidates: collection.Seq[File], project: Project): Unit = {
    val vcsManager = ProjectLevelVcsManager.getInstance(project)
    val currentVcsRoots = vcsManager.getAllVcsRoots
    val currentMappings = vcsManager.getDirectoryMappings

    val detectedRoots = {
      val detector = project.getService(classOf[VcsRootDetector])
      val detected = mutable.Set[VcsRoot](currentVcsRoots.toIndexedSeq: _*)
      vcsRootsCandidates
        .iterator
        .map(LocalFileSystem.getInstance.findFileByIoFile)
        .filter(_ != null)
        .foreach { virtualFile =>
          val isUnderDetectedVcsRoot = VfsUtilCore.isUnder(virtualFile, detected.map(_.getPath).asJava)
          if (!isUnderDetectedVcsRoot) {
            val roots = detector.detect(virtualFile).asScala
            detected ++= roots
          }
        }
      detected
    }

    val newMappings = detectedRoots
      .filterNot(currentVcsRoots.contains)
      .map(root => new VcsDirectoryMapping(root.getPath.getPath, root.getVcs.getName))
    val allMappings = (currentMappings.asScala ++ newMappings).asJava

    vcsManager.setDirectoryMappings(allMappings)
  }

  private def configureJdk(jdk: Option[SdkReference])(implicit project: ProjectContext): Unit = executeProjectChangeAction {
    val existingJdk = Option(ProjectRootManager.getInstance(project).getProjectSdk)
    val projectJdk =
      jdk
        .flatMap(findOrCreateSdkFromBsp)
        .orElse(existingJdk)
        .orElse(SdkUtils.mostRecentJdk)
    projectJdk.foreach(ProjectRootManager.getInstance(project).setProjectSdk)

    setLanguageLevel(projectJdk, project)
  }

  private def setLanguageLevel(projectJdk: Option[Sdk], project: ProjectContext): Unit = {
    projectJdk.foreach { jdk =>
      Option(LanguageLevel.parse(jdk.getVersionString)).foreach {
        languageLevel =>
          LanguageLevelProjectExtensionImpl.getInstanceImpl(project).setLanguageLevel(languageLevel)
      }
    }
  }

  private def findOrCreateSdkFromBsp(sdkReference: SdkReference): Option[Sdk] = {
    def createFromHome = {
      Option(sdkReference).collect {
        case JdkByHome(home) =>
          val suffix = if (home.getName == "jre") home.getParentFile.getName else home.getName
          val name = s"BSP_$suffix"
          val newJdk = JavaSdk.getInstance.createJdk(name, home.toString)
          ProjectJdkTable.getInstance.addJdk(newJdk)
          newJdk
      }
    }

    SdkUtils.findProjectSdk(sdkReference).orElse(createFromHome)
  }

  private def configurePythonVenv(pythonSdk: Option[PythonSdk])(implicit project: ProjectContext): Unit = executeProjectChangeAction {
    val basePath = project.getBasePath
//    val projectSdks =
//      Option(PyConfigurableInterpreterList.getInstance(project)).map(_.getAllPythonSdks())
    val projectSdkModel = new ProjectSdksModel()
    projectSdkModel.reset(project)
    val projectSdks = projectSdkModel.getSdks.filter(_.getSdkType.is[PythonSdkType]).toList

    val pyProjectVirtualEnv = PyProjectVirtualEnvConfiguration.INSTANCE
    val allPythonSdks = PySdkExtKt.findAllPythonSdks(Path.of(project.getBasePath))
    pythonSdk
      .foreach {
        case PythonSdk(version, venv) =>
          if (checkIfProjectVenvAdded(projectSdks, venvPath(basePath, venv))) {
            // TODO: notify that env is added
          }
          else {
            getExistingVenv(allPythonSdks.asScala.toList, venvPath(basePath, venv))
              .map { case sdk: ProjectJdkImpl =>
                sdk.setName(s"${sdk.getVersionString} (${project.getName})")
                SdkConfigurationUtil.addSdk(sdk)
              }
              .orElse {
                val sdk = pyProjectVirtualEnv.findPreferredVirtualEnvBaseSdk(projectSdks.asJava)
                extensions.invokeLater{
                  pyProjectVirtualEnv.createVirtualEnvSynchronously(sdk, projectSdks.asJava, s"${project.getBasePath}/$venv", null, project, null, new UserDataHolderBase(), false, false)
                }
                None
              }
          }
      }
  }

  private def checkIfProjectVenvAdded(sdks: List[Sdk], venvPath: String): Boolean = {
    sdks.exists(_.getHomePath == venvPath)
  }

  private def getExistingVenv(sdks: List[Sdk], venvPath: String): Option[Sdk] = {
    sdks.find(_.getHomePath == venvPath)
  }

  private def venvPath(basePath: String, venv: String): String = {
    s"$basePath/$venv"
  }
}
