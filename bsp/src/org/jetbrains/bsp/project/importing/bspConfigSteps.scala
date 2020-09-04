package org.jetbrains.bsp.project.importing

import java.awt.{BorderLayout, CardLayout}
import java.io.File

import ch.epfl.scala.bsp4j.BspConnectionDetails
import com.intellij.build.events.impl.{FinishBuildEventImpl, OutputBuildEventImpl, StartBuildEventImpl, SuccessResultImpl}
import com.intellij.build.{BuildView, BuildViewManager, DefaultBuildDescriptor}
import com.intellij.execution.ui.RunContentDescriptor
import com.intellij.ide.util.projectWizard.{ModuleWizardStep, WizardContext}
import com.intellij.openapi.progress.{ProgressIndicator, Task}
import com.intellij.openapi.project.ProjectManager
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.ui.TitledSeparator
import com.intellij.ui.components.JBList
import com.intellij.uiDesigner.core.{GridConstraints, GridLayoutManager, Spacer}
import com.intellij.util.ui.UI
import javax.swing._
import net.miginfocom.swing.MigLayout
import org.jetbrains.annotations.Nls
import org.jetbrains.bsp.project.importing.BspSetupConfigStep.ConfigSetupTask
import org.jetbrains.bsp.project.importing.bspConfigSteps._
import org.jetbrains.bsp.project.importing.setup._
import org.jetbrains.bsp.protocol.BspConnectionConfig
import org.jetbrains.bsp.settings.BspProjectSettings._
import org.jetbrains.bsp.{BspBundle, BspUtil}
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.build.BuildToolWindowReporter.CancelBuildAction
import org.jetbrains.plugins.scala.build.{BuildMessages, BuildToolWindowReporter, IndicatorReporter}
import org.jetbrains.plugins.scala.extensions.executeOnPooledThread
import org.jetbrains.plugins.scala.project.Version
import org.jetbrains.sbt.SbtUtil._
import org.jetbrains.sbt.project.{MillProjectImportProvider, SbtProjectImportProvider}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.Random

object bspConfigSteps {

  sealed private[importing] abstract class ConfigSetup
  case object NoSetup extends ConfigSetup
  case object SbtSetup extends ConfigSetup
  case object BloopSetup extends ConfigSetup
  case object BloopSbtSetup extends ConfigSetup
  case object MillSetup extends ConfigSetup
  case object FastpassSetup extends ConfigSetup

  private[importing] def configChoiceName(configs: ConfigSetup) = configs match {
    case NoSetup => BspBundle.message("bsp.config.steps.choice.no.setup")
    case SbtSetup => BspBundle.message("bsp.config.steps.choice.sbt")
    case BloopSetup => BspBundle.message("bsp.config.steps.choice.bloop")
    case BloopSbtSetup => BspBundle.message("bsp.config.steps.choice.sbt.with.bloop")
    case MillSetup => BspBundle.message("bsp.config.steps.choice.mill")
    case FastpassSetup => BspBundle.message("bsp.config.steps.choice.fastpass")
  }

  private[importing] def configName(config: BspConnectionDetails) =
    s"${config.getName} ${config.getVersion}"

  private[importing] def withTooltip(component: JComponent, @Nls tooltip: String) =
    UI.PanelFactory.panel(component).withTooltip(tooltip).createPanel()

  private[importing] def addTitledList(parent: JComponent, title: JComponent, list: JBList[String]): Unit = {
    val manager = new GridLayoutManager(3,1)
    manager.setSameSizeVertically(false)
    parent.setLayout(manager)

    val titleConstraints = new GridConstraints()
    titleConstraints.setRow(0)
    titleConstraints.setFill(GridConstraints.FILL_HORIZONTAL)
    parent.add(title, titleConstraints)

    val listConstraints = new GridConstraints()
    listConstraints.setRow(1)
    listConstraints.setFill(GridConstraints.FILL_BOTH)
    listConstraints.setIndent(1)
    parent.add(list, listConstraints)

    list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

    val spacer = new Spacer()
    val spacerConstraints = new GridConstraints()
    spacerConstraints.setRow(2)
    spacerConstraints.setVSizePolicy(GridConstraints.SIZEPOLICY_WANT_GROW | GridConstraints.SIZEPOLICY_CAN_GROW)
    parent.add(spacer, spacerConstraints)
  }

  private[importing] def addTitledList2(parent: JComponent, title: JComponent, list: JBList[String]): Unit = {
    val layout = new MigLayout("debug")
//    val manager = new GridLayoutManager(3,1)
//    manager.setSameSizeVertically(false)
    parent.setLayout(layout)

//    val titleConstraints = new GridConstraints()
//    titleConstraints.setRow(0)
//    titleConstraints.setFill(GridConstraints.FILL_HORIZONTAL)
    parent.add(title, "wrap, span, grow, dock north")

//    val listConstraints = new GridConstraints()
//    listConstraints.setRow(1)
//    listConstraints.setFill(GridConstraints.FILL_BOTH)
//    listConstraints.setIndent(1)
    parent.add(list, "wrap")

    list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

    val spacer = new Spacer()
//    val spacerConstraints = new GridConstraints()
//    spacerConstraints.setRow(2)
//    spacerConstraints.setVSizePolicy(GridConstraints.SIZEPOLICY_WANT_GROW | GridConstraints.SIZEPOLICY_CAN_GROW)
    parent.add(spacer, "wrap, grow")

  }

  def configSetupChoices(workspace: File): List[ConfigSetup] = {
    val workspaceConfigs = workspaceSetupChoices(workspace)
    if (workspaceConfigs.nonEmpty) workspaceConfigs
    else List(NoSetup)
  }

  def configureBuilder(builder: BspProjectImportBuilder, workspace: File, configSetup: ConfigSetup): BspConfigSetup = {
    val workspaceBspConfigs = BspConnectionConfig.workspaceBspConfigs(workspace)
    if (workspaceBspConfigs.size == 1) {
      builder.setPreImportConfig(NoPreImport)
      builder.setServerConfig(BspConfigFile(workspaceBspConfigs.head._1.toPath))
      new NoConfigSetup
    } else configSetup match {
        case bspConfigSteps.NoSetup =>
          builder.setPreImportConfig(AutoPreImport)
          builder.setServerConfig(AutoConfig)
          new NoConfigSetup
        case bspConfigSteps.BloopSetup =>
          builder.setPreImportConfig(NoPreImport)
          builder.setServerConfig(BloopConfig)
          new NoConfigSetup
        case bspConfigSteps.BloopSbtSetup =>
          builder.setPreImportConfig(BloopSbtPreImport)
          builder.setServerConfig(BloopConfig)
          new NoConfigSetup
        case bspConfigSteps.SbtSetup =>
          builder.setPreImportConfig(NoPreImport)
          // server config to be set in next step
          SbtConfigSetup(workspace)
        case bspConfigSteps.MillSetup =>
          builder.setPreImportConfig(NoPreImport)
          MillConfigSetup(workspace)
        case bspConfigSteps.FastpassSetup =>
          builder.setPreImportConfig(NoPreImport)
          val bspWorkspace = FastpassConfigSetup.computeBspWorkspace(workspace)
          builder.setExternalBspWorkspace(bspWorkspace)
          FastpassConfigSetup.create(workspace).fold(throw _, identity)
      }
  }

  def workspaceSetupChoices(workspace: File): List[ConfigSetup] = {

    val vfile = LocalFileSystem.getInstance().findFileByIoFile(workspace)

    val sbtChoice = if (SbtProjectImportProvider.canImport(vfile)) {
      val sbtVersion = Version(detectSbtVersion(workspace, getDefaultLauncher))
      if (sbtVersion.major(2) >= Version("1.4")) {
        // sbt >= 1.4 : user choose: bloop or sbt
        List(SbtSetup, BloopSbtSetup)
      } else {
        List(BloopSbtSetup)
      }
    } else Nil

    val millChoice =
      if (MillProjectImportProvider.canImport(vfile)) List(MillSetup)
      else Nil

    val bloopChoice =
      if (BspUtil.bloopConfigDir(workspace).isDefined) List(BloopSetup)
      else Nil

    val fastpassChoice = if(FastpassProjectImportProvider.canImport(vfile))
      List(FastpassSetup)
    else
      Nil


    (sbtChoice ++ millChoice ++ bloopChoice ++ fastpassChoice).distinct
  }
}

class BspSetupConfigStep(wizardContext: WizardContext, builder: BspProjectImportBuilder, setupTaskWorkspace: File)
  extends ModuleWizardStep {

  private var runSetupTask: BspConfigSetup = new NoConfigSetup

  private val workspaceBspConfigs = BspConnectionConfig.workspaceBspConfigs(setupTaskWorkspace)
  private lazy val workspaceSetupConfigs: List[ConfigSetup] = workspaceSetupChoices(setupTaskWorkspace)

  private val configSetupChoices = {
    if (workspaceBspConfigs.size == 1) List(NoSetup)
    else if (workspaceSetupConfigs.nonEmpty) workspaceSetupConfigs
    else List(NoSetup)
  }

  private val myComponent = new JPanel()
  private val chooseBspSetupModel = new DefaultListModel[String]
  private val chooseBspSetup = new JBList[String](chooseBspSetupModel)

  {
    val chooseSetupTitle = new TitledSeparator(BspBundle.message("bsp.config.steps.setup.config.choose.tool"))
    val titleWithTip = withTooltip(chooseSetupTitle, BspBundle.message("bsp.config.steps.setup.config.choose.tool.tooltip"))
    addTitledList(myComponent, titleWithTip, chooseBspSetup)
  }

  override def getComponent: JComponent = myComponent

  override def getPreferredFocusedComponent: JComponent = chooseBspSetup

  override def validate(): Boolean = {
    workspaceBspConfigs.nonEmpty ||
      configSetupChoices.size == 1 ||
      chooseBspSetup.getSelectedIndex >= 0
  }

  override def updateStep(): Unit = {
    chooseBspSetupModel.clear()
    val recommendedSuffix = BspBundle.message("bsp.config.steps.choose.config.recommended.suffix")
    val choiceStrings = configSetupChoices
      .map(configChoiceName) match {
      case h :: t => s"$h ($recommendedSuffix)" :: t
      case Nil => Nil
    }
    choiceStrings.foreach(choice => chooseBspSetupModel.addElement(choice))
    chooseBspSetup.setSelectedIndex(0)
  }

  override def updateDataModel(): Unit = {

    val configIndex =
      if (configSetupChoices.size == 1) 0
      else chooseBspSetup.getSelectedIndex

    runSetupTask = configureBuilder(builder, setupTaskWorkspace, configSetupChoices(configIndex))
  }

  override def isStepVisible: Boolean = {
    builder.preImportConfig == AutoPreImport &&
      configSetupChoices.size > 1 &&
      workspaceBspConfigs.isEmpty
  }

  override def onWizardFinished(): Unit = {
    // TODO this spawns an indicator window which is not nice.
    // show a live log in the window or something?
    updateDataModel() // without it runSetupTask is null
    builder.prepare(wizardContext)
    builder.ensureProjectIsDefined(wizardContext)
    val task = new ConfigSetupTask(runSetupTask)
    task.queue()
  }

}
object BspSetupConfigStep {

  private class ConfigSetupTask(setup: BspConfigSetup)
    extends Task.Modal(null, BspBundle.message("bsp.config.steps.setup.config.task.title"), true) {

    override def run(indicator: ProgressIndicator): Unit = {
      val reporter = new IndicatorReporter(indicator)
      setup.run(reporter)
    }

    override def onCancel(): Unit =
      setup.cancel()
  }

}

class BspChooseConfigStep(context: WizardContext, builder: BspProjectImportBuilder)
  extends ModuleWizardStep {

  private val myComponent = new JPanel(new BorderLayout)
  private val chooseBspConfig = new JBList[String]()
  private val chooseBspSetupModel = new DefaultListModel[String]
  chooseBspConfig.setModel(chooseBspSetupModel)

  private def bspConfigs = BspConnectionConfig.allBspConfigs(context.getProjectDirectory.toFile)

  {
    val chooseSetupTitle = new TitledSeparator(BspBundle.message("bsp.config.steps.choose.config.title"))
    val titleWithTip = withTooltip(chooseSetupTitle, BspBundle.message("bsp.config.steps.choose.config.title.tooltip"))
    addTitledList(myComponent, titleWithTip, chooseBspConfig)
  }

  override def getComponent: JComponent = myComponent

  override def validate(): Boolean = {
    // config already chosen in previous step
    val alreadySet = builder.serverConfig != AutoConfig

    // there should be at least one config at this point
    val configsExist = !chooseBspConfig.isEmpty
    val configSelected = (chooseBspConfig.getItemsCount == 1 || chooseBspConfig.getSelectedIndex >= 0)

    alreadySet || (configsExist && configSelected)
  }

  override def updateStep(): Unit = {
    chooseBspSetupModel.clear()
    bspConfigs
      .map { case (_,details) => configName(details) }
      .foreach(chooseBspSetupModel.addElement)
  }

  override def updateDataModel(): Unit = {
    val configIndex =
      if (chooseBspConfig.getItemsCount == 1) 0
      else chooseBspConfig.getSelectedIndex

    if (configIndex >= 0) {
      val (file,_) = bspConfigs(configIndex)
      val config = BspConfigFile(file.toPath)
      builder.setServerConfig(config)
    }
  }

  override def onWizardFinished(): Unit = {
    updateStep()
    if (builder.serverConfig == AutoConfig && chooseBspConfig.getItemsCount == 1) {
      val (file,_) = bspConfigs.head
      val config = BspConfigFile(file.toPath)
      builder.setServerConfig(config)
    }
  }

  override def isStepVisible: Boolean = {
    updateStep()
    builder.serverConfig == AutoConfig && chooseBspConfig.getItemsCount > 1
  }
}

case class DummyResult(n: Int)

class DummyProgressStep extends ModuleWizardStep {

  private val ready = Promise[Unit]()

  private val project = ProjectManager.getInstance().getDefaultProject
  private val workingDir = new File(".")

  private val progressView: BuildView = {
    val startTime = System.currentTimeMillis()
    val descriptor = new DefaultBuildDescriptor("dummyId", "dummy title", workingDir.getAbsolutePath, startTime)
    val viewManager = new BuildViewManager(project)
    new BuildView(project, descriptor, "whatKey?", viewManager)
  }

  private val cards = new CardLayout()
  private val panel = {

    val cardsPanel = new JPanel(cards)

    val chooserPanel = new JPanel()
    val chooseSetupTitle = new TitledSeparator(BspBundle.message("bsp.config.steps.setup.config.choose.tool"))
    val titleWithTip = withTooltip(chooseSetupTitle, BspBundle.message("bsp.config.steps.setup.config.choose.tool.tooltip"))
    val chooseBspSetupModel = new DefaultListModel[String]
    chooseBspSetupModel.addElement("foo")
    chooseBspSetupModel.addElement("thark")
    val chooseBspSetup = new JBList[String](chooseBspSetupModel)
    addTitledList2(chooserPanel, titleWithTip, chooseBspSetup)
    val button = new JButton("run")
    button.addActionListener(event => runProgressWindowTask())
    chooserPanel.add(button)

    val progressLayout = new MigLayout("fill, debug 300", "", "")
    val progressPanel = new JPanel(progressLayout)
    val title = new TitledSeparator(BspBundle.message("bsp.config.steps.choose.config.title"))
    progressPanel.add(title, "dock north")
    progressPanel.add(progressView, "grow, span")

    cardsPanel.add(chooserPanel, "chooser")
    cardsPanel.add(progressPanel, "progress")

    cardsPanel
  }

  override def getComponent: JComponent = panel

  override def updateDataModel(): Unit = {
//    runProgressWindowTask()
//    Await.ready(ready.future, 1.hour)
  }

  override def validate(): Boolean = true //ready.isCompleted

  override def onWizardFinished(): Unit = {
    runProgressWindowTask()
    Await.ready(ready.future, 1.hour)
  }

  private def runProgressWindowTask(): Unit = {

    cards.show(panel, "progress")

    val buildId = "myBuildId"
    val title = "myTitle"

    val reporter = new BuildToolWindowReporter(workingDir, BuildMessages.randomEventId, title, progressView, new CancelBuildAction(ready))

    reporter.start()

    def log(msg: String) = new OutputBuildEventImpl(buildId, msg.trim + System.lineSeparator(), true)

    reporter.log("it begins")

    executeOnPooledThread {
      reporter.log("smoo!")
      progressView.onEvent(Random.nextLong(), log("boo!"))
      Thread.sleep(600)
      progressView.onEvent(Random.nextLong(), log("bam!"))
      reporter.log("sam!")
      Thread.sleep(600)
      progressView.onEvent(Random.nextLong(), log("blip!"))
      reporter.log("sip!")
      Thread.sleep(4600)
      reporter.finish(BuildMessages.empty.status(BuildMessages.OK))
      ready.success(())
    }

  }
}
