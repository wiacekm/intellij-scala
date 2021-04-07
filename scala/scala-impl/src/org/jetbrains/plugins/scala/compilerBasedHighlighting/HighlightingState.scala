package org.jetbrains.plugins.scala.compilerBasedHighlighting

import com.intellij.codeInsight.daemon.impl.HighlightInfoType
import com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.plugins.scala.externalHighlighting.ExternalHighlighting

trait HighlightingState {
  def externalHighlightings(file: VirtualFile): Set[ExternalHighlighting]

  def filesWithHighlightings: Set[VirtualFile]

  def filesWithHighlightings(types: Set[HighlightInfoType]): Set[VirtualFile]
}

object HighlightingState {
  def apply(filesToState: Map[VirtualFile, FileCompilerGeneratedState]): HighlightingState = HighlightingStateImpl(filesToState)

  private case class HighlightingStateImpl(private val filesToState: Map[VirtualFile, FileCompilerGeneratedState]) extends HighlightingState {

    override def externalHighlightings(file: VirtualFile): Set[ExternalHighlighting] =
      filesToState.get(file).map(_.highlightings).getOrElse(Set.empty)

    override def filesWithHighlightings: Set[VirtualFile] = filesToState.filter {
      case (_, state) => state.highlightings.nonEmpty
    }.keySet

    override def filesWithHighlightings(types: Set[HighlightInfoType]): Set[VirtualFile] = filesToState.filter {
      case (_, state) => state.highlightings.map(_.highlightType).exists(types.contains)
    }.keySet
  }

}