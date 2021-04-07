package org.jetbrains.plugins.scala.compilerBasedHighlighting

import org.jetbrains.plugins.scala.externalHighlighting.ExternalHighlighting
import org.jetbrains.plugins.scala.util.CompilationId

case class FileCompilerGeneratedState(compilationId: CompilationId,
                                      highlightings: Set[ExternalHighlighting]) {

  def withExtraHighlightings(highlightings: Set[ExternalHighlighting]): FileCompilerGeneratedState =
    this.copy(highlightings = this.highlightings ++ highlightings)
}
